# duplicate detection 

# Load libraries
# install.packages("jsonlite")
library(jsonlite)
library(stringr)
library(ggplot2)
library(dplyr)
library(stringdist)
library(tidyr)

# Settings
boots = 5
test_split = 0.37
path = 'your file'

# LSH parameters
# varying b and r to get different t values
params = list(
  c(b=6, r=60),
  c(b=10, r=36),
  c(b=12, r=30),
  c(b=15, r=24),
  c(b=18, r=20),
  c(b=20, r=18),
  c(b=24, r=15),
  c(b=30, r=12),
  c(b=36, r=10),
  c(b=40, r=9),
  c(b=45, r=8),
  c(b=60, r=6),
  c(b=72, r=5),
  c(b=90, r=4),
  c(b=120, r=3),
  c(b=180, r=2)
)

# 1. Load Data
data = fromJSON(path, flatten = FALSE)

# lists to store things
ids = character()
titles = character()
features = list()

# loop through the json
for (i in 1:length(data)) {
  group = data[[i]]
  if (is.data.frame(group)) {
    for (j in 1:nrow(group)) {
      # check if modelID exists
      mid = if ("modelID" %in% names(group)) group$modelID[j] else NA
      t = if ("title" %in% names(group)) group$title[j] else NA
      
      f = list()
      if ("featuresMap" %in% names(group)) {
        frow = group$featuresMap[j, , drop = FALSE]
        f = as.list(frow)
        f = f[!sapply(f, is.na)]
      }
      
      ids = c(ids, mid)
      titles = c(titles, t)
      features[[length(features) + 1]] = f
    }
  }
}

df = data.frame(ModelID = ids, Title = titles, stringsAsFactors = FALSE)
df$FeatureMap = features

# Helper functions
getBrand = function(fm) {
  if (length(fm)==0) return(NA)
  k = grep("^brand$", names(fm), ignore.case=TRUE, value=TRUE)
  if (length(k) > 0) return(tolower(fm[[k[1]]]))
  return(NA)
}

getSize = function(fm) {
  if (length(fm)==0) return(NA)
  k = grep("size|diagonal", names(fm), ignore.case=TRUE, value=TRUE)
  if (length(k) > 0) {
    val = as.character(fm[[k[1]]])
    clean = gsub("[^0-9.]", "", val)
    num = as.numeric(clean)
    if (!is.na(num) && num > 10 && num < 100) return(round(num))
  }
  return(NA)
}

getToken = function(t) {
  if (is.na(t)) return(NA)
  # split string
  toks = unlist(strsplit(tolower(t), "[^a-z0-9]"))
  toks = toks[grepl("[0-9]", toks) & nchar(toks) > 3]
  toks = toks[!grepl("1080|720|hz|inch|watt", toks)]
  if (length(toks) > 0) return(toks[which.max(nchar(toks))])
  return(NA)
}

# Apply functions
df$Brand = sapply(df$FeatureMap, getBrand)
df$ScreenSize = sapply(df$FeatureMap, getSize)
df$ExtractedID = sapply(df$Title, getToken)

# Stop words
bad_words = c(
  "best","buy","newegg","amazon","com","thenerds","nerds","net",
  "sale","refurbished","shipping","free","price","offer","deal","save",
  "discount","stock","available","review","rating","reviews","ratings",
  "cart","add","shop","online","store","limited","warranty",
  "class","series","diag","diagonal","screen","size","type",
  "led","lcd","hdtv","tv","smart","wifi","3d","hdmi","usb",
  "audio","video","digital","analog","system","input","output",
  "black","silver","color",
  "a", "an", "the", "and", "or", "of", "in", "on", "at", "to", "for", 
  "with", "is", "it", "by", "from", "up"
)

# function to clean units
clean_units = function(txt) {
  txt = gsub("\\b(hertz|hz)\\b", "hz", txt, ignore.case=TRUE)
  txt = gsub("([0-9]+)\\s*(-)?\\s*(inches|inch|in\\.|''|\")\\b", "\\1inch", txt, ignore.case=TRUE)
  
  # metric conversion
  conv = function(x) {
    n = str_extract(x, "[0-9.]+")
    u = str_extract(x, "[a-z]+")
    v = as.numeric(n)
    if (is.na(v)) return(x)
    if (u == "cm") v = v / 2.54
    else if (u == "mm") v = v / 25.4
    else if (u == "m") v = v / 0.0254
    return(paste0(round(v, 1), "inch"))
  }
  txt = str_replace_all(txt, "[0-9.]+\\s*(cm|mm|m)\\b", conv)
  return(txt)
}

make_words = function(t, fm) {
  p = c(t)
  if ("brand" %in% names(fm)) p = c(p, fm[["brand"]])
  if (length(grep("size|diagonal", names(fm))) > 0) 
    p = c(p, unlist(fm[grep("size|diagonal", names(fm))]))
  
  txt = tolower(paste(p, collapse = " "))
  txt = clean_units(txt)
  txt = gsub("[^a-z0-9]", " ", txt)
  toks = unlist(strsplit(txt, "\\s+"))
  v = toks[nchar(toks) > 1 & !toks %in% bad_words]
  return(paste(unique(v), collapse=" "))
}

df$ModelWords = mapply(make_words, df$Title, df$FeatureMap, SIMPLIFY = TRUE, USE.NAMES = FALSE)
df$ModelTokens = strsplit(df$ModelWords, " ")

# Hashing
shingle_func = function(txt, q=3) {
  if (nchar(txt) < q) return(character(0))
  chars = strsplit(txt, "")[[1]]
  sh = character(length(chars)-q+1)
  for (i in 1:(length(chars)-q+1)) sh[i] = paste(chars[i:(i+q-1)], collapse="")
  return(unique(sh))
}

df$ModelShingles = lapply(df$ModelWords, shingle_func)
lsh_map = df$ModelShingles
names(lsh_map) = paste0("P", 1:nrow(df))
truth = trimws(tolower(df$ModelID))

# Minhash signature matrix
N = 360 
vocab = unique(unlist(lsh_map))
mapping = setNames(1:length(vocab), vocab)
set.seed(42)
hfs = matrix(replicate(N, sample(length(vocab))), nrow = N)
sigs = matrix(Inf, nrow = N, ncol = length(lsh_map))
colnames(sigs) = names(lsh_map)

pb = txtProgressBar(min=0, max=length(lsh_map), style=3)
for (j in 1:length(lsh_map)) {
  ids_ = mapping[lsh_map[[j]]]
  if(length(ids_)>0) for(i in 1:N) sigs[i, j] = min(hfs[i, ids_])
  setTxtProgressBar(pb, j)
}
close(pb)

# MSM similarity calculation
calc_sim = function(i1, i2, data) {
  # check blocking
  b1 = data$Brand[i1]; b2 = data$Brand[i2]
  if (!is.na(b1) && !is.na(b2) && b1 != b2) return(0)
  
  s1 = data$ScreenSize[i1]; s2 = data$ScreenSize[i2]
  if (!is.na(s1) && !is.na(s2) && s1 != s2) return(0)
  
  # check ID
  id1 = data$ExtractedID[i1]; id2 = data$ExtractedID[i2]
  sim_id = 0
  if (!is.na(id1) && !is.na(id2)) {
    d = stringdist(id1, id2, method="lv")
    ml = max(nchar(id1), nchar(id2))
    sim_id = 1 - (d / ml)
    if (sim_id > 0.85) sim_id = 1.0 
  }
  
  # check Content
  t1 = data$ModelTokens[[i1]]; t2 = data$ModelTokens[[i2]]
  int = length(intersect(t1, t2))
  un = length(union(t1, t2))
  sim_c = if(un > 0) int/un else 0
  
  if (!is.na(id1) && !is.na(id2)) {
    return(0.7 * sim_id + 0.3 * sim_c)
  } else {
    return(sim_c)
  }
}

# Evaluation
res = data.frame()

for (k in 1:boots) {
  
  # Split
  idx = 1:nrow(df)
  train = sample(idx, size = floor((1-test_split) * nrow(df)))
  test = setdiff(idx, train)
  
  # Ground truth
  gt = truth[test]
  tab = table(gt)
  dupes_count = sum(tab[tab > 1] * (tab[tab > 1] - 1) / 2)
  possible_pairs = (length(test)*(length(test)-1))/2
  
  name_idx = setNames(1:nrow(df), names(lsh_map))
  test_n = names(lsh_map)[test]
  
  for (p in params) {
    b = p['b']; r = p['r']
    
    # LSH bands
    cands = list()
    for (band in 1:b) {
      st = (band-1)*r+1; en = band*r
      v = apply(sigs[st:en,,drop=F], 2, paste, collapse="_")
      bk = split(names(v), v)
      for (bucket in bk) if(length(bucket)>1) cands = c(cands, combn(bucket, 2, simplify=F))
    }
    cands = unique(cands)
    
    # Filter
    pa = sapply(cands, `[`, 1); pb_ = sapply(cands, `[`, 2)
    in_t = (pa %in% test_n) & (pb_ %in% test_n)
    test_c = cands[in_t]
    
    # Calculate metrics
    frac = length(test_c) / possible_pairs
    
    p1_idx = name_idx[sapply(test_c, `[`, 1)]
    p2_idx = name_idx[sapply(test_c, `[`, 2)]
    g1 = truth[p1_idx]; g2 = truth[p2_idx]
    tp_lsh = sum(!is.na(g1) & !is.na(g2) & g1 == g2)
    
    pq = if(length(test_c)>0) tp_lsh / length(test_c) else 0
    pc = if(dupes_count>0) tp_lsh / dupes_count else 0
    f1s = if((pq+pc)>0) 2*pq*pc/(pq+pc) else 0 
    
    if (length(test_c) == 0) {
      res = rbind(res, data.frame(boot=k, b=b, r=r, frac=frac, 
                                  PQ=0, PC=0, F1_star=0, Final_F1=0))
      next
    }
    
    # MSM scores
    sc = mapply(function(x, y) calc_sim(x, y, df), p1_idx, p2_idx)
    
    # threshold loop
    best_f1 = 0
    for (mu in seq(0.2, 0.9, by=0.05)) {
      pr = sc > mu
      tp_m = sum(pr & (g1 == g2) & !is.na(g1))
      
      precision = if(sum(pr)>0) tp_m/sum(pr) else 0
      recall = if(dupes_count>0) tp_m/dupes_count else 0
      f1 = if((precision+recall)>0) 2*precision*recall/(precision+recall) else 0
      
      if(f1 > best_f1) best_f1 = f1
    }
    
    res = rbind(res, data.frame(
      boot=k, b=b, r=r, frac=frac, 
      PQ=pq, PC=pc, F1_star=f1s, Final_F1=best_f1
    ))
  }
}

# Average and Plot
avg = res %>% 
  group_by(b, r) %>% 
  summarise(
    avg_frac = mean(frac),
    avg_PQ = mean(PQ),
    avg_PC = mean(PC),
    avg_F1_star = mean(F1_star),
    avg_Final_F1 = mean(Final_F1),
    .groups = "drop"
  ) %>%
  arrange(avg_frac)

print(head(avg))

# Plot 1
plot1 = ggplot(avg, aes(x = avg_frac, y = avg_F1_star)) +
  geom_line(color = "steelblue", linewidth = 1) + 
  geom_point(color = "darkblue", size = 2) +
  labs(title = "LSH Efficiency: F1* vs Fraction of Comparisons", 
       subtitle = "How LSH pre-selection quality changes with workload",
       x = "Fraction of Comparisons (FoC)", 
       y = "F1* Measure (Harmonic mean of PQ & PC)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot1)

# Plot 2
plot2 = ggplot(avg, aes(x = avg_frac, y = avg_Final_F1)) +
  geom_line(color = "darkred", linewidth = 1) + 
  geom_point(color = "red", size = 2) +
  labs(title = "Final F1 vs Fraction of Comparisons", 
       subtitle = "Final duplicate detection performance vs. computational cost",
       x = "Fraction of Comparisons (FoC)", 
       y = "Final F1 Measure (MSM)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot2)

# Plot 3
plot3 = ggplot(avg, aes(x = avg_PC, y = avg_PQ)) +
  geom_line(color = "darkgreen", linewidth = 1) + 
  geom_point(color = "orange", size = 3) +
  geom_text(aes(label = b), vjust = -0.8, size = 3, check_overlap = TRUE) +
  labs(title = "LSH Trade-off: Pair Quality vs Pair Completeness",
       x = "Pair Completeness (Recall)", 
       y = "Pair Quality (Precision)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(plot3)
