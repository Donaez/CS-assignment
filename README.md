This R code implements a pipeline for detecting duplicate products (specifically TVs) in e-commerce data. It addresses the computational challenge of O(N^2)
 comparisons by using Locality Sensitive Hashing (LSH) for candidate generation and a Multi-Component Similarity Method (MSM) for final verification.

Key features: -Data Cleaning & Feature Extraction: Automatically extracts Brand, Screen Size, and Model IDs from messy JSON titles and feature maps. Includes unit normalization (e.g., converting cm to inches).

-MinHash & LSH: Implements MinHash signatures and LSH to drastically reduce the search space, avoiding a full pair-wise comparison.

-Multi-Component Similarity Method (MSM): A robust similarity function that combines:

-Blocking: Hard filters on Brand and Screen Size.

-ID Matching: Levenshtein distance on extracted model numbers.

-Model Words Matching: Jaccard similarity on model words.

-Bootstrapping Evaluation: Performs 5-fold bootstrapping with a 63/37 train-test split to ensure robust performance metrics.

Prerequisites: 

-install.packages(c("jsonlite", "stringr", "ggplot2", "dplyr", "stringdist", "tidyr"))

Settings: -Length of signature: N=360 -path: update this to your json-file path -params: a list of vectors containing the (b/r)-settings

Methodology: 
-Preprocessing: Data is loaded and flattened. Titles are tokenized, "stop words" (e.g., "sale", "free") are removed, and units are standardized. 

-Shingling & Hashing: Model words are converted to character q-grams (shingles) and hashed into a signature matrix using MinHash. 

-Candidate Generation: LSH buckets similar signatures. Only pairs falling into the same bucket are considered "Candidate Pairs." 

-Similarity Calculation: Candidate pairs undergo the MSM check. A duplicate is declared if the similarity score exceeds a learned threshold mu.
