# r-dataset-computers-ml
This repository contains a reproducible, single-file R script analysis using the "Computers" dataset. The focus is on a complete data-mining workflow in R: data loading and preprocessing, exploratory data analysis, clustering (k-means) and classification models (KNN, Naive Bayes, Decision Tree).

## How to run

This project keeps the full analysis in a single script file `main.R`.

### Requirements
- R 4.0+
- Required R packages used in the analysis:
  - Ecdat, caret, RWeka, rJava, cluster, class, e1071, fpc, clusterSim, and ggplot2

Install packages (example):
```r
install.packages(c("Ecdat","caret","cluster","class","e1071","fpc","clusterSim","ggplot2"))
# RWeka and rJava may require system Java (JDK) installed. See RWeka documentation if you run into installation issues.
```

### Run
- Interactive (RStudio): Open `main.R` and run the script.
- Command line:
```bash
Rscript main.R
```

Notes:
- The script loads the dataset with `data(Computers)` from the Ecdat package, so you do not need to upload a dataset file.
- Some plotting functions in the script (e.g. plotcluster, clusplot) will attempt to open graphical devices; when running non-interactively you may want to wrap plotting code to save figures to files.

## Files
- `main.R` — single-file R analysis (moved/renamed from your original submission).
- `archive/AzraSevvalKupeli_VeriMadenciligi_Odev.R` — archived original script copy.
- `LICENSE` — MIT license.
- `.gitignore` — R ignores.
