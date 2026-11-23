## 1. Overall processing flow

```text
Read add.csv ──► Build duplicate-row group IDs ──► Keep Height, Width, Ratio, Target
      │
      ├──► (EDA only) na.omit ──► Descriptive statistics and plots        [Part 2]
      │
      └──► Group-wise stratified train/test split ──► Impute + log transform (train information only)
                 ──► Logistic regression (stepAIC) ──► Evaluation and assumption checks   [Part 3]
                 ──► Random Forest, comparison, 30 repeated splits                        [Part 4]
```

## 2. Part 1 – Data preprocessing

| Code | What it does | Why |
|---|---|---|
| `read.csv(..., na.strings = c("?", " ?", ...), strip.white = TRUE)` | Reads the file and treats `?` and empty cells as missing (`NA`) | The raw file marks missing values with `?` |
| `add_goc[, -1]` | Drops the row-index column | It is not a variable |
| `sum(duplicated(add_goc))` | Counts duplicated rows over all raw columns | Many images share exactly the same features |
| `chuoi_goc` → `nhom_goc` | Concatenates the values of all 1558 features into one string, then assigns a **group ID** (rows with the same string get the same ID) | Used so that duplicated rows are not split across the two sets |
| `add_full <- data.frame(Height, Width, Ratio, Target, Nhom_goc)` | Keeps only the 3 variables, the label and the group ID; **keeps all 3,279 rows** | The assignment uses only 3 variables; no row is removed before splitting |
| `ifelse(Target == "ad.", 1, 0)` | Encodes the label: ad = 1, nonad = 0 | Needed for logistic regression |
| `add <- na.omit(add_full[, ...])` | Builds a version with only rows that have all 3 variables | **Used for EDA only** and for `add_clean.csv` |

## 3. Part 2 – Descriptive statistics (EDA)

Everything runs on `add` (the `na.omit` version):

- `thong_ke()` computes sample size, mean, sd, min, Q1, median, Q3 and max, applied separately to the ad and nonad groups.
- Frequency table and percentages of `Target`.
- Plots: histograms, barplot, boxplots by group, scatter plots with fitted logistic curves, and the correlation matrix (`corrplot`).

## 4. Part 3 – Logistic regression

### 4.1 Train / test split (70% / 30%)

```r
chia_nhom_phan_tang <- function(df, p = 0.7) { ... }
```

- The split is made on **duplicate-row groups**, not on individual rows, so the same record cannot appear in both sets.
- Each group gets a **stratum** made of its label (`ad` if the share of ad rows in the group is at least 0.5, otherwise `nonad`) and its size (`lon` = large if it has 3 or more rows, `nho` = small otherwise). `createDataPartition()` takes 70% of the groups in each stratum into train.
- The code checks `so_trung = sum(test$Nhom_goc %in% train$Nhom_goc)`; the result must be 0.
- The test set is **not filtered for outliers**.

### 4.2 Imputation and transformation (`tien_xu_ly_va_dac_trung`)

- The medians of `Height`, `Width` and `Ratio` come only from **train** and are used to fill missing values in both train and test (test contributes no statistic).
- It creates centered log variables (the mean is taken from train): `lh`, `lw`, `lr`, plus `lh2 = lh²`, `lr2 = lr²` and `Area = Height × Width`.
- The same function is used for the main split and for the 30 repeated splits, so the procedure is identical each time.

### 4.3 Models

| Model | Formula | Role |
|---|---|---|
| `mo_hinh_goc` | `Target ~ Height + Width + Ratio` | Original linear model required by the assignment, used as a baseline |
| `mo_hinh` | `Target ~ lh + lw + lh2 + lr2` | Model after log transformation, to handle non-linearity |
| `mo_hinh_tot` | `stepAIC(mo_hinh)` | Final model, selected by AIC (smaller is better) |

The code prints the AIC of all three models for comparison (they use the same training data, so the comparison is valid).

### 4.4 Evaluation on the test set

- `danh_gia()` computes accuracy, precision, recall, F1 and AUC (threshold 0.5).
- It prints the confusion matrix and the accuracy of an "always predict nonad" classifier. Since `ad` is the minority class, accuracy alone can be misleading.
- It draws the ROC curve and prints the AUC.

### 4.5 Assumption checks

| Assumption | How the code checks it | How to read it |
|---|---|---|
| Multicollinearity | `vif(mo_hinh_tot)` | VIF > 5 (or 10) is a concern |
| Influential points and outliers | `cooks.distance`, `rstandard` | Cook > 4/n, \|residual\| > 3 |
| Linearity between logit and variables | Box–Tidwell on the 3 original variables | p < 0.05 means non-linear; this is the reason for the log transformation |

## 5. Part 4 – Random Forest and comparison

- The Random Forest uses exactly the same train / test sets, with formula `Target_f ~ Height + Width + Ratio + Area` (`ntree = 500`, `mtry = 2`, `nodesize = 5`), and predicts the probability of class `ad`.
- It prints the confusion matrix and variable importance (`importance`, `varImpPlot`), draws both ROC curves on one plot and builds a comparison table.
- **Stability:** the whole procedure (group split, imputation, transformation, training, evaluation) is repeated 30 times; accuracy and AUC are recorded each time, then the mean and standard deviation are computed (a smaller standard deviation means more stable).