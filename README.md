# 🛡️ ForSure Analytics — Insurance Claims Modeling

An end-to-end actuarial data science project for French Motor Third-Party Liability (MTPL) insurance, combining classical statistical models (GLM, Weibull, Lognormal) with a deep learning Artificial Neural Network (ANN), wrapped in an interactive **R Shiny** dashboard.

---

## 📁 Project Structure

```
├── app.R                        # Shiny dashboard (UI + Server)
├── ANN_model.R                  # ANN model training script (Keras/TensorFlow)
├── prj.Rmd                      # Main analysis notebook
├── prj_version_annotée.Rmd      # Annotated version of the analysis
└── README.md
```

---

## 📊 Data

The project uses the **freMTPL** datasets from the [`CASdatasets`](http://cas.uqam.ca/) R package:

- `freMTPLfreq` — Policy-level exposure and claim frequency data
- `freMTPLsev` — Claim severity (amount) data

Both datasets are merged on `PolicyID` after aggregating claim amounts per policy.

---

## 🧠 Models

| Target | Model | Type |
|--------|-------|------|
| Claim Severity | Weibull GLM | Parametric |
| Claim Severity | Lognormal GLM | Parametric |
| Claim Severity | Stepwise GLM | Variable Selection |
| Claim Frequency | Poisson GLM | Count |
| Claim Frequency | Negative Binomial GLM | Overdispersed Count |
| Claim Frequency | Stepwise GLM | Variable Selection |
| Claim Frequency | ANN (Keras) | Deep Learning |

Performance is evaluated using the **Gini Index** (discrimination power).

---

## 🖥️ Shiny Dashboard

The `app.R` file provides an interactive dashboard with:

- **Dashboard** — Key KPIs and metrics overview
- **Data Exploration**
  - Summary statistics
  - Univariate analysis
  - Bivariate analysis with correlation matrix and maps
- **Predictive Models**
  - GLM model comparison (Weibull, Lognormal, Poisson, NB, Stepwise)
  - ANN training history, architecture, and Gini score

---

## ⚙️ Requirements

### R Packages

```r
install.packages(c(
  "shiny", "shinydashboard", "shinythemes", "bslib",
  "plotly", "DT", "ggplot2", "shinyWidgets", "corrplot",
  "shinycssloaders", "dplyr", "caret", "recipes"
))

# Deep learning
install.packages("keras")
library(keras)
install_keras()   # installs TensorFlow backend

# Actuarial data
install.packages("CASdatasets", repos = "http://cas.uqam.ca/pub/R/")
```

### Python (for Keras/TensorFlow)
- Python 3.8–3.11
- TensorFlow ≥ 2.x (installed automatically via `install_keras()`)

---

## 🚀 Running the Project

### 1. Reproduce the Analysis

Open and knit either notebook in RStudio:

```r
rmarkdown::render("prj.Rmd")
# or
rmarkdown::render("prj_version_annotée.Rmd")
```

### 2. Train the ANN Model

```r
source("ANN_model.R")
```

> This will generate `ann_model.h5` and the required `.rds` artefacts.

### 3. Launch the Shiny App

```r
shiny::runApp("app.R")
```

> ⚠️ Update the hardcoded paths in `app.R` (lines 161–192) to match your local directory structure before running.

---

## 🔧 Configuration

The `app.R` file currently loads data from absolute Windows paths (e.g. `C:/at/app/`). Before running, replace these with relative paths:

```r
# Replace this:
dataset <- readRDS("C:/at/app/data/Dataset.rds")

# With this:
dataset <- readRDS("data/Dataset.rds")
```

---

## 👤 Author

**Sahar Chiha**  
Actuarial Data Science Project  

---

## 📄 License

This project is for academic/educational purposes.
