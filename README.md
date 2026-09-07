# Analyzing-the-Impact-of-Study-Habits-Attendance-and-Digital-Tool-Usage-on-Academic-Performance

# STA 330 2.0: Data Analysis and Preparation of Reports

## Project Overview
This repository contains the complete statistical analysis, data preparation workflows, Minitab project workspace, and final report for **STA 330 2.0 Data Analysis and Preparation of Reports**.

* **Project Title:** Analyzing the Impact of Study Habits, Attendance, and Digital Tool Usage on Academic Performance
* **Group:** Team 12
* **Institution:** Department of Statistics, University of Sri Jayewardenepura

---

## Structure of the Analysis & Report

The project covers a end-to-end data analysis pipeline divided into the following key phases:

### 1. Data Preparation & Cleaning
* **Data Screening:** Cleaning raw survey data, handling missing entries, and checking for outliers.
* **Variable Encoding:** Categorizing and coding variables related to study habits, class attendance, digital tool usage, and GPA.

### 2. Descriptive Statistics & Exploratory Data Analysis (EDA)
* **Summary Metrics:** Measures of central tendency (mean, median) and dispersion (standard deviation) for quantitative variables.
* **Data Visualization:** Histograms, boxplots, and bar charts created to explore distributions and identify behavioral patterns among students.

### 3. Parametric & Non-Parametric Hypothesis Testing
* **Group Comparisons:** Statistical tests (e.g: Two-sample t-tests, Kruskal-Wallis) to assess differences in academic performance across demographic and study-habit groups.
* **Association Testing:** Chi-square tests of independence to evaluate relationships between categorical habits and performance levels.

### 4. Multiple Linear Regression & Diagnostics
* **Model Fitting:** Evaluating the combined impact of predictors on GPA. Significant predictors identified include class engagement ($\beta = 0.2367, p < 0.001) and fixed study schedules ($\beta = -0.226, p = 0.037$).
* **Assumption Validation:** Model diagnostics including residual analysis, Variance Inflation Factor (VIF) for multicollinearity, and normality testing of residuals.

### 5. Conclusions & Recommendations
* Practical insights and evidence-based recommendations for students and educators to improve academic outcomes.

---

## Repository Files

* `STA330_Final_Report.pdf` - Full project report detailing methodology, EDA, statistical tests, regression, and recommendations[cite: 1].
* `Data_Analysis.MPJ` - Minitab project workspace containing raw data worksheets, descriptive output, hypothesis test results, and regression plots.

---

## Minitab Analysis File (`.MPJ`) Contents

To view the complete statistical analysis:
1. Open **Minitab Statistical Software**.
2. Go to `File` > `Open Project...` and open the `.MPJ` file in this repository.
3. The file includes:
   * **Worksheet:** Cleaned dataset used for analysis.
   * **Descriptive Output:** Graphs, summary tables, and EDA charts.
   * **Test Output:** Hypothesis testing logs and ANOVA tables.
   * **Regression Session:** Regression equations, ANOVA tables, VIF metrics, and 4-in-1 residual plots.
