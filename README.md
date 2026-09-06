# Iowa Housing Price Prediction

> **Quality and usable living space explain much of the price variation, but location and feature interactions materially change the estimate.**

## Project at a glance

| Scope | Result |
|---|---:|
| Training observations | **1,460** |
| Candidate predictors | **79** |
| Variables with missing values | **19** |
| Final training adjusted R² | **0.875** |
| Modeling approach | Multiple and robust regression |

## Project question

Which structural, quality, and location characteristics are most strongly associated with residential sale prices in Ames, Iowa?

This project uses the Ames Housing dataset from the Kaggle House Prices competition. The analysis prioritizes interpretability: it cleans structural missingness according to the data dictionary, explores interactions, checks regression assumptions, and examines influential observations.

## Analytical workflow

1. Distinguish true missing values from the absence of a feature.
2. Impute lot frontage using neighborhood medians and preserve structural “None” categories.
3. Group sparse categorical levels and transform the right-skewed sale-price response.
4. Build nested multiple linear regression models using quality, size, age, neighborhood, and amenity variables.
5. Add interactions and nonlinear terms, then simplify with backward AIC selection.
6. Review residual, leverage, influence, and variance diagnostics.
7. Compare ordinary least squares with Huber robust regression.

## Key findings

- **Overall quality** had the strongest unadjusted relationship with log sale price (correlation approximately **0.82**).
- Living area, basement size, garage capacity, lot size, year built, and neighborhood added meaningful information.
- The impact of quality depended partly on living area and basement size, supporting interaction terms.
- Neighborhood effects persisted after controlling for structural characteristics.
- The final reported model achieved **adjusted R² ≈ 0.875 on the training data**.
- Influence diagnostics showed that the ordinary model tended to overestimate several unusual high-end properties; robust regression reduced their effect on coefficient estimation.

The adjusted R² is an in-sample explanatory metric, not test-set accuracy. A holdout or cross-validation evaluation is planned as a portfolio extension.

## Model development

The project compared a baseline model, an expanded additive model, interaction models, polynomial terms, and an AIC-selected model. Diagnostic results motivated a log transformation of SalePrice and selected area predictors. A Huber robust model provided a sensitivity analysis for neighborhood-level variance and influential properties.

## Repository structure

```text
R/
  ames_housing_analysis.R
data/
  README.md
output/
visuals/
report/
README.md
```

## Reproduce the analysis

1. Download `train.csv` from the Kaggle competition listed in [data/README.md](data/README.md).
2. Place it inside `data/`.
3. Open R in the repository root.
4. Install the packages listed at the beginning of the script.
5. Run:

```r
source("R/ames_housing_analysis.R")
```

## Tools

**R · tidyverse · MASS · car · caret · regression diagnostics · feature engineering**

## Project context

Collaborative data science project completed at Villanova University.\n\n**My contribution:** I contributed across the full analytical workflow, including data cleaning, missing-value treatment, categorical feature engineering, exploratory analysis, regression model development, diagnostics, visualization, and interpretation. The team worked collaboratively rather than dividing the project into separate individual sections. I later reorganized the code and documentation for reproducible portfolio presentation.
