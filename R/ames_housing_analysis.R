# =========================================================
# HOUSING PREDICTION PROJECT
# Data Cleaning & Preparation
# =========================================================


# Make sure the 
# Run once if needed:n
# install.packages(c("tidyverse", "car", "MASS", "leaps", "corrplot", "caret"))

library(tidyverse)
library(car)        # VIF
library(MASS)       # stepAIC
library(leaps)      # best subset
library(corrplot)   # correlation plot
library(caret)      # dummy variables / preprocessing\n\ndir.create("output", showWarnings = FALSE)

# IMPORT DATA
# =========================================================

dat <- read.csv(file.path("data", "train.csv"), stringsAsFactors = FALSE)

# Basic structure check
dim(dat)
str(dat)
summary(dat)

# PART 1: HANDLING MISSING VALUES
# =========================================================

missing_count <- sapply(dat, function(x) sum(is.na(x)))
missing_pct <- round(missing_count / nrow(dat) * 100, 2)

missing_summary <- data.frame(
  Variable = names(missing_count),
  Missing_Count = as.numeric(missing_count),
  Missing_Percent = as.numeric(missing_pct)
) %>%
  arrange(desc(Missing_Count))

missing_summary

# Create cleaned dataset
dat_clean <- dat

# GROUP 1: VARIABLES WITH VERY HIGH MISSINGNESS
# ---------------------------------------------------------

## PoolQC: keep original quality levels; NA indicates no pool
dat_clean$PoolQC[is.na(dat_clean$PoolQC)] <- "None"

## MiscFeature: drop due to extremely high missingness and limited usefulness
dat_clean <- dat_clean %>%
  dplyr::select(-MiscFeature)

## Alley: keep original alley types; NA indicates no alley access
dat_clean$Alley[is.na(dat_clean$Alley)] <- "None"

## Fence: keep original fence types; NA indicates no fence
dat_clean$Fence[is.na(dat_clean$Fence)] <- "None"

# GROUP 2: STRUCTURAL MISSING IN CATEGORICAL VARIABLES
# ---------------------------------------------------------
# For these variables, NA indicates the absence of the feature, so missing values are recoded as "None".

cols_none <- c(
  "FireplaceQu",
  "GarageType", "GarageFinish", "GarageQual", "GarageCond",
  "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "BsmtQual", "BsmtCond",
  "MasVnrType"
)

dat_clean[cols_none] <- lapply(dat_clean[cols_none], function(x) {
  x[is.na(x)] <- "None"
  x
})

# GROUP 3: TRUE NUMERIC MISSING VALUES
# ---------------------------------------------------------
# LotFrontage is imputed using neighborhood-level medians.

dat_clean <- dat_clean %>%
  group_by(Neighborhood) %>%
  mutate(
    LotFrontage = ifelse(
      is.na(LotFrontage),
      median(LotFrontage, na.rm = TRUE),
      LotFrontage
    )
  ) %>%
  ungroup()

# GROUP 4: NUMERIC MISSING DUE TO ABSENCE OF FEATURE
# ---------------------------------------------------------

dat_clean$MasVnrArea[is.na(dat_clean$MasVnrArea)] <- 0
dat_clean$GarageYrBlt[is.na(dat_clean$GarageYrBlt)] <- 0

# GROUP 5: SINGLE ORDINARY MISSING VALUE
# ---------------------------------------------------------

mod_val <- names(which.max(table(dat_clean$Electrical)))
dat_clean$Electrical[is.na(dat_clean$Electrical)] <- mod_val

# Final validation: check remaining missing values
missing_count <- sapply(dat_clean, function(x) sum(is.na(x)))
missing_count

# PART 2: CLEAN UP CATEGORICAL VARIABLES
# =========================================================

# Identify categorical variables
char_vars <- names(dat_clean)[sapply(dat_clean, is.character)]
char_vars

# Summarize number of levels
level_summary <- data.frame(
  Variable = char_vars,
  Num_Levels = sapply(dat_clean[char_vars], function(x) length(unique(x))),
  Has_Other = sapply(dat_clean[char_vars], function(x) any(x == "Other")),
  stringsAsFactors = FALSE
)

level_summary <- level_summary[order(-level_summary$Num_Levels), ]
level_summary

# Frequency table for each categorical variable
freq_list <- lapply(dat_clean[char_vars], function(x) {
  data.frame(
    Level = names(table(x)),
    Count = as.vector(table(x)),
    Proportion = as.vector(prop.table(table(x)))
  )
})

freq_list

# General rule:
# First, consider categorical variables with more than 5 levels,
# then determine whether the levels are meaningful enough to keep
# or sparse enough to group.

candidate_vars <- level_summary$Variable[level_summary$Num_Levels > 5]
candidate_vars # 19 candidate variables

# GROUP 1: Keep as-is
# ---------------------------------------------------------
vars_keep_as_is <- c(
  "Neighborhood",
  "HouseStyle",
  "RoofStyle",
  "RoofMatl",
  "Foundation",
  "Heating",
  "Functional",
  "SaleType",
  "SaleCondition"
)

# GROUP 2: Keep structural "None" as a separate category
# ---------------------------------------------------------
vars_keep_none_separate <- c(
  "FireplaceQu",
  "GarageType",
  "GarageFinish",
  "GarageQual",
  "GarageCond",
  "BsmtExposure",
  "BsmtFinType1",
  "BsmtFinType2",
  "BsmtQual",
  "BsmtCond",
  "MasVnrType"
)

# GROUP 3: Group rare levels into "Other"
# ---------------------------------------------------------
vars_group_rare <- c(
  "Exterior1st",
  "Exterior2nd",
  "Condition1",
  "Condition2"
)

# Check that all candidate variables are accounted for
setdiff(candidate_vars,
        c(vars_keep_as_is, vars_keep_none_separate, vars_group_rare))

# Function to group rare levels into "Other"
# Rare categories are grouped when their frequency is below 2%.
# A 1% threshold was initially tested but retained too many sparse levels.

group_rare_levels <- function(x, threshold = 0.02) {
  x <- as.character(x)
  freq <- prop.table(table(x))
  rare_levels <- names(freq[freq < threshold])
  x[x %in% rare_levels] <- "Other"
  factor(x)
}

# Apply grouping only to selected variables
dat_clean[vars_group_rare] <- lapply(dat_clean[vars_group_rare], group_rare_levels)

# Check proportions after grouping
for (v in vars_group_rare) {
  cat("\n====================\n")
  cat("Variable:", v, "\n")
  print(prop.table(table(dat_clean[[v]])))
}

# Convert all remaining character variables to factors
char_vars <- names(dat_clean)[sapply(dat_clean, is.character)]
dat_clean[char_vars] <- lapply(dat_clean[char_vars], as.factor)

# Check structure
str(dat_clean)

# PART 3: LOG TRANSFORMATION & EDA
# =========================================================

# Create log-transformed response
dat_clean$logSalePrice <- log(dat_clean$SalePrice)

# Compare original and transformed response
summary(dat_clean$SalePrice)
summary(dat_clean$logSalePrice)

# Histograms
hist(dat_clean$SalePrice,
     main = "Histogram of SalePrice",
     xlab = "SalePrice")

hist(dat_clean$logSalePrice,
     main = "Histogram of log(SalePrice)",
     xlab = "log(SalePrice)")

# Numerical EDA
num_vars <- names(dat_clean)[sapply(dat_clean, is.numeric)]
num_vars

cor_with_target <- sapply(dat_clean[num_vars], function(x) {
  cor(x, dat_clean$logSalePrice, use = "complete.obs")
})

cor_with_target <- sort(cor_with_target, decreasing = TRUE)
cor_with_target <- cor_with_target[!names(cor_with_target) %in% c("SalePrice", "logSalePrice")]

head(cor_with_target, 10)

# Correlation matrix for top numeric variables
top_num_vars <- names(head(cor_with_target, 10))
eda_vars <- c("logSalePrice", top_num_vars)

cor_matrix_top <- cor(dat_clean[eda_vars], use = "complete.obs")
round(cor_matrix_top, 2)

# Scatterplots for key numeric predictors
par(mfrow = c(2, 2))

plot(dat_clean$GrLivArea, dat_clean$logSalePrice,
     main = "log(SalePrice) vs GrLivArea",
     xlab = "GrLivArea", ylab = "log(SalePrice)")

plot(dat_clean$OverallQual, dat_clean$logSalePrice,
     main = "log(SalePrice) vs OverallQual",
     xlab = "OverallQual", ylab = "log(SalePrice)")

plot(dat_clean$TotalBsmtSF, dat_clean$logSalePrice,
     main = "log(SalePrice) vs TotalBsmtSF",
     xlab = "TotalBsmtSF", ylab = "log(SalePrice)")

plot(dat_clean$GarageCars, dat_clean$logSalePrice,
     main = "log(SalePrice) vs GarageCars",
     xlab = "GarageCars", ylab = "log(SalePrice)")


par(mfrow = c(1, 1))

# Categorical EDA
boxplot(logSalePrice ~ Neighborhood, data = dat_clean,
        main = "log(SalePrice) by Neighborhood",
        xlab = "Neighborhood", ylab = "log(SalePrice)",
        las = 2, cex.axis = 0.7)

boxplot(logSalePrice ~ Exterior1st, data = dat_clean,
        main = "log(SalePrice) by Exterior1st",
        xlab = "Exterior1st", ylab = "log(SalePrice)",
        las = 2, cex.axis = 0.8)

boxplot(logSalePrice ~ FireplaceQu, data = dat_clean,
        main = "log(SalePrice) by Fireplace Quality",
        xlab = "FireplaceQu", ylab = "log(SalePrice)",
        las = 2, cex.axis = 0.8)

# Final factor check
factor_vars <- names(dat_clean)[sapply(dat_clean, is.factor)]
factor_vars
lapply(dat_clean[factor_vars], levels)

# Export cleaned dataset for the next modeling step
write.csv(dat_clean, file.path("output", "ames_housing_train_cleaned.csv"), row.names = FALSE)
getwd()


# =========================================================
# Start of Will's Code
# =========================================================

# =========================================================
# More Exploring
# =========================================================

# Correlation Matrix of Numeric Variables
num_data <- dat_clean[sapply(dat_clean, is.numeric)] 
cor_matrix <- cor(num_data, use = "complete.obs")
print(cor_matrix)

# Identify absolute correlation coefficients greater than 0.7 as large
high_corr <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)

high_corr_pairs <- data.frame(
  var1 = rownames(cor_matrix)[high_corr[,1]],
  var2 = colnames(cor_matrix)[high_corr[,2]],
  correlation = cor_matrix[high_corr])

high_corr_pairs <- high_corr_pairs[high_corr_pairs$var1 < high_corr_pairs$var2, ] # remove duplicate pairs
high_corr_pairs <- high_corr_pairs[order(-abs(high_corr_pairs$correlation)), ] # sort for largest absolute correlation
high_corr_pairs


cor_with_target <- cor(num_data, use = "complete.obs")[, "logSalePrice"]

cor_table <- data.frame(
  variable = names(cor_with_target),
  correlation = cor_with_target)

# Filter out logSalePrice and SalePrice
cor_table <- cor_table[cor_table$variable != "logSalePrice", ]
cor_table <- cor_table[cor_table$variable != "SalePrice", ]

# Filter moderately strong to very strong correlations
high_corr_target <- cor_table[abs(cor_table$correlation) > 0.55, ]

# Sort by strength
high_corr_target <- high_corr_target[order(-abs(high_corr_target$correlation)), ]
high_corr_target


# =========================================================
# Model Building
# =========================================================

# Our top five correlations with logSalePrice are OverallQual, GRLiveArea, GarageCars,
# GarageArea, and TotalBsmtSF. GarageCars and GarageArea are highly correlated, so only
# GarageCars will be added to the base model to prevent multicollinearity.

# Create a baseline model with the top four correlations between prospective predictors and logSalePrice
model1 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF, data = dat_clean)
summary(model1) # decent baseline with 0.7942 R-squared
anova(model1)

# Expand model to include next highest predictors, skipping over prospective predictors with high correlations of predictors already in the model
model2 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + Neighborhood + FullBath + KitchenQual + LotArea, data = dat_clean)
summary(model2)
anova(model2) 

# Add interaction terms
model3 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea + OverallQual:GarageCars + OverallQual:TotalBsmtSF + GrLivArea:GarageCars + GrLivArea:TotalBsmtSF, data = dat_clean)
summary(model3)
anova(model3)

# Add polynomial terms
model4 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea + OverallQual:GarageCars + OverallQual:TotalBsmtSF + GrLivArea:GarageCars + GrLivArea:TotalBsmtSF + I(GrLivArea^2) + I(LotArea^2) + I(YearBuilt^2) + I(GarageCars^2), data = dat_clean)
summary(model4)
anova(model4)

# Perform backward elimination with AIC
model_final <- step(model4, direction = "backward")
summary(model_final)

# plot(model_final)
# GrLivArea:GarageCars, OverallQual:GarageCars, I(YearBuilt^2), I(GrLivArea^2) were all removed before reaching the optimal AIC.


# Kyle: VIF need to be check before adding interactions and second order terms
# vif(model_final) # there are a number of high GVIF values, consider reducing the number of predictors further

# model fit criteria check
model_metrics <- data.frame(
  Model = c("model1", "model2", "model3", "model4", "model_final"),
  Adj_R2 = c(
    summary(model1)$adj.r.squared,
    summary(model2)$adj.r.squared,
    summary(model3)$adj.r.squared,
    summary(model4)$adj.r.squared,
    summary(model_final)$adj.r.squared),
  AIC = c(AIC(model1), AIC(model2), AIC(model3), AIC(model4), AIC(model_final)),
  BIC = c(BIC(model1), BIC(model2), BIC(model3), BIC(model4), BIC(model_final)))

model_metrics

# Through AIC, BIC, and Adj R-squared, model_final is selected as the best model.

anova(model1, model2, model3, model4, model_final)

# Model 1 to Model 2: F = 23.64, p < 2.2e-16, Adding YearBuilt, Neighborhood, FullBath, KitchenQual, and LotArea resulted in a massive improvement.
# Model 2 to Model 3: F = 58.50, p < 2.2e-16, Adding interaction terms produced an extremely large improvement
# Model 3 to Model 4: F = 4.66, p = 0.00098, Polynomials improve the model
# Model 4 to Final Model: F = 0.51, p = 0.726, The removal of four terms did not significantly worsen the model, suggesting that they were not useful to start with.

coef(model_final) # check model signs




# =========================================================
# Start of Kyle's Code
# =========================================================

# =========================================================
# Model Diagnostics Phase 1
# =========================================================

library(readr)
dat_clean <- read_csv(file.path("output", "ames_housing_train_cleaned.csv"), show_col_types = FALSE)
attach(dat_clean)

summary(model_final)
# model_final <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt + Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea + OverallQual:GarageCars + OverallQual:TotalBsmtSF + GrLivArea:GarageCars + GrLivArea:TotalBsmtSF + I(GrLivArea^2) + I(LotArea^2) + I(YearBuilt^2) + I(GarageCars^2))
plot(model_final)
# Kyle: model without log transform violates normality assumption, equal variance

## Testing for normal assumption:
library(nortest)
sf.test(model_final$resid) # p-value of 2.2*10^-16 -> Fail the normality assumption

## Testing linearity assumption for each predictor
res <- rstudent(model_final)

X <- model.matrix(model_final)[, -1]

par(mfrow = c(3, 3))

for (i in 1:ncol(X)) {
  plot(X[, i], res,
       xlab = colnames(X)[i],
       ylab = "Studentized Residuals",
       pch = 16,
       main = paste("Residuals vs", colnames(X)[i]))
  
  lines(lowess(X[, i], res), col = "red", lwd = 2)
}

## Check variance homogeinity between neighborhood groups:
library(ggplot2)

dat_clean$res <- rstudent(model_final)
ggplot(dat_clean, aes(x = res, y = reorder(Neighborhood, res, FUN = median))) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Residuals by Neighborhood",
       x = "Studentized Deleted Residuals",
       y = "Neighborhood") +
  theme_minimal()

library(MASS)
bcox <- boxcox(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt 
               + Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea 
               + OverallQual:TotalBsmtSF + GrLivArea:TotalBsmtSF + I(LotArea^2) + I(GarageCars^2),
               lambda = seq(-2, 2, length = 20))

lamb <- bcox$x[bcox$y == max(bcox$y)]
lamb # 0.22, suggesting we use log transform on y

# =========================================================
# Model Diagnostics Phase 2: log transform on y
# =========================================================

#model_final_2 <- lm(log(SalePrice) ~ OverallQual + log(GrLivArea + 1) + GarageCars + log(TotalBsmtSF+1) + YearBuilt 
#                    + Neighborhood + FullBath + KitchenQual + log(LotArea +1) + OverallQual:log(GrLivArea +1) 
#                    + OverallQual:log(TotalBsmtSF+1)+ I(GarageCars^2))

# log transform saleprice, totalBSmtSF and its interaction (to fight outliers)

model_final_2 <- lm(log10(SalePrice) ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt 
                    + Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea +
                    + OverallQual:TotalBsmtSF+ GrLivArea:TotalBsmtSF+ I(LotArea^2) + I(GarageCars^2))
summary(model_final_2)


## Checking normality and equal variance assumption
plot(model_final_2)

## Testing linearity assumption for each predictor
res2 <- rstudent(model_final_2)

X2 <- model.matrix(model_final_2)[, -1]

par(mfrow = c(3, 3))

for (i in 1:ncol(X2)) {
  plot(X2[, i], res2,
       xlab = colnames(X2)[i],
       ylab = "Studentized Residuals",
       pch = 16,
       main = paste("Residuals vs", colnames(X2)[i]))
  
  lines(lowess(X2[, i], res), col = "red", lwd = 2)
}

## Testing equal variance among groups
dat_clean$res2 <- rstudent(model_final_2)
ggplot(dat_clean, aes(x = res2, y = reorder(Neighborhood, res2, FUN = median))) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Residuals by Neighborhood",
       x = "Studentized Deleted Residuals",
       y = "Neighborhood") +
  theme_minimal()

# =========================================================
# Model Diagnostics Phase 3: fixing TotalBsmtSF
# =========================================================

model_final_3 <- lm(log10(SalePrice) ~ OverallQual + GrLivArea + GarageCars + log10(TotalBsmtSF+1) + YearBuilt 
                    + Neighborhood + FullBath + KitchenQual + log10(1+LotArea) + OverallQual:GrLivArea +
                      + OverallQual:log10(TotalBsmtSF+1)+ GrLivArea:log10(TotalBsmtSF+1)+ I(GarageCars^2))
summary(model_final_3)
plot(model_final_3)

## Testing linearity assumption for each predictor
res3 <- rstudent(model_final_3)

X3 <- model.matrix(model_final_3)[, -1]

par(mfrow = c(3, 3))

for (i in 1:ncol(X3)) {
  plot(X3[, i], res3,
       xlab = colnames(X3)[i],
       ylab = "Studentized Residuals",
       pch = 16,
       main = paste("Residuals vs", colnames(X3)[i]))
  
  lines(lowess(X3[, i], res), col = "red", lwd = 2)
}

## Equal Variance between group
dat_clean$res3 <- rstudent(model_final_3)
ggplot(dat_clean, aes(x = res3, y = reorder(Neighborhood, res3, FUN = median))) +
  geom_boxplot(fill = "steelblue", outlier.color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(title = "Residuals by Neighborhood",
       x = "Studentized Deleted Residuals",
       y = "Neighborhood") +
  theme_minimal()

# =========================================================
# Outliers 
# =========================================================





# =========================================================
# Will Addition 4/26
# =========================================================

# =========================================================
# Testing models with new data
# =========================================================

test <- read.csv(file.path("data", "test.csv"), stringsAsFactors = FALSE)

# PART 1: HANDLING MISSING VALUES
# =========================================================

missing_count <- sapply(test, function(x) sum(is.na(x)))
missing_pct <- round(missing_count / nrow(test) * 100, 2)

missing_summary <- data.frame(
  Variable = names(missing_count),
  Missing_Count = as.numeric(missing_count),
  Missing_Percent = as.numeric(missing_pct)
) %>%
  arrange(desc(Missing_Count))

missing_summary

# Create cleaned dataset
test_clean <- test

# GROUP 1: VARIABLES WITH VERY HIGH MISSINGNESS
# ---------------------------------------------------------

## PoolQC: keep original quality levels; NA indicates no pool
test_clean$PoolQC[is.na(test_clean$PoolQC)] <- "None"

## MiscFeature: drop due to extremely high missingness and limited usefulness
test_clean <- test_clean %>%
  dplyr::select(-MiscFeature)

## Alley: keep original alley types; NA indicates no alley access
test_clean$Alley[is.na(test_clean$Alley)] <- "None"

## Fence: keep original fence types; NA indicates no fence
test_clean$Fence[is.na(test_clean$Fence)] <- "None"

# GROUP 2: STRUCTURAL MISSING IN CATEGORICAL VARIABLES
# ---------------------------------------------------------
# For these variables, NA indicates the absence of the feature, so missing values are recoded as "None".

cols_none <- c(
  "FireplaceQu",
  "GarageType", "GarageFinish", "GarageQual", "GarageCond",
  "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "BsmtQual", "BsmtCond",
  "MasVnrType"
)

test_clean[cols_none] <- lapply(test_clean[cols_none], function(x) {
  x[is.na(x)] <- "None"
  x
})

# GROUP 3: TRUE NUMERIC MISSING VALUES
# ---------------------------------------------------------
# LotFrontage is imputed using neighborhood-level medians.

test_clean <- test_clean %>%
  group_by(Neighborhood) %>%
  mutate(
    LotFrontage = ifelse(
      is.na(LotFrontage),
      median(LotFrontage, na.rm = TRUE),
      LotFrontage
    )
  ) %>%
  ungroup()

# GROUP 4: NUMERIC MISSING DUE TO ABSENCE OF FEATURE
# ---------------------------------------------------------

test_clean$MasVnrArea[is.na(test_clean$MasVnrArea)] <- 0
test_clean$GarageYrBlt[is.na(test_clean$GarageYrBlt)] <- 0

# GROUP 5: SINGLE ORDINARY MISSING VALUE
# ---------------------------------------------------------

mod_val <- names(which.max(table(test_clean$Electrical)))
test_clean$Electrical[is.na(test_clean$Electrical)] <- mod_val

# Final validation: check remaining missing values
missing_count <- sapply(test_clean, function(x) sum(is.na(x)))
missing_count

# PART 2: CLEAN UP CATEGORICAL VARIABLES
# =========================================================

# Identify categorical variables
char_vars <- names(test_clean)[sapply(test_clean, is.character)]
char_vars

# Summarize number of levels
level_summary <- data.frame(
  Variable = char_vars,
  Num_Levels = sapply(test_clean[char_vars], function(x) length(unique(x))),
  Has_Other = sapply(test_clean[char_vars], function(x) any(x == "Other")),
  stringsAsFactors = FALSE
)

level_summary <- level_summary[order(-level_summary$Num_Levels), ]
level_summary

# Frequency table for each categorical variable
freq_list <- lapply(test_clean[char_vars], function(x) {
  data.frame(
    Level = names(table(x)),
    Count = as.vector(table(x)),
    Proportion = as.vector(prop.table(table(x)))
  )
})

freq_list

# General rule:
# First, consider categorical variables with more than 5 levels,
# then determine whether the levels are meaningful enough to keep
# or sparse enough to group.

candidate_vars <- level_summary$Variable[level_summary$Num_Levels > 5]
candidate_vars 

# GROUP 1: Keep as-is
# ---------------------------------------------------------
vars_keep_as_is <- c(
  "Neighborhood",
  "HouseStyle",
  "RoofStyle",
  "RoofMatl",
  "Foundation",
  "Heating",
  "Functional",
  "SaleType",
  "SaleCondition"
)

# GROUP 2: Keep structural "None" as a separate category
# ---------------------------------------------------------
vars_keep_none_separate <- c(
  "FireplaceQu",
  "GarageType",
  "GarageFinish",
  "GarageQual",
  "GarageCond",
  "BsmtExposure",
  "BsmtFinType1",
  "BsmtFinType2",
  "BsmtQual",
  "BsmtCond",
  "MasVnrType"
)

# GROUP 3: Group rare levels into "Other"
# ---------------------------------------------------------
vars_group_rare <- c(
  "Exterior1st",
  "Exterior2nd",
  "Condition1",
  "Condition2"
)

# Check that all candidate variables are accounted for
setdiff(candidate_vars,
        c(vars_keep_as_is, vars_keep_none_separate, vars_group_rare))

# Function to group rare levels into "Other"
# Rare categories are grouped when their frequency is below 2%.
# A 1% threshold was initially tested but retained too many sparse levels.

group_rare_levels <- function(x, threshold = 0.02) {
  x <- as.character(x)
  freq <- prop.table(table(x))
  rare_levels <- names(freq[freq < threshold])
  x[x %in% rare_levels] <- "Other"
  factor(x)
}

# Apply grouping only to selected variables
test_clean[vars_group_rare] <- lapply(test_clean[vars_group_rare], group_rare_levels)

# Check proportions after grouping
for (v in vars_group_rare) {
  cat("\n====================\n")
  cat("Variable:", v, "\n")
  print(prop.table(table(test_clean[[v]])))
}

# Convert all remaining character variables to factors
char_vars <- names(test_clean)[sapply(test_clean, is.character)]
test_clean[char_vars] <- lapply(test_clean[char_vars], as.factor)

# Check structure
str(test_clean)

missing_count <- sapply(test_clean, function(x) sum(is.na(x)))
missing_count

# SalePrice is not present in Kaggle test.csv; predictions are generated from the fitted model.
