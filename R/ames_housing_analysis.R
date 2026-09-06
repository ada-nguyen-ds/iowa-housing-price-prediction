# Ames Housing: interpretable regression pipeline
# Run from the repository root after placing train.csv in data/

suppressPackageStartupMessages({
  library(tidyverse)
  library(MASS)
  library(car)
})

dir.create("output", showWarnings = FALSE)
dir.create("visuals", showWarnings = FALSE)
input_file <- file.path("data", "train.csv")
stopifnot(file.exists(input_file))
dat <- read.csv(input_file, stringsAsFactors = FALSE)

# Missing-value treatment follows the Ames data dictionary.
dat_clean <- dat
dat_clean$MiscFeature <- NULL
none_cols <- c("PoolQC","Alley","Fence","FireplaceQu","GarageType","GarageFinish",
               "GarageQual","GarageCond","BsmtExposure","BsmtFinType1","BsmtFinType2",
               "BsmtQual","BsmtCond","MasVnrType")
for (v in intersect(none_cols, names(dat_clean))) dat_clean[[v]][is.na(dat_clean[[v]])] <- "None"
dat_clean <- dat_clean %>% group_by(Neighborhood) %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), median(LotFrontage, na.rm = TRUE), LotFrontage)) %>%
  ungroup()
dat_clean$MasVnrArea[is.na(dat_clean$MasVnrArea)] <- 0
dat_clean$GarageYrBlt[is.na(dat_clean$GarageYrBlt)] <- 0
dat_clean$Electrical[is.na(dat_clean$Electrical)] <- names(which.max(table(dat_clean$Electrical)))

group_rare <- function(x, threshold=.02) {
  x <- as.character(x); freq <- prop.table(table(x)); rare <- names(freq[freq < threshold])
  x[x %in% rare] <- "Other"; factor(x)
}
rare_cols <- intersect(c("Exterior1st","Exterior2nd","Condition1","Condition2"), names(dat_clean))
dat_clean[rare_cols] <- lapply(dat_clean[rare_cols], group_rare)
char_cols <- names(dat_clean)[vapply(dat_clean, is.character, logical(1))]
dat_clean[char_cols] <- lapply(dat_clean[char_cols], factor)
stopifnot(sum(is.na(dat_clean)) == 0)
write.csv(dat_clean, file.path("output","ames_housing_train_cleaned.csv"), row.names=FALSE)

# Exploratory summaries
dat_clean$logSalePrice <- log10(dat_clean$SalePrice)
numeric_cols <- names(dat_clean)[vapply(dat_clean, is.numeric, logical(1))]
cors <- sapply(dat_clean[numeric_cols], function(x) cor(x, dat_clean$SalePrice, use="complete.obs"))
cor_table <- data.frame(variable=names(cors), correlation=as.numeric(cors)) %>%
  filter(variable != "SalePrice") %>% arrange(desc(abs(correlation)))
write.csv(cor_table, file.path("output","numeric_correlations.csv"), row.names=FALSE)

png(file.path("visuals","01_saleprice_transformation.png"), width=1200, height=520, res=130)
par(mfrow=c(1,2))
hist(dat_clean$SalePrice, main="Sale price", xlab="USD", col="#4C78A8", border="white")
hist(dat_clean$logSalePrice, main="Log10 sale price", xlab="log10(USD)", col="#1594A6", border="white")
dev.off()

# Nested explanatory models from the original course analysis
model1 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF, data=dat_clean)
model2 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt +
               Neighborhood + FullBath + KitchenQual + LotArea, data=dat_clean)
model3 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt +
               Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea +
               OverallQual:GarageCars + OverallQual:TotalBsmtSF + GrLivArea:GarageCars +
               GrLivArea:TotalBsmtSF, data=dat_clean)
model4 <- lm(SalePrice ~ OverallQual + GrLivArea + GarageCars + TotalBsmtSF + YearBuilt +
               Neighborhood + FullBath + KitchenQual + LotArea + OverallQual:GrLivArea +
               OverallQual:GarageCars + OverallQual:TotalBsmtSF + GrLivArea:GarageCars +
               GrLivArea:TotalBsmtSF + I(GrLivArea^2) + I(LotArea^2) +
               I(YearBuilt^2) + I(GarageCars^2), data=dat_clean)
model_aic <- stepAIC(model4, direction="backward", trace=FALSE)

metrics <- tibble(
  model=c("Baseline","Expanded","Interactions","Polynomial","AIC selected"),
  adjusted_r2=c(summary(model1)$adj.r.squared, summary(model2)$adj.r.squared,
                summary(model3)$adj.r.squared, summary(model4)$adj.r.squared,
                summary(model_aic)$adj.r.squared),
  AIC=c(AIC(model1),AIC(model2),AIC(model3),AIC(model4),AIC(model_aic)),
  BIC=c(BIC(model1),BIC(model2),BIC(model3),BIC(model4),BIC(model_aic))
)
write.csv(metrics, file.path("output","model_comparison.csv"), row.names=FALSE)

# Preferred transformed model and robust sensitivity analysis
model_final <- lm(log10(SalePrice) ~ OverallQual + GrLivArea + GarageCars +
  log10(TotalBsmtSF+1) + YearBuilt + Neighborhood + FullBath + KitchenQual +
  log10(LotArea+1) + OverallQual:GrLivArea +
  OverallQual:log10(TotalBsmtSF+1) + GrLivArea:log10(TotalBsmtSF+1) +
  I(GarageCars^2), data=dat_clean)
robust_model <- rlm(formula(model_final), data=dat_clean, psi=psi.huber, maxit=100)

coef_table <- data.frame(term=names(coef(model_final)), estimate=coef(model_final),
                         confint(model_final, level=.90), row.names=NULL)
names(coef_table)[3:4] <- c("ci_90_lower","ci_90_upper")
write.csv(coef_table, file.path("output","final_model_coefficients.csv"), row.names=FALSE)

influence <- tibble(
  Id=dat_clean$Id,
  studentized_residual=rstudent(model_final),
  cooks_distance=cooks.distance(model_final),
  leverage=hatvalues(model_final)
) %>% arrange(desc(abs(studentized_residual)))
write.csv(influence, file.path("output","influence_diagnostics.csv"), row.names=FALSE)

png(file.path("visuals","02_final_model_diagnostics.png"), width=1100, height=900, res=130)
par(mfrow=c(2,2)); plot(model_final)
dev.off()

png(file.path("visuals","03_residuals_by_neighborhood.png"), width=1100, height=850, res=130)
boxplot(resid(model_final) ~ dat_clean$Neighborhood, horizontal=TRUE, las=1,
        col="#DCEAF0", border="#2E6FA3", xlab="Residual", ylab="Neighborhood",
        main="Final-model residuals by neighborhood")
abline(v=0, lty=2, col="#C75643")
dev.off()

saveRDS(list(ols=model_final, robust=robust_model), file.path("output","fitted_models.rds"))
message("Analysis complete. Review output/ and visuals/.")
