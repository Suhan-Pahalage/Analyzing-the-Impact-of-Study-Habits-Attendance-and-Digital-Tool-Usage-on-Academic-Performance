
# Load required libraries
if (!require("car")) install.packages("car")
if (!require("qpcR")) install.packages("qpcR") # For PRESS statistic

library(car)
library(qpcR)

# 1. Fit the simple linear regression model
model <- lm(GPA ~ engaged_class, data = df)

# ----------------------------------------------------
# A. REGRESSION EQUATION
# ----------------------------------------------------
intercept <- coef(model)[1]
slope <- coef(model)[2]
sign_str <- ifelse(slope >= 0, "+", "-")

cat("Regression Equation\n")
cat(sprintf("  GPA  =  %.3f %s %.4f engaged_class\n\n\n", 
            intercept, sign_str, abs(slope)))

# ----------------------------------------------------
# B. COEFFICIENTS TABLE
# ----------------------------------------------------
mod_sum <- summary(model)
coefs <- mod_sum$coefficients
ci <- confint(model)

# Calculate VIF (N/A for 1 predictor, shown as 1.00 for consistency)
vif_val <- if(length(coef(model)) > 2) vif(model) else c(NA, 1.00)

coef_table <- data.frame(
  Term = c("Constant", "engaged_class"),
  Coef = sprintf("%.4f", coefs[, 1]),
  `SE Coef` = sprintf("%.4f", coefs[, 2]),
  `95% CI` = sprintf("(%.4f, %.4f)", ci[, 1], ci[, 2]),
  `T-Value` = sprintf("%.2f", coefs[, 3]),
  `P-Value` = sprintf("%.3f", coefs[, 4]),
  `VIF` = c("", sprintf("%.2f", vif_val[2])),
  check.names = FALSE
)

cat("Coefficients\n")
print(coef_table, row.names = FALSE)
cat("\n\n")

# ----------------------------------------------------
# C. MODEL SUMMARY (Base R - No qpcR dependency)
# ----------------------------------------------------
mod_sum <- summary(model)

s_val <- mod_sum$sigma
r_sq <- mod_sum$r.squared * 100
r_sq_adj <- mod_sum$adj.r.squared * 100

# 1. Calculate PRESS using hat values and residuals directly
resids <- residuals(model)
hats <- hatvalues(model)
press_residuals <- resids / (1 - hats)
press_val <- sum(press_residuals^2)

# 2. Calculate R-sq(pred)
y <- model$model[[1]] # Extracts response variable (GPA)
ss_tot <- sum((y - mean(y))^2)
r_sq_pred <- (1 - (press_val / ss_tot)) * 100

# 3. Calculate AICc and BIC
n <- length(y)
k <- length(coef(model)) # Number of parameters
aicc_val <- AIC(model) + (2 * k * (k + 1)) / (n - k - 1)
bic_val <- BIC(model)

# 4. Format into Minitab Summary Table
summary_table <- data.frame(
  `S` = sprintf("%.6f", s_val),
  `R-sq` = sprintf("%.2f%%", r_sq),
  `R-sq(adj)` = sprintf("%.2f%%", r_sq_adj),
  `PRESS` = sprintf("%.4f", press_val),
  `R-sq(pred)` = sprintf("%.2f%%", r_sq_pred),
  `AICc` = sprintf("%.2f", aicc_val),
  `BIC` = sprintf("%.2f", bic_val),
  check.names = FALSE
)

cat("Model Summary\n")
print(summary_table, row.names = FALSE)
