library(ggplot2)

p_box_gpa <- ggplot(df, aes(x = "", y = GPA)) +
  geom_boxplot(fill = "#C8C8C8", color = "black", width = 0.45, outlier.shape = 8) +
  facet_wrap(~ Gender) +
  labs(title = "Boxplot of GPA", caption = "Panel variable: Gender", x = "", y = "GPA") +
  scale_y_continuous(breaks = seq(1.0, 4.0, by = 0.5), limits = c(0.8, 4.2)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    axis.title.y = element_text(face = "bold", size = 10),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "bold", size = 10),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p_box_gpa)




if (!require("nortest")) install.packages("nortest")
library(nortest)

# 1. Run Anderson-Darling Test for Females
gpa_female <- df$GPA[df$Gender == "Female"]
ad_test <- ad.test(gpa_female)

# Print AD statistic and p-value
cat(sprintf("Mean: %.3f\nStDev: %.4f\nN: %d\nAD: %.3f\nP-Value: %.3f\n", 
            mean(gpa_female), sd(gpa_female), length(gpa_female), ad_test$statistic, ad_test$p.value))

# 2. Probability Plot
qqnorm(gpa_female, col = "red", pch = 19, main = "Probability Plot of GPA for female\nNormal",
       xlab = "GPA", ylab = "Percent")
qqline(gpa_female, col = "blue")
grid()




# F-Test for Equal Variances
var_test_res <- var.test(GPA ~ Gender, data = df, ratio = 1, alternative = "two.sided", conf.level = 0.95)

# Output Summary Table
cat("========================================================\n")
cat("Table 05 - Test and CI for Two Variances of GPA vs Gender\n")
cat("========================================================\n")
cat("Descriptive Statistics:\n")
aggregate(GPA ~ Gender, data = df, FUN = function(x) c(N = length(x), StDev = sd(x), Variance = var(x)))

cat("\nRatio of Standard Deviations:\n")
cat(sprintf("Estimated Ratio: %.6f\n", sqrt(var_test_res$estimate)))
cat(sprintf("95%% CI for Ratio: (%.3f, %.3f)\n", sqrt(var_test_res$conf.int[1]), sqrt(var_test_res$conf.int[2])))

cat("\nTest Result (F-Test):\n")
cat(sprintf("F-Statistic = %.2f, DF1 = %d, DF2 = %d, P-Value = %.3f\n", 
            var_test_res$statistic, var_test_res$parameter[1], var_test_res$parameter[2], var_test_res$p.value))



# Two-Sample T-Test (Equal Variances Assumed)
t_test_res <- t.test(GPA ~ Gender, data = df, var.equal = TRUE, conf.level = 0.95)

# Pooled Standard Deviation calculation
g_summary <- aggregate(GPA ~ Gender, data = df, FUN = function(x) c(N = length(x), Mean = mean(x), SD = sd(x), SE = sd(x)/sqrt(length(x))))

cat("========================================================\n")
cat("Table 06 - Two-Sample T-Test and CI of GPA, Gender\n")
cat("========================================================\n")
cat("Descriptive Statistics:\n")
print(g_summary)

# Calculate Pooled SD
n1 <- sum(df$Gender == "Female")
n2 <- sum(df$Gender == "Male")
s1 <- sd(df$GPA[df$Gender == "Female"])
s2 <- sd(df$GPA[df$Gender == "Male"])
s_pooled <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))

cat("\nEstimation for Difference:\n")
cat(sprintf("Difference (Female - Male): %.3f\n", diff(rev(t_test_res$estimate))))
cat(sprintf("Pooled StDev: %.3f\n", s_pooled))
cat(sprintf("95%% CI for Difference: (%.3f, %.3f)\n", t_test_res$conf.int[1], t_test_res$conf.int[2]))

cat("\nTest Result:\n")
cat(sprintf("T-Value = %.2f, DF = %d, P-Value = %.3f\n", 
            t_test_res$statistic, t_test_res$parameter, t_test_res$p.value))








#===========================================================================================
#============================================================================================
library(ggplot2)

p_box_digital <- ggplot(df, aes(x = digital_tool_usage, y = GPA)) +
  geom_boxplot(fill = "#C8C8C8", color = "black", width = 0.45, outlier.shape = 8) +
  labs(title = "Boxplot of GPA", x = "digital tools usage", y = "GPA") +
  scale_y_continuous(breaks = seq(1.0, 4.0, by = 0.5), limits = c(0.8, 4.2)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 9),
    axis.text = element_text(color = "black", size = 8.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA)
  )

print(p_box_digital)


library(ggplot2)
if (!require("gridExtra")) install.packages("gridExtra")
library(gridExtra)
library(grid)

# Fit ANOVA model to compute residuals and fitted values
model <- lm(GPA ~ digital_tool_usage, data = df)
df_res <- data.frame(
  Fitted = fitted(model),
  Residuals = residuals(model),
  Order = 1:nrow(df)
)

# Shared minimalist theme for the 4-in-1 grid
res_theme <- theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 8),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

# 1. Normal Probability Plot
p1 <- ggplot(df_res, aes(sample = Residuals)) +
  stat_qq(color = "red", size = 1.2) +
  stat_qq_line(color = "blue") +
  labs(title = "Normal Probability Plot", x = "Residual", y = "Percent") +
  res_theme

# 2. Versus Fits
p2 <- ggplot(df_res, aes(x = Fitted, y = Residuals)) +
  geom_point(color = "red", size = 1.2) +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Versus Fits", x = "Fitted Value", y = "Residual") +
  res_theme

# 3. Histogram
p3 <- ggplot(df_res, aes(x = Residuals)) +
  geom_histogram(fill = "#C8C8C8", color = "black", binwidth = 0.2) +
  labs(title = "Histogram", x = "Residual", y = "Frequency") +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
    axis.title = element_text(face = "bold", size = 8),
    panel.grid = element_blank()
  )

# 4. Versus Order
p4 <- ggplot(df_res, aes(x = Order, y = Residuals)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 1.2) +
  geom_hline(yintercept = 0, color = "black") +
  labs(title = "Versus Order", x = "Observation Order", y = "Residual") +
  res_theme

# Combine into 4-in-1 layout
grid.arrange(p1, p2, p3, p4, ncol = 2, top = textGrob("Residual Plots for GPA", gp = gpar(fontsize = 12, fontface = "bold")))


minitab_kruskal <- function(data, response, group) {
  y <- data[[response]]
  g <- factor(data[[group]])
  
  N <- length(y)
  r <- rank(y) # Overall ranks
  
  # Overall Mean Rank
  overall_mean_rank <- (N + 1) / 2
  
  # Group summary: N, Median, Mean Rank, Z-Value
  groups <- levels(g)
  stats_df <- data.frame(
    Group = character(), N = integer(), Median = numeric(), Mean_Rank = numeric(), Z_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grp in groups) {
    sub_y <- y[g == grp]
    sub_r <- r[g == grp]
    n_i <- length(sub_y)
    med_i <- median(sub_y, na.rm = TRUE)
    mr_i <- mean(sub_r)
    
    # Minitab Z-value formula for Kruskal-Wallis Mean Ranks
    z_i <- (mr_i - overall_mean_rank) / sqrt((N + 1) * (N - n_i) / (12 * n_i))
    
    stats_df <- rbind(stats_df, data.frame(
      Group = grp, N = n_i, Median = med_i, Mean_Rank = mr_i, Z_Value = z_i
    ))
  }
  
  # Kruskal-Wallis H unadjusted
  R_i <- tapply(r, g, sum)
  n_i <- tapply(r, g, length)
  H_unadj <- (12 / (N * (N + 1))) * sum((R_i^2) / n_i) - 3 * (N + 1)
  
  # Tie adjustment factor (C)
  ties <- table(r)
  C <- 1 - sum(ties^3 - ties) / (N^3 - N)
  H_adj <- H_unadj / C
  
  df_k <- length(groups) - 1
  p_unadj <- 1 - pchisq(H_unadj, df = df_k)
  p_adj <- 1 - pchisq(H_adj, df = df_k)
  
  # Console Display
  cat("=================================================================\n")
  cat(sprintf("Table 07 - Kruskal-Wallis Test of %s versus %s\n", response, group))
  cat("=================================================================\n")
  cat("Descriptive Statistics:\n\n")
  
  print_df <- stats_df
  colnames(print_df) <- c(group, "N", "Median", "Mean Rank", "Z-Value")
  print(sprintf("%-20s %5s %8s %10s %8s", group, "N", "Median", "Mean Rank", "Z-Value"))
  for(i in 1:nrow(stats_df)) {
    cat(sprintf("%-20s %5d %8.2f %10.1f %8.2f\n", 
                stats_df$Group[i], stats_df$N[i], stats_df$Median[i], stats_df$Mean_Rank[i], stats_df$Z_Value[i]))
  }
  cat(sprintf("%-20s %5d %8s %10.1f\n", "Overall", N, "", overall_mean_rank))
  
  cat("\nTest:\n")
  cat("Null hypothesis: All medians are equal\n")
  cat("Alternative hypothesis: At least one median is different\n\n")
  cat(sprintf("%-22s %3s %8s %8s\n", "Method", "DF", "H-Value", "P-Value"))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n", "Not adjusted for ties", df_k, H_unadj, p_unadj))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n\n", "Adjusted for ties", df_k, H_adj, p_adj))
  
  if (any(n_i < 5)) {
    cat("The chi-square approximation may not be accurate when some sample sizes are less than 5.\n")
  }
}

# Run the test function
minitab_kruskal(df, "GPA", "digital_tool_usage")





#========================================================================================
# Association between GPA and Fixed time schedule  
#=======================================================================================

library(ggplot2)

p_box_schedule <- ggplot(df, aes(x = "", y = GPA)) +
  geom_boxplot(fill = "#C8C8C8", color = "black", width = 0.45, outlier.shape = 8) +
  facet_wrap(~ fixed_study_schedule) +
  labs(
    title = "Boxplot of GPA", 
    caption = "Panel variable: Fixed study schedule", 
    x = "", 
    y = "GPA"
  ) +
  scale_y_continuous(breaks = seq(2.0, 4.0, by = 0.5), limits = c(1.8, 4.2)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    plot.caption = element_text(hjust = 0, size = 9),
    axis.title.y = element_text(face = "bold", size = 9),
    axis.ticks.x = element_blank(),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(face = "plain", size = 9.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p_box_schedule)


if (!require("nortest")) install.packages("nortest")
library(nortest)

gpa_yes <- df$GPA[df$fixed_study_schedule == "Yes"]
gpa_no <- df$GPA[df$fixed_study_schedule == "No"]
ad_yes <- ad.test(gpa_yes)

# Display Minitab Statistics Box
cat("--- Figure 16 Stats (Yes) ---\n")
cat(sprintf("Mean:    %.3f\nStDev:   %.4f\nN:       %d\nAD:      %.3f\nP-Value: %.3f\n", 
            mean(gpa_yes), sd(gpa_yes), length(gpa_yes), ad_yes$statistic, ad_yes$p.value))

# Probability Plot
qqnorm(gpa_yes, col = "red", pch = 19, 
       main = "Probability Plot of GPA for fixed schedule\nNormal",
       xlab = "GPA", ylab = "Percent")
qqline(gpa_yes, col = "blue")
grid(col = "grey70", lty = "dashed")



# F-Test for Equal Variances
var_test_sched <- var.test(GPA ~ fixed_study_schedule, data = df, ratio = 1, alternative = "two.sided", conf.level = 0.95)

# Calculate individual SD confidence intervals
calc_sd_ci <- function(x, conf = 0.95) {
  n <- length(x)
  s <- sd(x)
  alpha <- 1 - conf
  lower <- sqrt((n - 1) * s^2 / qchisq(1 - alpha/2, df = n - 1))
  upper <- sqrt((n - 1) * s^2 / qchisq(alpha/2, df = n - 1))
  return(c(lower, upper))
}

sd_ci_no <- calc_sd_ci(gpa_no)
sd_ci_yes <- calc_sd_ci(gpa_yes)

# Print Summary
cat("=================================================================\n")
cat("Table 08 - Test and CI for Two Variances of GPA vs Fixed study schedule\n")
cat("=================================================================\n")
cat("Method:\n")
cat("  σ₁: standard deviation of GPA when fixed_study_schedule = No\n")
cat("  σ₂: standard deviation of GPA when fixed_study_schedule = Yes\n")
cat("  Ratio: σ₁/σ₂\n")
cat("  F method was used. This method is accurate for normal data only.\n\n")

cat("Descriptive Statistics:\n")
cat(sprintf("%-20s %3s %7s %8s %16s\n", "fixed_study_schedule", "N", "StDev", "Variance", "95% CI for σ"))
cat(sprintf("%-20s %3d %7.3f %8.3f (%.3f, %.3f)\n", "No", length(gpa_no), sd(gpa_no), var(gpa_no), sd_ci_no[1], sd_ci_no[2]))
cat(sprintf("%-20s %3d %7.3f %8.3f (%.3f, %.3f)\n\n", "Yes", length(gpa_yes), sd(gpa_yes), var(gpa_yes), sd_ci_yes[1], sd_ci_yes[2]))

cat("Ratio of Standard Deviations:\n")
cat(sprintf("Estimated Ratio: %.5f\n", sqrt(var_test_sched$estimate)))
cat(sprintf("95%% CI for Ratio using F: (%.3f, %.3f)\n\n", sqrt(var_test_sched$conf.int[1]), sqrt(var_test_sched$conf.int[2])))

cat("Test:\n")
cat("  Null hypothesis:        H₀: σ₁ / σ₂ = 1\n")
cat("  Alternative hypothesis: H₁: σ₁ / σ₂ ≠ 1\n")
cat("  Significance level:     α = 0.05\n\n")
cat(sprintf("%-8s %10s %4s %4s %8s\n", "Method", "Statistic", "DF1", "DF2", "P-Value"))
cat(sprintf("%-8s %10.2f %4d %4d %8.3f\n", "F", var_test_sched$statistic, var_test_sched$parameter[1], var_test_sched$parameter[2], var_test_sched$p.value))




# Two-Sample T-Test (Equal Variances Assumed)
t_test_sched <- t.test(GPA ~ fixed_study_schedule, data = df, var.equal = TRUE, conf.level = 0.95)

# Pooled Standard Deviation calculation
n1 <- length(gpa_no)
n2 <- length(gpa_yes)
s1 <- sd(gpa_no)
s2 <- sd(gpa_yes)
s_pooled <- sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2) / (n1 + n2 - 2))

diff_mean <- mean(gpa_no) - mean(gpa_yes)

cat("=================================================================\n")
cat("Table 09 - Two-Sample T-Test and CI of GPA, Fixed study schedule\n")
cat("=================================================================\n")
cat("Method:\n")
cat("  μ₁: population mean of GPA when fixed_study_schedule = No\n")
cat("  μ₂: population mean of GPA when fixed_study_schedule = Yes\n")
cat("  Difference: μ₁ - μ₂\n")
cat("  Equal variances are assumed for this analysis.\n\n")

cat("Descriptive Statistics: GPA\n")
cat(sprintf("%-20s %3s %6s %7s %8s\n", "fixed_study_schedule", "N", "Mean", "StDev", "SE Mean"))
cat(sprintf("%-20s %3d %6.3f %7.3f %8.3f\n", "No", n1, mean(gpa_no), s1, s1/sqrt(n1)))
cat(sprintf("%-20s %3d %6.3f %7.3f %8.3f\n\n", "Yes", n2, mean(gpa_yes), s2, s2/sqrt(n2)))

cat("Estimation for Difference:\n")
cat(sprintf("%10s %12s %20s\n", "Difference", "Pooled StDev", "95% CI for Difference"))
cat(sprintf("%10.3f %12.3f (%.3f, %.3f)\n\n", diff_mean, s_pooled, t_test_sched$conf.int[1], t_test_sched$conf.int[2]))

cat("Test:\n")
cat("  Null hypothesis:        H₀: μ₁ - μ₂ = 0\n")
cat("  Alternative hypothesis: H₁: μ₁ - μ₂ ≠ 0\n")
cat(sprintf("%7s %4s %8s\n", "T-Value", "DF", "P-Value"))
cat(sprintf("%7.2f %4d %8.3f\n", t_test_sched$statistic, t_test_sched$parameter, t_test_sched$p.value))








#================================================================================
# Association between GPA and engaged in class 
#================================================================================

minitab_kruskal_engaged <- function(data, response, group) {
  y <- data[[response]]
  g <- factor(data[[group]])
  
  N <- length(y)
  r <- rank(y) # Overall ranks
  
  # Overall Mean Rank
  overall_mean_rank <- (N + 1) / 2
  
  # Group summary: N, Median, Mean Rank, Z-Value
  groups <- levels(g)
  stats_df <- data.frame(
    Group = character(), N = integer(), Median = numeric(), Mean_Rank = numeric(), Z_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grp in groups) {
    sub_y <- y[g == grp]
    sub_r <- r[g == grp]
    n_i <- length(sub_y)
    
    if (n_i > 0) {
      med_i <- median(sub_y, na.rm = TRUE)
      mr_i <- mean(sub_r)
      
      # Minitab Z-value formula for Kruskal-Wallis Mean Ranks
      z_i <- (mr_i - overall_mean_rank) / sqrt((N + 1) * (N - n_i) / (12 * n_i))
      
      stats_df <- rbind(stats_df, data.frame(
        Group = grp, N = n_i, Median = med_i, Mean_Rank = mr_i, Z_Value = z_i
      ))
    }
  }
  
  # Kruskal-Wallis H unadjusted
  R_i <- tapply(r, g, sum)
  n_i <- tapply(r, g, length)
  H_unadj <- (12 / (N * (N + 1))) * sum((R_i^2) / n_i) - 3 * (N + 1)
  
  # Tie adjustment factor (C)
  ties <- table(r)
  C <- 1 - sum(ties^3 - ties) / (N^3 - N)
  H_adj <- H_unadj / C
  
  df_k <- length(groups) - 1
  p_unadj <- 1 - pchisq(H_unadj, df = df_k)
  p_adj <- 1 - pchisq(H_adj, df = df_k)
  
  # Output formatted table
  cat("=================================================================\n")
  cat(sprintf("Table 10 - Kruskal-Wallis Test of %s versus %s\n", response, group))
  cat("=================================================================\n")
  cat("Descriptive Statistics\n\n")
  
  cat(sprintf("%-15s %5s %8s %10s %8s\n", group, "N", "Median", "Mean Rank", "Z-Value"))
  for(i in 1:nrow(stats_df)) {
    cat(sprintf("%-15s %5d %8.3f %10.1f %8.2f\n", 
                stats_df$Group[i], stats_df$N[i], stats_df$Median[i], stats_df$Mean_Rank[i], stats_df$Z_Value[i]))
  }
  cat(sprintf("%-15s %5d %8s %10.1f\n\n", "Overall", N, "", overall_mean_rank))
  
  cat("Test\n\n")
  cat("Null hypothesis        H₀: All medians are equal\n")
  cat("Alternative hypothesis H₁: At least one median is different\n\n")
  cat(sprintf("%-22s %3s %8s %8s\n", "Method", "DF", "H-Value", "P-Value"))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n", "Not adjusted for ties", df_k, H_unadj, p_unadj))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n\n", "Adjusted for ties", df_k, H_adj, p_adj))
  
  if (any(n_i < 5)) {
    cat("  The chi-square approximation may not be accurate when some sample sizes are less than 5.\n")
  }
}

# Run the test function (works if 'engaged_class' is numeric 1-5 or character labels)
minitab_kruskal_engaged(df, "GPA", "engaged_class")





minitab_kruskal_engaged <- function(data, response, group) {
  y <- data[[response]]
  g <- factor(data[[group]])
  
  N <- length(y)
  r <- rank(y) # Overall ranks
  
  # Overall Mean Rank
  overall_mean_rank <- (N + 1) / 2
  
  # Group summary: N, Median, Mean Rank, Z-Value
  groups <- levels(g)
  stats_df <- data.frame(
    Group = character(), N = integer(), Median = numeric(), Mean_Rank = numeric(), Z_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grp in groups) {
    sub_y <- y[g == grp]
    sub_r <- r[g == grp]
    n_i <- length(sub_y)
    
    if (n_i > 0) {
      med_i <- median(sub_y, na.rm = TRUE)
      mr_i <- mean(sub_r)
      
      # Minitab Z-value formula for Kruskal-Wallis Mean Ranks
      z_i <- (mr_i - overall_mean_rank) / sqrt((N + 1) * (N - n_i) / (12 * n_i))
      
      stats_df <- rbind(stats_df, data.frame(
        Group = grp, N = n_i, Median = med_i, Mean_Rank = mr_i, Z_Value = z_i
      ))
    }
  }
  
  # Kruskal-Wallis H unadjusted
  R_i <- tapply(r, g, sum)
  n_i <- tapply(r, g, length)
  H_unadj <- (12 / (N * (N + 1))) * sum((R_i^2) / n_i) - 3 * (N + 1)
  
  # Tie adjustment factor (C)
  ties <- table(r)
  C <- 1 - sum(ties^3 - ties) / (N^3 - N)
  H_adj <- H_unadj / C
  
  df_k <- length(groups) - 1
  p_unadj <- 1 - pchisq(H_unadj, df = df_k)
  p_adj <- 1 - pchisq(H_adj, df = df_k)
  
  # Output formatted table
  cat("=================================================================\n")
  cat(sprintf("Table 10 - Kruskal-Wallis Test of %s versus %s\n", response, group))
  cat("=================================================================\n")
  cat("Descriptive Statistics\n\n")
  
  cat(sprintf("%-15s %5s %8s %10s %8s\n", group, "N", "Median", "Mean Rank", "Z-Value"))
  for(i in 1:nrow(stats_df)) {
    cat(sprintf("%-15s %5d %8.3f %10.1f %8.2f\n", 
                stats_df$Group[i], stats_df$N[i], stats_df$Median[i], stats_df$Mean_Rank[i], stats_df$Z_Value[i]))
  }
  cat(sprintf("%-15s %5d %8s %10.1f\n\n", "Overall", N, "", overall_mean_rank))
  
  cat("Test\n\n")
  cat("Null hypothesis        H₀: All medians are equal\n")
  cat("Alternative hypothesis H₁: At least one median is different\n\n")
  cat(sprintf("%-22s %3s %8s %8s\n", "Method", "DF", "H-Value", "P-Value"))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n", "Not adjusted for ties", df_k, H_unadj, p_unadj))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n\n", "Adjusted for ties", df_k, H_adj, p_adj))
  
  if (any(n_i < 5)) {
    cat("  The chi-square approximation may not be accurate when some sample sizes are less than 5.\n")
  }
}

# Run the test function (works if 'engaged_class' is numeric 1-5 or character labels)
minitab_kruskal_engaged(df, "GPA", "engaged_class")




#==================================================================================
# Association between GPA and academic satisfaction  
#==================================================================================
library(ggplot2)

# Convert numeric codes (1 to 5) directly to the exact labels shown in Figure 19
df$Academic_satisfaction_1 <- factor(
  df$Academic_satisfaction,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Dissatisfied", "Moderately satisfied", "Satisfied", "Very dissatisfied", "Very satisfied")
)

# Plot Figure 19
p_box_sat <- ggplot(df[!is.na(df$Academic_satisfaction_1), ], aes(x = Academic_satisfaction_1, y = GPA)) +
  geom_boxplot(fill = "#C8C8C8", color = "black", width = 0.45, outlier.shape = 8) +
  labs(title = "Boxplot of GPA", x = "Academic satisfaction_1", y = "GPA") +
  scale_y_continuous(breaks = seq(2.0, 4.0, by = 0.5), limits = c(1.8, 4.1)) +
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    axis.title = element_text(face = "bold", size = 9),
    axis.text.x = element_text(color = "black", size = 8),
    axis.text.y = element_text(color = "black", size = 8.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

print(p_box_sat)








minitab_kruskal_sat <- function(data, response, group) {
  y <- data[[response]]
  g <- factor(data[[group]])
  
  N <- length(y)
  r <- rank(y) # Overall ranks
  
  # Overall Mean Rank
  overall_mean_rank <- (N + 1) / 2
  
  # Group summary statistics
  groups <- levels(g)
  stats_df <- data.frame(
    Group = character(), N = integer(), Median = numeric(), Mean_Rank = numeric(), Z_Value = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (grp in groups) {
    sub_y <- y[g == grp]
    sub_r <- r[g == grp]
    n_i <- length(sub_y)
    
    if (n_i > 0) {
      med_i <- median(sub_y, na.rm = TRUE)
      mr_i <- mean(sub_r)
      
      # Minitab Z-value formula for Kruskal-Wallis Mean Ranks
      z_i <- (mr_i - overall_mean_rank) / sqrt((N + 1) * (N - n_i) / (12 * n_i))
      
      stats_df <- rbind(stats_df, data.frame(
        Group = grp, N = n_i, Median = med_i, Mean_Rank = mr_i, Z_Value = z_i
      ))
    }
  }
  
  # Kruskal-Wallis H unadjusted
  R_i <- tapply(r, g, sum)
  n_i <- tapply(r, g, length)
  H_unadj <- (12 / (N * (N + 1))) * sum((R_i^2) / n_i) - 3 * (N + 1)
  
  # Tie adjustment factor (C)
  ties <- table(r)
  C <- 1 - sum(ties^3 - ties) / (N^3 - N)
  H_adj <- H_unadj / C
  
  df_k <- length(groups) - 1
  p_unadj <- 1 - pchisq(H_unadj, df = df_k)
  p_adj <- 1 - pchisq(H_adj, df = df_k)
  
  # Output formatted table matching Minitab
  cat("=================================================================\n")
  cat(sprintf("Table 11 - Kruskal-Wallis Test of %s versus %s\n", response, group))
  cat("=================================================================\n")
  cat("Descriptive Statistics\n\n")
  
  cat(sprintf("%-22s %5s %8s %10s %8s\n", group, "N", "Median", "Mean Rank", "Z-Value"))
  for(i in 1:nrow(stats_df)) {
    cat(sprintf("%-22s %5d %8.3f %10.1f %8.2f\n", 
                stats_df$Group[i], stats_df$N[i], stats_df$Median[i], stats_df$Mean_Rank[i], stats_df$Z_Value[i]))
  }
  cat(sprintf("%-22s %5d %8s %10.1f\n\n", "Overall", N, "", overall_mean_rank))
  
  cat("Test\n\n")
  cat("Null hypothesis        H₀: All medians are equal\n")
  cat("Alternative hypothesis H₁: At least one median is different\n\n")
  cat(sprintf("%-22s %3s %8s %8s\n", "Method", "DF", "H-Value", "P-Value"))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n", "Not adjusted for ties", df_k, H_unadj, p_unadj))
  cat(sprintf("%-22s %3d %8.2f %8.3f\n\n", "Adjusted for ties", df_k, H_adj, p_adj))
}

# Run test (works if group variable is named 'Academic_satisfaction' or 'Academic_satisfaction_1')
minitab_kruskal_sat(df, "GPA", "Academic_satisfaction")



















