# Step 1: Load required libraries
if (!require("readxl")) install.packages("readxl")
if (!require("car")) install.packages("car")

library(readxl)
library(car)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set baseline reference levels for categorical variables (1, 0 Coding)
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Ensure continuous predictors are numeric
df$Attendence <- as.numeric(df$Attendence)
df$engaged_class <- as.numeric(df$engaged_class)

# Set treatment/dummy coding (1, 0)
options(contrasts = c("contr.treatment", "contr.poly"))

# Step 4: Fit model without Study_Hours
model <- lm(GPA ~ Attendence + engaged_class + 
              Gender + digital_tool_usage + fixed_study_schedule + 
              Gender:digital_tool_usage, data = df)

# Step 5: View full regression results
summary(model)

# Step 6: Calculate Variance Inflation Factor (VIF / GVIF)
vif_results <- vif(model)
print(vif_results)












# Step 1: Install and load required libraries for plotting
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("patchwork")) install.packages("patchwork")

library(ggplot2)
library(patchwork)

# Extract fitted values, residuals, and observation order from your model
res_df <- data.frame(
  Fitted = fitted(model),
  Residuals = residuals(model),
  Order = 1:length(residuals(model))
)

# Set common theme to mimic Minitab's clean grid style
minitab_theme <- theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "grey50")
  )

# Plot 1: Normal Probability Plot
p1 <- ggplot(res_df, aes(sample = Residuals)) +
  stat_qq(color = "#004080", size = 1.8) +
  stat_qq_line(color = "firebrick", linewidth = 0.6) +
  labs(title = "Normal Probability Plot", x = "Residual", y = "Percent") +
  minitab_theme

# Plot 2: Versus Fits
p2 <- ggplot(res_df, aes(x = Fitted, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_point(color = "#004080", size = 1.8) +
  labs(title = "Versus Fits", x = "Fitted Value", y = "Residual") +
  minitab_theme

# Plot 3: Histogram
p3 <- ggplot(res_df, aes(x = Residuals)) +
  geom_histogram(bins = 8, fill = "#70a1d7", color = "black", alpha = 0.8) +
  labs(title = "Histogram", x = "Residual", y = "Frequency") +
  minitab_theme

# Plot 4: Versus Order
p4 <- ggplot(res_df, aes(x = Order, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_line(color = "#004080", linewidth = 0.5) +
  geom_point(color = "#004080", size = 1.8) +
  labs(title = "Versus Order", x = "Observation Order", y = "Residual") +
  minitab_theme

# Combine all 4 plots into a 2x2 grid with a overall title
combined_plot <- (p1 + p2) / (p3 + p4) + 
  plot_annotation(
    title = "Residual Plots for GPA",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  )

# Display the combined plot
print(combined_plot)




# Step 1: Add residuals to your dataset if not already done
df$RESI <- residuals(model)

# Step 2: Define the variables you want to test
vars_to_test <- c("Study_Hours", "Academic_satisfaction", "RESI")

# Ensure continuous variables are numeric
df$Study_Hours <- as.numeric(df$Study_Hours)
df$Academic_satisfaction <- as.numeric(df$Academic_satisfaction)

# Step 3: Function to generate Pairwise Pearson Correlations table
get_pairwise_correlations <- function(data, vars) {
  # Get all unique pairwise combinations
  pairs <- combn(vars, 2, simplify = FALSE)
  
  results <- lapply(pairs, function(p) {
    var1 <- p[1]
    var2 <- p[2]
    
    # Run Pearson correlation test
    test <- cor.test(data[[var1]], data[[var2]], method = "pearson", conf.level = 0.95)
    
    # Extract metrics
    n_obs <- sum(!is.na(data[[var1]]) & !is.na(data[[var2]]))
    r_val <- sprintf("%.3f", test$estimate)
    ci_lower <- sprintf("%.3f", test$conf.int[1])
    ci_upper <- sprintf("%.3f", test$conf.int[2])
    p_val <- sprintf("%.3f", test$p.value)
    
    data.frame(
      `Sample 1` = var1,
      `Sample 2` = var2,
      `N` = n_obs,
      `Correlation` = r_val,
      `95% CI for p` = paste0("(", ci_lower, ", ", ci_upper, ")"),
      `P-Value` = p_val,
      check.names = FALSE
    )
  })
  
  # Combine into a single table
  do.call(rbind, results)
}

# Step 4: Run function and display output
cor_table <- get_pairwise_correlations(df, vars_to_test)
print(cor_table, row.names = FALSE)




