# Step 1: Install and load required libraries
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("GGally")) install.packages("GGally")

library(ggplot2)
library(GGally)

# Step 2: Select quantitative variables
quant_vars <- c("GPA", "Study_Hours", "Attendance", "engaged_class", "Academic_satisfaction")

# Rename or ensure columns exist in df
# (Map your attendance variable if named differently, e.g. df$Attendance <- df$percentage_lecture_attendence)
if (!"Attendance" %in% names(df) && "percentage_lecture_attendence" %in% names(df)) {
  df$Attendance <- df$percentage_lecture_attendence
}

# Ensure variables are numeric
df_subset <- df[, quant_vars]
df_subset[] <- lapply(df_subset, as.numeric)

# Step 3: Define custom scatterplot function to match Minitab style
custom_points <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(color = "#004080", size = 1.2) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90"),
      axis.text = element_text(size = 7)
    )
}

# Step 4: Define function to plot Pearson r and p-value inside matrix panels
custom_cor <- function(data, mapping, ...) {
  xData <- eval_data_col(data, mapping$x)
  yData <- eval_data_col(data, mapping$y)
  
  test <- cor.test(xData, yData, method = "pearson")
  r_val <- test$estimate
  p_val <- test$p.value
  
  cor_text <- sprintf("r = %.3f  p = %.3f", r_val, p_val)
  
  ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = cor_text, size = 2.8) +
    theme_void() +
    theme(panel.border = element_rect(color = "grey70", fill = NA))
}

# Step 5: Build the Matrix Plot
matrix_plot <- ggpairs(
  df_subset,
  upper = list(continuous = custom_points),
  lower = list(continuous = custom_cor),
  diag = list(continuous = "blankDiag"),
  title = "Matrix Plot of GPA, Study_Hours, Attendance, engaged_class, Academic_satisfaction\nPearson Correlation"
) +
  theme_bw() +
  
  theme(
    axis.text = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 11, color = "grey20"),
    strip.background = element_rect(fill = "white", color = "grey70"),
    strip.text = element_text(face = "bold", size = 8)
  )

# Display the plot
print(matrix_plot)





 