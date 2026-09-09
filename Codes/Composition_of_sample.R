if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Standard helper function to generate Minitab-style pie charts
make_minitab_pie <- function(data, var_name, title, colors) {
  df_summary <- data %>%
    group_by(.data[[var_name]]) %>%
    summarise(Count = n(), .groups = "drop") %>%
    mutate(
      Percent = Count / sum(Count) * 100,
      Label = sprintf("%.1f%%", Percent)
    )
  
  ggplot(df_summary, aes(x = "", y = Percent, fill = .data[[var_name]])) +
    geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.3) +
    coord_polar(theta = "y", start = 0) +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 3.5) +
    scale_fill_manual(values = colors, name = "Category") +
    labs(title = title) +
    theme_void() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      legend.position = "right",
      legend.box.background = element_rect(color = "black", fill = "white"),
      legend.title = element_text(size = 9, face = "bold"),
      legend.text = element_text(size = 8)
    )
}

# 1. Figure 01 - Pie Chart of Gender
p_gender <- make_minitab_pie(
  df, "Gender", 
  "Pie Chart of Gender", 
  c("Female" = "#F2C088", "Male" = "#F092C5")
)
print(p_gender)

# 2. Figure 02 - Pie Chart of Digital Tools Usage
p_digital <- make_minitab_pie(
  df, "digital_tool_usage", 
  "Pie Chart of digital tools usage", 
  c("Daily" = "#A0A8FF", "Never" = "#FF99BB", "Occasionally" = "#C5FFFF", "Weekly" = "#FF9E59")
)
print(p_digital)

# 3. Figure 03 - Pie Chart of Fixed Study Schedule
p_schedule <- make_minitab_pie(
  df, "fixed_study_schedule", 
  "Pie Chart of fixed_study_schedule", 
  c("No" = "#71A0D6", "Yes" = "#C24646")
)
print(p_schedule)





if (!require("gmodels")) install.packages("gmodels")
library(gmodels)

# Table 01: Gender vs digital_tool_usage
CrossTable(
  df$Gender, df$digital_tool_usage,
  prop.r = TRUE,   # % of Row
  prop.c = TRUE,   # % of Column
  prop.t = TRUE,   # % of Total
  prop.chisq = FALSE,
  format = "SPSS"
)

# Table 02: Gender vs fixed_study_schedule
CrossTable(
  df$Gender, df$fixed_study_schedule,
  prop.r = TRUE,
  prop.c = TRUE,
  prop.t = TRUE,
  prop.chisq = FALSE,
  format = "SPSS"
)



 
#==========================================
#For quantitative variables
#==========================================


# Convert Engaged Class labels
df$EngagedClass_cat <- factor(df$engaged_class,
                              levels = c(1, 2, 3, 4, 5), # or match existing string values
                              labels = c("Engaged", "Moderately Engaged", "Not Engaged", "Slightly Engaged", "Very Engaged")
)

# Convert Academic Satisfaction labels
df$Academic_satisfaction_cat <- factor(df$Academic_satisfaction,
                                       levels = c(1, 2, 3, 4, 5), # or match existing string values
                                       labels = c("Dissatisfied", "Moderately satisfied", "Satisfied", "Very dissatisfied", "Very satisfied")
)




library(ggplot2)

# Custom Minitab theme for Bar Charts
minitab_bar_theme <- theme_bw() + 
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 12, color = "black"),
    axis.title = element_text(face = "bold", size = 10),
    axis.text = element_text(color = "black", size = 8.5),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "grey50", fill = NA)
  )

# ----------------------------------------------------
# Figure 04 - Bar chart of Engaged Class (Light Peach)
# ----------------------------------------------------
p_engaged <- ggplot(df, aes(x = EngagedClass_cat)) +
  geom_bar(fill = "#FCD19C", color = "black", width = 0.45) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 32), breaks = seq(0, 30, by = 5), expand = c(0, 0)) +
  labs(title = "Chart of EngagedClass", x = "EngagedClass", y = "Count") +
  minitab_bar_theme

print(p_engaged)

# ----------------------------------------------------
# Figure 05 - Bar chart of Academic satisfaction (Pink)
# ----------------------------------------------------
p_satisfaction <- ggplot(df, aes(x = Academic_satisfaction_cat)) +
  geom_bar(fill = "#FCA0D6", color = "black", width = 0.45) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5, size = 3) +
  scale_y_continuous(limits = c(0, 37), breaks = seq(0, 35, by = 5), expand = c(0, 0)) +
  labs(title = "Chart of Academic satisfaction", x = "Academic satisfaction", y = "Count") +
  minitab_bar_theme

print(p_satisfaction)












