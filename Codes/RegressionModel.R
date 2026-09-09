# Step 1: Install and load required libraries
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Convert categorical variables to factors and set reference levels
# (Matching: Gender baseline = "Male", digital tool usage baseline = "Weekly", Fixed study schedule baseline = "Yes")
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Step 4: Fit the General Linear Model (Regression with categorical & interaction terms)
model <- lm(GPA ~ Gender + digital_tool_usage + fixed_study_schedule + Gender:digital_tool_usage, data = df)

# Step 5: Output Coefficients, Standard Errors, T-values, and P-values
summary(model)


# Step 1: Install and load required library
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set baseline reference levels for categorical variables
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Note: Ensure percentage_lecture_attendence is numeric
df$percentage_lecture_attendence <- as.numeric(df$percentage_lecture_attendence)

# Step 4: Fit the model including the continuous variable & categorical interactions
model <- lm(GPA ~ percentage_lecture_attendence + Gender + digital_tool_usage + 
              fixed_study_schedule + Gender:digital_tool_usage, data = df)

# Step 5: View full regression results
summary(model)



# Step 1: Load required library
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set baseline reference levels for categorical variables
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Ensure continuous variables are numeric
df$percentage_lecture_attendence <- as.numeric(df$percentage_lecture_attendence)
df$Study_Hours <- as.numeric(df$Study_Hours)

# Step 4: Fit model with both continuous variables, categorical main effects, and interactions
model <- lm(GPA ~ Study_Hours + percentage_lecture_attendence + Gender + 
              digital_tool_usage + fixed_study_schedule + Gender:digital_tool_usage, 
            data = df)

# Step 5: View full regression results
summary(model)


# Step 1: Load required library
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set baseline reference levels for categorical variables
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Step 4: Ensure continuous variables are numeric
df$percentage_lecture_attendence <- as.numeric(df$percentage_lecture_attendence)
df$Study_Hours <- as.numeric(df$Study_Hours)
df$engaged_during_lectures <- as.numeric(df$engaged_during_lectures)

# Step 5: Fit model with 3 continuous predictors, categorical main effects, and interactions
model <- lm(GPA ~ engaged_during_lectures + Study_Hours + percentage_lecture_attendence + 
              Gender + digital_tool_usage + fixed_study_schedule + Gender:digital_tool_usage, 
            data = df)

# Step 6: View full regression results
summary(model)


model <- lm(GPA ~ Attendence + EngagedClass + Attendence:EngagedClass + 
              Study_Hours + Gender + digital_tool_usage + fixed_study_schedule + 
              Gender:digital_tool_usage, data = df)
summary(model)


# Visual diagnostic plots (look at Plot 4: Cook's distance)
plot(model)

# Find observations with Cook's distance > 4/N
cutoff <- 4 / nrow(df)
influential <- which(cooks.distance(model) > cutoff)
print(influential)


df[18, ]


# Fit model excluding observation 18
model_no18 <- lm(GPA ~ engaged_during_lectures + Study_Hours + percentage_lecture_attendence + 
                   Gender + digital_tool_usage + fixed_study_schedule + Gender:digital_tool_usage, 
                 data = df[-18, ])

# Compare coefficients side-by-side
summary(model)$coefficients
summary(model_no18)$coefficients


# Step 1: Load required library
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set reference coding (0,1) baseline levels to match Minitab
# (Baseline reference levels: Gender = Male, digital_tool_usage = Weekly, fixed_study_schedule = Yes)
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Ensure continuous variable is numeric
df$engaged_during_lectures <- as.numeric(df$engaged_during_lectures)

# Ensure R uses Treatment/Dummy coding (1, 0)
options(contrasts = c("contr.treatment", "contr.poly"))

# Step 4: Fit the model matching Minitab with (1, 0) coding
model_01 <- lm(GPA ~ engaged_during_lectures + Gender + digital_tool_usage + 
                 fixed_study_schedule + Gender:digital_tool_usage, 
               data = df)

# Step 5: Output full summary to match your new Minitab table
summary(model_01)



df$percentage_lecture_attendence <- as.numeric(df$percentage_lecture_attendence)

# Step 4: Fit full linear model including Attendance
model <- lm(GPA ~ percentage_lecture_attendence + engaged_during_lectures + 
              Study_Hours + Gender + digital_tool_usage + fixed_study_schedule + 
              Gender:digital_tool_usage, data = df)

# Step 5: View detailed model output
summary(model)

nobs(model)






# Step 1: Load required library
if (!require("readxl")) install.packages("readxl")
library(readxl)

# Step 2: Read data from your Excel file
file_path <- "C:/Users/suhan pahalage/Desktop/Book1.xlsx"
df <- read_excel(file_path)

# Step 3: Set baseline reference levels for categorical variables (1, 0 Coding)
df$Gender <- factor(df$Gender, levels = c("Male", "Female"))

df$digital_tool_usage <- factor(df$digital_tool_usage, 
                                levels = c("Weekly", "Daily", "Never", "Occasionally"))

df$fixed_study_schedule <- factor(df$fixed_study_schedule, levels = c("Yes", "No"))

# Ensure continuous predictors are numeric
df$percentage_lecture_attendence <- as.numeric(df$percentage_lecture_attendence)
df$engaged_during_lectures <- as.numeric(df$engaged_during_lectures)

# Set treatment/dummy coding (1, 0)
options(contrasts = c("contr.treatment", "contr.poly"))

# Step 4: Fit model without Study_Hours
model <- lm(GPA ~ percentage_lecture_attendence + engaged_during_lectures + 
              Gender + digital_tool_usage + fixed_study_schedule + 
              Gender:digital_tool_usage, data = df)

# Step 5: View full regression results
summary(model)






