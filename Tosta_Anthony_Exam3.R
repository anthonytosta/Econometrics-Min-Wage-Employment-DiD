##################################################
# ECON 418-518 Final Project
# Anthony Tosta
# The University of Arizona
# anthonytosta@arizona.edu 
# 12 December 2024
###################################################


#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Load packages
pacman::p_load(data.table)

# Load Data
dt <- as.data.table(read.csv("ECON_418-518_Exam_3_Data.csv", header = TRUE))
data <- fread(file_path)
getwd()

# Inspect the first few rows of the data
head(data)

# (ii) Create indicators for the "Nov" time period and "New Jersey" state
data[, nov_indicator := ifelse(time_period == "Nov", 1, 0)]
data[, nj_indicator := ifelse(state == 1, 1, 0)]

# Calculate mean total employment for each state in each time period
mean_employment <- data[, .(mean_total_emp = mean(total_emp, na.rm = TRUE)), by = .(state, time_period)]
print(mean_employment)

# (iii) Difference-in-Differences estimator using sample means
# Calculate the mean total employment for each combination of state and time period
means <- dcast(mean_employment, state ~ time_period, value.var = "mean_total_emp")

# Compute the DiD estimator
did_estimate <- (means[state == 1, Nov] - means[state == 1, Feb]) - (means[state == 0, Nov] - means[state == 0, Feb])

# Print the DiD estimate
print(paste("DiD Estimate:", did_estimate))

# (iv) Estimate the model using lm() and compute 95% confidence interval
# Estimate DiD model using lm()
did_model <- lm(total_emp ~ nj_indicator * nov_indicator, data = data)

# Extract the coefficient for the interaction term (ATT)
att <- coef(did_model)["nj_indicator:nov_indicator"]

# Calculate standard error of the ATT
se_att <- sqrt(vcov(did_model)["nj_indicator:nov_indicator", "nj_indicator:nov_indicator"])

# Construct a 95% confidence interval around the ATT
ci_lower <- att - 1.96 * se_att
ci_upper <- att + 1.96 * se_att

# Print the confidence interval
print(paste("95% Confidence Interval for ATT:", ci_lower, "to", ci_upper))

# Test the null hypothesis that ATT = 5
# Calculate the t-statistic for testing if ATT = 5
t_stat <- (att - 5) / se_att

# Print the t-statistic
print(paste("T-statistic for ATT = 5:", t_stat))

# Test the null hypothesis that ATT = 0
# Calculate the t-statistic for testing if ATT = 0
t_stat_0 <- (att - 0) / se_att

# Print the t-statistic
print(paste("T-statistic for ATT = 0:", t_stat_0))

# (vii) Add restaurant fixed effects to the DiD model and estimate using lm()
did_model_fe <- lm(total_emp ~ nj_indicator * nov_indicator + factor(restaurant_id), data = data)

# Extract the coefficient for the interaction term (ATT) with fixed effects
att_fe <- coef(did_model_fe)["nj_indicator:nov_indicator"]

# Print the ATT with fixed effects
print(paste("DiD Estimate with Restaurant Fixed Effects:", att_fe))
