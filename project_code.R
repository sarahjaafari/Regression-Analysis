# Install and load required packages
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("car") # Install the package

library(car) # Load the package
library(tidyr)
library(dplyr)
library(ggplot2)

# Read data
data <- read.table("class.data.txt", header = TRUE)

# Summary statistics
summary(data[, 4:8])

# Aggregate by gender
gender_summary <- aggregate(. ~ gender, data = data[, c("gender", "quiz1", "quiz2", "quiz3", "quiz4", "quiz5")], FUN = mean)
print(gender_summary)

# Aggregate by major
major_summary <- aggregate(. ~ major, data = data[, c("major", "quiz1", "quiz2", "quiz3", "quiz4", "quiz5")], FUN = mean)
print(major_summary)

# Boxplot for quiz scores by gender
boxplot(quiz1 ~ gender, data = data, main = "Quiz 1 Scores by Gender", xlab = "Gender", ylab = "Score")

# Boxplot for quiz scores by major
boxplot(quiz1 ~ major, data = data, main = "Quiz 1 Scores by Major", xlab = "Major", ylab = "Score")

# Line plot to show improvement of scores with time
average_scores <- aggregate(data[, 4:8], by = list(data$gender), FUN = mean)
quiz_means <- colMeans(data[, 4:8])
quiz_names <- names(data[, 4:8])
plot(quiz_means, type = "o", xaxt = "n", main = "Average Quiz Scores Over Time", xlab = "Quiz", ylab = "Average Score")
axis(1, at = 1:5, labels = quiz_names)

# Visualization of gender-based performance trends
data_long <- data %>%
  pivot_longer(cols = starts_with("quiz"), names_to = "QuizNumber", values_to = "Score")

ggplot(data_long, aes(x = QuizNumber, y = Score, fill = major)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 1, alpha = 0.7) +
  labs(title = "Quiz Score Distribution by Major and Quiz", x = "Quiz Number", y = "Quiz Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  facet_wrap(~ major, scales = "free_y")

# Fit the Base Model
base_model <- lm(Score ~ QuizNumber + gender + major, data = data_long)
summary(base_model)

# Variance Inflation Factor (VIF)
library(car)
vif(base_model)

# Correlations for numeric predictors
cor(data_long[, sapply(data_long, is.numeric)])

# Fit the Interaction Model
interaction_model <- lm(Score ~ QuizNumber * gender + QuizNumber * major + gender * major, data = data_long)
summary(interaction_model)

# Residual diagnostics for the base model
par(mfrow = c(2, 2))
plot(base_model)

# Breusch-Pagan test
library(lmtest)
bptest(base_model)

# Durbin Watson Test
durbinWatsonTest(base_model)

# Introducing a lagged term for quiz scores
data_long$LaggedScore <- lag(data_long$Score)
base_model_lagged <- lm(Score ~ QuizNumber + LaggedScore + gender + major, data = data_long)
summary(base_model_lagged)

# Fit the Polynomial Model
polynomial_model <- lm(Score ~ poly(QuizNumber, 2) * gender * major, data = data_long)
summary(polynomial_model)

# Extract influential points
influential_points <- c(38, 148, 150)
data_long[influential_points, ]

# Fit the model without influential points
model_no_influential <- lm(Score ~ QuizNumber + gender + major, data = data_long[-influential_points, ])
summary(model_no_influential)

# Compare models using AIC
AIC(base_model, interaction_model, polynomial_model)

# Overall trend in quiz scores
ggplot(data_long, aes(x = QuizNumber, y = Score)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Trend in Quiz Scores Over Time", x = "Quiz Number (Time)", y = "Score")

# Trend by gender
ggplot(data_long, aes(x = QuizNumber, y = Score, color = gender)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Quiz Score Trends by Gender", x = "Quiz Number (Time)", y = "Score")

# Trend by major
ggplot(data_long, aes(x = QuizNumber, y = Score, color = major)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Quiz Score Trends by Major", x = "Quiz Number (Time)", y = "Score")
