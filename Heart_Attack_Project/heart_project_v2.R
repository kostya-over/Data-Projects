library(tidyverse)
library(skimr)
library(GGally)
library(paradox)
library(corrplot)

df <- read.csv("Documents/grad_project/heart.csv")

# Exploring data

head(df)
skim(df)
dim(df)
str(df)
summary(df)

#Identifying Outliers

col <- c("age", "trtbps", "chol", "thalachh", "oldpeak")
boxplot(df[, col])


#Handling outliers

for (x in c("trtbps", "chol", "thalachh", "oldpeak")){
  value <- df[, x][df[, x] %in% boxplot.stats(df[, x])$out]
  df[, x][df[, x] %in% value] <- NA
}

sum(is.na(df$trtbps))
sum(is.na(df$chol))
sum(is.na(df$thalachh))
sum(is.na(df$oldpeak))

as.data.frame(colSums(is.na(df)))
colSums(is.na(df))

#. Removing outliers

library(tidyr)
df <- drop_na(df)
as.data.frame(colSums(is.na(df)))

boxplot(df[, col])

# EDA

fill_color <- "#02bfc4"
curve_color <- "#f7766d"

hist1 <- ggplot(df, aes(x = age)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = fill_color,
                 color = "black") +
  geom_density(
    color = curve_color) +
  labs(y = "Density",
       title = "Distribution of Age") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

hist2 <- ggplot(df, aes(x = trtbps)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = fill_color,
                 color = "black") +
  geom_density(
    color = curve_color) +
  labs(y = "Density",
       title = "Blood pressure (in mm Hg)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

hist3 <- ggplot(df, aes(x = chol)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = fill_color,
                 color = "black") +
  geom_density(
    color = curve_color) +
  labs(y = "Density",
       title = "Cholestoral in mg/dl") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

hist4 <- ggplot(df, aes(x = thalachh)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = fill_color,
                 color = "black") +
  geom_density(
    color = curve_color) +
  labs(y = "Density",
       title = "Maximum heart rate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

hist5 <- ggplot(df, aes(x = oldpeak)) +
  geom_histogram(aes(y = ..density..),
                 bins = 20,
                 fill = fill_color,
                 color = "black") +
  geom_density(
    color = curve_color) +
  labs(y = "Density",
       title = "ST depression") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

library("cowplot")
plot_grid(hist1, hist2, hist3, hist4, hist5, 
          ncol = 2, nrow = 3)

plot0 <- ggplot(data = df, aes(x = factor(sex), fill = factor(sex))) +
  geom_bar() +
  labs(x = "Gender", y = "Count", fill = "Gender") +
  ggtitle("Distribution of Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot1 <- ggplot(data = df, aes(x = factor(cp), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Chest Pain Type", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot2 <- ggplot(data = df, aes(x = factor(fbs), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Fasting Flood Sugar", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot3 <- ggplot(data = df, aes(x = factor(restecg), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Resting Electrocardiographic Results", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot4 <- ggplot(data = df, aes(x = factor(exng), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Exercise Induced Angina", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot5 <- ggplot(data = df, aes(x = factor(slp), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Slope", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot6 <- ggplot(data = df, aes(x = factor(caa), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Number of major vessels", y = "Count", fill = "Gender") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot7 <- ggplot(data = df, aes(x = factor(thall), fill = factor(sex))) +
  geom_bar(position = "dodge") +
  facet_grid(rows = vars(sex)) +
  labs(x = "Thal rate", y = "Count") +
  scale_fill_discrete(labels = c("Female", "Male"))

plot_grid(plot0, plot1, plot2, plot3, plot4, plot5, plot6, plot7, 
          ncol = 3, nrow = 3)

# Visualization of Correlation between different variables

cormat <- cor(cbind(df))
corrplot(cormat, method = "number")

# A/B testing
library(stats)

females <- df[df$sex == 0, ]
males <- df[df$sex == 1, ]

prop_female <- sum(females$output) / nrow(females)
prop_male <- sum(males$output) / nrow(males)

prop.test(x = c(sum(females$output), sum(males$output)),
          n = c(nrow(females), nrow(males)),
          alternative = "two.sided",
          conf.level = 0.95)



# Modeling
library(rsample)
library(caret)

df$output <- as.factor(df$output)
levels(df$output) <- c("LessLikely", "MoreLikely")

set.seed(1353)
split <- initial_split(df, prop = 0.8, strata = output)
train <- training(split)
test <- testing(split)

# Train a logistic regression model

train_control <- trainControl(method = "cv", number = 10)
model_cv <- train(output ~ ., data = train, method = "glm",
                  family = "binomial", trControl = train_control)

final_model <- model_cv$finalModel
summary(final_model)


predictions <- predict(model_cv, newdata = test, type = "prob")
predicted_classes <- ifelse(predictions[,2] > 0.5, "MoreLikely", "LessLikely")

# Confusion Matrix
confusionMatrix(as.factor(predicted_classes), test$output)

library(pROC)
roc_curve <- roc(test$output, predictions[,2])
plot(roc_curve)
auc(roc_curve)



#Tuned model
# Define hyperparameter grid for glmnet
hyper_grid <- expand.grid(alpha = c(0, 0.5, 1),    # 0 = ridge, 1 = lasso, 0.5 = elastic net
                          lambda = seq(0.001, 0.1, length = 10))

# Set up cross-validation control
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# Train the model using the grid search
tuned_model <- train(output ~ ., data = train, 
                     method = "glmnet",
                     trControl = train_control,
                     tuneGrid = hyper_grid,
                     family = "binomial")

# View the best tuning parameters
print(tuned_model$bestTune)

# Make predictions on the test set
tuned_predictions <- predict(tuned_model, newdata = test, type = "prob")

# Convert predictions to binary class (0 or 1) based on a threshold, usually 0.5
tuned_predicted_classes <- ifelse(tuned_predictions[,2] > 0.5, "MoreLikely", "LessLikely")

# Confusion Matrix
tuned_conf_matrix <- confusionMatrix(as.factor(tuned_predicted_classes), test$output)
print(tuned_conf_matrix)

# ROC Curve and AUC
roc_curve_tuned <- roc(test$output, tuned_predictions[,2])
plot(roc_curve_tuned)
auc_value_tuned <- auc(roc_curve_tuned)
print(auc_value_tuned)
