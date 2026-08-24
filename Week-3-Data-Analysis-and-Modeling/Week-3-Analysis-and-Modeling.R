# Week 3 - Statistical Analysis and Predictive Modeling

# Load dataset
data <- read.csv(
  "wdbc.data",
  header = FALSE
)

# Check dataset
dim(data)
head(data)
getwd()
list.files()
data <- read.csv("wdbc.data", header = FALSE)

dim(data)
head(data)
str(data)
summary(data)
sum(is.na(data))
colnames(data) <- c(
  "id",
  "diagnosis",
  "radius_mean",
  "texture_mean",
  "perimeter_mean",
  "area_mean",
  "smoothness_mean",
  "compactness_mean",
  "concavity_mean",
  "concave_points_mean",
  "symmetry_mean",
  "fractal_dimension_mean",
  "radius_se",
  "texture_se",
  "perimeter_se",
  "area_se",
  "smoothness_se",
  "compactness_se",
  "concavity_se",
  "concave_points_se",
  "symmetry_se",
  "fractal_dimension_se",
  "radius_worst",
  "texture_worst",
  "perimeter_worst",
  "area_worst",
  "smoothness_worst",
  "compactness_worst",
  "concavity_worst",
  "concave_points_worst",
  "symmetry_worst",
  "fractal_dimension_worst"
)

names(data)[1:6]

data$diagnosis <- factor(
  data$diagnosis,
  levels = c("B", "M"),
  labels = c("Benign", "Malignant")
)

table(data$diagnosis)
data$id <- NULL
dim(data)
str(data[, 1:6])

summary(data[, c(
  "radius_mean",
  "texture_mean",
  "perimeter_mean",
  "area_mean",
  "smoothness_mean",
  "compactness_mean"
)])
aggregate(
  radius_mean ~ diagnosis,
  data = data,
  FUN = mean
)
aggregate(
  area_mean ~ diagnosis,
  data = data,
  FUN = mean
)
install.packages("ggplot2")
library(ggplot2)
ggplot(data, aes(x = diagnosis, y = radius_mean)) +
  geom_boxplot() +
  labs(
    title = "Radius Mean by Diagnosis",
    x = "Diagnosis",
    y = "Mean Radius"
  ) +
  theme_minimal()
t_test_result <- t.test(
  radius_mean ~ diagnosis,
  data = data
)

t_test_result
aggregate(
  radius_mean ~ diagnosis,
  data = data,
  FUN = mean
)
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data)

round(cor_matrix[1:10, 1:10], 2)
library(corrplot)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)
cor_pairs <- as.data.frame(as.table(cor_matrix))

cor_pairs <- cor_pairs[
  cor_pairs$Var1 != cor_pairs$Var2,
]

cor_pairs <- cor_pairs[
  order(-abs(cor_pairs$Freq)),
]

head(cor_pairs, 10) 

hist(
  data$radius_mean,
  main = "Distribution of Mean Radius",
  xlab = "Mean Radius",
  ylab = "Frequency"
)
qqnorm(data$radius_mean)
qqline(data$radius_mean)
shapiro.test(data$radius_mean)
install.packages("car")
library(car)
leveneTest(
  radius_mean ~ diagnosis,
  data = data
)

hist(data$radius_mean)

qqnorm(data$radius_mean)
qqline(data$radius_mean)

shapiro.test(data$radius_mean)

leveneTest(radius_mean ~ diagnosis, data = data)
table(data$diagnosis)
data$diagnosis <- relevel(
  data$diagnosis,
  ref = "Benign"
)

set.seed(123)

train_index <- sample(
  1:nrow(data),
  size = floor(0.8 * nrow(data))
)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
dim(train_data)
dim(test_data)
exists("train_data")
exists("test_data")
table(train_data$diagnosis)
table(test_data$diagnosis)

logistic_model <- glm(
  diagnosis ~ radius_mean +
    texture_mean +
    perimeter_mean +
    area_mean +
    smoothness_mean +
    compactness_mean +
    concavity_mean +
    concave_points_mean +
    symmetry_mean +
    fractal_dimension_mean,
  data = train_data,
  family = binomial
)
summary(logistic_model)
predicted_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)
head(predicted_prob)
predicted_class <- ifelse(
  predicted_prob >= 0.5,
  "Malignant",
  "Benign"
)

predicted_class <- factor(
  predicted_class,
  levels = levels(test_data$diagnosis)
)
head(predicted_class)
summary(logistic_model)

library(caret)
conf_matrix <- confusionMatrix(
  predicted_class,
  test_data$diagnosis,
  positive = "Malignant"
)

conf_matrix
predictions <- predict(logistic_model, newdata = test_data, type = "response")
pred_class <- ifelse(predictions >= 0.5, "M", "B")
pred_class <- factor(pred_class, levels = c("B", "M"))
actual <- factor(test_data$diagnosis, levels = c("B", "M"))
length(pred_class)
length(actual)
library(caret)

result_cm <- confusionMatrix(
  data = pred_class,
  reference = actual
)

print("result_cm")
result_cm$overall["Accuracy"]
result_cm$byClass["Sensitivity"]
result_cm$byClass["Specificity"]
accuracy_percentage <- result_cm$overall["Accuracy"] * 100
accuracy_percentage

library(ggplot2)

cm_table <- as.data.frame(result_cm$table)

ggplot(cm_table, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), size = 6) +
  labs(
    title = "Confusion Matrix",
    x = "Actual Diagnosis",
    y = "Predicted Diagnosis"
  ) +
  theme_minimal()

table(actual)
levels(actual)
table(pred_class)

nrow(test_data)
table(test_data$diagnosis)
unique(test_data$diagnosis)

actual <- factor(
  test_data$diagnosis,
  levels = c("Benign", "Malignant")
)

table(actual)

predictions <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

pred_class <- ifelse(
  predictions >= 0.5,
  "Malignant",
  "Benign"
)

pred_class <- factor(
  pred_class,
  levels = c("Benign", "Malignant")
)

table(pred_class)

library(caret)

result_cm <- confusionMatrix(
  data = pred_class,
  reference = actual,
  positive = "Malignant"
)

print(result_cm)

result_cm$overall["Accuracy"]
result_cm$byClass["Sensitivity"]
result_cm$byClass["Specificity"] 

library(pROC)

roc_result <- roc(
  response = actual,
  predictor = predictions,
  levels = c("Benign", "Malignant"),
  direction = "<"
)

plot(
  roc_result,
  main = "ROC Curve"
)

auc_value <- auc(roc_result)
auc_value

auc_percentage <- as.numeric(auc_value) * 100
auc_percentage

accuracy <- as.numeric(result_cm$overall["Accuracy"]) * 100
sensitivity <- as.numeric(result_cm$byClass["Sensitivity"]) * 100
specificity <- as.numeric(result_cm$byClass["Specificity"]) * 100
auc <- as.numeric(auc_value)

cat("Model Performance Summary\n")
cat("-------------------------\n")
cat("Accuracy    :", round(accuracy, 2), "%\n")
cat("Sensitivity :", round(sensitivity, 2), "%\n")
cat("Specificity :", round(specificity, 2), "%\n")
cat("AUC         :", round(auc, 4), "\n")

ggplot(data, aes(x = diagnosis)) +
  geom_bar() +
  labs(
    title = "Distribution of Diagnosis",
    x = "Diagnosis",
    y = "Count"
  ) +
  theme_minimal()

numeric_data <- data[sapply(data, is.numeric)]

cor_matrix <- cor(
  numeric_data,
  use = "complete.obs"
)

corrplot(
  cor_matrix,
  method = "color",
  type = "upper",
  tl.cex = 0.7,
  tl.col = "black"
)

cor_pairs <- as.data.frame(as.table(cor_matrix))

names(cor_pairs) <- c("Variable1", "Variable2", "Correlation")

strong_cor <- subset(
  cor_pairs,
  abs(Correlation) >= 0.7 &
    Variable1 != Variable2
)

strong_cor

ggplot(
  strong_cor,
  aes(
    x = reorder(
      paste(Variable1, Variable2, sep = " - "),
      Correlation
    ),
    y = Correlation
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Strongly Correlated Variables",
    x = "Variable Pair",
    y = "Correlation"
  ) +
  theme_minimal()

plot(
  roc_result,
  main = "ROC Curve - Logistic Regression",
  col = "blue",
  lwd = 2
)

abline(
  a = 0,
  b = 1,
  lty = 2
)

legend(
  "bottomright",
  legend = paste("AUC =", round(as.numeric(auc_value), 4)),
  bty = "n"
)

cm_data <- as.data.frame(result_cm$table)

ggplot(
  cm_data,
  aes(
    x = Reference,
    y = Prediction,
    fill = Freq
  )
) +
  geom_tile() +
  geom_text(
    aes(label = Freq),
    size = 6
  ) +
  labs(
    title = "Confusion Matrix - Logistic Regression",
    x = "Actual Diagnosis",
    y = "Predicted Diagnosis"
  ) +
  theme_minimal()
