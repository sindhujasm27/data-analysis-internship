# Week 1 - Data Cleaning and Preliminary Analysis

print("R is working successfully!")
# Load tidyverse
library(tidyverse)

# Download Titanic dataset
url <- "https://raw.githubusercontent.com/datasciencedojo/datasets/master/titanic.csv"

titanic <- read_csv(url)

# View first 6 rows
head(titanic)
# Check dataset dimensions
dim(titanic)

# Check column names
names(titanic)

# Check data structure
str(titanic)
# Check total missing values
sum(is.na(titanic))

# Count missing values in each column
colSums(is.na(titanic))

# Percentage of missing values in each column
missing_percent <- colSums(is.na(titanic)) / nrow(titanic) * 100

missing_percent

# Create a missing-value summary
missing_summary <- data.frame(
  Column = names(titanic),
  Missing_Count = colSums(is.na(titanic)),
  Missing_Percentage = round(missing_percent, 2)
)

missing_summary 

# Calculate median age
median_age <- median(titanic$Age, na.rm = TRUE)

# Replace missing Age values with median
titanic$Age[is.na(titanic$Age)] <- median_age

# Check Age missing values again
sum(is.na(titanic$Age)) 

# Find the most frequent Embarked value
mode_embarked <- names(sort(table(titanic$Embarked), decreasing = TRUE))[1]

# Replace missing Embarked values
titanic$Embarked[is.na(titanic$Embarked)] <- mode_embarked

# Check missing values
sum(is.na(titanic$Embarked)) 

# Remove Cabin column
titanic <- titanic %>%
  select(-Cabin)

# Check remaining missing values
colSums(is.na(titanic)) 

# Save cleaned dataset
write_csv(titanic, "titanic_cleaned.csv")

# Check for duplicate rows
duplicate_rows <- sum(duplicated(titanic))

duplicate_rows

# Remove duplicate rows
titanic <- titanic %>%
  distinct()

# Check number of rows after removing duplicates
nrow(titanic) 

# Check structure of cleaned dataset
str(titanic)

# Summary statistics
summary(titanic)

# Final dimensions
dim(titanic)

# Final missing-value check
colSums(is.na(titanic))

# Final duplicate check
sum(duplicated(titanic)) 

# Summary statistics for the dataset
summary(titanic)

# Mean age
mean(titanic$Age, na.rm = TRUE)

# Median age
median(titanic$Age, na.rm = TRUE)

# Minimum age
min(titanic$Age, na.rm = TRUE)

# Maximum age
max(titanic$Age, na.rm = TRUE)

# Standard deviation
sd(titanic$Age, na.rm = TRUE)

# Number of passengers by survival status
table(titanic$Survived)

# Percentage of passengers who survived
prop.table(table(titanic$Survived)) * 100

# Survival by gender
table(titanic$Sex, titanic$Survived)

# Survival percentage by gender
prop.table(table(titanic$Sex, titanic$Survived), margin = 1) * 100

# Number of passengers in each class
table(titanic$Pclass)

# Survival by passenger class
table(titanic$Pclass, titanic$Survived) 

# Bar chart of survival status
ggplot(titanic, aes(x = factor(Survived))) +
  geom_bar() +
  labs(
    title = "Titanic Passenger Survival",
    x = "Survival Status",
    y = "Number of Passengers"
  ) 

ggplot(titanic, aes(x = Sex, fill = factor(Survived))) +
  geom_bar() +
  labs(
    title = "Survival Distribution by Gender",
    x = "Gender",
    y = "Number of Passengers",
    fill = "Survived"
  )

ggplot(titanic, aes(x = factor(Pclass), fill = factor(Survived))) +
  geom_bar() +
  labs(
    title = "Survival Distribution by Passenger Class",
    x = "Passenger Class",
    y = "Number of Passengers",
    fill = "Survived"
  )

ggplot(titanic, aes(x = Age)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Age Distribution of Titanic Passengers",
    x = "Age",
    y = "Number of Passengers"
  )

ggplot(titanic, aes(x = Fare)) +
  geom_histogram(bins = 30) +
  labs(
    title = "Fare Distribution",
    x = "Fare",
    y = "Number of Passengers"
  )

ggplot(titanic, aes(x = Age, y = Fare)) +
  geom_point(alpha = 0.5) +
  labs(
    title = "Relationship Between Age and Fare",
    x = "Age",
    y = "Fare"
  )

write_csv(titanic, "titanic_cleaned.csv")

# Overall survival rate
survival_rate <- mean(titanic$Survived) * 100
survival_rate

# Survival rate by gender
gender_survival <- titanic %>%
  group_by(Sex) %>%
  summarise(
    Passengers = n(),
    Survivors = sum(Survived),
    Survival_Rate = mean(Survived) * 100
  )

gender_survival 

class_survival <- titanic %>%
  group_by(Pclass) %>%
  summarise(
    Passengers = n(),
    Survivors = sum(Survived),
    Survival_Rate = mean(Survived) * 100
  )

class_survival

age_survival <- titanic %>%
  group_by(Survived) %>%
  summarise(
    Average_Age = mean(Age, na.rm = TRUE),
    Median_Age = median(Age, na.rm = TRUE)
  )

age_survival

fare_class <- titanic %>%
  group_by(Pclass) %>%
  summarise(
    Average_Fare = mean(Fare, na.rm = TRUE),
    Median_Fare = median(Fare, na.rm = TRUE)
  )

fare_class  

cat("Number of rows:", nrow(titanic), "\n")
cat("Number of columns:", ncol(titanic), "\n")
cat("Total missing values:", sum(is.na(titanic)), "\n")
cat("Duplicate rows:", sum(duplicated(titanic)), "\n")

survival_rate
gender_survival
class_survival
age_survival