# Load Netflix dataset
netflix<- read.csv("netflix_titles.csv")

# View first 6 rows
head(netflix)

# Number of rows and columns
dim(netflix)

# Column names
names(netflix)

# Dataset structure
str(netflix)

# Check missing values in each column
colSums(is.na(netflix))

# Check duplicate rows
sum(duplicated(netflix))

# Number of rows
nrow(netflix)

# Number of columns
ncol(netflix)

# Dataset structure
str(netflix)

library(janitor)

netflix <- clean_names(netflix)

names(netflix)

install.packages("ggplot2")
library(ggplot2)

ggplot(netflix, aes(x = type)) +
  geom_bar() +
  labs(
    title = "Distribution of Movies and TV Shows on Netflix",
    x = "Content Type",
    y = "Number of Titles"
  ) +
  theme_minimal()

yearly_content <- as.data.frame(table(netflix$release_year))
names(yearly_content) <- c("release_year", "number_of_titles")
yearly_content$release_year <- as.numeric(as.character(yearly_content$release_year))
library(ggplot2)
ggplot(yearly_content, aes(x = release_year, y = number_of_titles)) +
  geom_line() +
  labs(
    title = "Number of Netflix Titles by Release Year",
    x = "Release Year",
    y = "Number of Titles"
  ) +
  theme_minimal()

movie_data <- subset(netflix, type == "Movie")
movie_data$duration_min <- as.numeric(gsub(" min", "", movie_data$duration))
ggplot(movie_data, aes(x = duration_min)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Distribution of Movie Durations on Netflix",
    x = "Duration (Minutes)",
    y = "Number of Movies"
  ) +
  theme_minimal()
sum(is.na(movie_data$duration_min))
movie_duration <- movie_data[!is.na(movie_data$duration_min), ]
ggplot(movie_duration, aes(x = duration_min)) +
  geom_histogram(binwidth = 10) +
  labs(
    title = "Distribution of Movie Durations on Netflix",
    x = "Duration (Minutes)",
    y = "Number of Movies"
  ) +
  theme_minimal()

ggplot(movie_duration, aes(x = release_year, y = duration_min)) +
  geom_point(alpha = 0.4) +
  labs(
    title = "Relationship Between Release Year and Movie Duration",
    x = "Release Year",
    y = "Movie Duration (Minutes)"
  ) +
  theme_minimal()

# Remove rows where country is missing
country_data <- netflix[!is.na(netflix$country), ]

# Split multiple countries
country_list <- strsplit(country_data$country, ", ")

# Convert list into individual country names
country_names <- unlist(country_list)

# Count each country
country_counts <- as.data.frame(table(country_names))

# Rename columns
names(country_counts) <- c("country", "number_of_titles")

# Sort by number of titles
top_countries <- country_counts[
  order(-country_counts$number_of_titles),
]

# Select top 10
top_countries <- head(top_countries, 10)

# Create chart
ggplot(
  top_countries,
  aes(
    x = reorder(country, number_of_titles),
    y = number_of_titles
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Countries by Number of Netflix Titles",
    x = "Country",
    y = "Number of Titles"
  ) +
  theme_minimal()

rating_counts <- as.data.frame(table(netflix$rating))

names(rating_counts) <- c("rating", "number_of_titles")

rating_counts <- rating_counts[
  order(-rating_counts$number_of_titles),
]

ggplot(rating_counts, aes(
  x = reorder(rating, number_of_titles),
  y = number_of_titles
)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribution of Netflix Titles by Content Rating",
    x = "Content Rating",
    y = "Number of Titles"
  ) +
  theme_minimal()
