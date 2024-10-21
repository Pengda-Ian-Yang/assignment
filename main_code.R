
# Input data sets
library(data.table)
books <- fread("books_new.csv", sep = ";", header = TRUE, fill = TRUE)
RatingPGA = read.csv('RatingPGA.csv')
RatingPGB = read.csv('RatingPGB.csv')
users = read.csv('users.csv')


# Q1

library(tidyverse)
head(RatingPGA)
head(books)
head(users)

rating_books = RatingPGA %>% left_join(books, by= "ISBN")

Pub_Rating = aggregate(rating_books$Book.Rating~rating_books$Publisher, rating_books, mean)

Rating_by_order = Pub_Rating[order(Pub_Rating$`rating_books$Book.Rating`, decreasing = TRUE), ]

Top_pub = Rating_by_order[1:20, ]

library(knitr)
table = kable(Top_pub)
table

barplot(Top_pub$`rating_books$Book.Rating`~Top_pub$`rating_books$Publisher`, col='skyblue', main='', xlab='Publisher', ylab='Average Rating')


# Q2

rating_age = merge(RatingPGA, users, by.x = 'User', by.y = 'User.ID', all.x = TRUE)

rating_age_clean = rating_age[!is.na(rating_age$Age), ]

rating_age_clean$age_group = cut(rating_age_clean$Age,
                     breaks = c(0, 18, 30, 45, 60, Inf),
                     labels = c("0-18", "19-30", "31-45", "46-60", "61+"),
                     right = TRUE)

table(rating_age_clean$age_group)

boxplot(rating_age_clean$Book.Rating~rating_age_clean$age_group, data=rating_age_clean, xlab='Age', ylab='Book-Rating')


# Q3

# ratingPGA

rating_locationA = rating_age

rating_location_cleanA = rating_locationA[!is.na(rating_locationA$Location), ]

str(rating_location_cleanA)

library(tidyverse)

rating_location_cleanA$country <- sapply(strsplit(rating_location_cleanA$Location, " "), tail, n = 1)

country_ratingPGA = rating_location_cleanA %>%
  group_by(country) %>%                    
  summarise(mean_rating = mean(Book.Rating)) %>%  
  arrange(desc(mean_rating)) 

country_ratingPGA = country_ratingPGA[-c(1,3,5,8,9), ] # Delete invalid rows

top5_countryA = country_ratingPGA[1:5, ]

# ratingPGB

rating_locationB = merge(RatingPGB, users, by.x = 'User', by.y = 'User.ID', all.x = TRUE)

rating_location_cleanB = rating_locationB[!is.na(rating_locationB$Age), ]

rating_location_cleanB$country <- sapply(strsplit(rating_location_cleanB$Location, " "), tail, n = 1)

country_ratingPGB = rating_location_cleanB %>%
  group_by(country) %>%                    
  summarise(mean_rating = mean(Book.Rating)) %>%  
  arrange(desc(mean_rating)) 

country_ratingPGB = country_ratingPGB[-c(5,6,7), ] # Delete invalid rows

top5_countryB = country_ratingPGB[1:5, ]

# Comparison 2 data sets

par(mfrow = c(1, 2))
barplot(top5_countryA$mean_rating~top5_countryA$country)
barplot(top5_countryB$mean_rating~top5_countryB$country)


# Q4

# RatingPGA

rating_age_year = rating_age_clean %>%
  left_join(books %>% select(ISBN, 'Year-Of-Publication'), by = "ISBN") %>%
  filter('Year-Of-Publication' > 2000) %>%  
  mutate(AgeGroup = case_when(         
    Age <= 18 ~ "0-18",
    Age <= 30 ~ "19-30",
    Age <= 45 ~ "31-45",
    Age <= 60 ~ "46-60",
    TRUE ~ "61+"
  )) %>%
  group_by(AgeGroup) %>%                
  summarise(AverageRating = mean(Book.Rating))

# RatingPGB

rating_ageB = merge(RatingPGB, users, by.x = 'User', by.y = 'User.ID', all.x = TRUE)

rating_age_cleanB = rating_ageB[!is.na(rating_ageB$Age), ]

rating_age_yearB = rating_age_cleanB %>%
  left_join(books %>% select(ISBN, 'Year-Of-Publication'), by = "ISBN") %>%
  filter('Year-Of-Publication' > 2000) %>%  
  mutate(AgeGroup = case_when(         
    Age <= 18 ~ "0-18",
    Age <= 30 ~ "19-30",
    Age <= 45 ~ "31-45",
    Age <= 60 ~ "46-60",
    TRUE ~ "61+"
  )) %>%
  group_by(AgeGroup) %>%                
  summarise(AverageRating = mean(Book.Rating))

par(mfrow = c(1, 2))
barplot(rating_age_year$AverageRating~rating_age_year$AgeGroup)
barplot(rating_age_yearB$AverageRating~rating_age_yearB$AgeGroup)































