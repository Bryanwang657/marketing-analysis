
library(readxl)
library(dplyr)

# Read the ratings data
ratings <- readxl::read_excel("Ratings.xls", sheet = "Ratings")

print(colnames(ratings))


configs <- data.frame(
  ProfileNum = 1:8,
  HP5 = c(1, 0, 1, 0, 1, 0, 0, 1),  # HP4 is baseline
  BrandToro = c(0, 1, 1, 0, 0, 0, 1, 1),  # BrandSears is baseline
  Swath22 = c(1, 1, 0, 0, 1, 0, 1, 0),  # Swath20 is baseline
  Price250 = c(1, 0, 1, 0, 0, 1, 1, 0)  # Price400 is baseline
)

# Merge dummy variables and score data
ratings <- merge(ratings, configs, by.x = "Profile #", by.y = "ProfileNum")

# Fit the husband's regression model
model_husbands <- lm(`Husband Rating` ~ HP5 + BrandToro + Swath22 + Price250, data = ratings)
summary(model_husbands)

# Fit the wife's regression model
model_wives <- lm(`Wife Rating` ~ HP5 + BrandToro + Swath22 + Price250, data = ratings)
summary(model_wives)

