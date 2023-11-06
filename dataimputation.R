library(tidymodels)
library(tidyverse)


# setwd("C:/Users/rileyw/Ghosts-Ghouls-and-Goblins/")


missing <- read_csv("trainWithMissingValues.csv")
train <- read_csv("train.csv")

is.na(missing$bone_length) %>%
  sum()
missing$rotting_flesh %>%
  is.na() %>%
  sum()
missing$hair_length %>%
  is.na() %>%
  sum()
missing$has_soul %>%
  is.na() %>%
  sum()
missing$color %>%
  is.na() %>%
  sum()

missing <- missing %>% select(-id)


missing_recipe <- recipe(type ~ ., data = missing) %>%
  step_string2factor(c("color", "type")) %>%
  step_impute_knn(hair_length) %>%
  step_impute_knn(rotting_flesh) %>%
  step_impute_knn(bone_length)


prep(missing_recipe) %>%
  bake(new_data = missing) -> imputed

train <- train %>% select(-id)
rmse_vec(train[is.na(missing)], imputed[is.na(missing)])


