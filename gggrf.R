library(tidymodels)
library(tidyverse)
library(embed)
library(vroom)

# setwd("C:/Users/rileyw/Ghosts-Ghouls-and-Goblins/")

train <- read_csv("train.csv")
test <- read_csv("test.csv")

train %>%
  mutate(type = as_factor(type)) -> train

ggg_recipe <- recipe(type ~ ., data = train) %>%
  step_string2factor("color") %>%
  step_lencode_glm(color, outcome = vars(type))

# prep(ggg_recipe) %>%
#   bake(new_data = test)

rf_mod <- rand_forest( mtry = tune(),
                       min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(ggg_recipe)

Grid_tune <- grid_regular(mtry(range = c(1, 5)),
                          min_n(),
                          levels = 10)

folds = vfold_cv(train, v = 5)

cv_results <- rf_wf %>%
  tune_grid(grid = Grid_tune,
            resamples = folds,
            metrics = metric_set(accuracy))

params <- cv_results %>%
  select_best("accuracy")

final_wf <- rf_wf %>%
  finalize_workflow(params) %>%
  fit(train)

preds <- predict(final_wf, new_data = test, type = "class")

preds %>%
  mutate(id = test$id, type = .pred_class) %>%
  select(id, type) %>%
  vroom_write(., "randomforest.csv", delim = ",")

train %>%
  mutate(test = mean(type == 'Goblin')) %>%
ggplot(aes(x = rotting_flesh, y = type)) +
  geom_jitter()
