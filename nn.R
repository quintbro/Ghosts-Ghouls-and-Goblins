library(tidymodels)
library(tidyverse)
library(vroom)
library(embed)
library(keras)

# setwd("C:/Users/rileyw/Ghosts-Ghouls-and-Goblins/")

train <- read_csv("train.csv")
test <- read_csv("test.csv")

train %>%
  mutate(type = as_factor(type)) -> train

ggg_recipe <- recipe(type ~ ., data = train) %>%
  step_string2factor("color") %>%
  step_lencode_glm(color, outcome = vars(type))



nn_mod <- mlp(hidden_units = tune(),
              epochs = 50) %>%
  set_engine("nnet") %>%
  set_mode("classification")


nn_wf <- workflow() %>%
  add_model(nn_mod) %>%
  add_recipe(ggg_recipe)

tuning_grid <- grid_regular(hidden_units(range = c(1, 30)),
                            levels = 10)

folds = vfold_cv(train, v = 5)

cv_results <- nn_wf %>%
  tune_grid(grid = tuning_grid,
            resamples = folds,
            metrics = metric_set(accuracy))

params <- cv_results %>%
  select_best("accuracy")

cv_results %>% collect_metrics() %>%
  filter(.metric == "accuracy") %>%
  ggplot(aes(x = hidden_units, y = mean)) +
  geom_line() -> funplot

ggsave("nn.jpg", funplot)

final_wf <- nn_wf %>%
  finalize_workflow(params) %>%
  fit(train)

preds <- predict(final_wf, new_data = test, type = "class")

preds %>%
  mutate(id = test$id, type = .pred_class) %>%
  select(id, type) %>%
  vroom_write(., "nn.csv", delim = ",")
