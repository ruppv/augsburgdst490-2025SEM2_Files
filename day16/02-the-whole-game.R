### DST 490, Day 16
### Topic: testing different types of classification models
### Decision trees, random forests, k-nearest neighbors, naive Bayes, neural networks
### This R file incorporates all the models in a unified workflow

library(tidyverse)
library(tidymodels)
tidymodels_prefer()   ### Set any conflicts to prefer tidymodels

library(rpart)  ## Decision trees
library(partykit)  ## Decision trees
library(randomForest) # Random forests
library(kknn)  # Knearest neighbors
library(nnet)  # neural networks
library(discrim)  # Naive Bayes
library(klaR)  # Naive Bayes

### Load in the data
data_file <- here::here("day16","data","employment_education_all.Rda")
load(data_file)

### STEP 0: Set up the data for modeling
### Set the unemployment threshold for the model
unemployment_thresh <- 6  

### Create a new indicator variable for logistic regression
unemployment_education_data_new <- unemployment_education_all_data |>
  mutate(above_indicator = (unemployment_rate > unemployment_thresh),
         above_indicator = as.factor(above_indicator)) |>  # Set as a factor for regression
  na.omit()

### STEP 1: split the data into testing and training (spending the data budget)
set.seed(1501)  ## So we can replicate this later (seed number can be modified)

### We will do a stratified sample along the rural, urban continuum
unemployment_parts <- unemployment_education_data_new |>
  initial_split(prop = 0.8,strata = unemployment_rate)

# Identify the training and testing sets as separate data frames
train <- unemployment_parts |>
  training()

test <- unemployment_parts |>
  testing()

### STEP 2: Define our formulas for regression:
null_regression_formula <- as.formula("above_indicator ~ 1")

regression_formula <- as.formula(
  "above_indicator ~ less_than_hs + hs_only + some_college_associates +
  bachelors_plus"
)

### STEP 3: Set the different model engines
mod_null <- logistic_reg(mode = "classification") |>
  set_engine("glm")   ### Here we use the formula that above_indicator ~ 1

mod_logistic <- logistic_reg(mode = "classification") |>
  set_engine("glm")

mod_dtree <- decision_tree(mode = "classification") |>
  set_engine("rpart")

mod_forest <- rand_forest(mode = "classification", mtry = 2) |>
  set_engine("randomForest")

mod_knn <- nearest_neighbor(neighbors = 5, mode = "classification") |>
  set_engine("kknn", scale = TRUE)

mod_nn <- mlp(mode = "classification", hidden_units = 3) |>
  set_engine("nnet") 

mod_nb <- naive_Bayes(mode = "classification") |>
  set_engine("klaR")



### STEP 4: Combine models, formula, and data into a nested list structure

models <- tibble(
  type = c(
    "null", "logistic", "tree", "forest",
    "knn", "neural_net", "naive_bayes"
  ),
  mod = list(
    mod_null, mod_logistic, mod_dtree, mod_forest,
    mod_knn, mod_nn, mod_nb
  ),
  formula = list(null_regression_formula, regression_formula, regression_formula, regression_formula, regression_formula, regression_formula, regression_formula),
  train_above_indicator = list(pull(train,above_indicator)),  # Include the training data
  test_above_indicator = list(pull(test,above_indicator))
)

### STEP 5: Fit the models --> note the use of map!
models_fit <- models |>
  mutate(fit = map2(.x = mod,
                    .y = formula,
                    .f=~fit(.x,.y,data = train)))

### STEP 6: Predict on both the training and testing set (indicated with p_)
models_predict <- models_fit |>
  mutate(p_train_above_indicator = map(.x = fit,
                                      .f = ~pull(predict(.x, new_data = train, type = "class"), .pred_class)),
         p_test_above_indicator = map(.x = fit,
                                      .f = ~pull(predict(.x, new_data = test, type = "class"), .pred_class)))


### STEP 7: Compute the accuracy and the sensitivity for each model
models_results <- models_predict |>
  mutate(
    accuracy_train = map2_dbl(train_above_indicator, p_train_above_indicator, accuracy_vec),
    accuracy_test = map2_dbl(test_above_indicator, p_test_above_indicator, accuracy_vec),
    sens_test = map2_dbl(
      test_above_indicator,
      p_test_above_indicator,
      sens_vec,
      event_level = "second"
    ),
    spec_test = map2_dbl(test_above_indicator,
                         p_test_above_indicator,
                         spec_vec,
                         event_level = "second"
    )
  ) |>
  select(type,accuracy_train,accuracy_test,sens_test,spec_test)

### STEP 8: Generate the ROC curve
models_roc <- models_fit |>
  mutate(
    p_test_above_indicator_prob = map(
      .x=fit, 
      .f=~pull(predict(.x, new_data = test, type = "prob"),.pred_TRUE)
    )) |>
  select(type,test_above_indicator,p_test_above_indicator_prob)

### Now plot it all - yay!
models_roc |>
  select(type, test_above_indicator, p_test_above_indicator_prob) |>
  unnest(cols = c(test_above_indicator, p_test_above_indicator_prob)) |>
  group_by(type) |>
  roc_curve(truth = test_above_indicator, p_test_above_indicator_prob, event_level = "second") |>
  autoplot() + 
  geom_point(
    data = models_results, 
    aes(x = 1 - spec_test, y = sens_test, color = type), 
    size = 3
  )

models_roc |>
  select(type, test_above_indicator, p_test_above_indicator_prob) |>
  unnest(cols = c(test_above_indicator, p_test_above_indicator_prob)) |>
  group_by(type) |>
  roc_auc(truth = test_above_indicator, p_test_above_indicator_prob,event_level = "second") |> arrange(desc(.estimate))



