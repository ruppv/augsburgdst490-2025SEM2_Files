### DST 490, Day 16
### Topic: testing different types of classification models
### Decision trees, random forests, k-nearest neighbors, naive Bayes, neural networks

library(tidyverse)
library(tidymodels)
tidymodels_prefer()   ### Set any conflicts to prefer tidymodels

# Here are packages you might need to install:
library(rpart)  # Decision trees
library(partykit)  # Decision trees
library(rpart.plot) # for plotting decision trees
library(randomForest) # Random forests
library(discrim)  # Naive Bayes
library(klaR)  # Naive Bayes
library(kknn)  # Knearest neighbors
library(nnet)  # neural networks



### Load in the data
data_file <- here::here("day16","data","employment_education_all.Rda")
load(data_file)


### Set the unemployment threshold for the model
unemployment_thresh <- 6  

### Create a new indicator variable for logistic regression
unemployment_education_data_new <- unemployment_education_all_data |>
  mutate(above_indicator = (unemployment_rate > unemployment_thresh),
         above_indicator = as.factor(above_indicator)) |>  # Set as a factor for regression
  na.omit()

### Step 1: split the data into testing and training (spending the data budget)
set.seed(1501)  ## So we can replicate this later (seed number can be modified)

### We will do a stratified sample along the rural, urban continuum
unemployment_parts <- unemployment_education_data_new |>
  initial_split(prop = 0.8,strata = unemployment_rate)

# Identify the training and testing sets as separate data frames
train <- unemployment_parts |>
  training()

test <- unemployment_parts |>
  testing()


# Define our formula for regression:
regression_formula <- as.formula(
  "above_indicator ~ less_than_hs + hs_only + some_college_associates +
+   bachelors_plus"
)


### Method 1: Decision trees
mod_dtree <- decision_tree(mode = "classification") |>
  set_engine("rpart") |>
  fit(regression_formula, data = train)

# View a text of the decision tree
mod_dtree

# See the factor order for yprob:
levels(train$above_indicator)

# Visualize the fit as a tree
rpart.plot::rpart.plot(mod_dtree$fit)


# Add on a column for the predicted values
pred_tree <- train |>  
  bind_cols(predict(mod_dtree, train)) |> 
  rename(above_indicator_plus = .pred_class)

# Compute accuracy of the new model
accuracy(pred_tree,above_indicator,above_indicator_plus)

# Confusion matrix
pred_tree |>
  conf_mat(truth = above_indicator, estimate = above_indicator_plus)

### Method 2: Random forests
mod_forest <- rand_forest(
  mode = "classification",
  mtry = 2,  # Number of predictors randomly sampled at each split
  trees = 100   # number of trees
) |>
  set_engine("randomForest") |>
  fit(regression_formula, data = train)

# extract tree number 1 and glimpse the result
tree_1 <- randomForest::getTree(mod_forest$fit, k = 1, labelVar = TRUE)

glimpse(tree_1)

# Add on a column for the predicted values
pred_forest <-  train |>  
  bind_cols(predict(mod_forest, train)) |> 
  rename(above_indicator_plus = .pred_class)

# Compute accuracy of the new model
accuracy(pred_forest,above_indicator,above_indicator_plus)  ## 100%!

# Confusion matrix
pred_forest |>
  conf_mat(truth = above_indicator, estimate = above_indicator_plus)

# List out the variables that seem to have importance in fitting
randomForest::importance(mod_forest$fit)

# Make a plot
randomForest::varImpPlot(mod_forest$fit)



### Method 3: Naive Bayes
mod_nb <- naive_Bayes(mode = "classification") |>
  set_engine("klaR") |>  # Klassifikation und Regressionsanalyse for Naive Bayes
  fit(regression_formula, data = train)

# Add on a column for the predicted values
pred_nb <-  train |>  
  bind_cols(predict(mod_nb, train)) |> 
  rename(above_indicator_plus = .pred_class)

# Compute accuracy of the new model
accuracy(pred_nb,above_indicator,above_indicator_plus)


# Confusion matrix
pred_nb |>
  conf_mat(truth = above_indicator, estimate = above_indicator_plus)


### Method 4: K nearest neighbors
# distance metric only works with quantitative variables

mod_knn <- nearest_neighbor(neighbors = 5, mode = "classification") |>
  set_engine("kknn", scale = TRUE) |>  # k Nearest Neighbors, set to a common scale
  fit(regression_formula, data = train)

# Add on a column for the predicted values
pred_knn <-  train |>  
  bind_cols(predict(mod_knn, train)) |> 
  rename(above_indicator_plus = .pred_class)

# Compute accuracy of the new model
accuracy(pred_knn,above_indicator,above_indicator_plus)  

# Confusion matrix
pred_knn |>
  conf_mat(truth = above_indicator, estimate = above_indicator_plus)


### Method 5: Artificial neural networks
mod_nn <- mlp(mode = "classification", hidden_units = 3) |>
  set_engine("nnet") |>
  fit(regression_formula, data = train)

# Add on a column for the predicted values
pred_nn <-  train |>  
  bind_cols(predict(mod_nn, train)) |> 
  rename(above_indicator_plus = .pred_class)

# Compute accuracy of the new model
accuracy(pred_nn,above_indicator,above_indicator_plus)

# Confusion matrix
pred_nn |>
  conf_mat(truth = above_indicator, estimate = above_indicator_plus)

