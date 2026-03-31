  ### DST 490, Day 17
  ### Topic: V fold validation, and correlation plots
  
  
  library(tidyverse)
  library(tidymodels)
  tidymodels_prefer()   ### Set any conflicts to prefer tidymodels
  
  library(randomForest) # Random forests
  library(corrplot)  ## Correlation plots
  
  ### Load in the data
  data_file <- here::here("day17","employment_education_all.Rda")
  load(data_file)
  
  
  ### Set the unemployment threshold for the model
  unemployment_thresh <- 6
  
  ### Create a new indicator variable for logistic regression
  unemployment_education_data_new <- unemployment_education_all_data |>
    mutate(above_indicator = (unemployment_rate > unemployment_thresh),
           above_indicator = as.factor(above_indicator)) |>  # Set as a factor for regression
    na.omit()
  
  ### Step 1: split the data into testing and training (spending the data budget)
  set.seed(2501)  ## So we can replicate this later (seed number can be modified)
  
  ### We will do a stratified sample along unemployment rate
  unemployment_parts <- unemployment_education_data_new |>
    initial_split(prop = 0.8,strata = unemployment_rate)
  
  # Identify the training and testing sets as separate data frames
  train <- unemployment_parts |>
    training()
  
  test <- unemployment_parts |>
    testing()
  
  
  # Step 2: Do the v fold partitioning
  # Here we are further partitioning the training set into 10 different subsamples of
  # assessment and analysis
  county_folds <- vfold_cv(train, v = 10)
  
  # Preview what the folds look like
  county_folds
  
  # This next function just sets some diagnostics on the folds
  keep_pred <- control_resamples(save_pred = TRUE,
                                 save_workflow = TRUE)
  
  # Step 3: Define our model workflow
  
  # Define our formula for regression:
  regression_formula <- as.formula(
    "above_indicator ~ less_than_hs + hs_only + some_college_associates +
    bachelors_plus"
  )
  
  # Define a workflow set - which is a more general approach to modeling
  # See Tidymodeling with R, Chapter 7 https://www.tmwr.org/workflows
  
  # Set up the model
  rf_model <-
    rand_forest(
      mode = "classification", mtry = 2) |>
    set_engine("randomForest")
  
  # Define the workflow, adding the regression formula and model
  rf_wflow <-
    workflow() |> # Define a workflow
    add_formula(regression_formula) |>
    add_model(rf_model)
  
  
  # Step 4: fit the model to each of the workflows
  forest_resample <-
    rf_wflow |>
    fit_resamples(
      resamples = county_folds, control = keep_pred)
  
  # Step 5: Analyze metrics (BOTH analysis and assessment set)
  
  # The overall accuracy on the subsamples:
  collect_metrics(forest_resample)
  
  # The accuracy for for each resample
  collect_metrics(forest_resample, summarize = FALSE)
  
  # Pull out the individual model predictions on the analysis set
  assessment_results <- collect_predictions(forest_resample)
  
  assessment_results |> glimpse()
  
  ### STEP 7: Compute the accuracy and the sensitivity for each model
  assessment_results_summary <- assessment_results |>
    group_by(id) |>
    summarize(sensitivity = sens_vec(above_indicator,.pred_class,event_level = "second"),
              specificity = spec_vec(above_indicator,.pred_class,event_level = "second")
              )
  
  # Now we can do an ROC curve for each sample
  assessment_results |>
    group_by(id) |>
    roc_curve(truth = above_indicator, .pred_TRUE, event_level = "second") |>
    autoplot() +
    geom_point(
      data = assessment_results_summary,
      aes(x = 1 - specificity, y = sensitivity, color = id),
      size = 3
    )
  
  #### Exploring the predictor variables more
  
  # Correlation plot (uses library(corrplot))
  tmwr_cols <- colorRampPalette(c("#91CBD765", "#CA225E"))
  train |>
    select(less_than_hs,hs_only,some_college_associates,bachelors_plus) |>
    cor() |>
    corrplot(col = tmwr_cols(200), tl.col = "black", method = "ellipse")
  
  # Dimension reduction: using the SVD (see Chapter 12 of MDSR, https://mdsr-book.github.io/mdsr2e)
  
  # Step 1: Compute the SVD on the predictor variables
  train_svd <- train |>
    select(less_than_hs,hs_only,some_college_associates,bachelors_plus) |>
    svd()
  
  # Step 2: Define the number of clusters (principal components)
  num_clusters <- 2   # desired number of clusters
  
  # Step 3: Tidy the matrix and filter on those principal components
  # (Note: the resulting data frame will be the same as your data - the U matrix measures
  # the weight of each principal component on your dataset)
  
  train_svd_tidy <- train_svd |>
    tidy(matrix = "u") |>
    filter(PC <= num_clusters) |>
    mutate(PC = paste0("pc_", PC)) |>
    pivot_wider(names_from = PC, values_from = value) |>
    select(-row)
  
  
  # Step 4: Use k-means clustering to group the data in the Principal components!
  
  clusts <- train_svd_tidy |>
    kmeans(centers = num_clusters)
  
  tidy(clusts)
  
  # Step 5: Visualize the distribution of your data in each cluster - are there any patterns?
  clusts |>
    augment(train_svd_tidy) |>
    cbind(train) |>
    pivot_longer(cols = c("less_than_hs":"bachelors_plus")) |>
    ggplot(aes(x=value,fill=.cluster)) + geom_density(alpha=0.5) +
    facet_grid(.~name)
  
  
