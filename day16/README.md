# DST 490
## Days 16: Predictive modeling & machine learning

Today we are focusing on the application of different classification methods for model evaluation as part of the `tidymodels` package. Content based on [Modern Data Science with R](https://mdsr-book.github.io/mdsr2e/), Chapter 11

## Directory Structure

The directory is structured a little bit differently, suggesting a possible framework for organization of a data science project.

- The folder `data` contains all the datasets used in the analysis, saving it to a file for analysis.
- The file `00-data-process.R` processes the datasets for analysis
- The file `01-classifier-methods.R` showcases different classification methods used in class.
- The file `02-the-whole-game.R` has a unified workflow for all of the different classification models.

Additional packages needed:

- for decision trees: rpart, partykit, and rpart.plot
- for random forests: randomForest
- for Naive Bayes: discrim and klaR
- for K-Nearest neighbors: knnlibrary
- for neural networks: nnet

