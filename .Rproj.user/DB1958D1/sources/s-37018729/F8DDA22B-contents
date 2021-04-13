library(dplyr)
library(arrow)

# ________________________ FUNCTIONS _______________________
#' @description
#' convert dataset to parquet after cleaning, and save
#'
#' @return list of train and test set, and names of columns dropped
#' with data as factor with NA entries removed:
initDataset <- function(){
  # Load datasets
  train <- read.csv("./data/raw/train.csv")
  nTrain <- nrow(train)

  # remove ID from train
  train <- train[,-1]
  
  # Set character entries as factor
  train <- train %>% mutate_if(is.character, as.factor)
  
  # Manually set some integer based entries as factors
  # MSSubClass is a code
  train$MSSubClass <- as.factor(train$MSSubClass)
  # Month sold is not cyclic, so set as factor
  train$MoSold <- as.factor(train$MoSold)
  
  # get NA Columns
  threshold <- ceiling(nTrain/10)
  NACols <- getNACols(train, threshold)
  
  # select only columns with NA < threshold, and rows without NA
  train <- na.omit(select(train, -NACols))
  
  # save as parquet
  write_parquet(train, sink = "data/processed/train.parquet")
}

#' @description
#' Return column names of columns that contain NA > threshold
#' @param train the training set
#' @param threshold the threshold for NAs
#'
#' @return data as factor with NA entries removed:
getNACols <- function(train, threshold) {
  NASummary <- sapply(train, function(x) sum(is.na(x)))
  NACols <- c()
  for (i in 1:length(NASummary)){
    naCount <- NASummary[[i]]
    if (naCount > threshold) {
      NACols <- c(NACols, names(NASummary[i]))
    }
  }
  return(NACols)
}

#' @description
#' Print Out the variables names and class
#' @param data the dataset
#'
#' @return Printed statements:
printFeatures <- function(data) {
  types <- sapply(data, class)
  n <- length(types)
  varn <- names(types)

  for (i in 1:n){
    if (types[[i]] == "integer"){
      cat(varn[i], ":", types[[i]], "\n")
    } else {
      cat(varn[i], ":", types[[i]], levels(data[,i]),"\n")
    }
  }
}

# ____________________ SCRIPTS  ______________________

initDataset()
