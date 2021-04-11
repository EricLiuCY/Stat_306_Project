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
  holdo <- read.csv("./data/raw/test.csv")
  nTrain <- nrow(train)
  
  # remove ID from train
  train <- train[,-1]
  
  train <- train %>% mutate_if(is.character, as.factor)
  holdo <- holdo %>% mutate_if(is.character, as.factor)
  
  # get NA Columns
  threshold <- ceiling(nTrain/10)
  NACols <- getNACols(train, threshold)
  
  # select only columns with NA < threshold, and rows without NA
  train <- na.omit(select(train, -NACols))
  holdo <- na.omit(select(holdo, -NACols))
  
  # save as parquet
  # Save as parquets
  write_parquet(train, sink = "data/processed/train.parquet")
  write_parquet(holdo, sink = "data/processed/holdo.parquet")
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

#____________________ SCRIPTS FOR TESTING & Saving ________________

initDataset()
