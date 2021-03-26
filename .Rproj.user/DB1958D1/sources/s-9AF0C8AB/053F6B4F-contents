library(dplyr)

train <- read.csv("train.csv")

# drop ID
train <- train[,-1]

# Set levels
train <- train %>% mutate_if(is.character, as.factor)

types <- sapply(train, class)
n <- length(types)
varn <- names(types)

for (i in 1:n){
  if (types[[i]] == "integer"){
    cat(varn[i], ":", types[[i]], "\n")
  } else {
    cat(varn[i], ":", types[[i]], levels(train[,i]),"\n")
  }
}

integerLength <- length(types[types=="integer"])
CharactLegnth <- length(types[types=="factor"])

cat(integerLength, CharactLegnth)

