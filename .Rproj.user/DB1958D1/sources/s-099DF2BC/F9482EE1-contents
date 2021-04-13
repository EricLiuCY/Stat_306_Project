library(randomForest)
library(arrow)
library(dplyr)
library(leaps)
library(caret)

# __________________________ FUNCTIONS ______________________________

getMSE <- function(model, data) {
  actual <- data$SalePrice
  pred <- predict(model, newdata = data)
  return (mean((actual - pred)^2))
}

bestModelVanilla <- function(data) {
  model <- lm(formula = SalePrice ~ MSZoning + LotArea + Street + LandContour +
                LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 +
                BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt +
                YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + MasVnrType +
                MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtExposure +
                BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF +
                BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd +
                Functional + Fireplaces + GarageCars + GarageArea + GarageQual +
                GarageCond + WoodDeckSF + ScreenPorch + PoolArea + SaleType,
              data = data)
  return(model)
}

bestModelTransformed <- function(data) {
  model <- lm(formula = SalePrice ~ MSSubClass + MSZoning + LotArea + Street +
                LotShape + LandContour + Utilities + LotConfig + LandSlope +
                Neighborhood + Condition1 + Condition2 + OverallQual + 
                OverallCond + YearBuilt + YearRemodAdd + RoofMatl + 
                Exterior1st + MasVnrType + MasVnrArea + ExterCond + BsmtQual + 
                BsmtExposure + BsmtUnfSF + TotalBsmtSF + X2ndFlrSF + GrLivArea + 
                FullBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + 
                Functional + Fireplaces + GarageFinish + GarageCars + 
                GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + 
                OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + 
                PoolArea + SaleCondition + UnfinishedRatio + BedPerBath + 
                BathsPerLivAbv + hasOpenPorchSF + hasScreenPorch + 
                I(TotalBsmtSF^2) + I(MasVnrArea^2) + I(GarageArea^2) + 
                I(GrLivArea^2) + BsmtQual:UnfinishedRatio + 
                MasVnrType:I(MasVnrArea^2) + BsmtQual:I(TotalBsmtSF^2) + 
                BsmtExposure:I(TotalBsmtSF^2) + GarageFinish:I(GarageArea^2) + 
                MSZoning:LotArea + RoofMatl:X2ndFlrSF + MSSubClass:MSZoning +
                LotArea:LotShape + LotArea:LotConfig + LotArea:LandSlope +
                Neighborhood:I(GrLivArea^2) + LotArea:Condition1 + 
                LotArea:Condition2, data = data)
  return(model)
}

kFoldCV <- function(data, k, transformed) {
  # partition data
  folds <- createFolds(1:nrow(data), k = k, list = FALSE, returnTrain = FALSE)
  partedData <- list()
  for(i in 1:k ) {
    partedData <- append(partedData, list(data[folds==i,]))
  }
  
  # Train and get MSE
  avgMSE <- 0
  for (i in 1:k){
    train <- bind_rows(partedData[-i])
    holdo <- partedData[[i]]
    model <- NULL
    if (transformed) {
      model <- bestModelTransformed(train)
    } else {
      model <- bestModelVanilla(train)
    }
    avgMSE <- avgMSE + getMSE(model, holdo)
    break
  }
  return(avgMSE / k)
}

# partition data

# _______________________________ TRAIN SCRIPT ________________________
set.seed(888)
vTrain <- read_parquet("data/processed/train.parquet")
vTrainMSE <- kFoldCV(vTrain, nrow(vTrain), FALSE)

set.seed(888)
tTrain <- read_parquet("data/processed/transformed_train.parquet")
tTrainMSE <- kFoldCV(tTrain, nrow(tTrain), TRUE)

# __________________________ BEST VANILLA ______________________________

train <- read_parquet("data/processed/train.parquet")

fullModel_vanilla <- lm(SalePrice ~. , data = train)
finalModel_vanilla <- step(fullModel_vanilla)
summary(finalModel_vanilla)
getMSE(finalModel_vanilla, train)

# BEST VANILA
# lm(formula = SalePrice ~ MSZoning + LotArea + Street + LandContour + 
#      LotConfig + LandSlope + Neighborhood + Condition1 + Condition2 + 
#      BldgType + HouseStyle + OverallQual + OverallCond + YearBuilt + 
#      YearRemodAdd + RoofStyle + RoofMatl + Exterior1st + MasVnrType + 
#      MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtExposure + 
#      BsmtFinSF1 + BsmtFinSF2 + BsmtUnfSF + X1stFlrSF + X2ndFlrSF + 
#      BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + 
#      Functional + Fireplaces + GarageCars + GarageArea + GarageQual + 
#      GarageCond + WoodDeckSF + ScreenPorch + PoolArea + SaleType, 
#    data = train)
# Residual standard error: 22950 on 1187 degrees of freedom
# Multiple R-squared:  0.9249,	Adjusted R-squared:  0.9154 
# F-statistic: 97.44 on 150 and 1187 DF,  p-value: < 2.2e-16

# __________________________ BEST TRANSFORMED ______________________________

tTrain <- read_parquet("data/processed/transformed_train.parquet")
fullModel_transformed <- lm(SalePrice ~. + 
                              BsmtQual * UnfinishedRatio +
                              BsmtCond * TotalBsmtSF +
                              hasX2ndFlrSF * X2ndFlrSF +
                              hasLowQualFinSF * LowQualFinSF +
                              hasWoodDeckSF * WoodDeckSF +
                              hasOpenPorchSF * OpenPorchSF +
                              hasEnclosedPorch * EnclosedPorch +
                              hasX3SsnPorch * X3SsnPorch +
                              hasScreenPorch * ScreenPorch +
                              I(TotalBsmtSF^2) +
                              I(MasVnrArea^2) + 
                              I(BsmtFinSF2^2) + 
                              I(GarageArea^2) + 
                              I(GrLivArea^2) +
                              MasVnrType * I(MasVnrArea^2) +
                              BsmtFinType1 * I(BsmtFinSF1^2) + 
                              BsmtFinType2 * I(BsmtFinSF2^2) +
                              BsmtQual * I(TotalBsmtSF^2) +
                              BsmtExposure * I(TotalBsmtSF^2) +
                              GarageType * I(GarageArea^2) +
                              PavedDrive * I(GarageArea^2) +
                              GarageFinish * I(GarageArea^2) +
                              MSZoning * LotArea +
                              RoofMatl * I(GrLivArea^2) +
                              RoofMatl * X2ndFlrSF + 
                              Functional * I(GrLivArea^2) +
                              MSSubClass * MSZoning +
                              LotShape * LotArea +
                              LotShape * I(GrLivArea^2) +
                              Street * LotArea +
                              LandContour * LotArea +
                              LotConfig * LotArea +
                              LandSlope * LotArea +
                              Neighborhood * LotArea +
                              Neighborhood * I(GrLivArea^2) + 
                              Condition1 * LotArea +
                              Condition1 * I(GrLivArea^2) +
                              Condition2 * LotArea, data = tTrain)
summary(fullModel_transformed)

finalModel_transformed <- step(fullModel_transformed)
summary(finalModel_transformed)
getMSE(finalModel_transformed, tTrain)

# Best Model:
# lm(formula = SalePrice ~ MSSubClass + MSZoning + LotArea + Street + 
#      LotShape + LandContour + Utilities + LotConfig + LandSlope + 
#      Neighborhood + Condition1 + Condition2 + OverallQual + OverallCond + 
#      YearBuilt + YearRemodAdd + RoofMatl + Exterior1st + MasVnrType + 
#      MasVnrArea + ExterCond + BsmtQual + BsmtExposure + BsmtUnfSF + 
#      TotalBsmtSF + X2ndFlrSF + GrLivArea + FullBath + BedroomAbvGr + 
#      KitchenAbvGr + KitchenQual + Functional + Fireplaces + GarageFinish + 
#      GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + 
#      WoodDeckSF + OpenPorchSF + EnclosedPorch + X3SsnPorch + ScreenPorch + 
#      PoolArea + SaleCondition + UnfinishedRatio + BedPerBath + 
#      BathsPerLivAbv + hasOpenPorchSF + hasScreenPorch + I(TotalBsmtSF^2) + 
#      I(MasVnrArea^2) + I(GarageArea^2) + I(GrLivArea^2) + BsmtQual:UnfinishedRatio + 
#      MasVnrType:I(MasVnrArea^2) + BsmtQual:I(TotalBsmtSF^2) + 
#      BsmtExposure:I(TotalBsmtSF^2) + GarageFinish:I(GarageArea^2) + 
#      MSZoning:LotArea + RoofMatl:X2ndFlrSF + MSSubClass:MSZoning + 
#      LotArea:LotShape + LotArea:LotConfig + LotArea:LandSlope + 
#      Neighborhood:I(GrLivArea^2) + LotArea:Condition1 + LotArea:Condition2, 
#    data = tTrain)
# Residual standard error: 17840 on 1083 degrees of freedom
# (5 observations deleted due to missingness)
# Multiple R-squared:  0.9583,	Adjusted R-squared:  0.9488 
# F-statistic: 100.1 on 249 and 1083 DF,  p-value: < 2.2e-16
