#==============================================================================================================#
# Model 3 - SVM MODEL
library('e1071')
library(rcompanion)

TrainDs1 <- TrainDs
TestDs1 <- TestDs
TestDs1$SalePrice <- NA
All_SVM = rbind(TrainDs1, TestDs1)

rm(TrainDs1)
rm(TestDs1)

for(i in 2:ncol(All_SVM)){
  
  if (is.character(All_SVM[,i]))
  {
    All_SVM[, i] <- factor(All_SVM[, i])
  }
}
str(All_SVM)

TrainDs_SVM <- All_SVM[!is.na(All_SVM$SalePrice),]
TestDs_SVM <- All_SVM[is.na(All_SVM$SalePrice),]
TestDs_SVM$SalePrice <- NULL

set.seed(270)
seq_index = sample(seq_len(nrow(TrainDs_SVM)), size = 300)

NewTrainDs_SVM = TrainDs_SVM[-seq_index,]
ValidDs_SVM = TrainDs_SVM[seq_index,]
rm(seq_index)

SVM_Model <- svm(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Alley + LotShape + LandContour + LandSlope + Neighborhood + BldgType +
                   HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType +
                   MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF +
                   TotalBsmtSF + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath +
                   HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
                   GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF,data = NewTrainDs_SVM,kernel = "linear")

summary(SVM_Model)

#Prediction on Best Model
PredTrain_SVM = predict(SVM_Model,ValidDs_SVM)
table(predict=PredTrain_SVM,truth=ValidDs_SVM$SalePrice)

predDf_SVM = data.frame(Id= ValidDs_SVM$Id, SalePrice = PredTrain_SVM, oldprice = ValidDs_SVM$SalePrice)
predDf_SVM$err_rate = sqrt(abs((predDf_SVM$SalePrice^2) - (predDf_SVM$oldprice^2)))
predDf_SVM$pcterr = abs((predDf_SVM$oldprice - predDf_SVM$SalePrice )/predDf_SVM$oldprice)*100

median(predDf_SVM$err_rate)  # median prediction error in $ = 52634$
median(predDf_SVM$pcterr)     # median prediction error as % difference from correct value = 5.396%

validDf_SVM = data.frame(Id = predDf_SVM$Id, Actual = predDf_SVM$oldprice, Predicted = predDf_SVM$SalePrice)
head(validDf_SVM)
plot(Actual~Predicted,data = validDf_SVM)
cor(validDf_SVM)
rm(predDf_SVM)

# predict for Test dataset:
predTest_SVM <- predict(SVM_Model, TestDs_SVM)
predDf_SVM = data.frame(Id= TestDs_SVM$Id, SalePrice = predTest_SVM)

final$SVM_SalePrice = predDf_SVM$SalePrice
head(final$SVM_SalePrice)

#==============================================================================================================#
# Model 4 - RANDOM FOREST MODEL

#Call Required Library Packages
suppressWarnings(suppressMessages(library(RODBC)))
library("randomForest")
library(rcompanion)

TrainDs1 <- TrainDs
TestDs1 <- TestDs
TestDs1$SalePrice <- NA
All_RF = rbind(TrainDs1, TestDs1)

rm(TrainDs1)
rm(TestDs1)

for(i in 2:ncol(All_RF)){
  
  if (is.character(All_RF[,i]))
  {
    All_RF[, i] <- factor(All_RF[, i])
  }
}
str(All_RF)

TrainDs_RF <- All_RF[!is.na(All_RF$SalePrice),]
TestDs_RF <- All_RF[is.na(All_RF$SalePrice),]
TestDs_RF$SalePrice <- NULL

set.seed(270)
seq_index = sample(seq_len(nrow(TrainDs_RF)), size = 300)

NewTrainDs_RF = TrainDs_RF[-seq_index,]
ValidDs_RF = TrainDs_RF[seq_index,]
rm(seq_index)

RFmodel = randomForest(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Alley + LotShape + LandContour + LandSlope + Neighborhood + BldgType +
                         HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType +
                         MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF +
                         TotalBsmtSF + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath +
                         HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
                         GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF ,
                       data = NewTrainDs_RF,  ntree = 700,  importance = TRUE)

# predict for validation dataset:
predTrain_RF <- predict(RFmodel, ValidDs_RF)
predDf_RF = data.frame(Id= ValidDs_RF$Id, SalePrice = predTrain_RF, oldprice = ValidDs_RF$SalePrice)
predDf_RF$err_rate = sqrt(abs((predDf_RF$SalePrice^2) - (predDf_RF$oldprice^2)))
predDf_RF$pcterr = abs((predDf_RF$oldprice - predDf_RF$SalePrice )/predDf_RF$oldprice)*100

median(predDf_RF$err_rate)  # median prediction error in $ = 55470$
median(predDf_RF$pcterr)     # median prediction error as % difference from correct value = 5.624%

validDf_RF = data.frame(Id = predDf_RF$Id, Actual = predDf_RF$oldprice, Predicted = predDf_RF$SalePrice)
head(validDf_RF)
plot(Actual~Predicted,data = validDf_RF)
cor(validDf_RF)
rm(predDf_RF)

# predict for Test dataset:
predTest_RF <- predict(RFmodel, TestDs_RF)
predDf_RF = data.frame(Id= TestDs_RF$Id, SalePrice = predTest_RF)

final$RF_SalePrice = predDf_RF$SalePrice
head(final$RF_SalePrice)

#==============================================================================================================#
# Model 5 - NEURAL NET MODEL

#Call Required Library Packages
suppressWarnings(suppressMessages(library(RODBC)))
library("nnet")
library(rcompanion)

TrainDs1 <- TrainDs
TestDs1 <- TestDs
TestDs1$SalePrice <- NA
All_NN = rbind(TrainDs1, TestDs1)

rm(TrainDs1)
rm(TestDs1)

for(i in 2:ncol(All_NN)){
  
  if (is.character(All_NN[,i]))
  {
    All_NN[, i] <- factor(All_NN[, i])
  }
}
str(All_NN)

TrainDs_NN <- All_NN[!is.na(All_NN$SalePrice),]
TestDs_NN <- All_NN[is.na(All_NN$SalePrice),]
TestDs_NN$SalePrice <- NULL

set.seed(270)
seq_index = sample(seq_len(nrow(TrainDs_NN)), size = 300)

NewTrainDs_NN = TrainDs_NN[-seq_index,]
ValidDs_NN = TrainDs_NN[seq_index,]
rm(seq_index)

NNmodel = nnet(SalePrice ~ MSSubClass + MSZoning + LotFrontage + LotArea + Alley + LotShape + LandContour + LandSlope + Neighborhood + BldgType +
                 HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType +
                 MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF +
                 TotalBsmtSF + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath +
                 HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
                 GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF ,
               data = NewTrainDs_NN, size = 3, skip = TRUE, linout = TRUE)

# predict for validation dataset:
predTrain_NN <- predict(NNmodel, ValidDs_NN)
predDf_NN = data.frame(Id= ValidDs_NN$Id, SalePrice = predTrain_NN, oldprice = ValidDs_NN$SalePrice)
predDf_NN$err_rate = sqrt(abs((predDf_NN$SalePrice^2) - (predDf_NN$oldprice^2)))
predDf_NN$pcterr = abs((predDf_NN$oldprice - predDf_NN$SalePrice )/predDf_NN$oldprice)*100

median(predDf_NN$err_rate)  # median prediction error in $ = 63924$
median(predDf_NN$pcterr)     # median prediction error as % difference from correct value = 7.406%

validDf_NN = data.frame(Id = predDf_NN$Id, Actual = predDf_NN$oldprice, Predicted = predDf_NN$SalePrice)
head(validDf_NN)
plot(Actual~Predicted,data = validDf_NN)
cor(validDf_NN)
rm(predDf_NN)

# predict for Test dataset:
predTest_NN <- predict(NNmodel, TestDs_NN)
predDf_NN = data.frame(Id= TestDs_NN$Id, SalePrice = predTest_NN)

final$NN_SalePrice = predDf_NN$SalePrice
head(final$NN_SalePrice)

write.csv(final, "Allmodels_Predicted.csv", row.names = FALSE)

