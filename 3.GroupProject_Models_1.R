#==============================================================================================================#
# Model 1 - LINEAR REGRESSION MODEL

#Creating the Replica of original Dataset
TrainDs_linear <- TrainDs
TestDs_linear <- TestDs

#Call all required library packages:
suppressWarnings(suppressMessages(library(RODBC)))
library(data.table)
library(ggplot2)
library(plotly)
library(car)
library(rcompanion)

#convert the categorical variables into factor data type for Train Dataset
for(i in 2:ncol(TrainDs_linear)){
  if (!is.numeric(TrainDs_linear[,i]))
  {
    TrainDs_linear[,i] <- data.frame(apply(TrainDs_linear[i], 2, as.factor))
    #print('Catogorical')
  }
}
summary(TrainDs_linear)

#convert the categorical variables into factor data type for Test Dataset
for(j in 2:ncol(TestDs_linear)){
  if (!is.numeric(TestDs_linear[,j]))
  {
    TestDs_linear[,j] <- data.frame(apply(TestDs_linear[j], 2, as.factor))
    #print('Catogorical')
  }
}

#convert the categorical variables into numerical data type.
trainIndex <- sapply(TrainDs_linear, is.factor)
TrainDs_linear[trainIndex] <- lapply(TrainDs_linear[trainIndex], as.numeric)
str(TrainDs_linear)
testIndex <- sapply(TestDs_linear, is.factor)
TestDs_linear[testIndex] <- lapply(TestDs_linear[testIndex], as.numeric)

set.seed(270)
seq_index = sample(seq_len(nrow(TrainDs_linear)), size = 300)

NewTrainDs_linear = TrainDs_linear[-seq_index,]
ValidDs_linear = TrainDs_linear[seq_index,]
rm(seq_index)

Step_lm <- step(lm(SalePrice ~  MSSubClass + MSZoning + LotFrontage + LotArea + Alley + LotShape + LandContour + LandSlope + Neighborhood + BldgType +
                     HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType +
                     MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF +
                     TotalBsmtSF + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath +
                     HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
                     GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF,data = NewTrainDs_linear),direction = "both")
summary(Step_lm)
vif(Step_lm)

lmodel <- lm(formula = SalePrice ~ MSSubClass + LotFrontage + LotArea + LandContour + LandSlope + 
               OverallQual + OverallCond + YearBuilt + RoofStyle + MasVnrType + MasVnrArea + 
               ExterQual + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + GrLivArea + BsmtFullBath + 
               KitchenQual + Functional + Fireplaces +  GarageCars + WoodDeckSF, data = NewTrainDs_linear)

summary(lmodel)

par("mar")
par(mar=c(1,1,1,1))
par(mfrow=c(2,2))
plot(lmodel)

#Validation of model and error rates
predTrain_lm = data.frame(predict(lmodel, ValidDs_linear, interval = "prediction"))
predDf_lm = data.frame(Id = ValidDs_linear$Id, SalePrice = predTrain_lm$fit, oldprice = ValidDs_linear$SalePrice)
predDf_lm$err_rate = sqrt(abs((predDf_lm$SalePrice^2) - (predDf_lm$oldprice^2)))
predDf_lm$pcterr = abs((predDf_lm$oldprice - predDf_lm$SalePrice )/predDf_lm$oldprice)*100

median(predDf_lm$err_rate)  # median prediction error in $ = 62058$
median(predDf_lm$pcterr)    # median prediction error as % difference from correct value = 7.889%

validTestDs = data.frame(Id = predDf_lm$Id, Actual = predDf_lm$oldprice, Predicted = predDf_lm$SalePrice)
head(validTestDs)
plot(Actual~Predicted,data = validTestDs)
cor(validTestDs)
# predict values for test
predTest_lm = data.frame(predict(lmodel, TestDs_linear, interval = "prediction"))
final = data.frame(Id = TestDs_linear$Id, Linear_SalePrice = predTest_lm$fit)
head(final)
#write.csv(final, "model1_linreg.csv", row.names = FALSE)

#==============================================================================================================#
# Model 2 - TREE CLASSIFIER MODEL

#Call Required Library Packages
suppressWarnings(suppressMessages(library(RODBC)))
library(tree)

TrainDs1 <- TrainDs
TestDs1 <- TestDs
TestDs1$SalePrice <- NA
All_DT = rbind(TrainDs1, TestDs1)

rm(TrainDs1)
rm(TestDs1)

for(i in 2:ncol(All_DT)){
  
  if (is.character(All_DT[,i]))
  {
    All_DT[, i] <- factor(All_DT[, i])
  }
}
str(All_DT)

TrainDs_tree <- All_DT[!is.na(All_DT$SalePrice),]
TestDs_tree <- All_DT[is.na(All_DT$SalePrice),]
TestDs_tree$SalePrice <- NULL

set.seed(270)
seq_index = sample(seq_len(nrow(TrainDs_tree)), size = 300)

NewTrainDs_tree = TrainDs_tree[-seq_index,]
ValidDs_tree = TrainDs_tree[seq_index,]
rm(seq_index)

 
tmodel = tree(SalePrice ~  MSSubClass + MSZoning + LotFrontage + LotArea + Alley + LotShape + LandContour + LandSlope + Neighborhood + BldgType +
                HouseStyle + OverallQual + OverallCond + YearBuilt + YearRemodAdd + RoofStyle + Exterior1st + Exterior2nd + MasVnrType +
                MasVnrArea + ExterQual + Foundation + BsmtQual + BsmtCond + BsmtExposure + BsmtFinType1 + BsmtFinSF1 + BsmtUnfSF +
                TotalBsmtSF + HeatingQC + CentralAir + Electrical + X1stFlrSF + X2ndFlrSF + GrLivArea + BsmtFullBath + FullBath +
                HalfBath + BedroomAbvGr + KitchenAbvGr + KitchenQual + TotRmsAbvGrd + Functional + Fireplaces + FireplaceQu + GarageType +
                GarageYrBlt + GarageFinish + GarageCars + GarageArea + GarageQual + GarageCond + PavedDrive + WoodDeckSF + OpenPorchSF, data = NewTrainDs_tree)
summary(tmodel)

plot(tmodel)
text(tmodel)

# predict for validation dataset:
predTrain_tree <- predict(tmodel, ValidDs_tree)
predDf_tree = data.frame(Id= ValidDs_tree$Id, SalePrice = predTrain_tree, oldprice = ValidDs_tree$SalePrice)
predDf_tree$err_rate = sqrt(abs((predDf_tree$SalePrice^2) - (predDf_tree$oldprice^2)))
predDf_tree$pcterr = abs((predDf_tree$oldprice - predDf_tree$SalePrice )/predDf_tree$oldprice)*100

median(predDf_tree$err_rate)  # median prediction error in $ = 78860$
median(predDf_tree$pcterr)    # median prediction error as % difference from correct value = 13.1%

validDf_tree = data.frame(Id = predDf_tree$Id, Actual = predDf_tree$oldprice, Predicted = predDf_tree$SalePrice)
head(validDf_tree)
plot(Actual~Predicted,data = validDf_tree)
cor(validDf_tree)
rm(predDf_tree)

# predict for test1:
predTest_tree <- predict(tmodel, TestDs_tree)
predDf_tree = data.frame(Id = TestDs_tree$Id , SalePrice = predTest_tree)
predDf_tree$SalePrice[is.na(predDf_tree$SalePrice)] <- median(predDf_tree$SalePrice)

#write.csv(predDf, "model2_tree.csv", row.names = FALSE)

final$Tree_SalePrice = predDf_tree$SalePrice
head(final$Tree_SalePrice)  
  

