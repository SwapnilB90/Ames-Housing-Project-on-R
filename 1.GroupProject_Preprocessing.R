#Cleaning up memory of current R session:
rm(list=ls(all=TRUE)) 
#Decimal places - only 4
options(digits = 4)

#Required library packages:
suppressWarnings(suppressMessages(library(RODBC)))
library(data.table) 
library(ggplot2)
library(plotly)
library(corrplot)

#Load the Data Sets
TrainDs = data.frame(fread(file.choose()), stringsAsFactors = FALSE)
TestDs = data.frame(fread(file.choose()), stringsAsFactors = FALSE)

summary(TrainDs)

#Converting NA to No for below columns as per description in Train Dataset
TrainDs$Alley[is.na(TrainDs$Alley)] <- "No"
TrainDs$BsmtQual[is.na(TrainDs$BsmtQual)] <- "No"
TrainDs$BsmtCond[is.na(TrainDs$BsmtCond)] <- "No"
TrainDs$BsmtExposure[is.na(TrainDs$BsmtExposure)] <- "No"
TrainDs$BsmtFinType1[is.na(TrainDs$BsmtFinType1)] <- "No"
TrainDs$BsmtFinType2[is.na(TrainDs$BsmtFinType2)] <- "No"
TrainDs$FireplaceQu[is.na(TrainDs$FireplaceQu)] <- "No"
TrainDs$GarageType[is.na(TrainDs$GarageType)] <- "No"
TrainDs$GarageFinish[is.na(TrainDs$GarageFinish)] <- "No"
TrainDs$GarageQual[is.na(TrainDs$GarageQual)] <- "No"
TrainDs$GarageCond[is.na(TrainDs$GarageCond)] <- "No"
TrainDs$PoolQC[is.na(TrainDs$PoolQC)] <- "No"
TrainDs$Fence[is.na(TrainDs$Fence)] <- "No"
TrainDs$MiscFeature[is.na(TrainDs$MiscFeature)] <- "No"

#Converting NA to No for below columns as per description in Test Dataset
TestDs$Alley[is.na(TestDs$Alley)] <- "No"
TestDs$BsmtQual[is.na(TestDs$BsmtQual)] <- "No"
TestDs$BsmtCond[is.na(TestDs$BsmtCond)] <- "No"
TestDs$BsmtExposure[is.na(TestDs$BsmtExposure)] <- "No"
TestDs$BsmtFinType1[is.na(TestDs$BsmtFinType1)] <- "No"
TestDs$BsmtFinType2[is.na(TestDs$BsmtFinType2)] <- "No"
TestDs$FireplaceQu[is.na(TestDs$FireplaceQu)] <- "No"
TestDs$GarageType[is.na(TestDs$GarageType)] <- "No"
TestDs$GarageFinish[is.na(TestDs$GarageFinish)] <- "No"
TestDs$GarageQual[is.na(TestDs$GarageQual)] <- "No"
TestDs$GarageCond[is.na(TestDs$GarageCond)] <- "No"
TestDs$PoolQC[is.na(TestDs$PoolQC)] <- "No"
TestDs$Fence[is.na(TestDs$Fence)] <- "No"
TestDs$MiscFeature[is.na(TestDs$MiscFeature)] <- "No"

#Checking for missing Values in Train
NAcol= which(colSums(is.na(TrainDs)) > 0)
sort(colSums(sapply(TrainDs[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')
#sapply(TrainDs, function(x) sum(is.na(x)))
#Replacing missing values with Mean and Mode depends on condition wheather it is Numeric or Categorical in Train Ds
for(i in 2:ncol(TrainDs)){
  if (is.numeric(TrainDs[,i]))
  {
    #print('Numeric')
    TrainDs[is.na(TrainDs[,i]), i] <- mean(TrainDs[,i], na.rm = TRUE)
  }
  else
  {
    #print('Catogorical')
    TrainDs[is.na(TrainDs[,i]), i] <- mode(TrainDs[,i])
  }
}

#Checking for missing Values in Train again
sapply(TrainDs, function(x) sum(is.na(x)))

#Checking for missing Values in Test
NAcol= which(colSums(is.na(TestDs)) > 0)
sort(colSums(sapply(TestDs[NAcol], is.na)), decreasing = TRUE)
cat('There are', length(NAcol), 'columns with missing values')
#sapply(TestDs, function(x) sum(is.na(x)))

#Replacing missing values with Mean and Mode depends on condition wheather it is Numeric or Categorical in Test Ds
for(j in 2:ncol(TestDs)){
  if (is.numeric(TestDs[,j]))
  {
    #print('Numeric')
    TestDs[is.na(TestDs[,j]), j] <- mean(TestDs[,j], na.rm = TRUE)
  }
  else
  {
    #print('Catogorical')
    TestDs[is.na(TestDs[,j]), j] <- mode(TestDs[,j])
  }
}

#Checking for missing Values in Test again
sapply(TestDs, function(x) sum(is.na(x)))
 
#For Correlation 
TrainDs1 <- TrainDs
TestDs1 <- TestDs
TestDs1$SalePrice <- NA
All = rbind(TrainDs1, TestDs1)
rm(TrainDs1)
rm(TestDs1)

numericVars = which(sapply(All, is.numeric)) 
numericVarNames = names(numericVars) #saving names vector for use later on
cat('There are', length(numericVars), 'numeric variables')
all_numVar = All[, numericVars]
cor_numVar = cor(all_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
#sorted on decreasing cor with price
cor_sorted = as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
CorHigh
cor_numVar = cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")#correlation plot
#high correlation with sales price:
#overallQuality:It rates the overall material and finish of 
#the house on a scale from 1 (very poor) to 10 (very excellent).
#plot between sale price and overall quality
ggplot(data=All[!is.na(All$SalePrice),], aes(x=factor(OverallQual), y=SalePrice))+
  geom_boxplot(col='blue') + labs(x='Overall Quality') +
  scale_y_continuous(breaks= seq(0, 800000, by=100000))
