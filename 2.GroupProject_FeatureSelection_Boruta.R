#Creating the Replica of original Dataset
#Importing data into R
TrainDs_boruta <- TrainDs

#install.packages("Boruta")
suppressWarnings(suppressMessages(library(RODBC)))
library(Boruta)

#convert the categorical variables into factor data type.
for(i in 2:ncol(TrainDs_boruta)){
  if (!is.numeric(TrainDs_boruta[,i]))
  {
    TrainDs_boruta[,i] <- data.frame(apply(TrainDs_boruta[i], 2, as.factor))
    #print('Catogorical')
  }
}

for(i in 2:ncol(TrainDs_boruta)){
  print(typeof(TrainDs_boruta[,i]))
}
  
#Applying Boruta on Train Dataset
set.seed(123)
boruta.train <- Boruta(SalePrice~.-Id, data = TrainDs_boruta, doTrace = 2)
print(boruta.train)

#Taking decision on tentative attributes.  
#The tentative attributes will be classified as confirmed or rejected by comparing the median Z score 
#of the attributes with the median Z score of the best shadow attribute. 
final.boruta <- TentativeRoughFix(boruta.train)
print(final.boruta)

#Checking the confirmed attributes.
getSelectedAttributes(final.boruta, withTentative = F)

#creating a data frame of the final result derived from Boruta.
boruta.df <- attStats(final.boruta)
class(boruta.df)
 
print(boruta.df)

