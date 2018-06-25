library(randomForest)
library(randomForestExplainer)
library(randomForestSRC)
library(data.table)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(caret)


setwd("C:/Users/FYY/Google Drive/CS3654/Data Sets/dataset")
business = fread("business.csv")



#Take out business that are restaurants
business_rest = business[which(grepl("Restaurants", business$categories))]
#Subset with some attributes related to resturants.
business_rest_foo = subset(business_rest,
                           select = c(stars,city,
                                      state,is_open,review_count,
                                      attributes.RestaurantsPriceRange2,
                                      attributes.HasTV, 
                                      attributes.WiFi,
                                      attributes.RestaurantsReservations,
                                      attributes.GoodForMeal.breakfast,
                                      attributes.GoodForMeal.lunch,
                                      attributes.GoodForMeal.brunch,
                                      attributes.GoodForMeal.dinner,
                                      attributes.GoodForMeal.latenight,
                                      attributes.GoodForMeal.dessert,
                                      attributes.OutdoorSeating,
                                      attributes.BusinessParking.street,
                                      attributes.BusinessParking.validated,
                                      attributes.BusinessParking.lot,
                                      attributes.BusinessParking.valet,
                                      attributes.BusinessParking.garage,
                                      attributes.BikeParking,
                                      attributes.RestaurantsAttire,
                                      attributes.RestaurantsGoodForGroups,
                                      attributes.BusinessAcceptsCreditCards,
                                      attributes.NoiseLevel,
                                      attributes.RestaurantsDelivery,
                                      attributes.RestaurantsTakeOut,
                                      attributes.Alcohol))

#Fil blank cell with NA
business_rest_foo[business_rest_foo == ""] = NA

#Remove rows with NA, result is final restaurant data frame
rest_complete = na.omit(business_rest_foo)
#Change char variable to factors
rest_complete = as.data.frame(unclass(rest_complete))

#Create training data and crossvalidationData
train = createDataPartition(rest_complete$stars, p = 0.7, list = F)
train.data = rest_complete[train,]
crossValidationData = rest_complete[-train,]
dim(train.data)

#Hist of stars
ggplot(data = rest_complete, aes(x = stars))+geom_histogram(binwidth = 0.5)


#Apply random forest to all variables with train data
rf.1 = randomForest(as.factor(stars) ~ ., data = train.data, 
                    importance = TRUE, ntree = 100)


#Check importance of variables
imp_var = importance(rf.1) 
vars = dimnames(imp_var)[[1]]
imp_var = data.frame(vars = vars, imp = as.numeric(imp_var[,1]))

#Sort variable importance by decreasing
imp_var = imp_var[order(imp_var$vars, decreasing = T),]
imp_var


#plot importance plot
varImpPlot(rf.1, main = "Variable Importance: All Variables", pch=16, col = 'blue')

#Predict with crossvalidationdata
predict_rf1 = predict(rf.1, crossValidationData, type = "class")
confusion_matrix = confusionMatrix(predict_rf1, as.factor(crossValidationData$stars))
#Confusion matrix is to describe the preformance of a classification model
confusion_matrix






