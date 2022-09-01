bc_=read.csv("C:/Users/12866/Desktop/my_data.csv")
bc=bc_[, -1]
names(bc)
dim(bc)
#####################
set.seed(1)
train.rows <- sample(rownames(bc), nrow( bc)*0.7)
train.data <- bc[train.rows , ]
valid.rows <- setdiff(rownames( bc), train.rows)
valid.data <- bc[valid.rows , ]
dim(train.data)
dim(valid.data)
##########################3
library(e1071)
par(mfrow=c(3,3))
num.cols <- colnames(bc[, -22])
for (i in num.cols) {
  ifelse( skewness(bc[[i]])>1 ,   
          print(c(i, skewness(bc[[i]]))),
          print("Not highly skewed")
  )
}
##########################
train.norm <- train.data
valid.norm <- valid.data
cols <- colnames(train.data[, -22])
for (i in cols) {
  valid.norm[[i]] <- 
    (valid.norm[[i]] - min(train.data[[i]])) / (max(train.data[[i]]) - min(train.data[[i]]))
  train.norm[[i]] <- 
    (train.norm[[i]] - min(train.data[[i]])) / (max(train.data[[i]]) - min(train.data[[i]]))
}
summary(train.norm)
summary(valid.norm)
#########################Neural net with 1 hidden layer of 3 nodes
library(neuralnet)
set.seed(1)
customer.nn.2 <- neuralnet(as.factor(AcceptedCmp6) ~ .,           
                           data = train.norm,             
                           linear.output = FALSE,      
                           hidden = 3,
                           stepmax = 1e+6)
library(caret)
par(mfcol = c(1,1))
plot(customer.nn.2, rep = "best")
predict.nn.2 <- predict(customer.nn.2, valid.norm)
predicted.class.2 <- apply(predict.nn.2, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.2), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")
##############################


##############################lasso_1se 
set.seed(1)
customer.nn.lasso_1se <- neuralnet(as.factor(AcceptedCmp6) ~ 
                                  Kidhome+Teenhome+Recency+Meat+
                                  Gold+NumDealsPurchases+NumCatalogPurchases+
                                  NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+
                                  NumOfferAccepted2+FamilySize+IsParent+
                                  Education_PhD+Education_Undergraduate+Marital_Status_Married+
                                  Marital_Status_Single, 
                                  #Kidhome + Recency + Wines + Meat + Fish + Gold + 
                                  #NumDealsPurchases + NumCatalogPurchases + NumStorePurchases + NumWebVisitsMonth +
                                  #AcceptedCmp1 + AcceptedCmp2 + AcceptedCmp3 + AcceptedCmp4 + AcceptedCmp5 +
                                  #FamilySize + Education_PhD,           
                                data = train.norm,             
                                linear.output = FALSE,      
                                hidden = 3,
                                stepmax = 1e+6)
par(mfcol = c(1,1))
plot(customer.nn.lasso_1se, rep = "best")
predict.nn.lasso_1se <- predict(customer.nn.lasso_1se, valid.norm)
predicted.class.lasso_1se <- apply(predict.nn.lasso_1se, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.lasso_1se), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")
#################################lasso.min
set.seed(1)
customer.nn.lasso.min <- neuralnet(as.factor(AcceptedCmp6) ~ 
                                     Income+Kidhome+Recency+Wines+
                                     Fruits+Meat+Fish+Gold+NumDealsPurchases+
                                     NumWebPurchases+NumCatalogPurchases+
                                     NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+
                                     AcceptedCmp5+NumOfferAccepted2+FamilySize+
                                     IsParent+Education_Master+Education_PhD+Education_Undergraduate+
                                     Marital_Status_Married+Marital_Status_Together,
                      
                                   data = train.norm,             
                                   linear.output = FALSE,      
                                   hidden = 3,
                                   stepmax = 1e+6)
par(mfcol = c(1,1))
plot(customer.nn.lasso.min, rep = "best")
predict.nn.lasso.min<- predict(customer.nn.lasso.min, valid.norm)
predicted.class.lasso.min <- apply(predict.nn.lasso.min, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.lasso.min), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")
################################rigde
set.seed(1)
customer.nn.ridge <- neuralnet(as.factor(AcceptedCmp6) ~. ,
                        
                                   data = train.norm,             
                                   linear.output = FALSE,      
                                   hidden = 3,
                                   stepmax = 1e+6)
par(mfcol = c(1,1))
plot(customer.nn.ridge, rep = "best")
predict.nn.ridge<- predict(customer.nn.ridge, valid.norm)
predicted.class.ridge <- apply(predict.nn.ridge, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.ridge), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")
################################ela.1se
set.seed(1)
customer.nn.ela.1se <- neuralnet(as.factor(AcceptedCmp6) ~
                                 Kidhome+Teenhome+Recency+Meat+Gold+
                                 NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+
                                 NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+
                                 AcceptedCmp5+AcceptedCmp1+NumOfferAccepted2+
                                 FamilySize+IsParent+Education_PhD+Education_Undergraduate+
                                 Marital_Status_Married+Marital_Status_Other+Marital_Status_Single+
                                 +Marital_Status_Together,
                      
                               data = train.norm,             
                               linear.output = FALSE,      
                               hidden = 3,
                               stepmax = 1e+6)
par(mfcol = c(1,1))
plot(customer.nn.ela.1se, rep = "best")
predict.nn.ela.1se<- predict(customer.nn.ela.1se, valid.norm)
predicted.class.ela.1se <- apply(predict.nn.ela.1se, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.ela.1se), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")   
################################ela.min
set.seed(1)
customer.nn.ela.min <- neuralnet(as.factor(AcceptedCmp6) ~
                                   Kidhome+Teenhome+Recency+Meat+Gold+
                                   NumDealsPurchases+NumWebPurchases+NumCatalogPurchases+
                                   NumStorePurchases+NumWebVisitsMonth+AcceptedCmp3+
                                   AcceptedCmp5+AcceptedCmp1+NumOfferAccepted2+
                                   FamilySize+IsParent+Education_PhD+Education_Undergraduate+
                                   Marital_Status_Married+Marital_Status_Other+Marital_Status_Single+
                                   +Marital_Status_Together,
                                 
                                 data = train.norm,             
                                 linear.output = FALSE,      
                                 hidden = 3,
                                 stepmax = 1e+6)
par(mfcol = c(1,1))
plot(customer.nn.ela.min, rep = "best")
predict.nn.ela.min<- predict(customer.nn.ela.min, valid.norm)
predicted.class.ela.min <- apply(predict.nn.ela.min, 1, which.max) - 1
confusionMatrix(as.factor(predicted.class.ela.min), 
                as.factor(valid.norm$AcceptedCmp6), 
                positive = "1")   

