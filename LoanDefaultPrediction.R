#Loading the required packages.
library( rpart )
library( rpart.plot )
library( ROCR )
library( randomForest )
library( gbm )

#Setting the seed for reproducibility of random number generation
SEED = 1
set.seed( SEED )

TARGET = "TARGET_BAD_FLAG"

#Step1
#Read the data file
directory_path = "C:/Trine_R/Week5/HMEQ_Scrubbed" 
file_name = "HMEQ_Scrubbed.csv"
setwd(directory_path)
data_frame = read.csv(file_name)

#Structure of the data
str(data_frame)

#Summary of the data
summary(data_frame)

#Printing first 6 records
head(data_frame)

#check the dimension
dim(data_frame)

#Exclude TARGET_LOSS_AMT
data_frame_flag = data_frame
data_frame_flag$TARGET_LOSS_AMT = NULL

################################################################################################
#Step2
#Split the data to train and test
FLAG = sample( c( TRUE, FALSE ), nrow(data_frame_flag), replace=TRUE, prob=c(0.6,0.4))
data_frame_train = data_frame_flag[FLAG, ]
data_frame_test = data_frame_flag[! FLAG, ]

#Building a decision tree
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=data_frame_train, TARGET_BAD_FLAG ~ ., control=tr_set, method="class", parms=list(split='information') )
rpart.plot( tr_model )
tr_model$variable.importance

#Predict using test data for decision tree
pt = predict( tr_model, data_frame_test, type="prob" )
head( pt )
pt2 = prediction( pt[,2], data_frame_test$TARGET_BAD_FLAG )
pt3 = performance( pt2, "tpr", "fpr" )

#Random forest
rf_model = randomForest( data=data_frame_train, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

#Predict using test data for rf
pr = predict( rf_model, data_frame_test )
head( pr )
pr2 = prediction( pr, data_frame_test$TARGET_BAD_FLAG )
pr3 = performance( pr2, "tpr", "fpr" )


#Gradient boosting
gb_model = gbm( data=data_frame_train, TARGET_BAD_FLAG ~ ., n.trees=500, distribution="bernoulli" )
summary.gbm( gb_model, cBars=10 )

#Predict using test data for gb
pg = predict( gb_model, data_frame_test, type="response" )
head( pg )
pg2 = prediction( pg, data_frame_test$TARGET_BAD_FLAG )
pg3 = performance( pg2, "tpr", "fpr" )

#Using the testing data set, create a ROC curves for all models. They must all be on the same plot.
plot( pt3, col="yellow", lwd=3 )
plot( pr3, col="violet",lwd=3, add=TRUE )
plot( pg3, col="green",lwd=3, add=TRUE )
abline(0,1,lty=2)
legend("bottomright",c("TREE","RANDOM FOREST", "GRADIENT BOOSTING"),col=c("yellow","violet","green"), bty="y", lty=1 )

#Display the Area Under the ROC curve (AUC) for all models.
aucT = performance( pt2, "auc" )@y.values
aucR = performance( pr2, "auc" )@y.values
aucG = performance( pg2, "auc" )@y.values

print( paste("TREE AUC=", aucT) )
print( paste("RF AUC=", aucR) )
print( paste("GB AUC=", aucG) )

################################################################################################
#Step3
#Split the data to train and test
data_frame_amt = data_frame
data_frame_amt$TARGET_BAD_FLAG = NULL
FLAG = sample( c( TRUE, FALSE ), nrow(data_frame_amt), replace=TRUE, prob=c(0.6,0.4))
data_frame_train = data_frame_amt[FLAG, ]
data_frame_test = data_frame_amt[! FLAG, ]

summary(data_frame_amt)

#Building a decision tree
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=data_frame_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )

#Listing the important variables. for decision tree
tr_model$variable.importance

#Predict using test data for decision tree
pt = predict( tr_model, data_frame_test )
head( pt )
RMSEt = sqrt( mean( ( data_frame_test$TARGET_LOSS_AMT - pt )^2 ) )

#Random forest
rf_model = randomForest( data=data_frame_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

#Predict using test data for RF
pr = predict( rf_model, data_frame_test )
head( pr )
RMSEr = sqrt( mean( ( data_frame_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING

gb_model = gbm( data=data_frame_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

#Predict using test data for GB
pg = predict( gb_model, data_frame_test, type="response" )
head( pg )
RMSEg = sqrt( mean( ( data_frame_test$TARGET_LOSS_AMT - pg )^2 ) )

print(RMSEt)
print(RMSEr)
print(RMSEg)


################################################################################################
#Step4
#Split the data to train and test
data_frame_flag = data_frame
data_frame_flag$TARGET_LOSS_AMT = NULL
FLAG = sample( c( TRUE, FALSE ), nrow(data_frame_flag), replace=TRUE, prob=c(0.7,0.3))
data_frame_train = data_frame_flag[FLAG, ]
data_frame_test = data_frame_flag[! FLAG, ]

#Use any model from Step 2 in order to predict the variable TARGET_BAD_FLAG
#using Random forest for this
rf_model = randomForest( data=data_frame_flag, TARGET_BAD_FLAG ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

#Predict using test data
pr = predict( rf_model, data_frame_test )
head( pr )
RMSEr = sqrt( mean( ( data_frame_test$TARGET_BAD_FLAG - pr )^2 ) )
print(RMSEr)

#Develop three models: Decision Tree, Random Forest, and Gradient Boosting to predict the variable TARGET_LOSS_AMT using only records where TARGET_BAD_FLAG is 1.
#Split the data to train and test
data_frame_amt = subset( data_frame, TARGET_BAD_FLAG == 1 )
data_frame_amt$TARGET_BAD_FLAG = NULL
FLAG = sample( c( TRUE, FALSE ), nrow(data_frame_amt), replace=TRUE, prob=c(0.6,0.4))
data_frame_train = data_frame_amt[FLAG, ]
data_frame_test = data_frame_amt[! FLAG, ]

#Building a decision tree
tr_set = rpart.control( maxdepth = 10 )
tr_model = rpart( data=data_frame_train, TARGET_LOSS_AMT ~ ., control=tr_set, method="poisson" )
rpart.plot( tr_model )
rpart.plot( tr_model, digits=-3, extra=100 )

#Listing the important variables. for decision tree
tr_model$variable.importance

#Random forest
rf_model = randomForest( data=data_frame_train, TARGET_LOSS_AMT ~ ., ntree=500, importance=TRUE )
importance( rf_model )
varImpPlot( rf_model )

#Predict using test data for RF
pr = predict( rf_model, data_frame_test )
head( pr )
RMSEr = sqrt( mean( ( data_frame_test$TARGET_LOSS_AMT - pr )^2 ) )

# GRADIENT BOOSTING

gb_model = gbm( data=data_frame_train, TARGET_LOSS_AMT ~ ., n.trees=500, distribution="poisson" )
summary.gbm( gb_model, cBars=10 )

#Using your models, predict the probability of default and the loss given default.
#Split data again as we have deleted the TARGET_BAD_FLAG above
data_frame_flag = data_frame
# Convert TARGET_BAD_FLAG to a factor (for classification) as it will give error saying prob is not meaningful for regression.
data_frame_flag$TARGET_BAD_FLAG = as.factor(data_frame_flag$TARGET_BAD_FLAG)
FLAG = sample( c( TRUE, FALSE ), nrow(data_frame_flag), replace=TRUE, prob=c(0.7,0.3))
data_frame_train = data_frame_flag[FLAG, ]
data_frame_test = data_frame_flag[! FLAG, ]

# Random Forest model to predict probability of default (TARGET_BAD_FLAG)
rf_default = randomForest(TARGET_BAD_FLAG ~ ., data=data_frame_train, ntree=500, importance=TRUE)

# Predict the probability of default on the test set (probability of TARGET_BAD_FLAG = 1)
prob_default = predict(rf_default, newdata=data_frame_test, type="prob")[, 2]

# Random Forest model to predict loss given default (TARGET_LOSS_AMT)
rf_amt = randomForest(TARGET_LOSS_AMT ~ ., data=data_frame_train, ntree=500, importance=TRUE)

# Predict the loss given default on the test set
predict_loss_given_default = predict(rf_amt, newdata=data_frame_test)

# Multiply probability of default by predicted loss given default
prob_severity = prob_default * predict_loss_given_default
print(prob_severity)

# Actual loss values (TARGET_LOSS_AMT)
actual_loss = data_frame_test$TARGET_LOSS_AMT

# RMSE for the Probability/Severity model
rmse_prob_serverity = sqrt(mean((actual_loss - prob_severity)^2))

print(rmse_prob_serverity)






