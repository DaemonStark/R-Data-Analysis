#FUnction to perform logistic regression using churn call data to predict whether the customer will leave or stay
# Cross validation of 10 folds is used for this model
logistic_regression_cv <- function()
{
#read data from csv file
churn_data <- read.csv("data/churn.csv",header = T,sep = ",");

churn_data_frame<- as.data.frame(churn_data);

#create a training model with 60% of the original data.
train <- createDataPartition(churn_data_frame$LEAVE,p=0.60,list = FALSE)

train_data <- churn_data_frame[ train, ]
test_data <- churn_data_frame[ -train, ]

ctrl <-trainControl(method = "repeatedcv",number = 10,savePredictions = TRUE)

model<-train(LEAVE~INCOME+HOUSE+HANDSET_PRICE+COLLEGE,data = train_data,method="glm",family = "binomial",trControl=ctrl,tuneLength=5)

pred <- predict(model,newdata = test_data)

matrixValidation <- confusionMatrix(data=pred,test_data$LEAVE)

print(matrixValidation);

}
