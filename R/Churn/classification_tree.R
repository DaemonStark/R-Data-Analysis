class_tree <- function()
{
  #import caret package install.packages("caret")
  library("caret"); #or "tree" package

  #read data from csv file
  churn_data <- read.csv("data/churn.csv",header = T,sep = ",");

  churn_data_frame<- as.data.frame(churn_data);

  #create a training model with 60% of the original data.
  train <- createDataPartition(churn_data_frame$LEAVE,p=0.60,list = FALSE);

  train_data <- churn_data_frame[ train, ];
  test_data <- churn_data_frame[ -train, ];

  ctrl <-trainControl(method = "repeatedcv",number = 10,savePredictions = TRUE)

  model <- train(LEAVE~INCOME+HOUSE+HANDSET_PRICE+COLLEGE, data = train_data, method='ctree', tuneLength=10,
                 trControl=ctrl);

  pred_tree<-predict(model,newdata = test_data);

  treeValid <- confusionMatrix(data=pred_tree,test_data$LEAVE);

  summary(model);

  plot(model);

  #Anoter process using ctree #party package
  model<-ctree(LEAVE~INCOME+HOUSE+HANDSET_PRICE+COLLEGE,data = train_data);
  plot(model)

}
