merge<-function()
{
  setwd("C:/Rashnil/UNC/Study/Fall2016/AWS Hadoop/AWS/data")

  file_list <- list.files(pattern = "*.json");

  for (file in file_list){

    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- fromJSON(file)
      data.csv <-data.frame(dataset);
    }

    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <-read.table(file)
      dataset<-rbind(dataset, temp_dataset)
      rm(temp_dataset)
    }

  }
  write.table(data.csv,file = "Test.csv")
}
