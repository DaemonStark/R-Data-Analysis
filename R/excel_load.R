fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv";

download.file(fileUrl,destfile = "R/microSurvey/data.csv");

val <- microData$VAL;

microData <-read.csv(file = "R/microSurvey/data.csv",sep = ",");

sum(na.omit(val)>=24);

xlurl<- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx"
download.file(xlurl,destfile = "R/microSurvey/data2.xlxs")

library("xlsx")

titanic <- read.xlsx("R/microSurvey/titanic.xls",sheetIndex = 1)
names(titanic)

chisq.test(titanic$pclass,titanic$survived)

fisher.test(titanic$pclass,titanic$survived)

mushroom <- read.csv("R/microSurvey/mushroom.csv",header = T)
names(mushroom)
