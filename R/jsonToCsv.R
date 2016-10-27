jsonToCsv <- function(indir,outdir)
{
library("rjson");
files.names <- list.files(path=indir,pattern="*.json",all.files = T);
options(digits.secs=6); #Set the timing to track in millisecs.
startTime<-Sys.time();
for (i in 1:length(files.names))
{
  json.file <- file.path(indir,files.names[i]);
  json.reader <- fromJSON(file = json.file,unexpected.escape = "skip" );
  json.frames <- as.data.frame(json.reader);
  filtered.dataframe <- json.frames[,-9]  #Remove column 9 abstract.
  CSVFileName <- paste(substr(files.names[i],1,7),"csv",sep = ".");
  out.file <- file.path(outdir,CSVFileName);
  write.table(filtered.dataframe,file=out.file,sep = ",",row.names = F,col.names = T);
}
endTime<-Sys.time();
TotalTime<-endTime-startTime;
cat("No of Files Processed: ",length(files.names))
cat("\nTotal Time Taken: ", TotalTime);

}
