files.names <- list.files(path=indir,pattern="*.json",all.files = T);options(digits.secs=6);
startTime<-Sys.time();
for (i in 1:length(files.names))
{
  json.file <- file.path(jsonpath,files.names[i]);
  json.reader <- fromJSON(file = json.file); 
  json.frames <- as.data.frame(json.reader);
  filtered.dataframe <- json.frames[,-9]
  header <- head(json.frames,1);
  CSVFileName <- paste(substr(files.names[i],1,7),"csv",sep = ".");
  out.file <- file.path(jsonpath,CSVFileName);
  write.table(filtered.dataframe,file=out.file,sep = ",",row.names = F,col.names = T);
}
endTime<-Sys.time();
TotalTime<-endTime-startTime;