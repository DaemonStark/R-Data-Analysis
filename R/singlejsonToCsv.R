# Functionn to create CSV file in the specified output repository for the received json file received as input
# using fromJSON function of rjson package

singlejsonToCsv <- function(json.file)
{ #import library rjson
  library("rjson");
  
  #setting the output directory path.
  outdir="C:/Rashnil/UNC/Study/Fall2016/AWS Hadoop/AWS/Altmetric Sample Data/CSV/10";
  
  #Read JSON file into a variable and create dataframe using fromjson
  json.reader <- fromJSON(file = json.file,unexpected.escape = "keep");
  json.frames <- as.data.frame(json.reader);
  
  # Create file cSV file name using JSON file name.
  CSVFileName <- paste(substr(json.file),1,7),"csv",sep = ".");
  
  #Get the output directory path and write the csv file.
  out.file <- file.path(outdir,CSVFileName);
  write.table(json.frames,file=out.file,sep = ",",row.names = F,col.names = T);
  
  #Remove attributes after use
  remove(json.frames);
  remove(json.reader);
  remove(CSVFileName);
}
