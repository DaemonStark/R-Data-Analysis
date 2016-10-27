CsvCall <- function()
{
  indir="C:/Rashnil/UNC/Study/Fall2016/AWS Hadoop/AWS/data";
  library("rjson");
  files.names <- list.files(path=indir,pattern="*.json",all.files = T);
  for (i in 1:length(files.names))
  {
    json.file <- file.path(indir,files.names[i]);
    singlejsonToCsv(json.file);
  }

  }
