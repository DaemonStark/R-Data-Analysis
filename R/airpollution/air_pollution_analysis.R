load_nc_air_pm_data <- function(path)
{
  air_data <- read.csv(path,header = T,sep = ",");

  air_data_frame <- air_data[,c(1,4,5,6,12,17,18)];

return(air_data_frame);


}

nc_pm_box <-function(data)
{
  par(mfrow=c(2,1),mar=c(4,4,2,1));

  boxplot(data,col = "blue");
  abline(h=15.5);

  barplot(table(data),col="wheat",main="counties");
}


nc_pm_hist <- function(data)
{

  hist(data,col = "green"
       ,breaks = 100
       , main = "Air Pollution PM2.5 Level");
  abline(v=15.5,lwd=2);
  abline(v=median(data),col="magenta",lwd=4);

  rug(data)

}

nc_scatter_plot <-function(data_frame)
{
  with(data_frame,plot(data_frame$SITE_LATITUDE,data_frame$Daily.Mean.PM2.5.Concentration,col=data_frame$CBSA_NAME,ylab="Pm2.5",xlab="Latitude" ));
  abline(h=15.5,lwd=2,lty=2);
}
