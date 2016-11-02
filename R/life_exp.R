
#State Life Expectancy data
state<- data.frame(state.x77,region=state.region);
xyplot(Life.Exp ~Income|region,data=state,layout=c(4,1));


hist(airquality$Ozone)
with(airquality,plot(airquality$Wind,airquality$Ozone))
with(airquality,plot(airquality$Wind,airquality$Ozone,pch=1))

with(airquality,plot(airquality$Wind,airquality$Ozone,pch="@"))
boxplot(airquality$Ozone~airquality$Month)

with(airquality,plot(Wind,Ozone,main="Ozone and Wind"))
legend("topright",pch=2,col=c("blue","red"),legend = c("May","No Month"))
model <- lm(Ozone~Wind,airquality)
abline(model,lwd=2)

#Visualize air quality by looking at scatter plots
atmosphere <- function(airquality)
{
  par(mfrow=c(1,3),mar=c(4,4,2,1),oma=c(0,0,2,0));
  with(airquality,{
    plot(Wind,Ozone,main="Ozone and Wind")
    plot(Solar.R,Ozone,main="Solar and Ozone")
    plot(Temp,Ozone,main="Temp vs Ozone")
    mtext("Ozone and Weather in NYC",outer = TRUE)
    });

}



