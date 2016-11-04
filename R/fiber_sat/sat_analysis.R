sat_analysis <- function()
{
  #install package faraway install.packages("faraway")
  library("faraway");
  sat_data<- sat;

  rows <- nrow(sat_data);

  model1 <- lm(total~expend+ratio+salary+takers);

  pairs(sat_data);

  p <- length(model$coefficients);
  n <- length(sat_data$total);

  sigma <- summary(model1)$sigma;

  inf <- lm.influence(model1);

  std <- model$residuals/summary(model)$sigma;


}

stanres<-function(fit,lms=summary(fit),lmi=lm.influence(fit))
{
  h<-lmi$hat
  e<-residuals(fit)
  s<-lms$sigma
  si<-lmi$sigma
  e/(s*(1-h)^.5)
}



par(mfrow=c(1,3));
plot(model$fitted.values,std);
title("Standardized residual plot");
abline(h=c(-2,2),lty="dotted");
indent = (max(model$fitted.value)-min(model$fitted.value))/20
for(i in 1:n){ if(abs(std[i])> 2)text(model$fitted.value[i]+indent,std[i],i,cex=0.6)}

Internally.Studentized.Residual<-stanres(model);
plot(Internally.Studentized.Residual);title("Internally Standardized Residual");
abline(h=c(-2,2),lty="dotted");

for(i in 1:n){ if(abs(Internally.Studentized.Residual[i])> 2)
  {text(i+indent,Internally.Studentized.Residual[i],i,cex=0.6)}
  }


studres<-function(fit,lmi=lm.influence(fit)){
  h<-lmi$hat
  e<-residuals(fit)
  si<-lmi$sigma
  5
  e/(si*(1-h)^.5)
}

Externally.Studentized.Residual<-studres(model)
plot(Externally.Studentized.Residual);title("Externally Studentized Residual")
abline(h=c(-2,2),lty="dotted")
for(i in 1:n){ if(abs(Externally.Studentized.Residual[i])> 2)
text(i+indent,Externally.Studentized.Residual[i],i,cex=0.6)}

par(mfrow=c(1,4))
qqnorm(Externally.Studentized.Residual);abline(c(0,0),c(1,1))
plot(inf$hat);title("leverage plot")
abline(h=2*p/n,lty=3) ### high leverage points
leverage<-c(inf$hat>2*p/n)
for(i in 1:n){ if(leverage[i]==T)text(i+indent,inf$hat[i],i,cex=0.8)}

dffits<-function(fit,lmi=lm.influence(fit)){
  h<-lmi$hat
  e<-residuals(fit)
  si<-lmi$sigma
  h^0.5*e/(si*(1-h))
}

DFFITS <- dffits(model)
plot(DFFITS);abline(h=2*sqrt(p/n),lty=2);abline(h=-2*sqrt(p/n),lty=2);title("DFFITS")
DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n))
for(i in 1:n){ if(DF.detected[i]==T)text(i+indent,DFFITS[i],i,cex=0.8)}

sxxi<- diag(summary(model)$cov.unscaled)
si <- inf$sigma
bi<- coef(inf);
DFBETAS<-bi/(si %o% sxxi^0.5);
plot(DFBETAS[,1],pch=1,ylab='dfbetas',ylim=c(min(DFBETAS),max(DFBETAS)));
abline(h=2*sqrt(1/n),lty=2);abline(h=-2*sqrt(1/n),lty=2);title("DFBETAS")

for (k in 1:p) {
points(DFBETAS[,k],pch=k)
DFB.detected<- c(abs(DFBETAS[,k])> 2*sqrt(1/n))
for(i in 1:n){ if(DFB.detected[i]==T)text(i+indent,DFBETAS[i,k],i,cex=0.6)}
}
legend("bottomleft", c("beta0","beta1","beta2","beta3","beta4"), pch = c(1:p))


par(mfrow=c(1,4))
plot(model,which=4)
first <- ((n-p-1) / (n-p))+ Externally.Studentized.Residual^2/(n-p)
COVRATIO <- first^(-p) /(1-inf$hat)
plot(COVRATIO);abline(h=3*p/n+1,lty=2);abline(h=-3*p/n+1,lty=2);title("COVRATIO")
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){ if(CR.detected[i]==T)text(i+indent,COVRATIO[i],i,cex=0.6)}
for(i in 1:n){ if(CR.detected[i]==T)text(i+indent,COVRATIO[i],i,cex=0.8)}

source("dnsim.R")
dnsim(sat_data$total,expend+ratio+salary+takers,1000,TRUE)

