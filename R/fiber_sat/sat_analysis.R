  #install package faraway install.packages("faraway")
  #import faraway package
  library("faraway");

  #Create a data frame for performing diagnostics
  sat_original<- sat;

  rows <- nrow(sat_original);

  attach(sat_original)

  #Run stepwise model selection to determine the right model
  model_0 <-lm(total~1,data = fiber_log) # Initial model
  model_1 <-lm(total~expend+ratio+salary+takers,data = fiber_log) # Full model
  step(model_0,scope = list(lower=model_0,upper=model_1),direction = "forward")


  model_original <- lm(sat_original$total ~ sat_original$takers + sat_original$expend);

  pairs(sat_original);

  p <- length(model_original$coefficients);
  n <- length(sat_original$total);
  sigma <- summary(model_original)$sigma;
  inf <- lm.influence(model_original);
  std <- model_original$residuals/summary(model_original)$sigma;


stanres<-function(fit,lms=summary(fit),lmi=lm.influence(fit))
{
  h<-lmi$hat
  e<-residuals(fit)
  s<-lms$sigma
  si<-lmi$sigma
  e/(s*(1-h)^.5)
}



par(mfrow=c(1,3));
plot(model_original$fitted.values,std);
title("Standardized residual plot");
abline(h=c(-2,2),lty="dotted");
indent = (max(model_original$fitted.value)-min(model_original$fitted.value))/20
for(i in 1:n){ if(abs(std[i])> 2)text(model_original$fitted.value[i]+indent,std[i],i,cex=0.7)}

Internally.Studentized.Residual<-stanres(model_original);
plot(Internally.Studentized.Residual);title("Internally Standardized Residual");
abline(h=c(-2,2),lty="dotted");

for(i in 1:n){ if(abs(Internally.Studentized.Residual[i])> 2)
  {text(i+indent,Internally.Studentized.Residual[i],i,cex=0.7)}
  }


studres<-function(fit,lmi=lm.influence(fit)){
  h<-lmi$hat
  e<-residuals(fit)
  si<-lmi$sigma
  e/(si*(1-h)^.5)
}

Externally.Studentized.Residual<-studres(model_original)
plot(Externally.Studentized.Residual);title("Externally Studentized Residual")
abline(h=c(-2,2),lty="dotted")
for(i in 1:n){ if(abs(Externally.Studentized.Residual[i])> 2)
text(i+indent,Externally.Studentized.Residual[i],i,cex=0.6)}

par(mfrow=c(1,4))
qqnorm(Externally.Studentized.Residual);abline(c(0,0),c(1,1))
plot(inf$hat);title("leverage plot")
abline(h=2*p/n,lty=3) ### high leverage points
leverage<-c(inf$hat>2*p/n)
for(i in 1:n){ if(leverage[i]==T)text(i+indent,inf$hat[i],i,cex=0.7)}

dffits<-function(fit,lmi=lm.influence(fit)){
  h<-lmi$hat
  e<-residuals(fit)
  si<-lmi$sigma
  h^0.5*e/(si*(1-h))
}

DFFITS <- dffits(model_original)
plot(DFFITS);abline(h=2*sqrt(p/n),lty=2);abline(h=-2*sqrt(p/n),lty=2);title("DFFITS")
DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n))
for(i in 1:n){ if(DF.detected[i]==T)text(i+indent,DFFITS[i],i,cex=0.8)}

sxxi<- diag(summary(model_original)$cov.unscaled)
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
legend("bottomleft", c("beta0","beta1","beta2"), pch = c(1:p))


par(mfrow=c(1,4))
plot(model_original,which=4)

first <- ((n-p-1) / (n-p))+ Externally.Studentized.Residual^2/(n-p)
COVRATIO <- first^(-p) /(1-inf$hat)
plot(COVRATIO);abline(h=3*p/n+1,lty=2);abline(h=-3*p/n+1,lty=2);title("COVRATIO")
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){ if(CR.detected[i]==T)text(i+indent,COVRATIO[i],i,cex=0.8)}

#source("dnsim.R")
#dnsim(sat_original$total,expend+ratio+salary+takers,1000,TRUE)

source("R/fiber_sat/ext_env.R")
ext_env(sat_original$total,expend,takers,1000)

source("R/fiber_sat/dffits.R")
dffit_sum(sat_original$total,expend,takers,1000)

#################################Deletion##################

sat_data_del <-sat_original[-c(29,34,48),]

model_del <- lm(sat_data_del$total ~ sat_data_del$takers + sat_data_del$expend);

pairs(sat_data_del)
p <- length(model_del$coefficients);
n <- length(sat_data_del$total);
sigma <- summary(model_del)$sigma;
inf <- lm.influence(model_del);
std <- model_del$residuals/summary(model_del)$sigma;

par(mfrow=c(1,3));
plot(model_del$fitted.values,std);
title("Standardized residual plot");
abline(h=c(-2,2),lty="dotted");
indent = (max(model_del$fitted.value)-min(model_del$fitted.value))/20
for(i in 1:n){ if(abs(std[i])> 2)text(model_del$fitted.value[i]+indent,std[i],i,cex=0.7)}

Internally.Studentized.Residual<-stanres(model_del);
plot(Internally.Studentized.Residual);title("Internally Standardized Residual");
abline(h=c(-2,2),lty="dotted");
for(i in 1:n){ if(abs(Internally.Studentized.Residual[i])> 2)
{text(i+indent,Internally.Studentized.Residual[i],i,cex=0.7)}}


Externally.Studentized.Residual<-studres(model_del)
plot(Externally.Studentized.Residual);title("Externally Studentized Residual")
abline(h=c(-2,2),lty="dotted")
for(i in 1:n){ if(abs(Externally.Studentized.Residual[i])> 2)
text(i+indent,Externally.Studentized.Residual[i],i,cex=0.7)}

par(mfrow=c(1,4))
qqnorm(Externally.Studentized.Residual);abline(c(0,0),c(1,1))
plot(inf$hat);title("leverage plot")
abline(h=2*p/n,lty=3) ### high leverage points
leverage<-c(inf$hat>2*p/n)
for(i in 1:n){ if(leverage[i]==T)text(i+indent,inf$hat[i],i,cex=0.7)}

DFFITS <- dffits(model_del)
plot(DFFITS);abline(h=2*sqrt(p/n),lty=2);abline(h=-2*sqrt(p/n),lty=2);title("DFFITS")
DF.detected <- c(abs(DFFITS)> 2*sqrt(p/n))
for(i in 1:n){ if(DF.detected[i]==T)text(i+indent,DFFITS[i],i,cex=0.8)}

sxxi<- diag(summary(model_del)$cov.unscaled)
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
legend("bottomleft", c("beta0","beta1","beta2"), pch = c(1:p))

par(mfrow=c(1,4))
plot(model_del,which=4)

first <- ((n-p-1) / (n-p))+ Externally.Studentized.Residual^2/(n-p)
COVRATIO <- first^(-p) /(1-inf$hat)
plot(COVRATIO);abline(h=3*p/n+1,lty=2);abline(h=-3*p/n+1,lty=2);title("COVRATIO")
CR.detected <- c(abs(COVRATIO-1)>3*p/n)
for(i in 1:n){ if(CR.detected[i]==T)text(i+indent,COVRATIO[i],i,cex=0.8)}

source("R/fiber_sat/ext_env.R")
ext_env(sat_data_del$total,sat_data_del$expend,sat_data_del$takers,1000)

source("R/fiber_sat/dffits.R")
dffit_sum(sat_data_del$total,sat_data_del$expend,sat_data_del$takers,1000)
