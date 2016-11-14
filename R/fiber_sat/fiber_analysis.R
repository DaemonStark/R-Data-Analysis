 # Reading the file
fiber <- read.table(file.choose(),header = F);
fiber_log <- log(fiber)

#Assign variable names
names(fiber_log) <- c("no","X1","X2","X3","X4");

#Run stepwise model seletion to determine the right model
model_0 <-lm(X1~1,data = fiber_log) # Initial model
model_1 <-lm(X1~X2+X3+X4,data = fiber_log) # Full model


#running the stepwise forward model selection process
step(model_0,scope = list(lower=model_0,upper=model_1),direction = "forward")

  rows <- nrow(fiber);

  model_original <- lm(X1 ~ X2 + X4);
  pairs(fiber)

  p <- length(model_original$coefficients);
  n <- length(fiber_log$X1);

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
abline(h=c(-1.5,1.5),lty="dotted");
indent = (max(model_original$fitted.value)-min(model_original$fitted.value))/10
for(i in 1:n){ if(abs(std[i])> 1.5)text(model_original$fitted.value[i]+indent,std[i],i,cex=0.7)}


Internally.Studentized.Residual<-stanres(model_original);
plot(Internally.Studentized.Residual);title("Internally Standardized Residual");
abline(h=c(-1.5,1.5),lty="dotted");
indent=1.2
for(i in 1:n){ if(abs(Internally.Studentized.Residual[i])> 1.5)
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
abline(h=c(-1.5,1.5),lty="dotted")
for(i in 1:n){ if(abs(Externally.Studentized.Residual[i])> 1.5)
  text(i+indent,Externally.Studentized.Residual[i],i,cex=0.7)}

indent <-1.8;
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
#dnsim(sat_data$total,expend+ratio+salary+takers,1000,TRUE)

source("R/fiber_sat/ext_env.R")
ext_env(fiber_log$X1,fiber_log$X2,fiber_log$X4,1000)

source("R/fiber_sat/dffits.R")
dffit_sum(fiber_log$X1,fiber_log$X2,fiber_log$X4,1000)

#################################Deletion##################

fiber_del <- fiber_log[-c(8,18),]
model_del <- lm(fiber_del$X1 ~ fiber_del$X2 + fiber_del$X4);

pairs(fiber_del)
p <- length(model_del$coefficients);
n <- length(fiber_del$X1);
sigma <- summary(model_del)$sigma;
inf <- lm.influence(model_del);
std <- model_del$residuals/summary(model_del)$sigma;

par(mfrow=c(1,3));
plot(model_del$fitted.values,std);
title("Standardized residual plot");
abline(h=c(2,2),lty="dotted");
indent = (max(model_del$fitted.value)-min(model_del$fitted.value))/20
for(i in 1:n){ if(abs(std[i])> 2)text(model_del$fitted.value[i]+indent,std[i],i,cex=0.7)}

Internally.Studentized.Residual<-stanres(model_del);
plot(Internally.Studentized.Residual);title("Internally Standardized Residual");
abline(h=c(2,2),lty="dotted");
indent=1.2;
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
ext_env(fiber_del$X1,fiber_del$X2,fiber_del$X4,1000)

source("R/fiber_sat/dffits.R")
dffit_sum(fiber_del$X1,fiber_del$X2,fiber_del$X4,10000)
