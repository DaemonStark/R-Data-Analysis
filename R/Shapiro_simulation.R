function(y, x, nsim=1000)
{
  ### y: response x: covariates nsim: Number of simulation repetition
  n<-length(y)
  ### Compute Shapiro-Watson Test R* for given model
  resid <- lm(y~x)$resid
  NU <- 0;
  DE_A <- 0; # Stores the ai ^ 2 value in denominator
  DE_E <- 0; # Stores the ei^2 value in denominator

  sorted_resid <- sort(resid); # Sort the rediduals in ascending order.

  ### Setting up Numerator and Denominator of R* Statistic
  for(i in 1:n){
    a <- qnorm((i-0.375)/(n+0.25));
    NU <- NU + (sorted_resid[i]*a);   # Calculate numerator ai*ei
    DE_A <- DE_A + (a^2); # Calculate the first expression in denominator
    DE_E <- DE_E + (sorted_resid[i]^2);  # Calculate the second expression in denominator
  }
  DE <- sqrt(DE_A * DE_E);
  R <- NU/DE;
  ### Look for the distribution of D under null hypothesis
  Count <- 0
  R_list <- array(,nsim)
  for(i in 1:nsim){
    temp<-rnorm(n);
    resid <- lm(temp~x)$resid;
    sorted_resid <- sort(resid);
    NU <- 0;
    DE_A <- 0; # Stores the ai ^ 2 value in denominator
    DE_E <- 0; # Stores the ei^2 value in denominator
    for(j in 1:n){
      a <- qnorm((j-0.375)/(n+0.25));
      NU <- NU + (sorted_resid[j]*a);   # Calculate numerator ai*ei
      DE_A <- DE_A + (a^2); # Calculate the first expression in denominator
      DE_E <- DE_E + (sorted_resid[j]^2)  # Calculate the second expression in denominator
    }
    DE <- sqrt(DE_A * DE_E);
    R_list[i]<- NU/DE
    if(R > NU/DE){Count <- Count+1}
  }
  ### Produce the result, S-Ws statistic and p-value of the test
  hist(R_list, nclass=30,main='Distribution of S-W\'s R statistics');
  abline(v=R, lty=3)
  result<-c(R, Count/nsim)
  names(result) <- c("R statistic", "p-value")
  return(result)
}
