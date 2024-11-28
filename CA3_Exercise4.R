#3.3)

#1) and 2)

df<-with(mtcars,data.frame(y=mpg,x1=disp,x2=hp,x3=wt))
nll_lm<- function(par,...){
  df<-list(...)$df
  beta0<-par[1]
  beta1<-par[2]
  beta2<-par[3]
  beta3<-par[4]
  sigma<-par[5]

  y_pred<- beta0+beta1*df$x1 + beta2* df$x2 + beta3*df$x3
  resid<-df$y-y_pred
  nll<- -sum(dnorm(resid,0,sigma,log=T))
  return(nll)

}
mod<-lm(mpg~disp+hp+wt,data=mtcars)
mod_sum<-summary(mod)
param<- c(mod$coefficients[1:4],mod_sum$sigma)
nll_lm(par=param,df=df)

#3)

init_params<-c(beta0=mean(df$y),
               beta1=0,
               beta2=0,
               beta3=0,
               sigma=sd(df$y))

lower_bound<-c(-Inf,-Inf,-Inf,-Inf,1e-8)
upper_bound<-c(Inf,Inf,Inf,Inf,Inf)
optimisation<-optim(par=init_params,
                    fn= nll_lm,
                    df=df,
                    method="L-BFGS-B",
                    lower = lower_bound,
                    upper= upper_bound)
optimised_params<-optimisation$par
optimised_params




#4)
#It was necessary to implement the negative log likelihood function because it provides a function that optim can minimize in order to estimate the parameters.

#5)

X<-cbind(1,mtcars$disp,mtcars$hp,mtcars$wt)
y<-as.matrix(mtcars$mpg)
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
beta_hat
optimised_params[1:4]

##6)

residuals<-y - X %*% beta_hat
n<- nrow(X)
p<-ncol(X)
sigma_hat<- sqrt(sum(residuals^2)/(n-p))
sigma_hat
optimised_params[5]



##7)
##The reason for the differences in sigma hat is due to the fact that my optimisation with nll accounts for all predictors when estimating sigma hat and beta hat and does so at the same time, whereas using the base R approach I am calculating sigma hat based on the residuals after I have estimated beta hat.

##8)

regression_coeff<-c(mod$coefficients,mod_sum$sigma)

modified_optim<-optim(par=regression_coeff,
                      fn= nll_lm,
                      df=df,
                      method="L-BFGS-B",
                      lower = lower_bound,
                      upper= upper_bound)
modified_optim$par
##3.4

##1)
#####Modified
beta_hat_4<-mod$coefficients
beta_hat_4
sigma_hat_4<-mod_sum$sigma
sigma_hat_4
