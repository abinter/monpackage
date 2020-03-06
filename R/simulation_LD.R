#' Imputation of zero values
#'
#' Impute missing values, below the limit of detection
#'
#' @param var vector of values to impute
#' @param LD limit of detection
#' @param ID number of individuals
#' @param dist shape of the distribution : lognormal (by default) or normal
#' @param N number of random variables to be generated
#'
#' @examples
#' data<-c(0,45,48,65,47)
#' LOD<-40
#' simulation_LD(data,LOD,5)
#'
#' @import survival stats
#' @export

simulation_LD<-function (var,LD,ID,N=75000,dist="lognormal")
{
  data<-as.data.frame(var)
  data$lower<-NA
  data$upper<-NA
  data$lower[data$var==0 & !is.na(data$var)]<-NA
  data$upper[data$var==0 & !is.na(data$var)]<-LD
  data$lower[data$var!=0 & !is.na(data$var)]<-var[data$var!=0 & !is.na(data$var)]
  data$upper[data$var!=0 & !is.na(data$var)]<-var[data$var!=0 & !is.na(data$var)]

  coef<-survreg(Surv(upper,lower, type="interval2",)~1,data=data,dist=dist)
  mean<-coef$coefficients
  sd<-coef$scale

  DA<- rnorm(N,mean,sd)
  DA<-DA[DA<=log(LD)]
  DA<-exp(DA[1:ID])
  data$DA<-DA

  data$var_imputed<-NA
  data$var_imputed[var==0 & !is.na(data$var)]<-data$DA[data$var==0 & !is.na(data$var)]
  data$var_imputed[var!=0 & !is.na(data$var)]<-data$var[data$var!=0 & !is.na(data$var)]

  return (data$var_imputed)
}
