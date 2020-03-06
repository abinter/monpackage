#' Imputation of NA
#'
#' Impute missing values, below the limit of detection
#'
#' @param var vector of values to impute
#' @param LD limit of detection
#' @param ID number of individuals
#' @param N number of random variables to be generated
#'
#' @examples
#' data<-c(NA,45,48,65,47)
#' LOD<-40
#' simulation_NA(data,LOD,5)
#'
#' @return vector of values
#' @import survival stats
#' @export

simulation_NA<-function (var,LD,ID,N=5000000)
{
  data<-as.data.frame(var)
  data$lower<-NA
  data$upper<-NA
  data$lower[is.na(data$var)]<-NA
  data$upper[is.na(data$var)]<-LD
  data$lower[!is.na(data$var)]<-data$var[!is.na(data$var)]
  data$upper[!is.na(data$var)]<-data$var[!is.na(data$var)]

  coef<-survreg(Surv(upper,lower, type="interval2",)~1,data=data,dist="lognormal")
  mean<-coef$coefficients
  sd<-coef$scale

  DA<- rnorm(N,mean,sd)
  DA<-DA[DA<=log(LD)]
  DA<-exp(DA[1:ID])
  data$DA<-DA

  data$var_imputed<-NA
  data$var_imputed[is.na(data$var)]<-data$DA[is.na(data$var)]
  data$var_imputed[!is.na(data$var)]<-data$var[!is.na(data$var)]

  return (data$var_imputed)
}
