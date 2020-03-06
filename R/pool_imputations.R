#' Pool imputations
#'
#' Pool estimators from multiple imputations
#'
#' @param beta estimates from regression models
#' @param sd standard errors from regression models
#' @param N number of multiple imputations or simulations
#'
#' @return pooled beta and 95\% confidence interval
#'
#' @examples
#' beta<-c(45,42)
#' sd<-c(14,19)
#' pool_imputations(beta,sd,2)
#' @import magrittr
#' @importFrom magrittr %>%
#' @export

pool_imputations<-function(beta,sd,N){
  Q<-beta %>% sum()/N
  U<-sd**2 %>% sum()/N
  B<- (beta-Q)**2 %>% sum()/(N-1)
  T<-U+(1+(1/N))*B
  t<-(N-1)*(1+(U/((1+N**(-1))*B)))**2
  qt<-qt(0.975,t)
  beta_inf<-Q-qt*sqrt(T)
  beta_sup<-Q+qt*sqrt(T)
  beta_CI<-cbind(Q,beta_inf,beta_sup)
  return(beta_CI)
}
