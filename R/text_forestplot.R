#' Text for forestplot
#'
#' Estimates and 95%CI in total and by sex
#'
#' @param exposure Names indicating the explicative parameter (such as exposure indicator)
#' @param data Specificied as on the example : "O:/Codes R/Forestplot/shbg.rdata"
#'
#' @examples
#' data<-as.data.frame(c("all","girls","all","girls","boys","boys"))
#' colnames(data)<-"pop"
#' data$Pvalue<-c(0.05,0.02,0.1,0.001,0.8,0.05)
#' data$Estimate<-c(1.1,1.2,2.3,1.26,1.01,1.1)
#' data$CI<-c("[1.05,1.15]","[1.15;1.25]","[0.9;3.1]","[1.20;1.30]","[0.6;1.6]","[1.05,1.15]")
#' text_forestplot(data,c("example n° 1","example n°2"))
#'
#' @export

text_forestplot<-function(data,exposure){
  for (i in 1:nrow(data)){
    if (data$Pvalue[i]<=0.05){
      data$pval1[i]<-"**"
    }else if (data$Pvalue[i]<=0.10){
      data$pval1[i]<-"*"
    }else{
      data$pval1[i]<-""
    }
  }

  All<-data[as.character(data$pop)=="all",]
  Boys<-data[as.character(data$pop)=="boys",]
  Girls<-data[as.character(data$pop)=="girls",]

  pvalue<-paste(All$pval1,Boys$pval1,Girls$pval1,sep="\n")
  estim.all<-formatC(round(All$Estimate,2), format='f', digits=2 ,2)
  estim.boys<-formatC(round(Boys$Estimate,2), format='f', digits=2 ,2)
  estim.girls<-formatC(round(Girls$Estimate,2), format='f', digits=2 ,2)
  for (i in 1:length(estim.all)){
    if (estim.all[i]=="NA") {estim.all[i]<-"-"}
    if (estim.boys[i]=="NA") {estim.boys[i]<-"-"}
    if (estim.girls[i]=="NA") {estim.girls[i]<-"-"}
  }
  estim<-paste(estim.all,estim.boys,estim.girls,sep="\n")
  for (i in 1:length(All$CI)){
    if (All$CI[i]=="[  NA  ;   NA ]") {All$CI[i]<-"-"}
    if (Boys$CI[i]=="[  NA  ;   NA ]") {Boys$CI[i]<-"-"}
    if (Girls$CI[i]=="[  NA  ;   NA ]") {Girls$CI[i]<-"-"}
  }
  CI<-paste(All$CI,Boys$CI,Girls$CI,sep="\n")
  return(cbind(exposure,estim,CI,pvalue))
}

