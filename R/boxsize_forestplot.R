#' Size for the boxes of forestplot
#'
#' Boxes have to be smaller for stratified analyzed (boys/girls) than total.
#'
#' @param data Specificied as on the example : "O:/Codes R/Forestplot/shbg.rdata"
#'
#' @examples
#' data<-as.data.frame(c("all","girls","all","all","boys"))
#' colnames(data)<-"pop"
#' boxsize_forestplot(data)
#'
#' @export

boxsize_forestplot<-function(data){
  for (i in 1: nrow(data)){
    if (as.character(data$pop[i])=="all"){
      data$box[i]<-0.17
    }else{
      data$box[i]<-0.10
    }
  }
  All<-data[as.character(data$pop)=="all",]
  Boys<-data[as.character(data$pop)=="boys",]
  Girls<-data[as.character(data$pop)=="girls",]

  return(c(All$box,Boys$box,Girls$box))
}
