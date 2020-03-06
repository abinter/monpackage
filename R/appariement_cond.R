#'Appairing cases and controls
#'
#' We registered case and control population ids. Then we sample without replacement controls.
#'
#' @param data Dataframe with variables for cases and controls
#' @param case_cond Variable distinging cases from controls
#' @param cond Appairing variable
#' @param valeur_cond Value of the apparing variable
#' @param nb_temoins Number of controls for one case
#' @param id Identifier columns ( in character)
#'
#' @examples
#'data<-as.data.frame(c(1,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,1,0,1))
#'colnames(data)<-"hyperactivity"
#'data$sex<-c(1,1,2,1,1,2,1,1,1,1,1,2,2,2,1,1,1,1,2,1)
#'data$id<-seq(1,length(data$hyperactivity))
#'appariement_cond(data,data$hyperactivity,data$sex,1,2,"id")
#'
#' @export


appariement_cond<-function(data,case_cond, cond,valeur_cond,nb_temoins,id){
  case<-data[(!is.na(case_cond) & case_cond==1 & cond==valeur_cond),id]
  pop<-data[(!is.na(case_cond) & case_cond==0 & cond==valeur_cond),id]
  control<-NA
  for (i in 1:length(case)){
    Usample<-sample(pop,nb_temoins,replace=F)
    if (i==1){
      control<-Usample
    }else{
      control<-c(control,Usample)
    }
    j<-1
    for (j in 1:nb_temoins){
      pop<-pop[(pop != Usample[j])]
    }
  }
  return(control)
}

