#' Forestplot graphs
#'
#' Print graphs by groups (in total and by sex)
#'
#' @param data Specificied as on the example : "O:/Codes R/Forestplot/shbg.rdata"
#' @param exposure Names indicating the explicative parameter (such as exposure indicator)
#' @param titre Title of the graph
#'
#' @return A forest plot
#'
#' @import grid forestplot
#'
#' @examples
#' data<-as.data.frame(c("all","girls","all","girls","boys","boys"))
#' colnames(data)<-"pop"
#' data$Pvalue<-c(0.05,0.02,0.1,0.001,0.8,0.05)
#' data$Estimate<-c(1.1,1.2,2.3,1.26,1.01,1.1)
#' data$LowerCL<-c(1.05,1.15,0.9,1.20,0.6,1.05)
#' data$UpperCL<-c(1.15,1.25,3.1,1.30,1.6,1.15)
#' data$CI<-c("[1.05,1.15]","[1.15;1.25]","[0.9;3.1]","[1.20;1.30]","[0.6;1.6]","[1.05,1.15]")
#' forestplot_sex(data,c("example n°1","example n°2"),"Example")
#'
#' @export

forestplot_sex<-function (data,exposure,titre){
  All<-data[as.character(data$pop)=="all",]
  Boys<-data[as.character(data$pop)=="boys",]
  Girls<-data[as.character(data$pop)=="girls",]
  gp<-gpar(col="grey", fill="white")
  res_plot<-forestplot(mean=cbind(All$Estimate, Boys$Estimate, Girls$Estimate),
             lower=cbind(All$LowerCL, Boys$LowerCL, Girls$LowerCL),
             upper=cbind(All$UpperCL, Boys$UpperCL, Girls$UpperCL),
             title=titre,
             labeltext=text_forestplot(data,exposure),
             txt_gp = fpTxtGp(label = gpar(cex=0.4)),
             legend=c("All", "Boys","Girls"),
             legend_args=fpLegend(pos = "top", r = unit(.2, "npc"), title = NULL),
             clip=c(-1, 1),
             xticks=c(-1,-0.5,0,0.5,1),
             boxsize=boxsize_forestplot(data),
             col=fpColors(box=nice_colors()$bysex,lines="black",zero="grey"),
             #hrzl_lines=list("2" = gpar(lwd=0.3, columns=1:5),
             xlab="Beta estimates",
             new_page=F,
             xticks.digits=5,
             ci.vertices=T,
             ci.vertices.height=.05,
             align="c",
             fn.ci_norm=fpDrawNormalCI,
             lwd.ci=1.5,
             graph.pos= "right",
             graphwidth=unit(1.5, "inches"),
             colgap=unit(0.02,"inches"),
             lineheigth=unit(0.20, "inches"),
             line.margin=unit(0.20,"inches"),
             cex=5,
             mar=unit(rep(2, times = 4), "mm"))
  return(res_plot)
}


