#' Distribution graphs
#'
#' Graphs to check the shape of a continuous variable's distribution
#'
#' @param x a vector of continuous value
#'
#' @examples
#' Dist.forme(c(14,15,18,21,20,22))
#'
#' @import graphics
#'
#' @export

Dist.forme<-function(x)
{
  par(mfrow=c(2,2))
  x.naomit<-na.omit(x)
  hist(x.naomit, col="gray",
       prob=TRUE,xlim=c(min(x.naomit),max(x.naomit)), main="")
  curve(dnorm(x,mean=mean(x.naomit),sd=sd(x.naomit)),add=TRUE,lwd
        =2,col="red")
  boxplot(x.naomit)
  iqd<-summary(x.naomit)[5] - summary(x.naomit)[2]
  points(mean(x.naomit), col = "orange", pch = 18)
  plot(density(x.naomit,width=2*iqd),xlab="x",ylab="",type="l",main="")
  qqnorm(x.naomit)
  qqline(x.naomit)
}
