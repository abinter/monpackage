#' Tercile
#'
#' Categorize a continuous variable among terciles
#'
#' @param data A continuous variable
#' @param na Boolean, omit (T) or not (F) missing values
#'
#' @return data_cl, factor with 3 levels
#'
#' @examples
#' tercile(c(112,15,18,15,415,41,5,4,44,51,51,2,65,4,8,5,5,8,45,489))
#'
#' @export

tercile<-function (data,na=F){
  quant<-quantile(data, probs = seq(0, 1, 1/3), na.rm = na)
  print(quant)
  data_cl<-0
  data_cl[data<quant[2]]<-1
  data_cl[data>=quant[2] & data<quant[3]]<-2
  data_cl[data>=quant[3]]<-3
  return(data_cl)
}

