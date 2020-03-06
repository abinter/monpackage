#' Friendly colors
#'
#' Select nice colors for graphs
#' @return A list of color set : raspberries, strawberries, sunday, blues, diversity, pairedcol, redNblue and bysex
#'
#' @import RColorBrewer
#'
#' @examples
#' nice_colors()$bysex
#'
#'
#' @export

nice_colors<- function(){
  colors<-list(raspberries=brewer.pal(9,"RdPu"),strawberries=brewer.pal(9,"PuRd"),sunday=brewer.pal(9,"YlOrRd"),blues=brewer.pal(9,"BuPu"),diversity=brewer.pal(12,"Set3"),
               pairedcol=brewer.pal(12,"Paired"),redNblue=brewer.pal(11,"RdYlBu"),bysex=c("#000000","#318CE7","#C71585"),bysex2=c("#000000","#3690C0","#CE1256"))
  return(colors)
}
