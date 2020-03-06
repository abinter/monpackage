#' Install and load packages
#'
#' Automatically install a package from Lyon CRAN mirror and load it
#'
#' @param name_library Name of the package
#'
#' @importFrom utils install.packages
#'
#' @examples
#' install_load_packages("installr")
#'
#' @export

install_load_packages<- function(name_library) {
  install.packages(name_library,dependencies=T,repos="https://pbil.univ-lyon1.fr/CRAN/")
    parse(text = paste("library (",name_library,")",sep=""))
}
