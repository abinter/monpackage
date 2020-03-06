#' Correlation graphics
#'
#' Build a correlation matrix for correlation plots
#'
#' @param mat A matrix of continuous variables
#'
#' @import stats corrplot graphics
#' @examples
#'
#' Pierre <- c(8,9,15)
#' Nathalie <- c(17,12,13)
#' Jacques <- c(11,15,7)
#' Julie <- c(5,12,19)
#' matrice <- matrix(c(Pierre, Nathalie, Jacques, Julie), nrow=4, ncol=3, byrow=TRUE)
#' M<-cor(na.omit(matrice),method="spearman")
#' p.mat <- cortest(na.omit(matrice))
#' #corrplot(M, method="shade",type="upper",tl.col="black", tl.srt=45,p.mat = p.mat,
#' #sig.level = 0.05, insig = "blank",addCoef.col = "black",diag=F)
#' @export

cortest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
