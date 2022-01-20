#' matrix2dataframe
#'
#' Converts a numeric matrix to a data frame with decreasing or increasing values:
#' First column row index, second column col index and third column the value.
#' If the matrix is symmetric, only the upper triangle is taken into account.
#'
#' @param m numeric: a matrix of values
#' @param decreasing logical: should the sort order be increasing or decreasing (default: \code{TRUE})
#' @inheritParams base::isSymmetric
#'
#' @return a data frame with an attribute \code{matrix} with \code{m}
#' @export
#'
#' @examples
#' # non-symmetric
#' x <- matrix(runif(9), ncol=3)
#' matrix2dataframe(x)
matrix2dataframe <- function(m, decreasing=TRUE, tol=100*.Machine$double.eps, tol1=8*tol, ...) {
  stopifnot(is.matrix(m))
  ret       <- m
  if (is.null(colnames(ret))) colnames(ret) <- sprintf("col %.0f", 1:ncol(ret))
  if (is.null(rownames(ret))) rownames(ret) <- sprintf("row %.0f", 1:nrow(ret))
  diag(ret) <- NA
  df <- list(row=NULL, col=NULL, val=NULL)
  symmetrical <- isSymmetric(m, tol=tol, tol1=tol1, ...)
  while (any(!is.na(ret))) {
    maxval <- if (decreasing) max(ret, na.rm=TRUE) else min(ret, na.rm=TRUE)
    ind    <- which(ret==maxval, arr.ind = TRUE)
    indu   <- if (symmetrical) matrix(ind[ind[,1]<ind[,2],], ncol=2) else ind
    df$row <- c(df$row, rownames(ret)[indu[,1]])
    df$col <- c(df$col, colnames(ret)[indu[,2]])
    df$val <- c(df$val, ret[indu])
    ret[ind] <- NA
  }
  if (!is.null(attr(m, "coeff"))) names(df)[3] <- attr(m, "coeff")
  structure(as.data.frame(df), matrix=m, call=attr(m, "call"))
}
