#' matrix2dataframe
#'
#' Converts a numeric matrix to a data frame with decreasing or increasing values:
#' first column row index, second column col index and third column the value.
#' If the matrix is symmetrical then just the upper triangle will be considered.
#'
#' @param m numeric: a matrix of values
#' @param decreasing logical: should the sort order be increasing or decreasing (default: \code{TRUE})
#' @param tol numeric: tolerance used to detect if matrix is symmetrical (default: \code{1e-9})
#'
#' @return a data frame with an attribute \code{matrix} with \code{m}
#' @export
#'
#' @examples
#' # non-symmetric
#' x <- matrix(runif(9), ncol=3)
#' matrix2dataframe(x)
#' # symmetric
#' x <- x+t(x)
#' matrix2dataframe(x)
matrix2dataframe <- function(m, decreasing=TRUE, tol=1e-9) {
  stopifnot(is.matrix(m))
  ret       <- m
  if (is.null(colnames(ret))) colnames(ret) <- sprintf("col %.0f", 1:ncol(ret))
  if (is.null(rownames(ret))) rownames(ret) <- sprintf("row %.0f", 1:nrow(ret))
  diag(ret) <- NA
  df <- list(row=NULL, col=NULL, val=NULL)
  symmetrical <- (ncol(m)==nrow(m)) && all(abs(m-t(m))<=tol, na.rm=TRUE)
  while (any(!is.na(ret))) {
    maxval <- if (decreasing) max(ret, na.rm=TRUE) else min(ret, na.rm=TRUE)
    ind    <- which(ret==maxval, arr.ind = TRUE)
    indu   <- if (symmetrical) matrix(ind[ind[,1]<ind[,2],], ncol=2) else ind
    df$row <- c(df$row, rownames(ret)[indu[,1]])
    df$col <- c(df$col, colnames(ret)[indu[,2]])
    df$val <- c(df$val, ret[indu])
    ret[ind] <- NA
  }
  structure(as.data.frame(df), matrix=ret, symmetrical=symmetrical)
}
