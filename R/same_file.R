#' same_file
#'
#' @param m matrix object with row- and columnnames
#' @param replacement value for replacement (default: \code{0})
#'
#' @return matrix
#' @export
#'
#' @examples
#' m <- matrix(runif(25), ncol=5)
#' colnames(m) <- rownames(m) <- c(sprintf("m[%.f]", 1:3), sprintf("m2[%.f]", 1:2))
#' m
#' same_file(m)
same_file <- function(m, replacement=0) {
  stopifnot ("matrix" %in% class(m))
  coln <- sapply(strsplit(colnames(m), '[', fixed=TRUE), '[', 1)
  rown <- sapply(strsplit(rownames(m), '[', fixed=TRUE), '[', 1)
  m[outer(rown, coln, "==")] <- replacement
  m
}
