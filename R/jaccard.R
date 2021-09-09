#' jaccard
#'
#' Computes for each pair of sourcecode objects the Jaccard coefficient and returns
#' a data frame with coefficients in decreasing order. A larger coefficients means a
#' greater similarity. If
#' \itemize{
#' \item \code{type=="vars"} then the results of \code{all.vars(.)},
#' \item \code{type=="funs"} then the results of \code{setdiff(all.names(.), all.vars(.)}, and
#' \item \code{type=="names"} then the results of \code{all.names(.)}
#' }
#' are used.
#'
#' @param prgs sourcecode object
#' @param type character: either \code{"vars"}, \code{"funs"}, \code{"names"} (default: \code{"vars"})
#' @param all logical: should the Jaccard coefficient computed based on all sourcecode objects or just the two considered (default: \code{FALSE})
#' @param ignore.case logical: If TRUE, case is ignored for computing (default: \code{TRUE})
#' @param minlen integer: minimal name length to be considered (default: \code{2})
#' @param decreasing logical: should the sort order be increasing or decreasing (default: \code{TRUE})
#' @param tol numeric: tolerance used to detect if matrix is symmetrical (default: \code{1e-9})
#'
#' @return a data frame with the results
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files)
#' names(prgs) <- basename(names(prgs))
#' jaccard(prgs)            # variables only
#' jaccard(prgs, type="f")  # functions only
#' jaccard(prgs, type="n")  # all names
jaccard <- function (prgs, type=c("vars", "funs", "names"),
                     all=FALSE, ignore.case=TRUE, minlen=2,
                     decreasing=TRUE, tol=1e-9) {
  stopifnot ("sourcecode" %in% class(prgs))
  type    <- match.arg(type)
  vars    <- list()
  allvars <- NULL
  for (file in names(prgs)) {
    if (type=="vars") vars[[file]]  <- all.vars(prgs[[file]])
    if (type=="names") vars[[file]] <- all.names(prgs[[file]])
    if (type=="funs") vars[[file]]  <- setdiff(all.names(prgs[[file]]), all.vars(prgs[[file]]))
    if (ignore.case) vars[[file]]   <- tolower(vars[[file]])
    if (minlen>1) vars[[file]]      <- vars[[file]][nchar(vars[[file]])>=minlen]
    if (all) allvars <- c(allvars, vars[[file]])
  }
  n   <- length(prgs)
  ret <- matrix(0, ncol=n, nrow=n)
  for (i in 1:n) {
    for (j in i:n) {
      if (!all) allvars <- unique(c(vars[[i]], vars[[j]]))
      ret[j,i] <- ret[i, j] <- as.numeric(length(intersect(vars[[i]], vars[[j]])))/as.numeric(length(allvars))
    }
  }
  colnames(ret) <- rownames(ret) <- names(prgs)
  matrix2dataframe(ret, decreasing=decreasing, tol=tol)
}
