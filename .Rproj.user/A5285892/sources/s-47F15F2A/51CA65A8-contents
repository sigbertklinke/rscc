#' @rdname similarities
#' @export
sims <- function(...) { similarities(...) }

#' @rdname similarities
#' @aliases sims
#' @title similarities
#' @description \code{sims} and \code{similarities} compute both for each pair of sourcecode objects
#' the similarity coefficients and returns a data frame with coefficients in decreasing order.
#' A larger coefficient means a greater similarity. If
#' \itemize{
#' \item \code{type=="vars"} then the results of \code{all.vars(.)},
#' \item \code{type=="funs"} then the results of \code{setdiff(all.names(.), all.vars(.)}, and
#' \item \code{type=="names"} then the results of \code{all.names(.)}
#' }
#' are used.
#'
#' @param prgs sourcecode object
#' @param type character: either \code{"vars"}, \code{"funs"}, \code{"names"} (default: \code{"vars"})
#' @param all logical: should the similarity coefficients computed based on all sourcecode objects or just the two considered (default: \code{FALSE})
#' @param ignore.case logical: If TRUE, case is ignored for computing (default: \code{TRUE})
#' @param minlen integer: minimal name length to be considered (default: \code{2})
#' @param decreasing logical: should the sort order be increasing or decreasing (default: \code{TRUE})
#' @param tol numeric: tolerance used to detect if matrix is symmetrical (default: \code{1e-9})
#' @param coeff character: coefficient to compute (default: \code{"jaccard"}), abbreviations can be used
#' @param same.file logical: should be similarities kept if in same file or set to zero (default: \code{TRUE})
#' @param ... all parameters in \code{sims} are given to \code{similarities}
#'
#' @return a data frame with the results
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files)
#' names(prgs) <- basename(names(prgs))
#' similarities(prgs)            # variables only
#' similarities(prgs, type="f")  # functions only
#' similarities(prgs, type="n")  # all names
similarities <- function (prgs, type=c("vars", "funs", "names"),
                          all=FALSE, ignore.case=TRUE, minlen=2,
                          decreasing=TRUE, tol=1e-9, same.file=TRUE,
                          coeff=c("jaccard", "braun", "dice", "hamann", "kappa", "kulczynski", "ochiai",
                                  "phi", "russelrao", "matching", "simpson", "sneath", "tanimoto", "yule")) {
  stopifnot ("sourcecode" %in% class(prgs))
  type    <- match.arg(type)
  coeff   <- match.arg(coeff)
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
  nvars <- lengths(vars)
  if (any(nvars==0)) warning("no names found in ", paste0(names(vars)[nvars==0], collapse=", "))
  n   <- length(prgs)
  sim <- matrix(0, ncol=n, nrow=n)
  for (i in 1:n) {
    for (j in 1:n) {
      sim[i, j] <- similarity_coeff(vars[[i]], vars[[j]], if(all) allvars else NULL, coeff=coeff)
    }
  }
  colnames(sim) <- rownames(sim) <- names(prgs)
  if(!same.file) {
    names(prgs)
      files <- sapply(strsplit(names(prgs), "[", fixed=TRUE), '[', 1)
      sim[outer(files, files, "==")] <- 0
  }
  ret <- matrix2dataframe(sim, decreasing=decreasing, tol=tol)
  names(ret)[3] <- coeff
  structure(ret, class=c("similarity", class(ret)), similarity=sim)
}
