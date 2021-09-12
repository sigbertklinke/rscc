#' @rdname similarities
#' @export
sims <- function(...) { similarities(...) }

#' @rdname similarities
#' @aliases sims
#' @title similarities
#' @description \code{sims} and \code{similarities} both calculate for each pair of source code objects
#' the similarity coefficients and return a data frame with the coefficients in descending order.
#' A larger coefficient means a greater similarity.
#'
#' @param docs document object
#' @param all logical: should the similarity coefficients computed based on all sourcecode objects or just the two considered (default: \code{FALSE})
#' @param coeff character: coefficient to compute (default: \code{"jaccard"}), abbreviations can be used
#' @param ... all parameters in \code{sims} are given to \code{similarities}
#'
#' @return a data frame with the results
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files, basename=TRUE)
#' docs  <- documents(prgs)
#' similarities(docs)
similarities <- function (docs, all=FALSE,
                          coeff=c("jaccard", "braun", "dice", "hamann", "kappa", "kulczynski", "ochiai",
                                  "phi", "russelrao", "matching", "simpson", "sneath", "tanimoto", "yule")) {
  stopifnot ("documents" %in% class(docs))
  coeff   <- match.arg(coeff)
  allwords <- NULL
  if (all) {
    for (i in seq(docs)) allwords <- c(allwords, docs[[i]])
  }
  nvars <- lengths(docs)
  if (any(nvars==0)) warning("no names found in ", paste0(names(docs)[nvars==0], collapse=", "))
  n   <- length(docs)
  sim <- matrix(0, ncol=n, nrow=n)
  for (i in 1:n) {
    for (j in 1:n) {
      sim[i, j] <- similarity_coeff(docs[[i]], docs[[j]], allwords, coeff=coeff)
    }
  }
  colnames(sim) <- rownames(sim) <- names(docs)
  structure(sim, class=c("matrix", class(sim)), similarity=sim, coeff=coeff)
}
