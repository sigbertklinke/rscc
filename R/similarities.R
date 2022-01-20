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
#' # further steps
#' # m  <- similarities(docs)
#' # df <- matrix2dataframe(m)
#' # head(df, n=20)
#' # browse(prgs, df, n=5)
similarities <- function (docs, all=FALSE,
                          coeff=c("jaccard", "braun", "dice", "hamann", "kappa", "kulczynski", "ochiai",
                                  "phi", "russelrao", "matching", "simpson", "sneath", "tanimoto", "yule")) {
  stopifnot ("documents" %in% class(docs))
  coeff <- match.arg(coeff)
  udocs <- lapply(docs, unique)
  nvars <- lengths(udocs)
  if (any(nvars==0)) warning("no names found in ", paste0(names(docs)[nvars==0], collapse=", "))
  if (all) allwords <- unique(unlist(udocs))
  n     <- length(udocs)
  sim   <- matrix(0, ncol=n, nrow=n)
  for (i in 1:n) {
    for (j in i:n) { # assume symmetry of coefficients
      if (!all) allwords <- unique(c(udocs[[i]], udocs[[j]]))
      sim[i, j] <- sim[j, i] <- sim_coeff(udocs[[i]], udocs[[j]], allwords, coeff)
    }
  }
  colnames(sim) <- rownames(sim) <- names(udocs)
  structure(sim, class=c("matrix", class(sim)), similarity=sim, coeff=coeff, call=c(attr(docs, "call"), deparse(match.call())))
}
