#' documents
#'
#' Creates word vectors from parsed sourec code objects. If
#' \itemize{
#' \item \code{type=="vars"} then the names of \code{all.vars(.)},
#' \item \code{type=="funs"} then the namas of \code{setdiff(all.names(.), all.vars(.)}, and
#' \item \code{type=="names"} then the names of \code{all.names(.)}
#' }
#' are used.
#'
#' @param prgs prgs sourcecode object
#' @param type character: either \code{"vars"}, \code{"funs"}, \code{"names"} (default: \code{"vars"})
#' @param ignore.case logical: If TRUE, case is ignored for computing (default: \code{TRUE})
#' @param minlen integer: minimal name length to be considered (default: \code{2})
#' @param ... unused
#'
#' @return a
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files, basename=TRUE)
#' docs  <- documents(prgs)
#' docs
documents <- function(prgs,  type=c("vars", "funs", "names"), ignore.case=TRUE, minlen=2, ...) {
  stopifnot ("sourcecode" %in% class(prgs))
  type <- match.arg(type)
  docs <- list()
  for (file in names(prgs)) {
    if (type=="vars") docs[[file]]  <- all.vars(prgs[[file]])
    if (type=="names") docs[[file]] <- all.names(prgs[[file]])
    if (type=="funs") docs[[file]]  <- setdiff(all.names(prgs[[file]]), all.vars(prgs[[file]]))
    if (ignore.case) docs[[file]]   <- tolower(docs[[file]])
    if (minlen>1) docs[[file]]      <- docs[[file]][nchar(docs[[file]])>=minlen]
  }
  structure(docs, class=c("documents", class(docs)))
}
