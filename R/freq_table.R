#' freq_table
#'
#' Computes a frequency table of words and documents.
#'
#' @param docs documents object
#' @param ... unused
#'
#' @return a matrix with similarities
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files, basename=TRUE)
#' docs  <- documents(prgs)
#' freq_table (docs)
freq_table <- function(docs, ...) {
   stopifnot ("documents" %in% class(docs))
   vdoc <- vword <- NULL
   for (doc in names(docs)) {
     vdoc  <- c(vdoc, rep(doc, length(docs[[doc]])))
     vword <- c(vword, docs[[doc]])
   }
   table(vdoc, vword)
}
