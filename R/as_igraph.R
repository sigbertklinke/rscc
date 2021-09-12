#' as.igraph
#'
#' Converts a data frame of similarity coefficients into a graph.
#'
#' @param x a similarity object
#' @inheritParams base::isSymmetric
#' @param ... further parameters used by [igraph::graph_from_adjacency_matrix]
#'
#' @return an igraph object
#' @md
#' @importFrom igraph graph_from_adjacency_matrix
#' @export
#'
#' @examples
#' files <- list.files(path=system.file("examples", package="rscc"), pattern="*.R$", full.names = TRUE)
#' prgs  <- sourcecode(files, basename=TRUE)
#' docs  <- documents(prgs)
#' simm  <- similarities(docs)
#' # a similarity coefficients equal to zero does not create an edge!
#' g <- as_igraph(simm, diag=FALSE)
#' # thicker edges have higher similarity coefficients
#' plot(g, edge.width=1+3*igraph::E(g)$weight)
as_igraph <- function (x, tol=100*.Machine$double.eps, tol1=8*tol, ...) {
  stopifnot(is.matrix(x))
  args <- list(...)
  args$adjmatrix <- x
  if (is.null(args$mode))     args$mode <- if(isSymmetric(x, tol=tol, tol1=tol1)) "undirected" else "directed"
  if (is.null(args$weighted)) args$weighted <- TRUE
  do.call(graph_from_adjacency_matrix, args)
}
