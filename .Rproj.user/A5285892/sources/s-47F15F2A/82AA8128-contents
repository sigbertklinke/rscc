#' as.igraph
#'
#' @param x a similarity object
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
#' coeff <- similarities(prgs)        # variables only
#' # a similarity coefficients equal to zero does not create an edge!
#' g <- as_igraph(coeff, diag=FALSE)
#' # thicker edges have higher similarity coefficients
#' plot(g, edge.width=1+3*igraph::E(g)$weight)
as_igraph <- function (x, ...) {
  args <- list(...)
  args$adjmatrix  <- attr(x, "similarity")
  if (is.null(args$mode))     args$mode <- if(attr(x, "symmetrical")) "undirected" else "directed"
  if (is.null(args$weighted)) args$weighted <- TRUE
  do.call(graph_from_adjacency_matrix, args)
}
