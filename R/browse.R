#' browse
#'
#' Creates a temporary HTML file with source codes and opens it into a browser using \code{browseURL}.
#' Note that the source code is reformatted.
#'
#' @param prgs sourcecode object
#' @param simdf similarity object
#' @param n integer: comparisons to show (default: \code{simf[,3]>0})
#' @param width.cutoff integer: an integer in [20, 500]: if a line's character length is at or over this number, the function will try to break it into a new line (default: \code{60})
#' @param css character: file name of CSS style for highlighting the R code
#'
#' @return invisibly the name of the temporary HTML file
#' @importFrom utils browseURL
#' @importFrom formatR tidy_source
#' @importFrom highlight highlight renderer_html
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files)
#' simm  <- similarities(documents(prgs))
#' simdf <- matrix2dataframe(simm)
#' if (interactive()) browse(prgs, simdf)
browse <- function(prgs, simdf, n=(simdf[,3]>0), width.cutoff=60, css=NULL) {
  stopifnot("sourcecode" %in% class(prgs))
  stopifnot("data.frame" %in% class(simdf))
  nsimdf <- nrow(simdf)
  show <- rep(FALSE, nsimdf)
  if (is.numeric(n)) {
    if (length(n)==1) show[1:min(nsimdf,n)] <- TRUE else show[n] <- TRUE
  }
  if (is.logical(n)) {
    show <- rep(n, length.out=nsimdf)
  }
  stopifnot(any(show))
  if (is.null(css)) css <- system.file("stylesheets", "default.css", package="rscc")
  style <- paste0(readLines(css), collapse="\n")
  html  <- sprintf("<!DOCTYPE html>\n<html>\n<head><title>%s</title><style>%s</style></head><body>",
                   as.character(Sys.time()), style)
  sind <- which(show)
  html <- c(html, "<p>", paste0("[<a href=\"#", sind, "\">", sprintf("%.2f", simdf[sind,3]), "</a>]", collapse="&nbsp; "), "</p>")
  tmpfile <- tempfile(fileext=".R")
  for (i in 1:nsimdf) {
    if (show[i]) {
      html <- c(html, sprintf("<h2 id=\"%.0f\">%s = %f</h2>", i, names(simdf)[3], simdf[i,3]),
                "<table width=\"100%\">",
                sprintf("<tr bgcolor=\"darkgrey\"><th>%s</th><th>%s</th></tr>", simdf[i,1], simdf[i,2]))
      src1 <- tidy_source(text=as.character(prgs[[simdf[i,1]]]), output=FALSE,  width.cutoff=width.cutoff)
      writeLines(src1$text.tidy, tmpfile)
      src1 <- highlight(tmpfile, output=NULL, renderer = renderer_html(document=FALSE))
      src2 <- tidy_source(text=as.character(prgs[[simdf[i,2]]]), output=FALSE,  width.cutoff=width.cutoff)
      writeLines(src2$text.tidy, tmpfile)
      src2 <- highlight(tmpfile, output=NULL, renderer = renderer_html(document=FALSE))
      html <- c(html, sprintf("<tr><td valign=\"top\">%s</td><td valign=\"top\">%s</td></tr></table>",
                              paste0(src1, collapse=""), paste0(src2, collapse="")))
    }
  }
  html <- c(html, "</body></html>")
  tmp  <- tempfile(fileext=".html")
  writeLines(html, tmp)
  browseURL(tmp)
  invisible(tmp)
}
