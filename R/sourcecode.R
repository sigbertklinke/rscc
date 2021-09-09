#' @rdname sourcecode
#' @export
sourcecode <- function(x, ...) UseMethod("sourcecode")

#' @rdname sourcecode
#' @title sourcecode
#' @description reads and parses files with R source code
#' @param x character: filenames
#' @param basename logical: should only the basename used for sourcecode objects (default: \code{FALSE})
#' @param silent logical: should the report of messages be suppressed (default: \code{FALSE})
#' @param minlines integer: only expressions with \code{minlines} lines are considered (default: \code{-1}), if \code{minlines<0} then whole files will be considered
#' @param ... unused
#'
#' @return a sourcecode object
#' @importFrom crayon red
#' @export
#'
#' @examples
#' # example files are taken from https://CRAN.R-project.org/package=SimilaR
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names=TRUE)
#' prgs  <- sourcecode(files)
sourcecode.default <- function(x, basename=FALSE, silent=FALSE, minlines=-1, ...) {
  parsed <- list()
  cat("\n")
  enclist <- iconvlist()
  for (file in x) {
    bfile <- if (basename) basename(file) else file
    parsedfile <- try(parse(file), silent=TRUE)
    if ("try-error" %in% class(parsed[[bfile]])) {
      if (!silent) cat(red(file, "\n", parsed[[bfile]]), "\n")
    } else {
      if (minlines<0) {
        parsed[[bfile]] <- parsedfile
      } else {
        lb    <- sapply(gregexpr("\n", as.character(parsedfile), fixed=TRUE),
                                 function(v) { if(v[1]<0) 0 else length(v)} )
        bfile <- sprintf("%s[%.0f]", bfile, seq(parsedfile))
        for (i in seq(parsedfile)) {
          if (lb[i]>=minlines) parsed[[bfile[i]]] <- parsedfile[i]
        }
      }
      if (!silent) cat(file, "\n")
    }
  }
  structure(parsed, class="sourcecode")
}
