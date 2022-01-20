#' tfidf
#'
#' Computes the term frequency–inverse document frequency uses tha cosine of the angles between the documents
#' as similarity measure. Since R source code is provided no stemming or stop words are applied.
#'
#' @param docs document object
#'
#' @return similarity matrix
#' @importFrom tm Corpus TermDocumentMatrix VectorSource
#' @export
#'
#' @examples
#' files <- list.files(system.file("examples", package="rscc"), "*.R$", full.names = TRUE)
#' prgs  <- sourcecode(files, basename=TRUE, silent=TRUE)
#' docs  <- documents(prgs)
#' tfidf(docs)
#' # further steps
#' # m  <- tfidf(docs)
#' # df <- matrix2dataframe(m)
#' # head(df, n=20)
#' # browse(prgs, df, n=5)
tfidf <- function(docs) {
  # from https://www.r-bloggers.com/2013/03/build-a-search-engine-in-20-minutes-or-less/
  get.tf.idf.weights <- function(tf.vec, df, N.docs) {
    # Computes tfidf weights from a term frequency vector and a document
    # frequency scalar
    weight = rep(0, length(tf.vec))
    weight[tf.vec > 0] = (1 + log2(tf.vec[tf.vec > 0])) * log2(N.docs/df)
    weight
  }
  #
  get.weights.per.term.vec <- function(tfidf.row, N.docs) {
    term.df <- sum(tfidf.row[1:N.docs] > 0)
    tf.idf.vec <- get.tf.idf.weights(tfidf.row, term.df, N.docs)
    return(tf.idf.vec)
  }
  dq              <- sapply(docs, function(e, sw) { paste0(e, collapse = " ") })
  my.corpus       <- Corpus(VectorSource(dq))
  term.doc.matrix <- TermDocumentMatrix(my.corpus)
  tfidf.matrix <- t(apply(term.doc.matrix, c(1), FUN = get.weights.per.term.vec, N.docs=length(docs)))
  colnames(tfidf.matrix) <- colnames(term.doc.matrix)
  tfidf        <- scale(tfidf.matrix, center = FALSE, scale = sqrt(colSums(tfidf.matrix^2)))
  structure(crossprod(tfidf), tfidf=tfidf.matrix, coeff='tfidf', call=c(attr(docs, "call"), deparse(match.call())))
}
