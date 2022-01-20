#' similarity_coeff
#'
#' Computes a similarity coefficient based on the unique elements \code{set1} and \code{set2}
#' in relation to \code{setfull}. If \code{setfull} is \code{NULL} then \code{setfull} is set
#' to \code{unique(c(set1, set2))}. For more details, see the vignette \code{vignette("rscc")}.
#'
#' @param set1 vector: elements to compare
#' @param set2 vector: elements to compare
#' @param setfull vector: elements to compare (default: \code{NULL})
#' @param coeff character: coefficient to compute (default: \code{"jaccard"}), abbreviations can be used
#'
#' @return a numeric similarity coefficient
#' @export
#'
#' @examples
#' s1 <- 1:3
#' s2 <- 1:5
#' similarity_coeff(s1, s2)
#' s1 <- letters[1:3]
#' s2 <- LETTERS[1:5]
#' similarity_coeff(s1, s2)
similarity_coeff <- function(set1, set2, setfull=NULL,
                             coeff=c("jaccard", "braun", "dice", "hamann", "kappa", "kulczynski", "ochiai",
                                    "phi", "russelrao", "matching", "simpson", "sneath", "tanimoto", "yule")) {
  coeff <- match.arg(coeff)
  if (is.null(setfull)) setfull <- c(set1, set2)
  sim_coeff(unique(set1), unique(set2), unique(setfull), coeff)
}
