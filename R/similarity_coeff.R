#' similarity_coeff
#'
#' Computes a similarity coefficient based on the unique elements \code{set1} and \code{set2} in relation to \code{setfull}.
#' If \code{setfull} is \code{NULL} then \code{setfull} is set to \code{unique(c(set1, set2))}.
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
  coeff   <- match.arg(coeff)
  if (is.null(setfull)) setfull <- c(set1, set2)
  setfull <- unique(setfull)
  inset1 <- setfull %in% unique(set1)
  inset2 <- setfull %in% unique(set2)
  p   <- length(setfull)
  n11 <- sum(inset1 & inset2)
  n10 <- sum(inset1 & !inset2)
  n01 <- sum(!inset1 & inset2)
  n00 <- sum(!inset1 & !inset2)
  ret <- switch(coeff,
                jaccard    = n11/(n01+n10+n11),
                braun      = n11/max(n01+n11, n10+n11),
                dice       = 2*n11/(n01+n10+2*n11),
                kappa      = 1/(1+p/2*(n01+n10)/(n00*n11-n01*n10)),
                kulczynski = n11/(n01+n10),
                ochiai     = n11/sqrt((n11+n10)*(n11+n10)),
                phi        = (n11*n00-n10*n01)/sqrt((n11+n10)*(n11+n10)*(n00+n10)*(n00+n10)),
                russelrao  = n11/p,
                matching   = (n00+n11)/p,
                simpson    = n11/min(n01+n11, n10+n11),
                sneath     = n11/(n11+2*n01+2*n10),
                tanimoto   = (n11+n00)/(n11+2*n01+2*n10+n00),
                yule       = (n11*n00-n01*n10)/(n11*n00-n01*n10),
                NA)
  if (is.na(ret)) return(0)
  ret
}
