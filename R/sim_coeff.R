#' sim_coeff
#'
#' Internal function for faster computation. No checks on input will be performed.
#'
#' @param set1 character: unique vector of words
#' @param set2 character: unique vector of words
#' @param setfull character: unique vector of texts to compare
#' @param coeff character: name of similarity coefficient to use
#'
#' @return value of similarity coefficient
sim_coeff <- function(set1, set2, setfull, coeff) {
  inset1 <- setfull %in% set1
  inset2 <- setfull %in% set2
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
