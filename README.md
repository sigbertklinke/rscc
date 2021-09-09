# rscc 
Visualizes a matrix object plainly as heatmap. It provides a 

* single S3 function `plot` for matrices,
* function `assignColors` which assigns a specific color to each value of a vector, and
* specific functions for loadings, correlation and p-values matrices.

View the vignette on [GitHub](https://htmlpreview.github.io/?https://github.com/sigbertklinke/plot.matrix/blob/master/vignettes/plot.matrix.html) or after installing with

```R
library("rscc")
vignette("rscc") 
```

# Installation  

## From CRAN

```R
install.packages("rscc")
```

## From github

Note that from github you install the current development version.

```R
library("devtools")
install_github("sigbertklinke/rscc")
```

# History
  * 2021-09-09 version 0.1.0
