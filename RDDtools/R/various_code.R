### MISC
is.even <- function (a) {
  a%%2 == 0
}


Kernel_tri <- function(X, center, bw) {
  ifelse(abs(X - center) > bw, 0, 1 - (abs(X - center) / bw))
}

.onLoad <- function(libname, pkgname)
  packageStartupMessage("\nRDDtools ", utils::packageVersion("RDDtools"), 
                        "\nPLEASE NOTE THIS is currently only a development version. \nRun vignette('RDDtools') for the documentation")
