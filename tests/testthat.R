## inhibit overloading of ephemeris provided by
## swephR and swephRdata
Sys.setenv(SE_EPHE_PATH = "")

library(testthat)
library(swephR)
library(ARCHAEOCOSMO)

test_check("ARCHAEOCOSMO")
