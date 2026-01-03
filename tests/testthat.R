# This file is part of the standard testthat setup for R packages.
# It loads the package and testthat, then runs all tests.

library(testthat)
library(nmschooldata)

test_check("nmschooldata")
