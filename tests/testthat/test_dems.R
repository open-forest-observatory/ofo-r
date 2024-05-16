library(testthat)
library(ofo) # Replace with the name of your package


test_that("generating CHM", {
  #function call to execute CHM generation

  expect_equal(chm_from_coregistered_dsm_dtm(dsm, dtm), chm_tru)
})


