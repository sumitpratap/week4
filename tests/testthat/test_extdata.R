#' First test case

context('test_extdata')

test_that(".csv data files are available", {
  testthat::expect_equal(list.files(system.file("extdata", package = "week4")),
               c("accident_2013.csv.bz2",
                 "accident_2014.csv.bz2",
                 "accident_2015.csv.bz2"))
})