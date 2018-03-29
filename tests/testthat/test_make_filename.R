#' Second test case

context('test_make_filename')

test_that("Check number of rows for 2013 accidents file", {

expect_match(make_filename(2013),'accident_2013.csv.bz2')

})