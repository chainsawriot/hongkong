require(testthat)
context("as.lunar and is.lunar works as expected")
test_that("correct conversion of Lunar Dates without check", {
  expect_that(as.lunar("1981-06-20", FALSE), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
  expect_that(as.lunar("1995-08-10", FALSE), equals(c(Year=1995, Month=8, Day=10, Leap=0)))
  expect_that(as.lunar("1995-08L-10", FALSE), equals(c(Year=1995, Month=8, Day=10, Leap=1)))
  expect_that(as.lunar("1981/06/20", FALSE), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
  expect_that(as.lunar("1981 06 20", FALSE), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
}
          )
test_that("as.lunar correctly throws error due to formatting error or invalid input", {
  expect_that(as.lunar("19810620", FALSE), throws_error())
  expect_that(as.lunar(123, FALSE), throws_error())
}
          )
test_that("as.lunar correct conversion of Lunar Dates with check", {
  expect_that(as.lunar("1981-06-20"), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
  expect_that(as.lunar("1995-08-10"), equals(c(Year=1995, Month=8, Day=10, Leap=0)))
  expect_that(as.lunar("1995-08L-10"), equals(c(Year=1995, Month=8, Day=10, Leap=1)))
  expect_that(as.lunar("1981/06/20"), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
  expect_that(as.lunar("1981 06 20"), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
}
          )
test_that("as.lunar correctly throws error due to failed is.lunar", {
  expect_that(as.lunar("1981-06-31"), throws_error())
  expect_that(as.lunar("2010-11-30"), throws_error())
  expect_that(as.lunar("1995-09L-10"), throws_error())
  expect_that(as.lunar("1995-07L-20"), throws_error())
  expect_that(as.lunar("2999-07L-20"), throws_error())
  expect_that(as.lunar("1845-01-20"), throws_error())
}
          )
