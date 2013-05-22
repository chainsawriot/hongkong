require(testthat)
context("Conversion from  Lunar Dates to Solar Dates")
test_that("correct conversion of Lunar Dates", {
  expect_that(lunarCal(as.Date("1981-07-21")), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
}
          )

