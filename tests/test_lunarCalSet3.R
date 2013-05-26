require(testthat)
context("Conversion from Lunar Dates to Solar Date")
test_that("correct conversion of Lunar Dates to Solar Dates", {
    expect_that(lunarCal(c(Year=1981, Month=6, Day=20, Leap=0)), equals(as.Date("1981-07-21")))
    expect_that(lunarCal(c(Year=1987, Month=6, Day=1, Leap=0)), equals(as.Date("1987-06-26")))
    expect_that(lunarCal(c(Year=1950, Month=5, Day=12, Leap=0)), equals(as.Date("1950-06-26")))
    expect_that(lunarCal(c(Year=2099, Month=11, Day=20, Leap=0)), equals(as.Date("2099-12-31")))
    expect_that(lunarCal(c(Year=2099, Month=11, Day=20, Leap=0)), equals(as.Date("2099-12-31")))
}
)
test_that("correct conversion of Leap month", {
    expect_that(lunarCal(c(Year=2012, Month=4, Day=15, Leap=1)), equals(as.Date("2012-06-04")))
    expect_that(lunarCal(c(Year=1903, Month=5, Day=7, Leap=1)), equals(as.Date("1903-07-01")))
    expect_that(lunarCal(c(Year=1922, Month=5, Day=6, Leap=1)), equals(as.Date("1922-06-30")))
    expect_that(lunarCal(c(Year=1995, Month=8, Day=1, Leap=1)), equals(as.Date("1995-09-25")))
}
)
test_that("correct conversion of Year with Leap Month", {
    expect_that(lunarCal(c(Year=2012, Month=7, Day=4, Leap=0)), equals(as.Date("2012-08-20")))
    expect_that(lunarCal(c(Year=2010, Month=12, Day=2, Leap=0)), equals(as.Date("2011-01-05")))
}
)
test_that("correct conversion when ignore Leap is FALSE", {
    expect_that(lunarCal(c(Year=1996, Month=8, Day=11), ignoreLeap=FALSE), equals(c(as.Date("1995-09-25")))
    expect_that(lunarCal(c(Year=2010, Month=12, Day=2, Leap=0)), equals(as.Date("2011-01-05")))
}
)
