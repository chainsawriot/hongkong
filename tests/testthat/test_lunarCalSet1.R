require(testthat)
context("Conversion from Solar Dates to Lunar Dates")
test_that("correct conversion of Solar Dates to Lunar Dates", {
    expect_that(lunarCal(as.Date("1981-07-21")), equals(c(Year=1981, Month=6, Day=20, Leap=0)))
    expect_that(lunarCal(as.Date("1987-06-26")), equals(c(Year=1987, Month=6, Day=1, Leap=0)))
    expect_that(lunarCal(as.Date("1950-06-26")), equals(c(Year=1950, Month=5, Day=12, Leap=0)))
    expect_that(lunarCal(as.Date("2099-12-31")), equals(c(Year=2099, Month=11, Day=20, Leap=0)))
    expect_that(lunarCal(as.Date("2099-12-31")), equals(c(Year=2099, Month=11, Day=20, Leap=0)))
}
)
test_that("correct conversion of Leap month", {
    expect_that(lunarCal(as.Date("2012-06-04")), equals(c(Year=2012, Month=4, Day=15, Leap=1)))
    expect_that(lunarCal(as.Date("1903-07-01")), equals(c(Year=1903, Month=5, Day=7, Leap=1)))
    expect_that(lunarCal(as.Date("1922-06-30")), equals(c(Year=1922, Month=5, Day=6, Leap=1)))
    expect_that(lunarCal(as.Date("1995-09-25")), equals(c(Year=1995, Month=8, Day=1, Leap=1))) #一九九五閏八月
}
)
test_that("correct conversion of Year with Leap Month", {
    expect_that(lunarCal(as.Date("2012-08-20")), equals(c(Year=2012, Month=7, Day=4, Leap=0)))
    expect_that(lunarCal(as.Date("2011-01-05")), equals(c(Year=2010, Month=12, Day=2, Leap=0)))
}
)
test_that("Throw error when Solar Date is not in the supported range, or solarDate is not a POSIX", {
    expect_that(lunarCal(as.Date("1892-01-05")), throws_error())
    expect_that(lunarCal(as.Date("2200-01-05")), throws_error())
    expect_that(lunarCal(x="2000-01-05"), throws_error()) #not a date, but a string!
    expect_that(lunarCal(x=123), throws_error())
    expect_that(lunarCal(123), throws_error())
}
)
test_that("Formatting of lunar date", {
    expect_that(lunarCal(x=as.Date("1981-07-21"), toString=TRUE), matches("辛酉年六月廿日"))
    expect_that(lunarCal(x=as.Date("1981-07-21"), toString=TRUE, withZodiac=TRUE), matches("辛酉年六月廿日肖鷄"))
    expect_that(lunarCal(x=as.Date("1995-09-25"), toString=TRUE), matches("乙亥年閏八月初一日"))
    expect_that(lunarCal(x=as.Date("2011-01-05"), toString=TRUE), matches("庚寅年十二月初二日"))
    expect_that(lunarCal(x=as.Date("1950-06-26"), toString=TRUE), matches("庚寅年五月十二日"))
    expect_that(lunarCal(x=as.Date("2099-12-31"), toString=TRUE, withZodiac=TRUE), matches("己未年十一月廿日肖羊"))
    }
)

