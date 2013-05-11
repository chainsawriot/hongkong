hkHoliday <- function(yearInt) {
  # return all the holidays in a given year
  # According to the General Holidays Ordinance
  holidays <- c()
  ## 1st day of Jan
  firstDayJan <- as.Date(paste0(yearInt, "-1-1"))
  if (wday(firstDayJan) == 1) {
    firstDayJan <- firstDayJan + days(1)
  }
  ## lunar New Year
  lnyDays <- c()
  for (i in 1:3) {
    lnyDays[i] <- lunarCal(lunarDate=c(Year=YearInt, Month=1, Day=i))
    if (wdays(lnyDays[i]) == 1) {
      if (yearInt >= 2011) {
        lnyDays[i] <- lunarCal(lunarDate=c(Year=YearInt, Month=1, Day=4))
      } else {
        ### last day of lny, previous yr
      }
    }
  }
  return(firstDayJan)
}
