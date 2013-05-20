hkHoliday <- function(yearInt, push=TRUE) {
  # return all the holidays in a given year
  # According to the General Holidays Ordinance
  composeHoliday <- function(day, mon, year, push=TRUE, lunar=FALSE, minus=FALSE) {
    if (lunar) {
      holidayDate <- lunarCal(lunarDate=c(Year=year, Month=mon, Day=day))
    } else {
      holidayDate <- as.Date(paste0(year, "-", mon, "-", day))
    }
    if (push) {
      holidayDate <- pushToWorkday(holidayDate, minus=minus)
    }
    return(holidayDate)
  }
  pushToWorkday <- function(holidayDate, minus=FALSE) {
    if (wday(holidayDate) == 1 & !minus) {
      holidayDate <- holidayDate + days(1)
    } else if (wday(holidayDate) == 1 & minus) {
      holidayDate <- holidayDate - days(1)
    }
    return(holidayDate)
  }
  Easter <- function(year) {
    ### calculate the Easter Sunday, taken from timeDate package
    ### Diethelm Wuertz, Yohan Chalabi and Martin Maechler with contributions from Joe W. Byers, and others
    ### GPL-2
    a <- year%%19
    b <- year%/%100
    c <- year%%100
    d <- b%/%4
    e <- b%%4
    f <- (b+8)%/%25
    g <- (b-f+1)%/%3
    h <- (19*a+b-d-g+15)%%30
    i <- c%/%4
    k <- c%%4
    l <- (32+2*e+2*i-h-k)%%7
    m <- (a+11*h+22*l)%/%451
    easter.month = (h+l-7*m+114)%/%31
    p <- (h+l-7*m+114)%%31
    easter.day = p+1
    return(as.Date(paste0(year, "-", easter.month, "-", easter.day)))

  }
  holidays <- c()
  ## 1st day of Jan
  holidays['firstDayJan'] <- composeHoliday(1, 1, yearInt, push, FALSE)
  class(holidays) <- "Date"
  ## lunar New Year
  lnyDays <- c()
  for (i in 1:3) {
    toAdd <- lunarCal(lunarDate=c(Year=yearInt, Month=1, Day=i))
    if (wday(toAdd) == 1 & push) {
      if (yearInt >= 2011) { ### rule changed after 2011
        toAdd <- lunarCal(lunarDate=c(Year=yearInt, Month=1, Day=4))
      } else if (i == 1 & push) {
        toAdd <- toAdd - days(1)
      } else if (push) {
        toAdd <- lnyDays[1] - days(1)
      }
    }
    lnyDays[i] <- toAdd
    class(lnyDays) <- "Date"
  }
  holidays['lnyDay1'] <- lnyDays[1]
  holidays['lnyDay2'] <- lnyDays[2]
  holidays['lnyDay3'] <- lnyDays[3]
  ## Ching Ming
  cmd <- c(5L, 6L, 6L, 5L, 5L, 6L, 6L, 5L, 5L, 6L, 6L, 5L, 5L, 5L, 6L,
           5L, 5L, 5L, 6L, 5L, 5L, 5L, 6L, 5L, 5L, 5L, 6L, 5L, 5L, 5L, 6L,
           5L, 5L, 5L, 6L, 5L, 5L, 5L, 6L, 5L, 5L, 5L, 6L, 5L, 5L, 5L, 5L,
           5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L,
           5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 4L, 5L, 5L, 5L,
           4L, 5L, 5L, 5L, 4L, 5L, 5L, 5L, 4L, 5L, 5L, 5L, 4L, 5L, 5L, 5L,
           4L, 5L, 5L, 5L, 4L, 5L, 5L, 5L, 4L, 5L, 5L, 5L, 4L, 4L, 5L, 5L,
           4L, 4L, 5L, 5L, 4L, 4L, 5L, 5L, 4L, 4L, 5L, 5L, 4L, 4L, 5L, 5L,
           4L, 4L, 5L, 5L, 4L, 4L, 5L, 5L, 4L, 4L, 5L, 5L, 4L, 4L, 4L, 5L,
           4L, 4L, 4L, 5L, 4L, 5L, 4L, 5L, 4L, 4L, 4L, 5L, 4L, 4L, 4L, 5L,
           4L, 4L, 4L, 5L, 4L, 4L, 4L, 5L, 4L, 4L, 4L, 5L, 4L, 4L, 4L, 4L,
           4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L,
           4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L)
  
  holidays['chingMing'] <- composeHoliday(cmd[yearInt - 1900], 4, yearInt, push, FALSE)
  ## Good Friday
  easterDate <- Easter(yearInt)
  holidays['goodFriday'] <- easterDate - days(2)
  
  ## Day after Good Friday
  holidays['dayAfterGoodFriday'] <- easterDate - days(1)
  ## Easter Monday
  holidays['easterMonday'] <- easterDate + days(1)
  ## Labour Day
  holidays['labourDay'] <- composeHoliday(1, 5, yearInt, push, FALSE)
  ## Buddha birthday
  holidays['buddhaBirthday'] <- composeHoliday(8, 4, yearInt, push, TRUE)
  ## Tuen Ng
  holidays['tuenNg'] <- composeHoliday(5, 5, yearInt, push, TRUE)
  ## HKSAR establishment day
  holidays['firstJuly'] <- composeHoliday(1, 7, yearInt, push, FALSE)
  ## National Day
  holidays['natlDay'] <- composeHoliday(1, 10, yearInt, push, FALSE)
  ## day after mid autumn
  holidays['midAutumn'] <- composeHoliday(16, 8, yearInt, push, TRUE, TRUE)
  ## chung Yeung
  holidays['chungYeung'] <- composeHoliday(9, 9, yearInt, push, TRUE)
  ## X'mas
  ## first weekday after X'mas
  xmasDay <- as.Date(paste0(yearInt, "-12-25"))
  if (wday(xmasDay) == 1 & push) {
    xmasDay <- xmasDay + days(2)
  }
  holidays['xmasDay'] <- xmasDay
  holidays['xmasDayAfter'] <- composeHoliday(26, 12, yearInt, push, FALSE)
  ### detect collisions : very messy, need a better logic
  z <- as.data.frame(table(holidays), stringsAsFactors=FALSE)
  ##print(z)
  if (sum(z$Freq != 1) != 0 & push) { ## Collision detected
    colliedDates <- z$holidays[z$Freq > 1]
    ##print(colliedDates)
    for (i in 1:length(colliedDates)) {
      whichCollied <- names(holidays)[holidays == colliedDates[i]]
      ##print(whichCollied)
      if (length(whichCollied) == 2) {
        toMove <- whichCollied[1]
        holidays[toMove] <- holidays[toMove] + days(1)
      }
    }
  }
  return(holidays)
}
