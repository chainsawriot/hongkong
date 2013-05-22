### Maintainer: Chung-hong Chan ("chainsawriot" or "Fuk Chan")
### Affliation: Journalism and Media Studies Centre, University of Hong Kong, Hong Kong
### Email: chainsawtiney@gmail.com
### for the R package "hongkong"

library(lubridate)

lunarCal <- function(solarDate = NULL, toString = FALSE, withZodiac = FALSE, lunarDate=NULL, ignoreLeap=TRUE) {
  ### PURPOSE: convert the solar date to lunar date, in the form of (year, mon, day, leap)

  ### Data extracted from liblunar
  lunarMonthData <- c(
    0xF0EA4, 0xF1D4A, 0x52C94, 0xF0C96, 0xF1536, 0x42AAC, 0xF0AD4, 0xF16B2, 0x22EA4, 0xF0EA4,  # 1901-1910
    0x6364A, 0xF164A, 0xF1496, 0x52956, 0xF055A, 0xF0AD6, 0x216D2, 0xF1B52, 0x73B24, 0xF1D24,  # 1911-1920
    0xF1A4A, 0x5349A, 0xF14AC, 0xF056C, 0x42B6A, 0xF0DA8, 0xF1D52, 0x23D24, 0xF1D24, 0x61A4C,  # 1921-1930
    0xF0A56, 0xF14AE, 0x5256C, 0xF16B4, 0xF0DA8, 0x31D92, 0xF0E92, 0x72D26, 0xF1526, 0xF0A56,  # 1931-1940
    0x614B6, 0xF155A, 0xF0AD4, 0x436AA, 0xF1748, 0xF1692, 0x23526, 0xF152A, 0x72A5A, 0xF0A6C,  # 1941-1950
    0xF155A, 0x52B54, 0xF0B64, 0xF1B4A, 0x33A94, 0xF1A94, 0x8152A, 0xF152E, 0xF0AAC, 0x6156A,  # 1951-1960
    0xF15AA, 0xF0DA4, 0x41D4A, 0xF1D4A, 0xF0C94, 0x3192E, 0xF1536, 0x72AB4, 0xF0AD4, 0xF16D2,  # 1961-1970
    0x52EA4, 0xF16A4, 0xF164A, 0x42C96, 0xF1496, 0x82956, 0xF055A, 0xF0ADA, 0x616D2, 0xF1B52,  # 1971-1980
    0xF1B24, 0x43A4A, 0xF1A4A, 0xA349A, 0xF14AC, 0xF056C, 0x60B6A, 0xF0DAA, 0xF1D92, 0x53D24,  # 1981-1990
    0xF1D24, 0xF1A4C, 0x314AC, 0xF14AE, 0x829AC, 0xF06B4, 0xF0DAA, 0x52D92, 0xF0E92, 0xF0D26,  # 1991-2000
    0x42A56, 0xF0A56, 0xF14B6, 0x22AB4, 0xF0AD4, 0x736AA, 0xF1748, 0xF1692, 0x53526, 0xF152A,  # 2001-2010
    0xF0A5A, 0x4155A, 0xF156A, 0x92B54, 0xF0BA4, 0xF1B4A, 0x63A94, 0xF1A94, 0xF192A, 0x42A5C,  # 2011-2020
    0xF0AAC, 0xF156A, 0x22B64, 0xF0DA4, 0x61D52, 0xF0E4A, 0xF0C96, 0x5192E, 0xF1956, 0xF0AB4,  # 2021-2030
    0x315AC, 0xF16D2, 0xB2EA4, 0xF16A4, 0xF164A, 0x63496, 0xF1496, 0xF0956, 0x50AB6, 0xF0B5A,  # 2031-2040
    0xF16D4, 0x236A4, 0xF1B24, 0x73A4A, 0xF1A4A, 0xF14AA, 0x5295A, 0xF096C, 0xF0B6A, 0x31B54,  # 2041-2050
    0xF1D92, 0x83D24, 0xF1D24, 0xF1A4C, 0x614AC, 0xF14AE, 0xF09AC, 0x40DAA, 0xF0EAA, 0xF0E92,  # 2051-2060
    0x31D26, 0xF0D26, 0x72A56, 0xF0A56, 0xF14B6, 0x52AB4, 0xF0AD4, 0xF16CA, 0x42E94, 0xF1694,  # 2061-2070
    0x8352A, 0xF152A, 0xF0A5A, 0x6155A, 0xF156A, 0xF0B54, 0x4174A, 0xF1B4A, 0xF1A94, 0x3392A,  # 2071-2080
    0xF192C, 0x7329C, 0xF0AAC, 0xF156A, 0x52B64, 0xF0DA4, 0xF1D4A, 0x41C94, 0xF0C96, 0x8192E,  # 2081-2090
    0xF0956, 0xF0AB6, 0x615AC, 0xF16D4, 0xF0EA4, 0x42E4A, 0xF164A, 0xF1516, 0x22936           # 2090-2099
  )
  ### magic numbers
  referenceDate <- as.Date("1901-2-19")
  maxDate <- as.Date("2100-2-18")
  lYear <- 1901 ## Initial L Year, Month and Day are 1901, 1, 1. The day one of the Lunar Calendar in this program
  lMonth <- 1
  lDay <- 1

  ### Helper functions
  getYearDayCount <- function(lunarYearInt) {
    lunMonData <- lunarMonthData[lunarYearInt - year(referenceDate) + 1]
    totalDays <- 0
    nonleap <- bitwShiftR(lunMonData, 16) == 15
    maxMonth <- ifelse(nonleap, 12, 13)
    for (i in 1:maxMonth) {
      day <- getMonthDayCount(lunarYearInt, i)
      totalDays <- totalDays + day
    }
    return(totalDays)
  }
  getMonthDayCount <- function(lunarYearInt, lunarMonthInt) {
    lunMonData <- lunarMonthData[lunarYearInt - year(referenceDate) + 1]
    return(29 +  bitwAnd(bitwShiftR(lunMonData, lunarMonthInt), 1))
  }
  
  formatLunar <- function(lunar, withZodiac=FALSE) {
    ## magic number
    stems <- c("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸")
    branches <- c("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")
    zodiac <- c("鼠", "牛", "虎", "兔", "龍", "蛇", "馬", "羊", "猴", "鷄", "狗", "豬") ### Cantonese version, not Vietnamese, OK?
    prefixDay <- c("初", "十", "廿", "卅")
    numerals <- c("一", "二", "三", "四", "五", "六", "七", "八", "九", "十", "十一", "十二")
    ## actual calculation
    stemIndex <- (lunar["Year"]-3) %% 10
    stemIndex <- ifelse(stemIndex==0, length(stems), stemIndex)
    branchIndex <- (lunar["Year"]-3) %% 12
    branchIndex <- ifelse(branchIndex==0, length(branches), branchIndex)
    if (lunar["Month"] == 1) {
      monthStr <- "正" 
    } else {
      monthStr <- numerals[lunar["Month"]]
    }
    monthStr <- ifelse(lunar["Leap"] == 1, paste0("閏", monthStr), monthStr)
    dayStr <- paste0(prefixDay[(lunar["Day"] %/% 10) + 1], ifelse(lunar["Day"] %% 10 == 0, "", numerals[lunar["Day"] %% 10]))
    dayStr <- ifelse(lunar["Day"] == 10, paste0("初", dayStr), dayStr)
    zodiacStr <- ifelse(withZodiac, paste0("肖", zodiac[branchIndex]), "")
    return(paste0(stems[stemIndex], branches[branchIndex], "年", monthStr, "月", dayStr, "日", zodiacStr))
  }

  getLeapMonth <- function(lunarDate, referenceDate) {
    bitwShiftR(lunarMonthData[lunarDate["Year"] - year(referenceDate) + 1], 16)
  }
  ## define a function to check for validity of LunarDate

  
  isValidLunarDate <- function(lunarDate, referenceDate) {
      ### check for correct format
      if (sum(!c("Year", "Day", "Month") %in% names(lunarDate)) != 0) { ### Leap is optional
          return(FALSE)
      }
      if (lunarDate["Month"] < 1 | lunarDate["Month"] > 12 | lunarDate["Day"] < 1 | lunarDate["Day"] > 30) {
          return(FALSE)
      }
      ### check for the validity of lunarDate["Day"]
          #print(timeSpan)
      leapMonth <- getLeapMonth(lunarDate["Year"],referenceDate)
      if ("Leap" %in% names(lunarDate) & lunarDate["Leap"] == 1 & lunarDate["Month"] != leapMonth) {
          return(FALSE)
      }
      if ("Leap" %in% names(lunarDate) & lunarDate["Leap"] == 1 & lunarDate["Month"] == leapMonth) {
          testMonth <- lunarDate["Month"] + 1
      } else if (leapMonth <=12 & lunarDate["Month"] > leapMonth) {
          testMonth <- lunarDate["Month"] + 1
      } else {
          testMonth <- lunarDate["Month"]
      }
      #print(testMonth)
      #print(getMonthDayCount(lunarDate["Year"], testMonth))
      if (lunarDate["Day"] > getMonthDayCount(lunarDate["Year"], testMonth)) {
          return(FALSE)
      }
      return(TRUE)         
  }
  
  convertSolarDate <- function(solarDate, toString, withZodiac, referenceDate, maxDate) {
    ### assert solarDate >= referenceDate and <= maxDate
    if (solarDate < referenceDate | solarDate > maxDate) {
      stop(paste0("solarDate out of the supported range:", referenceDate, " to ", maxDate))
    }
    timeSpan <- as.integer(solarDate - referenceDate)
    ### get the number of day of a given year
    yearDayCount <-  getYearDayCount(lYear)
    while (timeSpan >= yearDayCount) {
      timeSpan <- timeSpan - yearDayCount
      lYear <- lYear + 1
      yearDayCount <- getYearDayCount(lYear)
    }
    monthDayCount <- getMonthDayCount(lYear, lMonth)
    while (timeSpan >= monthDayCount) {
      timeSpan <- timeSpan - monthDayCount
      lMonth <- lMonth + 1
      monthDayCount <- getMonthDayCount(lYear, lMonth)
    }
    leapMonth <-  bitwShiftR(lunarMonthData[lYear - year(referenceDate) + 1], 16)
    lMonthIsLeap <- FALSE
    if (lMonth > leapMonth) {
      lMonth <- lMonth - 1
      if (lMonth == leapMonth) {
        lMonthIsLeap <- TRUE
      }
    }
    lDay <- lDay + timeSpan
    if (toString) {
      return(formatLunar(c(Year = lYear, Month = lMonth, Day = lDay, Leap = lMonthIsLeap), withZodiac = withZodiac))
    } else {
      return(c(Year = lYear, Month = lMonth, Day = lDay, Leap = lMonthIsLeap))
    }
  }
  
  convertLunarDate <- function(lunarDate, referenceDate, ignoreLeap = TRUE, maxDate) {
    ### lunarDate should be created with c(Year = Year, Month = Month, Day = Day)
    ### assert everything is complete
    if (lunarDate["Year"] < year(referenceDate) | lunarDate["Year"] > year(maxDate)) {
      stop(paste0("lunarDate out of the supported range:", referenceDate, " to ", maxDate))
    }
    if (!isValidLunarDate(lunarDate, referenceDate)) {
        stop("invalid lunarDate")
    }
    timeSpan <- 0
    for (y in year(referenceDate):(lunarDate["Year"]-1)) {
      timeSpan <- timeSpan + getYearDayCount(y)
    }
    #print(timeSpan)
    leapMonth <-  bitwShiftR(lunarMonthData[lunarDate["Year"] - year(referenceDate) + 1], 16)
    offsetMonth <- lunarDate["Month"]-1
    if (lunarDate["Month"] > leapMonth) { ## adjusted for the leap month
      offsetMonth <- offsetMonth + 1
    }
    if (lunarDate["Month"] > 1) {
      for (m in 1:offsetMonth) {
        timeSpan <- timeSpan + getMonthDayCount(lunarDate["Year"], m)
      }
    }
    timeSpan = timeSpan + lunarDate["Day"] - 1
    #print(timeSpan)
    if (leapMonth == lunarDate["Month"] & !ignoreLeap) {
      return(c((referenceDate + days(timeSpan)), (referenceDate + days(timeSpan + getMonthDayCount(lunarDate["Year"], leapMonth)) ) ))
    } else {
      return(referenceDate + days(timeSpan))
    }
  }
  if (!is.null(solarDate)) {
    return(convertSolarDate(solarDate, toString, withZodiac, referenceDate, maxDate))
  } else {
    return(convertLunarDate(lunarDate, referenceDate, ignoreLeap = ignoreLeap, maxDate))
  }
}
