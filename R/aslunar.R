as.lunar <- function(x) {
  ### automatically formatting the string of x to a lunar date
  ### should be in the format of "2012-01-30". Leap Month should be denoted as "1995-08L-12"
  if (class(x) != "character" | !str_detect(x, "^[0-9]{1,4}[-/ ][0-9]{2}[Ll]?[-/ ][0-9]{2}$")) {
    stop("invalid input: x must be in the format of 2012-01-03 or 1995/08L/12")
  }
  elements <- unlist(str_split(x, "[-/ ]", 3))
  lYear <- as.integer(elements[1])
  if (str_detect(elements[2], "[Ll]$")) {
    lLeap <- 1
  } else {
    lLeap <- 0
  }
  lMonth <- as.integer(str_replace(elements[2], "[Ll]$", ""))
  lDay <- as.integer(elements[3])
  return(c(Year = lYear, Month = lMonth, Day = lDay, Leap = lLeap))
}

isValidLunarDate <- function(lunarDate, conversionData=NULL) {
  if (is.null(conversionData)) {
    ### load from default from package
    data(conversionData, package="hongkong")
  }
  
  ### check for correct format
  if (sum(!c("Year", "Day", "Month") %in% names(lunarDate)) != 0) {
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
