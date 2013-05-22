\name{lunarCal}
\alias{lunarCal}
\title{Convert Solar date to and from lunar date}
\usage{
  lunarCal(x, toString = FALSE, withZodiac = FALSE,
    ignoreLeap = TRUE)
}
\arguments{
  \item{x}{date to convert, can be solar date or lunar
  date. If the class of x #' is a date, x is converted to
  lunar date. If the class of x is a vector with the format
  of lunar date, x is converted to solar date.}

  \item{toString}{format the output lunar date to Chinese
  string}

  \item{withZodiac}{Append the Chinese Zodiac sign to the
  string output of lunar date}

  \item{ignoreLeap}{ignore leap month when the converted
  lunar date can be a leap month}
}
\value{
  Date (solar date) or lunar date
}
\description{
  This function allows the conversion between solar date
  and lunar date.
}

