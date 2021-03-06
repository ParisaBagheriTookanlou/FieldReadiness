% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statModels.R
\name{simWeather}
\alias{simWeather}
\title{Simulate the weather data}
\usage{
simWeather(param, coefPre, coefTem, coefGR, rndValues)
}
\arguments{
\item{param}{Parameter values given in R function \code{setParam}}

\item{coefPre}{Coeficiant regarding the change in precipitation}

\item{coefTem}{Coeficiant regarding the change in temperature}

\item{coefGR}{Coeficiant regarding the change in global radiation}

\item{rndValues}{A set of random values between 0 and 1 for specifying a wet and rainy day.}
}
\value{
A list containing simulated weather data
}
\description{
Simulate the weather data
}
