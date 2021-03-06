% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statModels.R
\name{Hydro}
\alias{Hydro}
\title{Soil water content based on weather information initialized by centerpoints in cycling time}
\usage{
Hydro(
  Mois_WedcentrePoints,
  wed_data,
  disTem_val,
  disPre_val,
  disGR_val,
  disEvap_val
)
}
\arguments{
\item{Mois_WedcentrePoints}{Soil water contents based on centerpoints defined for weather information}

\item{wed_data}{A data frame consists of all combinations of weather information centerpoints}

\item{disTem_val}{Temprature data determined by centerpoints for the temprature}

\item{disPre_val}{Precipitation data determined by centerpoints for the precipitation}

\item{disGR_val}{Global Radiation data determined by centerpoint for the global radiation}

\item{disEvap_val}{Evatranspiration data determined by centerpoint for the evatranspiration}
}
\value{
Soil water content based on weather information initialized by centerpoints in cycling time
}
\description{
Soil water content based on weather information initialized by centerpoints in cycling time
}
