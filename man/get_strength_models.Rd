% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict_strength_pre_season.R
\name{get_strength_models}
\alias{get_strength_models}
\title{Generate Strength Models for Football Seasons}
\usage{
get_strength_models(data_league_for_models, model_years)
}
\arguments{
\item{data_league_for_models}{Data frame with match data pre-filtered for relevant years}

\item{model_years}{Numeric vector of season end years to create models for}
}
\value{
List containing three named lists (models_fh, models_sh, models_full),
each with goalmodel objects for the specified years
}
\description{
Creates first half (FH), second half (SH), and full season goalmodels for specified years.
Automatically determines season length and splits games accordingly.
}
\keyword{internal}
