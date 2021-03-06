% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.R
\name{predict_on_cells}
\alias{predict_on_cells}
\title{Predict a model on a dataset. Intended to predict on to a cell count}
\usage{
predict_on_cells(model, data, draws = 1000)
}
\arguments{
\item{model}{a lm, glm, glmer, stan_glmer, or sparsereg object}

\item{data}{a data frame with the appropriate predictors, output from count_cellsize}

\item{draws}{number of posterior draws in stan}
}
\value{
A vector of predicted values
}
\description{
Predict a model on a dataset. Intended to predict on to a cell count
}
