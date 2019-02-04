
#' RMSE between two vectors
#'
#' @param mu A vector, conventionally the estimand
#' @param muhat same length vectorm conventionally the estimate
#' @export
unit_rmse <- function(mu = truth, muhat) sqrt(mean((mu - muhat)^2))


#' Inverse logit function (xb to  p)
#'
#'
expit <- function(x) (1 + exp(-x)) ^ (-1)

#' Logit function (p to xb)
#'
#' log(p / (1 - p))
#'
logit <- function(x) log(x) - log(1 - x)
