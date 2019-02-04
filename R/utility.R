
#' RMSE between two vectors
#'
#' @param mu A vector, conventionally the estimand
#' @param muhat same length vectorm conventionally the estimate
#' @export
unit_rmse <- function(mu = truth, muhat) sqrt(mean((mu - muhat)^2))
