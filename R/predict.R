#' Predict a model on a dataset. Intended to predict on to a cell count
#'
#' @param model a lm, glm, glmer, stan_glmer, or sparsereg object
#' @param data a data frame with the appropriate predictors, output from count_cellsize
#' @param draws number of posterior draws in stan
#'
#' @return A vector of predicted values
#'
#' @export
predict_on_cells <- function(model, data, draws = 1000) {

  # stan models require predict_posterior
  if (inherits(model, "stanreg")) {
    pp <- posterior_predict(model, data, draws = draws)   # `draw` rows and one column for each observation of data
    pp_pr <- colMeans(pp)
    return(pp_pr)
  }

  # sparsereg
  if (inherits(model, "sparsereg")) {
    return(predict(model, data, type = "response"))
  }

  # lmer
  if (inherits(model, "glmerMod")) {
    predict(model, data, type = "response", allow.new.levels = TRUE) %>%
      as.vector() %>%
      return()
  }
}
