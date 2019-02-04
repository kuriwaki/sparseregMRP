#' wrapper function for MRP estimates
#'
#' @param model a lm, glm, glmer, or sparsereg object
#' @param est_name pick a name for your estimate
#' @param cells cell counts produced from count_cellsize
#' @param geovar variable for geography of interest
#' @param fracvar variabel for the cell fraction
#' @param return_vec whether to return the results as a data frame or vector
#'
#' @export
mrp_estimate <- function(model, est_name, cells = size_cell, geovar = stt, fracvar = frac_of_state, return_vec = FALSE) {

  # initialize
  geovar <- enquo(geovar)
  est_name <- enquo(est_name)
  est_name <- quo_name(est_name)

  fvar_name <- enquo(fracvar)
  fvar_name <- quo_name(fvar_name)

  # get predictions for each cell
  preds_vec <- predict_on_cells(model, cells)
  stopifnot(length(preds_vec) == NROW(cells))
  cells_with_pred <- mutate(cells, preds = preds_vec)

  # post-stratify
  df_geo <- sum_to_geo(cells_with_pred, geovar = stt, predvar = preds, fracvar = fvar_name, name = est_name)

  if (return_vec) pull(df_geo, !!est_name)
  if (!return_vec) select(df_geo, !!geovar, !!est_name)
}




#' Sum up the number of observations based on a geography
#'
#'
#' @param data the dataset that has all the variables
#' @param geovar the variable indicating the geography to sum up to
#' @param predvar the variable for the predicted value
#' @param fracvar the variable that indicates the relative size of the cell, a string
#' @param name name for the new var, a string
#'
sum_to_geo <- function(data, geovar, predvar, fracvar, name) {
  geovar <- enquo(geovar)
  predvar <- enquo(predvar)
  pvarname <- quo_name(predvar)

  data %>%
    group_by(!!geovar) %>%
    summarize(!!name := sum(.data[[pvarname]] * .data[[fracvar]]))
}
