suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(glue))

#' Given a individual level population dataset and variables of interest, count the number of
#' population and the fraction of that subpopulationin the geography of interest
#'
#' @param size A population, or census data frame
#' @param popvar The variable that indicates the population
#' @param geovar The variable that indicates the final grouping variable of interest
#' @param ... variables that are in the regression
#'
#' @examples
#' size <- read_csv("https://www.shirokuriwaki.com/datasets/popsize08.csv")
#' size_cell <- count_cellsize(size, popvar = pop2008, geovar = stt, eth, age, sex, inc, edu)
count_cellsize <- function(size, popvar, geovar, ...) {
  pop <- enquo(popvar)
  geo <- enquo(geovar)
  geo_name <- quo_name(geo)
  vars <- enquos(...)

  # size of geography
  size_state <- size  %>%
    group_by(!!geo) %>%
    summarize(nstate = sum(!!pop))

  # size of geography and other separating variables
  cells <- size %>%
    group_by(!!geo, !!!vars) %>%
    summarize(ncell = sum(!!pop)) %>%
    ungroup() %>%
    left_join(size_state, by = geo_name) %>%
    mutate(frac_of_state = ncell / nstate) %>%
    select(-nstate)

  # store attributes for future use
  attr(cells, "geography") <- geo_name
  attr(cells, "count") <- "ncell"
  attr(cells, "fraction of geography") <- "frac_of_state"

  # check
  if (!all(c(attr(cells, "geography"), attr(cells, "count"), attr(cells, "fraction of geography")) %in%
           colnames(cells))) {
    stop("variable name mismatch")
  }

  cells
}


#' Synthetic expansion
#'
#' Creates a synthetic post-stratification dataset by simple multiplication, i.e.
#'  distribution of variables in new cells (entered as marginals) are orthogonal to
#'  original counts
#'
#' @param start starting cell counts. These will have been estimated by census data.
#' @param marginal new information we want to incorporate by synthetic approach,
#'  usually only as a marginal distribution. Must be a dataframe with three variables:
#'  the geography variable (same name for original count data), one variable that indicates the
#'  category of the new variable (e.g. "voted" and "not voted"), and a variable called "frac" which
#'  indicates the proportion of people of that category within that geography --
#'  i.e. all values within a geography must sum to 1.
#'
#'
synth_cellsize <- function(start, marginal) {
  geovar <- attr(start, "geography")
  countvar <- attr(start, "count")
  fracvar <- attr(start, "fraction of geography")

  oldvars <- setdiff(colnames(start), c(geovar, countvar, fracvar))
  newvar <- setdiff(colnames(marginal), c("frac", geovar))

  # checks
  if (!geovar %in% colnames(marginal)) stop(glue("new df must be keyed on variable called {geovar}"))
  if (!"frac" %in% colnames(marginal)) stop(glue("please name variable of fractions by variable called frac"))
  if (length(colnames(marginal)) != 3) stop("currently, please limit the marginal info to three columns: georaphy, variable, and fraction")

  # join
  sm <- left_join(start, marginal, by = geovar)
  if (any(is.na(sm$frac))) stop("use complete dataset without missing values")

  # expand by simple multiplication
  sm_expanded <- sm %>%
    mutate(ncell =  .data[[countvar]] * frac,
           frac_of_state  = .data[[fracvar]] * frac)

  # reorder
  sm_expanded %>%
    select(!!geovar,
           !!oldvars,
           !!newvar,
           !!countvar,
           !!fracvar)
}


#' Predict a model on a dataset. Intended to predict on to a cell count
#'
#' @param model a lm, glm, glmer, stan_glmer, or sparsereg object
#' @param data a data frame with the appropriate predictors, output from count_cellsize
#' @draws number of posterior draws in stan
#'
#' @return A vector of predicted values
#'
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

#' @param data the dataset that has all the variables
#' @param geovar the variable indicating the geography to sum up to
#' @param predvar the variable for the predicted value
#' @param fracvar the variable that indicates the relative size of the cell, a string
#' @param name name for the new var, a string
sum_to_geo <- function(data, geovar, predvar, fracvar, name) {
  geovar <- enquo(geovar)
  predvar <- enquo(predvar)
  pvarname <- quo_name(predvar)

  data %>%
    group_by(!!geovar) %>%
    summarize(!!name := sum(.data[[pvarname]] * .data[[fracvar]]))
}


#' wrapper function for MRP estimates
#'
#' @param model a lm, glm, glmer, or sparsereg object
#' @param est_name pick a name for your estimate
#' @param cells cell counts produced from count_cellsize
#' @param geovar variable for geography of interest
#' @param fracvar variabel for the cell fraction
#' @param return_vec whether to return the results as a data frame or vector
#'
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

#' RMSE between two vectors
unit_rmse <- function(mu = truth, muhat) sqrt(mean((mu - muhat)^2))


#' function to evaluate RMSE and correlation
#' @param truth variable name of the truth to predict (no quotes)
#' @param est_suffix regex for variable names of estimates to consider
#'
#' @return A dataframe with three columns, \code{model}, \code{rmse}, and \code{corr}.
#' One row for each model specified
#'
eval_mrp <- function(tbl, truth, est_suffix) {

  truth <- enquo(truth)

  truth_vec <- pull(tbl, !!truth)
  eval_df <- select(tbl, dplyr::matches(est_suffix))

  rmse <- apply(eval_df, MARGIN = 2, function(x) unit_rmse(mu = truth_vec, muhat = x))
  corr <- apply(eval_df, MARGIN = 2, function(x) cor(truth_vec, x))

  tibble(model = names(rmse), rmse = rmse, corr = corr)
}

#' function to show diagnostic graphic
#' @param x the xaxis variable (no quotes)
#' @param y the yaxis variable (no quotes)
#' @param ggtemplate an empty ggplot object with formatting layout
#' @param data a dataframe which includes columns x and y
#' @param null the global mean a null benchmark
#'
scatter_mrp <- function(x, y, ggtemplate = gg0, data = mrp_df, null = global_mean) {
  labrename <- function(vec) {
    recode(vec,
           rvote = "McCain 2008 Vote Share",
           rvote_unweighted = "Unweighted State Averages",
           rvote_weighted = "Weighted State Averages",
           rvote_global = "Weighted US Average",
           rvote_glm_fixef_linear = "GLM linear dummy RP",
           rvote_glm_ranef_linear = "GLMER linear MRP",
           rvote_glm_ranef_probit = "GLMER probit MRP",
           rvote_glm_ranef_logit = "GLMER logit MRP",
           rvote_glm_fixef_0linear = "GLM linear dummy w/o state",
           rvote_spe_fixef_linear = "sparsereg EM linear dummy RP",
           rvote_spe_fixef_probit = "sparsereg EM probit dummy RP",
           rvote_spe_fixef_logit = "sparsereg EM logit dummy RP",
           rvote_spe_ranef_linear = "sparsereg EM linear MRP",
           rvote_spe_ranef_probit = "sparsereg EM probit MRP",
           rvote_spe_ranef_logit = "sparsereg EM logit MRP",
           rvote_spg_fixef_linear = "sparsereg Gibbs linear dummy RP",
           rvote_spg_fixef_probit = "sparsereg Gibbs probit dummy RP",
           rvote_spg_ranef_linear = "sparsereg Gibbs linear MRP",
           rvote_spg_ranef_probit = "sparsereg Gibbs probit MRP"
    )
  }
  xvar <- enquo(x)
  xvar_name <- quo_name(xvar)

  muhat <- enquo(y)
  muhat_name <- quo_name(muhat)


  dg <- eval_mrp(data, truth = rvote, est_suffix = "(rvote|glmer|stan|sparsereg)")
  dg <- filter(dg, model == muhat_name)
  cap <- glue("RMSE: {signif(dg$rmse[1], 3)} and Corr: {signif(dg$corr[1], 3)}")
  if(!is.null(null)) {
    global_rmse <- unit_rmse(mu = data$rvote, null)
    cap <- glue("{cap}.\n {round(100*(global_rmse - dg$rmse[1]) / global_rmse)} percent reduction in RMSE over global mean")
  }

  gg <- ggtemplate %+% data +
    aes_string(x = xvar_name, y = muhat_name) +
    labs(subtitle = cap,
         #         title = labrename(muhat_name),
         x = labrename(xvar_name),
         y = labrename(muhat_name))
  gg
}
