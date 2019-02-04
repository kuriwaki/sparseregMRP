
#' function to evaluate RMSE and correlation
#' @param truth variable name of the truth to predict (no quotes)
#' @param est_suffix regex for variable names of estimates to consider
#'
#' @return A dataframe with three columns, \code{model}, \code{rmse}, and \code{corr}.
#' One row for each model specified
#'
#' @importFrom glue glue
#'
#' @export
eval_mrp <- function(tbl, truth, est_suffix) {

  truth <- enquo(truth)

  truth_vec <- pull(tbl, !!truth)
  eval_df <- select(tbl, dplyr::matches(est_suffix))

  rmse <- apply(eval_df, MARGIN = 2, function(x) unit_rmse(mu = truth_vec, muhat = x))
  corr <- apply(eval_df, MARGIN = 2, function(x) cor(truth_vec, x))

  tibble(model = names(rmse), rmse = rmse, corr = corr)
}




#' Diagnostic graphic
#'
#' Scatterplot with x as truth and y as estiamte
#'
#' @param x the xaxis variable (no quotes)
#' @param y the yaxis variable (no quotes)
#' @param ggtemplate an empty ggplot object with formatting layout
#' @param data a dataframe which includes columns x and y
#' @param null the global mean a null benchmark
#'
#' @import ggrepel
#'
#' @export
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
