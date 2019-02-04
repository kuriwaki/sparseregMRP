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
#' @importFrom glue glue
#'
#' @export
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
