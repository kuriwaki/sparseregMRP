#' Given a individual level population dataset and variables of interest, count the number of
#' population and the fraction of that subpopulationin the geography of interest
#'
#' @param size A population, or census data frame
#' @param popvar The variable that indicates the population
#' @param geovar The variable that indicates the final grouping variable of interest
#' @param ... variables that are in the regression
#'
#'
#' @examples
#' size <- read_csv("https://www.shirokuriwaki.com/datasets/popsize08.csv")
#' size_cell <- count_cellsize(size, popvar = pop2008, geovar = stt, eth, age, sex, inc, edu)
#'
#' @export
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

