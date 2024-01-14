# ParaAnita R Package
# Mark Eisler - Jan 2024
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# levels_data.R


# ========================================
#' @title
#' Levels of all Factors in Data
#'
#' @description
#' `levels_data()` returns the levels for all factors in data. 
#'
#' `nlevels_data()` returns the number of levels for all factors in data. 
#'
#' @details
#' Does what it says on the tin.
#'
#' @seealso [`levels`][base::levels].
#' @family levels_data
#'
#' @inheritParams binom_contingency
#'
#' @return `levels_data()` returns a named `list` comprising the `levels` of each `factor` in `data`. `nlevels_data()`
#'   returns a named `integer vector` comprising the number of `levels` of each `factor` in `data`.
#'
#' @export
#' @examples
#' ## Using gss_cat dataset from {forcats} package
#' \dontshow{
#'    if (!requireNamespace("forcats", quietly = TRUE)) 
#'        warning("package 'forcats' must be installed")
#'    try(gss_cat <- forcats::gss_cat)
#' }
#'
#' gss_cat |> names()
#' gss_cat |> levels_data()
#' gss_cat |> nlevels_data()
#'
#' \dontshow{
#'     rm(gss_cat)
#' }

levels_data <- function(data)
    map(data |> select(where(is.factor)), levels)

# ========================================
# Number of Levels of all Factors in Data
#' @rdname levels_data
#' @export

nlevels_data <- function(data)
    map_int(data |> select(where(is.factor)), nlevels)