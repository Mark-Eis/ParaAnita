# ParaAnita R Package
# Mark Eisler - Dec 2023
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
# #' d <- bernoulli_data(probs = c(0.8, 0.4, 0, 0.3, 0.6 ))
# #' d |> binom_contingency(dv)
# #' d |> levels_data()
# #' d |> good_levels(dv, iv)
# #' d |> drop_null(dv, iv) |> levels_data()
# #' d |> drop_null(dv, iv) |> binom_contingency(dv)
# #' d |> binom_contingency(dv) |> drop_zero(iv)
# #'
# #' identical(
# #'   d |> drop_null(dv, iv) |> binom_contingency(dv),
# #'   d |> binom_contingency(dv) |> drop_zero(iv)
# #' )
# #'
# #' d_ls <- map2(c(0.5, 0.4, 1, 1), c(0.1, 0, 0.6, 0), seq, length.out = 5) |>
# #'     map(\(x) bernoulli_data(probs = x)) |>
# #'     (\(x) setNames(x, paste0("data", seq_along(x))))()
# #'
# #' d_ls |> map(\(d) d |> binom_contingency(dv))
# #' d_ls |> map(levels_data)
# #' d_ls |> map(\(d) d |> good_levels(dv, iv))
# #' d_ls |> map(\(d) d |> drop_null(dv, iv) |> binom_contingency(dv))
# #' d_ls |> map(\(d) d |> binom_contingency(dv) |> drop_zero(iv))
# #'
# #' identical(
# #'   d_ls |> map(\(d) d |> drop_null(dv, iv) |> binom_contingency(dv)), 
# #'   d_ls |> map(\(d) d |> binom_contingency(dv) |> drop_zero(iv))
# #' )
# #'
# #' rm(d, d_ls)
#'
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