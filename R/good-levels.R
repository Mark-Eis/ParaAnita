# ParaAnita R Package
# Mark Eisler - Jul 2024
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# good-levels.R


# ========================================
#' @title
#' Remove Levels of Independent Variable where Dependent Variable All Success or All Failure
#'
#' @description
#' `good_levels()` identifies `levels` of an independent variable for which values of a Bernoulli dependent
#' variable are neither all zero nor all one i.e., those for which \eqn{0 < p < 1}{0 < p < 1}.
#'
#' `drop_null()` drops all data with `levels` of an independent variable for which a Bernoulli dependent
#' variable has values either all zero or all one i.e., those identified by `good_levels()`.
#'
#' `drop_zero()` drops all data with `levels` of an independent variable for which a binomial dependent variable
#' has either all successes or failures.
#'
#' `levels_data` returns the levels for all factors in data. 
#'
#' `nlevels_data` returns the number of levels for all factors in data. 
#'
#' @details
#' For a Bernoulli trial dataset with a numeric dependent variable coded as \var{0} or \var{1}, `good_levels()`
#' identifies  [`levels`][base::levels] of an independent variable for which values of the dependent variable are
#' neither all zero nor all one i.e., \eqn{0 < p < 1}{0 < p < 1}.
#'
#' For a similar dataset, `drop_null()` drops all rows of data other than those with `levels` of the
#' independent variable identified by `good_levels()`. Unused factor levels are dropped from the independent
#' variable.
#'
#' For a binomial dataset, `drop_zero()` drops rows of data having either all successes and no failures, or no
#' successes and all failures.
#'
#' @seealso [`binom_contingency`][binom_contingency] and [`levels`][base::levels].
#' @family levels_data
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of a Bernoulli dependent variable that
#'   should be `numeric` with values of \var{0} and \var{1}; or in the case of `drop zero()`, a binomial dependent
#'   variable, default `cbind(.data$pn, .data$qn)`, representing the number of successes and failures respectively,
#'   see [`glm()`][stats::glm].
#'
#' @param .ind_var <[`data-masking`][rlang::args_data_masking]> quoted name of the independent variable, which may be
#'   a `factor`, or a `character vector`. 
#'
#' @inheritParams binom_contingency
#'
#' @return `good_levels()` returns a character vector comprising the `levels` of `.ind_var` for which the
#'   corresponding values of `.dep_var` are neither all zero nor all one. Both `drop_null()` and
#'   `drop_zero()` return a data frame or a data frame extension e.g., a [`tibble`][tibble::tibble-package],
#'   equivalent to data, including only rows with levels of `.ind_var` for which `.dep_var` values are neither
#'   all zero nor all one, or neither having all successes nor all failures respectively.
#'
#' @export
#' @examples
#' d <- bernoulli_data(probs = c(0.8, 0.4, 0, 0.3, 0.6 ))
#' d |> binom_contingency(dv)
#' d |> levels_data()
#' d |> good_levels(dv, iv)
#' d |> drop_null(dv, iv) |> levels_data()
#' d |> drop_null(dv, iv) |> binom_contingency(dv)
#' d |> binom_contingency(dv) |> drop_zero(iv)
#'
#' identical(
#'   d |> drop_null(dv, iv) |> binom_contingency(dv),
#'   d |> binom_contingency(dv) |> drop_zero(iv)
#' )
#'
#' d_ls <- map2(c(0.5, 0.4, 1, 1), c(0.1, 0, 0.6, 0), seq, length.out = 5) |>
#'     lapply(\(x) bernoulli_data(probs = x)) |>
#'     (\(x) setNames(x, paste0("data", seq_along(x))))()
#'
#' d_ls |> lapply(\(d) d |> binom_contingency(dv))
#' d_ls |> lapply(levels_data)
#' d_ls |> lapply(\(d) d |> good_levels(dv, iv))
#' d_ls |> lapply(\(d) d |> drop_null(dv, iv) |> binom_contingency(dv))
#' d_ls |> lapply(\(d) d |> binom_contingency(dv) |> drop_zero(iv))
#'
#' identical(
#'   d_ls |> lapply(\(d) d |> drop_null(dv, iv) |> binom_contingency(dv)), 
#'   d_ls |> lapply(\(d) d |> binom_contingency(dv) |> drop_zero(iv))
#' )
#'
#' rm(d, d_ls)
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

good_levels <- function(.data, .dep_var, .ind_var) {
    .ind_var <- enquo(.ind_var)
    .data |>
        contingency_table({{.dep_var}}, !!.ind_var) |>
        filter(.data$`1` != 0, .data$`0` != 0) |>
        pull(!!.ind_var) |>
        fct_drop() |>
        levels()
}

# ========================================
# Remove Levels Of Independent Variable Having Bernoulli Dependent Variable Values Of Either All Zero Or All One
#' @rdname good_levels
#' @export

drop_null <- function(.data, .dep_var, .ind_var) {
    .ind_var <- enquo(.ind_var)
    .data |> (\(.x)    # anon func here because otherwise filter() passes .data pronoun to good_levels()
        filter(.x, !!.ind_var %in% good_levels(.x, {{.dep_var}}, !!.ind_var)) |>
        mutate(across(!!.ind_var, fct_drop))
    )()
}

# # ========================================
# # Remove levels of independent variable having binomial dependent variable values of either all zero or all one
# #' @rdname good_levels
# #' @export

# drop_zero <- function(.data, .ind_var, .dep_var = cbind(.data$pn, .data$qn)) {
    # .dep_var <- enquo(.dep_var)
    # .data |>
        # filter(as.logical((!!.dep_var)[, 1]), as.logical((!!.dep_var)[, 2])) |>
        # mutate(across({{.ind_var}}, fct_drop))
# }


# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
#  S3method drop_zero()
#
#' @rdname good_levels
#' @export

drop_zero <- function(object, ...)
    UseMethod("drop_zero")

# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
# for a binomial contingency table
#  S3method drop_zero.binom_contingency()
#
#' @rdname good_levels
#' @export

drop_zero.binom_contingency <- function(object, ...)
    object |>
        filter(as.logical(.data$pn), as.logical(.data$qn)) |>
        mutate(across(where(is.factor), fct_drop))
