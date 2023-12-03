# ParaAnita R Package
# Mark Eisler - Jun 2023
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# good-levels.R


# ========================================
#' @title
#' Remove Levels of Independent Variable Having Values of Bernoulli Dependent Variable All Zero or All One
#'
#' @description
#' \code{good_levels()} identifies \code{\link{levels}} of an independent variable for which values of the dependent
#' variable are neither all zero nor all one i.e., those for which \eqn{0 < p < 1}{0 < p < 1}.
#'
#' \code{drop_null()} drops all data with \code{levels} of the independent variable for which the dependent variable
#' has values either all zero or all one.
#'
#' \code{levels_data} returns the levels for all factors in data. 
#'
#' \code{nlevels_data} returns the number of levels for all factors in data. 
#'
#' @details
#' For a Bernoulli trial dataset with a numeric dependent variable coded as \var{0} or \var{1}, \code{good_levels()}
#' identifies \code{\link{levels}} of an independent variable for which values of the dependent variable are neither
#' all zero nor all one i.e., \eqn{0 < p < 1}{0 < p < 1}.
#'
#' For a similar dataset, \code{drop_null()} drops all rows of data other than those with \code{levels} of the
#' independent variable identified by \code{good_levels()}. Unused factor levels are dropped from the independent
#' variable.
#'
#' For a binomial dataset, \code{drop_zero()} drops all rows of data having either zero successes or zero failures. 
#'
#' @seealso [`binom_contingency`][binom_contingency] and [`levels`][base::levels].
#' @family good_levels
#'
#' @param .ind_var <[`data-masked`][rlang::args_data_masking]> quoted name of the independent variable, which may be a
#'   \code{factor}, or character vector. 
#'
#' @inheritParams binom_contingency
#'
#' @return \code{good_levels()} returns a character vector comprising the \code{levels} of \code{.ind_var} for which the
#'   corresponding values of \code{.dep_var} are neither all zero nor all one. Both \code{drop_null()} and
#'   \code{drop_zero()} return a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package])
#'   equivalent to data, including only rows with levels of \code{.ind_var} for which \code{.dep_var} values are neither
#'   all zero nor all one or having neither zero successes nor zero failures respectively.
#'
#' @export
#' @examples
#' d <- bernoulli_data(probs = c(0.8, 0.4, 0, 0.3, 0.6 ))
#' d |> binom_contingency(dv, iv)
#' d |> levels_data()
#' d |> good_levels(dv, iv)
#' d |> drop_null(dv, iv) |> binom_contingency(dv, iv)
#' d |> drop_null(dv, iv) |> levels_data()
#'
#' d_ls <- map2(c(0.5, 0.4, 1, 1), c(0.1, 0, 0.6, 0), seq, length.out = 5) |>
#'     map(~ bernoulli_data(probs = .x)) |>
#'     (\(x) setNames(x, paste0("data", seq_along(x))))()
#' d_ls |> map(\(d) d |> binom_contingency(dv, iv))
#' d_ls |> map(levels_data)
#' d_ls |> map(\(d) d |> good_levels(dv, iv))
#' d_ls |> map(\(d) d |> drop_null(dv, iv) |> binom_contingency(dv, iv))
#' d_ls |> map(\(d) d |> binom_contingency(dv, iv) |> drop_zero(iv))
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
    .dep_var <- enquo(.dep_var)
    .ind_var <- enquo(.ind_var)
    .data |>
        contingency_table(!!.dep_var, !!.ind_var) |>
        filter(`1` != 0, `0` != 0) |>
        mutate(across(!!.ind_var, fct_drop)) |>
        (`[[`)(.ind_var |> as_name()) |>
        levels()
}

# ========================================
# Remove Levels Of Independent Variable Having Bernoulli Dependent Variable Values Of Either All Zero Or All One
#' @rdname good_levels
#' @export

drop_null <- function(.data, .dep_var, .ind_var) {
    .dep_var <- enquo(.dep_var)
    .ind_var <- enquo(.ind_var)
    .data |> (\(.x)
        filter(.x, !!.ind_var %in% good_levels(.x, !!.dep_var, !!.ind_var)) |>
        mutate(across(!!.ind_var, fct_drop))
    )()
}

# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
#' @rdname good_levels
#' @export

drop_zero <- function(.data, .ind_var, .dep_var = cbind(pn, qn)) {
    .dep_var <- enquo(.dep_var)
    .data |>
        filter(as.logical((!!.dep_var)[, 1]), as.logical((!!.dep_var)[, 2])) |>
        mutate(across({{.ind_var}}, fct_drop))
}

# ========================================
# Levels of all Factors in Data
#' @rdname good_levels
#' @export

levels_data <- function(data)
    map(data |> select(where(is.factor)), levels)

# ========================================
# Number of Levels of all Factors in Data
#' @rdname good_levels
#' @export

nlevels_data <- function(data)
    map_int(data |> select(where(is.factor)), nlevels)