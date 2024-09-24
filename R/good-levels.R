# ParaAnita R Package
# Mark Eisler - Sep 2024
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# good-levels.R


# ========================================
#' @title
#' Levels of Independent Variable where a Bernoulli Dependent Variable is Neither All Success Nor All Failure
#'
#' @description
#' `good_levels()` identifies `levels` of an independent variable for which values of a Bernoulli dependent
#' variable are neither all one (success) nor all zero (failure).
#'
#' `drop_zero()` drops data with `levels` of an independent variable for which values of a Bernoulli dependent
#' variable are either all one (success) or all zero (failure).
#'
#' @details
#' For a Bernoulli trial dataset with a numeric dependent variable coded as \var{0} or \var{1}, the generic function
#' `good_levels()` identifies [`levels`][base::levels] of an independent variable for which values of the dependent
#' variable are neither all zero nor all one i.e., \eqn{0 < p < 1}{0 < p < 1}.
#'
#' For a similar Bernoulli trial dataset, the generic function `drop_zero()` drops all rows of data other than those
#' with `levels` of the independent variable identified by `good_levels()`. Unused factor levels are dropped from the
#' independent variable and from any other factors in the data.
#'
#' The `drop_zero()` S3 method for objects of class `"binom_contingency"` returns a binomial contingency table
#' equivalent to the original having been created using [`binom_contingency()`][binom_contingency] with argument
#' `.drop_zero = TRUE`.
#'
#' `drop_null()` is deprecated, please use `drop_zero()`.
#'
#' @note
#' Dropping [`levels`] of explanatory factors for which values of a Bernoulli dependent variable are either all zero
#' or all one, prevents warning messages that ‘fitted probabilities numerically 0 or 1 occurred’ when fitting
#' generalized linear models using [`glm()`][stats::glm] or calculating odds ratios using [`odds_ratio()`][odds_ratio]; 
#' see [`binom_contingency()`][binom_contingency].
#'
#' @seealso [`binom_contingency`][binom_contingency] and [`levels`][base::levels].
#' @family levels_data
#'
#' @param object a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]), or an object 
#'   of class `"binom_contingency"`.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of a Bernoulli dependent variable that
#'   should be `numeric` with values of \var{0} and \var{1}.
#'
#' @param .ind_var <[`data-masking`][rlang::args_data_masking]> quoted name of the independent variable, which may be
#'   a `factor`, or a `character vector`. 
#'
#' @return
#' \item{`good_levels()`}{returns a `character vector` comprising `levels` of `.ind_var` for which the corresponding
#'   values of `.dep_var` are neither all one (success) nor all zero (failure)}
#'
#' \item{`drop_zero()`}{returns an object of the same [`class`][base::class] as that provided by argument `object`:
#'   either a data frame (or a data frame extension e.g., a [`tibble`][tibble::tibble-package]) comprising only rows
#'   with levels of the independent variable for which values of the Bernoulli dependent variable are neither all
#'   zero nor all one; or a [`"binom_contingency"`][binom_contingency] object excluding any rows for which values of 
#'   the Bernoulli dependent variable are either all one (success) or all zero (failure).}
#'
#' @export
#' @examples
#' (d_bern <- bernoulli_data(probs = c(0.8, 0.4, 0, 0.3, 0.6 )))
#' d_bern |> levels_data()
#'
#' ## S3 methods for class 'data.frame' 
#' d_bern |> good_levels(dv, iv)
#' d_bern |> drop_zero(dv, iv)
#' d_bern |> drop_zero(dv, iv) |> levels_data()
#'
#' (d_bin <- d_bern |> binom_contingency(dv, iv))
#'
#' ## S3 methods for class 'binom_contingency' 
#' d_bin |> good_levels(iv)
#' d_bin |> drop_zero()
#'
#' ## Results identical whether drop_zero() used
#' ## before or after binom_contingency()
#' identical(
#'   d_bern |> drop_zero(dv, iv) |> binom_contingency(dv, iv),
#'   d_bin |> drop_zero()
#' )
#'
#' ## Results identical whether drop_zero() used
#' ## during or after binom_contingency()
#' identical(
#'   d_bern |> binom_contingency(dv, iv, .drop_zero = TRUE),
#'   d_bin |> drop_zero()
#' )
#'
#' rm(d_bern, d_bin)

# ========================================
# Levels of independent variable having binomial dependent variable values of either all zero or all one
# S3method good_levels()
#
# @rdname good_levels
# @export

good_levels <- function(object, ...)
    UseMethod("good_levels")

# ========================================
# Levels of independent variable having Bernouilli dependent variable values of either all zero or all one
# for a data frame
#  S3method good_levels.data.frame()
#
#' @rdname good_levels
#' @export

good_levels.data.frame <- function(object, .dep_var, .ind_var, ...) {

    check_dots_empty()
    .dep_var <- enquo(.dep_var)
    .ind_var <- enquo(.ind_var)
    if (quo_is_missing(.dep_var))
        stop("`.dep_var` is absent but must be supplied.", call. = FALSE)
    if (quo_is_missing(.ind_var))
        stop("`.ind_var` is absent but must be supplied.", call. = FALSE)

    object |>
        binom_contingency(!!.dep_var, !!.ind_var, .drop_zero = TRUE) |>
        pull(!!.ind_var) |>
        as.factor() |>
        levels()
}

# ========================================
# Levels of independent variable having binomial dependent variable values of either all zero or all one
# for a binomial contingency table
#  S3method good_levels.binom_contingency()
#
#' @rdname good_levels
#' @export

good_levels.binom_contingency <- function(object, .ind_var, ...) {

    check_dots_empty()
    .ind_var <- enquo(.ind_var)
    if (quo_is_missing(.ind_var))
        stop("`.ind_var` is absent but must be supplied.", call. = FALSE)

    object |>
        filter(as.logical(.data$pn), as.logical(.data$qn)) |>
        purge_fcts() |>
        pull(!!.ind_var) |>
        levels()
}

# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
#  S3method drop_zero()
#
#' @rdname good_levels
#' @export

drop_zero <- function(object, ...)
    UseMethod("drop_zero")

# ========================================
# Remove levels of independent variable having Bernouilli dependent variable values of either all zero or all one
# for a data frame
#  S3method drop_zero.data.frame()
#
#' @rdname good_levels
#' @export

drop_zero.data.frame <- function(object, .dep_var, .ind_var, ...) {

    check_dots_empty()
    .ind_var <- enquo(.ind_var)

    filter(object, !!.ind_var %in% good_levels(object, {{.dep_var}}, !!.ind_var)) |>
    purge_fcts()
}

# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
# for a binomial contingency table
#  S3method drop_zero.binom_contingency()
#
#' @rdname good_levels
#' @export

drop_zero.binom_contingency <- function(object, ...) {

    check_dots_empty()
    object |> drop_zero_depvar()
}

# ========================================
# Remove Levels Of Independent Variable Having Bernoulli Dependent Variable Values Of Either All Zero Or All One
# drop_null() - Deprecated, now synonym for drop zero
#' @rdname good_levels
#' @export

drop_null <- drop_zero

# ========================================
#  Drop unused factor levels in data
#  purge_fcts()
#
#  Not exported

purge_fcts <- function(.data)
    .data |> mutate(across(where(is.factor), fct_drop))
