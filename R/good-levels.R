# ParaAnita R Package
# Mark Eisler - Sep 2024
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
#' variable has values either all zero or all one i.e., those identified by `good_levels()`. Deprecated,
#' use `drop_zero()` S3 method for class `'data.frame'`.
#'
#' `drop_zero()` drops all data with `levels` of an independent variable for which a binomial dependent variable
#' has either all successes or failures.
#'
#' @details
#' For a Bernoulli trial dataset with a numeric dependent variable coded as \var{0} or \var{1}, `good_levels()`
#' identifies  [`levels`][base::levels] of an independent variable for which values of the dependent variable are
#' neither all zero nor all one i.e., \eqn{0 < p < 1}{0 < p < 1}. The S3 method for class
#' [`binom_contingency`][binom_contingency] work similarly.
#'
#' For a similar dataset, `drop_null()` drops all rows of data other than those with `levels` of the
#' independent variable identified by `good_levels()`. Unused factor levels are dropped from the independent
#' variable. Deprecated, use `drop_zero()` S3 method for class `'data.frame'`.
#'
#' For a Bernoulli trial dataset as a [`data.frame`][data.frame] or a binomial dataset instantiated as a
#' [`binom_contingency`][binom_contingency] object, `drop_zero()` drops rows of data with `levels` of an independent
#' variable having either all successes and no failures, or no successes and all failures.
#'
#' @seealso [`binom_contingency`][binom_contingency] and [`levels`][base::levels].
#' @family levels_data
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of a Bernoulli dependent variable that
#'   should be `numeric` with values of \var{0} and \var{1}.
#'
#' @param .ind_var <[`data-masking`][rlang::args_data_masking]> quoted name of the independent variable, which may be
#'   a `factor`, or a `character vector`. 
#'
#' @param object a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]), or a
#'   `"binom_contingency"` object.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @inheritParams binom_contingency
#'
#' @return `good_levels()` returns a character vector comprising the `levels` of `.ind_var` for which the
#'   corresponding values of `.dep_var` are neither all zero nor all one. `drop_zero()` returns a data frame or a
#'   data frame extension e.g., a [`tibble`][tibble::tibble-package], equivalent to data, including only rows with
#'   levels of `.ind_var` for which `.dep_var` values are neither all zero nor all one, or neither having all
#'   successes nor all failures respectively.
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
#' ## S3 method for class 'binom_contingency' 
#' d_bin |> good_levels(iv)
#' d_bin |> drop_zero(iv)
#'
#' identical(
#'   d_bern |> drop_zero(dv, iv) |> binom_contingency(dv, iv),
#'   d_bin |> drop_zero(iv)
#' )
#'
# #' d |> drop_null(dv, iv) |> levels_data()
# #' d |> drop_null(dv, iv) |> binom_contingency(dv)
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
#' rm(d_bern, d_bin, d_ls)

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

good_levels.data.frame <- function(.data, .dep_var, .ind_var, ...) {

    check_dots_empty()
    .dep_var <- enquo(.dep_var)
    .ind_var <- enquo(.ind_var)
    if (quo_is_missing(.dep_var))
        stop("`.dep_var` is absent but must be supplied.", call. = FALSE)
    if (quo_is_missing(.ind_var))
        stop("`.ind_var` is absent but must be supplied.", call. = FALSE)

    .data |>
        binom_contingency(!!.dep_var, !!.ind_var, .drop_zero = TRUE) |>
        pull(!!.ind_var) |>
        levels()
}

# ========================================
# Levels of independent variable having binomial dependent variable values of either all zero or all one
# for a binomial contingency table
#  S3method good_levels.binom_contingency()
#
#' @rdname good_levels
#' @export

good_levels.binom_contingency <- function(.data, .ind_var, ...) {

    check_dots_empty()
    .ind_var <- enquo(.ind_var)
    if (quo_is_missing(.ind_var))
        stop("`.ind_var` is absent but must be supplied.", call. = FALSE)

    .data |>
        drop_zero_depvar() |>
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
    mutate(across(!!.ind_var, fct_drop))
}

# ========================================
# Remove levels of independent variable having binomial dependent variable values of either all zero or all one
# for a binomial contingency table
#  S3method drop_zero.binom_contingency()
#
#' @rdname good_levels
#' @export

# drop_zero.binom_contingency <- function(object, ...)
    # object |>
        # filter(as.logical(.data$pn), as.logical(.data$qn)) |>
        # mutate(across(where(is.factor), fct_drop))

drop_zero.binom_contingency <- function(object, ...) {

    check_dots_empty()
    object |> drop_zero_depvar()
}

# ========================================
# Remove Levels Of Independent Variable Having Bernoulli Dependent Variable Values Of Either All Zero Or All One
# drop_null() - Deprecated
#' @rdname good_levels
#' @export

drop_null <- function(.data, .dep_var, .ind_var) {
    .ind_var <- enquo(.ind_var)
    .data |> (\(.x)    # anon func here because otherwise filter() passes .data pronoun to good_levels()
        filter(.x, !!.ind_var %in% good_levels(.x, {{.dep_var}}, !!.ind_var)) |>
        mutate(across(!!.ind_var, fct_drop))
    )()
}
