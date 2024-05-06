# ParaAnita R Package
# Mark Eisler - May 2024
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# contingency-table.R

# ========================================
#' @title
#' Contingency Tables for Two or More Categorical Variables
#'
#' @description
#' `contingency_table()` compiles a contingency table for two or more categorical variables, the first of which is
#' typically an outcome (dependent) varable to be used for the column headings, while the remainder are typically
#' explanatory (independent) variables that will appear in the contingency table either as factors or optionally as row
#' headings.
#'
#' `xcontingency_table()` compiles a contingency table for a categorical outcome varable and multiple categorical
#' explanatory variables that are \dQuote{crossed} to obtain a single explanatory factor.
#'
#' @details
#' Categorical variables (i.e. factors or character vectors) in `.data` required as factors in the resulting
#' contingency table may be selected for inclusion or exclusion using the \code{\dots} argument and the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax from package \pkg{\link[dplyr]{dplyr}}, including use of
#' \dQuote{selection helpers}. If no \code{\dots} arguments are supplied, all categorical variables in `.data`
#' (other than `.dep_var`) will be used.
#'
#' A [list][base::list] of [defused R expressions][rlang::topic-defuse], as for instance created by
#' [`expl_fcts()`][expl_fcts], may be used as the \code{\dots} arguments and should be injected using the
#' [splice-operator][rlang::splice-operator], `!!!`, see examples.
#'
#' If `.wt = NULL`, the number of rows for each unique combination of the dependent and independent variables are
#' counted. If `.wt` is the quoted name of a numeric variable representing frequency weights, these are summated for
#' each unique combination of the dependent and independent variables.
#'
#' If `.rownames = TRUE`, the resulting contingency table will be a conventional `data.frame` rather than a
#' `tibble` and the first categorical variable (other than `.dep_var`) will be used for row headings rather than
#' as a factor. Having row headings allows the result to be passed as an argument to [`chisq.test()`][stats::chisq.test],
#' [`fisher.test()`][stats::fisher.test] or [`chsqfish()`][chsqfish], e.g., conveniently using `|>` in a
#' [piped sequence][pipeOp] (see examples). However, using `.rownames = TRUE` for a contingency table with more than
#' one explanatory (independent) variable will most likely result in the error message
#' \dQuote{duplicate 'row.names' are not allowed}, in which case `xcontingency_table()` should be used instead.
#'
#' Multiple categorical explanatory variables in a contingency table compiled by `xcontingency_table()` are
#' \dQuote{crossed} using [`fct_cross()`][forcats::fct_cross].
#'
#' @seealso  [`defused R expressions`][rlang::topic-defuse], [`fct_cross()`][forcats::fct_cross],
#'   [`splice-operator`][rlang::splice-operator] and [`tibble`][tibble::tibble-package];
#'   [`Print_Methods`][Print_Methods] for S3 method for printing objects of class `"contingency_table"`.
#' @family contingency_table
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of the dependent variable, which may be a
#'   `character vector`, `factor`, or `numeric`. 
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> quoted name(s) of one or more `factors` or
#'   `character vectors` in `.data`, to be included in (or excluded from) the output.
#'
#' @param .wt <[`data-masking`][rlang::args_data_masking]> quoted name of a numeric column in `.data` containing
#'   frequency weights; default `NULL`.
#'
#' @param .crossname a character string to be used as the name of the column of for the crossed variables. If
#'   omitted, the names of the crossed variables are used combined in \dQuote{snake case}.
#'
#' @param .rownames `logical`. If `TRUE`, value is a data frame with the levels of the first
#'   (or crossed) independent variable as row names, rather than a tibble; default `FALSE`.
#'
#' @return
#' For `contingency_table()`, an object of class `"contingency_table"`, `"announce"`, inheriting from
#'   [`tibble`][tibble::tibble-package], or a [`data.frame`][base::data.frame], depending on whether
#'   `.rownames = FALSE` (default) or `TRUE`.
#'
#' Similarly for `xcontingency_table()`, an object of class `"xcontingency_table"`, `"announce"` inheriting from
#'   [`tibble`] or a `data.frame`, again depending on the value of `rownames`.
#'
#' @keywords manip
#' @export
#' @examples
#' (d <- tibble(
#'     iv = letters[1:4] |> sample(10, replace = TRUE),
#'     dv = c(0L:3L) |> sample(10, replace = TRUE)
#' ))
#'
#' d |> contingency_table(dv)
#'
#' d |> contingency_table(dv, .rownames = TRUE)
#' 
#' ## Use .data pronoun for more informative error messages
#' d |> contingency_table(.data$dv)
#' 
#' try(d |> contingency_table(dx))
#' 
#' try(d |> contingency_table(.data$dx))
#'
#' (d <- tibble(
#'     iv = letters[1:4] |> sample(10, replace = TRUE) |> as.factor(),
#'     dv = c("Success", "Fail", "Borderline")  |> sample(10, replace = TRUE)
#'   ))
#'
#' d |> contingency_table(dv)
#'
#' d |> contingency_table(dv, .rownames = TRUE)
#'
#' (d <- tibble(
#'     iv = letters[1:4] |> sample(100, replace = TRUE),
#'     dv = c("Success", "Fail", "Borderline")  |> sample(100, replace = TRUE)
#'   ) |> count(iv, dv))
#'
#' d |> contingency_table(dv, .wt = n)
#'
#' d |> contingency_table(dv, .wt = n, .rownames = TRUE) |>
#'     print_lf() |>
#'     chisq.test()
#' 
#' ## Use .data pronoun for more informative error messages
#' d |> contingency_table(dv, .wt = .data$n)
#' 
#' try(d |> contingency_table(dv, .wt = .data$x))
#'
#' rm(d)
#' 
#' ## Using gss_cat dataset from {forcats} package
#' \dontshow{
#'   if (!requireNamespace("forcats", quietly = TRUE)) 
#'     warning("package 'forcats' must be installed")
#'   try(gss_cat <- forcats::gss_cat)
#' }
#'
#' gss_cat |> contingency_table(race, relig, denom)
#'
#' gss_cat |> contingency_table(race, !c(marital, rincome:partyid))
#'
#' ## Invokes warning and error message about duplicate 'row.names'
#' try(gss_cat |> contingency_table(race, relig, denom, .rownames = TRUE)) 
#'
#' ## Using xcontingency_table() avoids warning and error
#' gss_cat |> xcontingency_table(race, relig, denom)
#'
#' gss_cat |> xcontingency_table(race, !c(marital, rincome:partyid))
#'
#' gss_cat |> xcontingency_table(race, relig, denom, .crossname = "Denomination")
#'
#' gss_cat |>
#'     xcontingency_table(race, relig, denom, .rownames = TRUE) |>
#'     head(10)
#'
#' ## Two more esoteric examples
#' ivars <- exprs(relig, denom)
#' gss_cat |> contingency_table(race, !!!ivars)
#'
#' ivars <- c("relig", "denom")
#' gss_cat |> contingency_table(race, any_of(ivars))
#'
#' rm(ivars)
#'
#' \dontshow{
#'   rm(gss_cat)
#' }
#'

contingency_table <- function(.data, .dep_var, ..., .wt = NULL, .rownames = FALSE) {
    .dep_var <- enquo(.dep_var)
    .wt = enquo(.wt)
    stopifnot(
        is.data.frame(.data),
        !quo_is_missing(.dep_var),
        eval_tidy(expr(is.atomic(!!.dep_var)), .data),
        eval_tidy(expr(is.numeric(!!.wt %||% 1)), .data)
    )

    .dep_var <- quo_set_expr(.dep_var, rm_datapro(quo_get_expr(.dep_var)))

    if (...length())
        pos <- eval_select(expr(!!.dep_var | (c(...) & chr_or_fct())), data = .data)
    else 
        pos <- eval_select(expr(!!.dep_var | chr_or_fct()), data = .data)

    ctab <- .data |>
        summarise(n = !!.wt %||% n(), .by = c(!!!syms(names(pos)))) |>
        pivot_wider(names_from = !!.dep_var, values_from = n, values_fill = 0)

    if (.rownames)
        column_to_rownames(ctab, names(pos)[2])
    else
        new_contingency_table(ctab)
}

# ========================================
#  Constructor for a Contingency Table
#  new_contingency_table()
#
# Not exported

new_contingency_table <- function(x = data.frame(NULL), prt_str = "Contingency Table", ...) {
    stopifnot(is.data.frame(x))
    x <- announce(x, prt_str)
    structure(x, class = c("contingency_table", class(x)), ...)
}

# ========================================
#  Contingency Table for Crossed Categorical Variables
#
#' @rdname contingency_table
#' @export

xcontingency_table <- function(.data, .dep_var, ..., .crossname = NULL, .wt = NULL, .rownames = FALSE) {
    .dep_var <- enquo(.dep_var)
    .wt = enquo(.wt)
    stopifnot(
        is.data.frame(.data),
        !quo_is_missing(.dep_var),
        eval_tidy(expr(is.atomic(!!.dep_var)), .data),
        eval_tidy(expr(is.numeric(!!.wt %||% 1)), .data)
    )

    .dep_var <- quo_set_expr(.dep_var, rm_datapro(quo_get_expr(.dep_var)))

    if (...length())
        pos <- eval_select(expr(!(!!.dep_var) & (c(...) & chr_or_fct())), data = .data)
    else 
        pos <- eval_select(expr(!(!!.dep_var) & chr_or_fct()), data = .data)

    .crossname <- sym(.crossname %||% paste(names(pos), collapse = "_"))
    ctab <- .data |>
        mutate(!!.crossname := fct_cross(!!!syms(names(pos)))) |>
        contingency_table(!!.dep_var, !!.crossname, .rownames = .rownames)
    if (.rownames)
        ctab
    else
        new_xcontingency_table(ctab)
}

# ========================================
#  Constructor for a Crossed Contingency Table
#  new_xcontingency_table()
#
# Not exported

# new_xcontingency_table <- function(x = data.frame(NULL), ...) {
    # stopifnot(is.data.frame(x))
    # structure(x, class = c("xcontingency_table", class(x)), ...)
# }

new_xcontingency_table <- function(x = data.frame(NULL), ...) {
    stopifnot(is.data.frame(x))
    structure(x, class = c("xcontingency_table", class(x)), lead = "Crossed Contingency Table", ...)
}


# ========================================
#' Binomial Contingency Table for Data with a Binary Outcome
#'
#' @description
#' [`binom_contingency()`] creates a binomial contingency table for data with a binary dependent variable and
#' one or more categorical independent variables, optionally including totals, proportions and confidence intervals.
#'
#' @details
#' Categorical variables (i.e. factors or character vectors) in `.data` required as factors in the resulting
#' contingency table may be selected for inclusion or exclusion using the \code{\dots} argument and the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax from package \pkg{\link[dplyr]{dplyr}}, including use of
#' \dQuote{selection helpers}. If no \code{\dots} arguments are supplied, all categorical variables in `.data`
#' (other than `.dep_var`) will be used.
#'
#' A [list][base::list] of [defused R expressions][rlang::topic-defuse], as for instance created by
#' [`expl_fcts()`][expl_fcts], may be used as the \code{\dots} arguments and should be injected using the
#' [splice-operator][rlang::splice-operator], `!!!`, see examples.
#'
#' Use `drop_zero = TRUE` to drop [`levels`][base::levels] of explanatory factors for which values of
#' `.dep_var` are either all zero or all one, to prevent a warning messages that ‘fitted probabilities
#' numerically 0 or 1 occurred’ when fitting generalized linear models using [`glm()`][stats::glm] or calculating
#' odds ratios using [`odds_ratio()`][odds_ratio]; see examples and Venables & Ripley (2002, pp. 197–8).
#'
#' `as_binom_contingency()` attempts to coerce an object to [`class`][base::class] `"binom_contingency"`. If
#' `.pn` or `.qn` arguments are not provided, these will be assumed to be columns `"pn"` and `"qn"` respectively.
#'
#' @note
#' Confidence intervals are calculated using [`prop.test()`][stats::prop.test], and are based on Wilson's score method
#'   \emph{without} continuity correction (Newcombe, 1998).
#'
#' @references
#' Confidence interval from R's `prop.test()` differs from hand calculation and result from SAS.
#'   \href{https://stats.stackexchange.com/questions/183225/confidence-interval-from-rs-prop-test-differs-from-hand-calculation-and-resul/183238#183238}{
#'   Stack Exchange}.
#'
#' Newcombe R.G. (1998). Two-Sided Confidence Intervals for the Single Proportion: Comparison of Seven Methods.
#'   \emph{Statistics in Medicine}, \strong{17}, 857-872.
#'   \href{https://doi.org/10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E}{
#'     \doi{10.1002/(SICI)1097-0258(19980430)17:8<857::AID-SIM777>3.0.CO;2-E}}.
#'
#' Venables, W.N. and Ripley, B.D. (2002) \emph{Modern Applied Statistics with S.} New York: Springer.
#'   \href{https://doi.org/10.1007/978-0-387-21706-2}{\doi{10.1007/978-0-387-21706-2}}.
#'
#' Yates' continuity correction in confidence interval returned by `prop.test`.
#'   \href{https://stats.stackexchange.com/questions/5206/yates-continuity-correction-in-confidence-interval-returned-by-prop-test}{
#'   Stack Exchange}.
#'
#' @seealso [`drop_zero()`][drop_zero], [`glm()`][stats::glm], [`odds_ratio()`][odds_ratio],
#'   [`prop.test()`][stats::prop.test] and [`tibble`][tibble::tibble-package];
#'   [`Print_Methods`][Print_Methods] for S3 method for printing objects of class `"binom_contingency"`.
#'
#' @family contingency_table
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of a binary dependent variable, which
#'   should be `numeric` with values of \var{0} and \var{1}. 
#'
#' @param \dots
#'   for `binomial_contingency()`:  <[`tidy-select`][dplyr::dplyr_tidy_select]> quoted name(s) of one or more
#'   `factor` or `character vector` columns in `.data`, to be included in (or excluded from) the output.
#'
#'   for `as_binomial_contingency()`: further arguments passed to or from other methods.
#'
#' @param .level the confidence level required; default \var{0.95}.
#'
#' @param .drop_zero `logical`. If `TRUE`, [`levels`][base::levels] of explanatory factors for which values of
#'   `.dep_var` are either all zero or all one are dropped from the output; default `FALSE`.
#'
#' @param .propci `logical`. If `TRUE`, each row of the output `"binom_contingency"` object includes totals, proportions
#'   and confidence intervals; default `FALSE`.
#'
#' @param object a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]), to be coerced to
#'   a `"binom_contingency"` object.
#'
#' @param .pn,.qn `character vector`, names of columns in `object` representing numbers of successes and
#'   failures in Bernoulli trials; default `NULL`.
#'
#' @inheritParams contingency_table
#'
#' @return
#' An object of class `"binom_contingency"`, `"announce"`, inheriting from [`tibble`][tibble::tibble-package],
#'   with columns `pn` and `qn` representing the number of "successes" and "failures" respectively, and further columns
#'   for independent (explanatory) variables.
#'
#' If `.propci = TRUE` additional columns are output representing totals, proportions and confidence intervals.
#'
#' @keywords manip
#' @export
#' @examples
#' ## Bernoulli data with a single explanatory variable
#' (d <- bernoulli_data())
#'
#' d |> binom_contingency(dv)
#'
#' d |> binom_contingency(dv, .propci = TRUE)
#'
#' ## Use .data pronoun for more informative error messages
#' d |> binom_contingency(.data$dv)
#' 
#' try(d |> binom_contingency(dx))
#' 
#' try(d |> binom_contingency(.data$dx))
#'
#' ## NB this section is intended to be pasted in, rather than run by example()
#' \dontrun{
#'     oldopt <- options(warn = 0, nwarnings = 50)
#'
#'     ## Bernoulli data with identical responses for
#'     ##   the last level of the explanatory variable
#'     d <- bernoulli_data(probs = seq(0.4, 0, length.out = 5))
#'     d |> binom_contingency(dv)
#'
#'     ## Elicits mutiple warnings in glm.fit()
#'     ##   'fitted probabilities numerically 0 or 1 occurred'
#'     d |> binom_contingency(dv) |>
#'         glm(cbind(pn, qn) ~ iv, binomial, data = _) |>
#'         confint()
#'     summary(warnings())
#'
#'     ##  Argument .drop_zero = TRUE in binom_contingency()
#'     ##    prevents these warnings
#'     d |> binom_contingency(dv, .drop_zero = TRUE)
#'
#'     d |> binom_contingency(dv, .drop_zero = TRUE) |>
#'         glm(cbind(pn, qn) ~ iv, binomial, data = _) |>
#'         confint()
#'
#'     options(oldopt)
#' }
#'
#' ## Bernoulli data with multiple explanatory variables
#' (d <- list(
#'     iv2 = list(i = c("a", "c", "e", "g"), j = c("b", "d", "f", "h")),
#'     iv3 = list(k = c("a", "b", "c", "d"), l = c("e", "f", "g", "h")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |> add_grps(bernoulli_data(levels = 8), iv, .key = _))
#'
#' d |> binom_contingency(dv)
#'
#' d |> binom_contingency(dv, iv, iv3)
#'
#' d |> binom_contingency(dv, !c(iv2, iv4))
#'
#' d |> binom_contingency(dv, !!!expl_fcts(d))
#'
#' d |> binom_contingency(dv, .propci = TRUE)
#'
#' d |> binom_contingency(dv, .drop_zero = TRUE)
#'
#' d |>
#'    binom_contingency(dv, iv2, iv3, .drop_zero = TRUE) |>
#'    glm(cbind(pn, qn) ~ ., binomial, data = _) |>
#'    summary()
#'
#' d |>
#'    binom_contingency(dv, iv2, iv3, .drop_zero = TRUE) |>
#'    glm(cbind(pn, qn) ~ ., binomial, data = _) |>
#'    odds_ratio()
#'
#' ## Use {dplyr} selection helpers e.g., last_col(), num_range() and starts_with()
#' d |> binom_contingency(dv, last_col(1L))  ## Offset of 1L used, since last column of d is dv
#'
#' d |> binom_contingency(dv, !last_col(1L))
#'
#' d |> binom_contingency(dv, num_range("iv", 2:3))
#'
#' d |> binom_contingency(dv, !num_range("iv", 2:3))
#'
#' d |> binom_contingency(dv, starts_with("iv"))
#'
#' d |> binom_contingency(dv, !starts_with("iv")) ## Here, negation excludes all explanatory factors
#'
#' ## as_binom_contingency() 
#'
#' (d <- data.frame(
#'         iv = letters[1:5],
#'         s = c(34, 31, 16, 0, 10),
#'         f = c(32, 35, 50, 66, 56)
#'     ))
#'
#' as_binom_contingency(d, .pn = "s", .qn = "f")
#'
#' as_binom_contingency(d, .pn = "s", .qn = "f", .drop_zero = TRUE)
#'
#' (d <- binom_data())
#'
#' d |> as_binom_contingency()
#'
#' d |> as_binom_contingency(.propci = TRUE)
#'
#' rm(d)


binom_contingency <- function(.data, .dep_var, ..., .drop_zero = FALSE, .propci = FALSE, .level = 0.95) {
    .dep_var <- enquo(.dep_var)
    stopifnot(is.data.frame(.data), !quo_is_missing(.dep_var), eval_tidy(expr(all(!!.dep_var %in% 0:1)), .data))

    ctab <- contingency_table(.data = .data, .dep_var = !!.dep_var, ...) |>
        rename(pn = "1", qn = "0") |>
        relocate("qn", .after = "pn")

    if(.drop_zero)
        ctab <- drop_zero(ctab)
    if(.propci)
        ctab <- propci(ctab, .level)
    new_binom_contingency(ctab)
}


# ========================================
#  Constructor for a Binomial Contingency Table
#  new_binom_contingency()
#
#  Not exported

new_binom_contingency <- function(x = data.frame(pn = integer(), qn = integer()), ...) {
    stopifnot(inherits(x, "contingency_table"))
    structure(x, class = c("binom_contingency", class(x)), lead  = "Binomial Contingency Table", ...)
}


# ========================================
#  Drop all success (or failure) levels of explanatory factor
#  drop_zero()
#
#  Not exported

drop_zero <- function(ctab)
    ctab |>
        filter(as.logical(.data$pn), as.logical(.data$qn)) |>
        mutate(across(where(is.factor), fct_drop))


# ========================================
#  Totals, proportions and confidence intervals
#  propci()
#
#  Not exported

propci <- function(ctab, level)
    mutate(ctab,
        n = .data$pn + .data$qn,
        proptest = map2(.data$pn, n, \(x, n) prop.test(x, n, conf.level = level, correct = FALSE)),
        p = .data$proptest |> map_dbl("estimate"),
        lower = .data$proptest |> map_dbl(list("conf.int", 1)),
        upper = .data$proptest |> map_dbl(list("conf.int", 2)),
        across("proptest", ~ NULL)
    ) |>
    structure("conf.level" = level)


# ========================================
#  Convert to a Binomial Contingency Table
#  S3method as_binom_contingency()
#
#' @rdname binom_contingency
#' @export

as_binom_contingency <- function(object, ...)
    UseMethod("as_binom_contingency")

# ========================================
#  Convert to a Binomial Contingency Table for a Data Frame
#  S3method as_binom_contingency.data.frame()
#
#' @rdname binom_contingency
#' @export

# as_binom_contingency.data.frame <- function(
    # object,
    # ...,
    # .successes = NULL,
    # .failures = NULL,
    # .drop_zero = FALSE,
    # .propci = FALSE,
    # .level = 0.95
# ) {
    # check_dots_used()
    # stopifnot(inherits(object, "data.frame"))
    # stopifnot(all(c(.successes %||% "pn", .failures %||% "qn") %in% names(object)))
    # if (!length(eval_select(expr(chr_or_fct()), data = object)))
        # stop("\'", deparse(substitute(object)), "'\ must have at least one character vector or factor column.")
    # object <- rename(object, all_of(c(pn = .successes %||% "pn", qn = .failures %||% "qn")))
    # if(!all(is.integer(object[["pn"]]), is.integer(object[["qn"]]))) {
        # message("Coercing \"pn\" and/or \"qn\" to integer")
        # object <- mutate(object, across(all_of(c("pn", "qn")), as.integer))
    # }
    # if(.drop_zero)
        # object <- drop_zero(object)
    # if(.propci)
        # object <- propci(object, .level)
    # if (!inherits(object, "contingency_table"))
        # object <- new_contingency_table(object)
    # new_binom_contingency(object)
# }
as_binom_contingency.data.frame <- function(
    object,
    ...,
    .pn = NULL,
    .qn = NULL,
    .drop_zero = FALSE,
    .propci = FALSE,
    .level = 0.95
) {
    check_dots_used()
    .pn <- enquo(.pn)
    .qn <- enquo(.qn)

    if (!length(eval_select(expr(chr_or_fct()), data = object)))
        stop("`object` must have at least one character vector or factor column.")
    if (quo_is_null(.pn))
        .pn <- expr(pn)
    else
        if(tryCatch(
            error = function(cnd) {
                cat("Error:\n! Column `", as_name(.pn), "` not found in `object`\n", sep = "")
                TRUE
            }, {
	            names(object)[eval_select(.pn, object)] <- "pn"
                FALSE           	
            }
        )) return(invisible(NULL))
    if (quo_is_null(.qn))
        .qn <- expr(qn)
    else
        if(tryCatch(
            error = function(cnd) {
                cat("Error:\n! Column `", as_name(.qn), "` not found in `object`\n", sep = "")
                TRUE
            }, {
	            names(object)[eval_select(.qn, object)] <- "qn"
                FALSE           	
            }
        )) return(invisible(NULL))
    if(!all(is.integer(object$pn), is.integer(object$qn))) {
        message("Coercing `.pn` and/or `.qn` to integer")
        object <- mutate(object, across(all_of(c("pn", "qn")), as.integer))
    }
    if(.drop_zero)
        object <- drop_zero(object)
    if(.propci)
        object <- propci(object, .level)
    if (!inherits(object, "contingency_table"))
        object <- new_contingency_table(object)
    new_binom_contingency(object)
}


# ========================================
#' @title
#' Explanatory Factors in Data as List of Expressions
#'
#' @description
#' Create a list of defused expressions representing the names of all or a selection of explanatory factors or character
#' vectors in a dataset. 
#'
#' @details
#' By default, `expl_fcts()` creates a [`list`][base::list] of [`symbols`][base::symbol] i.e.,
#' [defused R expressions][rlang::topic-defuse], representing the names of all or a selection of explanatory factors (or
#' character vectors) in `.data`, using [`syms()`][rlang::syms] from package \pkg{\link[rlang]{rlang}}. Alternatively,
#' if `.val = "data_syms"`, a list of symbols prefixed with the \code{\link[rlang]{.data}} pronoun is returned instead.
#' Finally, if `.val = "character"`, `expl_fcts()` returns a `character vector` of the names of the explanatory factors
#' (or character vectors) in `.data`
#'
#' Variables in `.data` may be selected for inclusion or exclusion using the \code{\dots} argument and the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax from package \pkg{\link[dplyr]{dplyr}}, including use of
#' \dQuote{selection helpers}. If no \code{\dots} arguments are supplied, all categorical variables in `.data` will
#' be included in the list.
#' 
#' A list of `symbols` returned by `expl_fcts()` may be \dQuote{injected} into the \code{\dots} arguments of
#' [`contingency_table()`][contingency_table], [`xcontingency_table()`][xcontingency_table],
#' [`binom_contingency()`][binom_contingency] and other similar functions, using the
#' [splice-operator][rlang::splice-operator] `!!!`. If `.val = "character"`, the functions [`all_of()`][tidyselect::all_of]
#' or [`any_of()`][tidyselect::any_of] should be used to wrap the resulting `character vector` of names instead of using
#' `!!!`. A list of `symbols` returned by `expl_fcts()` may also be used to provide a list argument with injection
#' support to [`lapply()`][base::lapply] (or \pkg{\link[purrr]{purrr}} package [map()][purrr::map] functions), using the
#' [injection-operator][rlang::injection-operator] `!!` (see examples).
#' 
#' @param .named `logical`, whether to name the elements of the list. If `TRUE`, unnamed inputs are
#'   automatically named with [`set_names()`][rlang::set_names]; default `FALSE`.
#' 
#' @param .val the type of output required. The default `"syms"` returns a list of `symbols`; the alternative
#'   `"data_syms"` returns a list of `symbols` prefixed with the \code{\link[rlang]{.data}} pronoun. The `"character"`
#'   option returns a `character vector`.
#'
#' @inheritParams contingency_table
#'
#' @seealso [`!!`][rlang::injection-operator], [`!!!`][rlang::splice-operator], [`all_of`][tidyselect::all_of],
#'   [`any_of`][tidyselect::any_of], [`set_names()`][rlang::set_names], [`defused R expressions`][rlang::topic-defuse],
#'   [`map()`][purrr::map] and [`symbol`][base::symbol].
#' @family contingency_table
#'
#' @return A `list` of `symbols` representing the names of selected explanatory `factors` or `character vectors` in
#'   `.data`; unless `.val = "data_syms"`, in which case the `symbols` are prefixed with the \code{\link[rlang]{.data}}
#'   pronoun or `.val = "character"` whereupon the selected names are returned as a `character vector` instead.
#'
#' @keywords manip models
#' @export
#' @examples
#' (d <- list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |> add_grps(bernoulli_data(levels = 6), iv, .key = _))
#'
#' d |> expl_fcts()
#'
#' d |> expl_fcts(.named = TRUE)
#'
#' d |> expl_fcts(.val = "data_syms")
#'
#' d |> expl_fcts(.named = TRUE, .val = "data_syms")
#'
#' d |> expl_fcts(.val = "character")
#'
#' d |> expl_fcts(.named = TRUE, .val = "character")
#'
#' ## Select or exclude factors
#' d |> expl_fcts(iv, iv3)
#'
#' d |> expl_fcts(!c(iv, iv3))
#'
#' ## Use {dplyr} selection helpers e.g., last_col(), num_range() and starts_with()
#' d |> expl_fcts(last_col(1L))  ## Offset of 1L used, since last column of d is dv
#'
#' d |> expl_fcts(!last_col())
#'
#' d |> expl_fcts(num_range("iv", 2:3))
#'
#' d |> expl_fcts(!num_range("iv", 2:3))
#'
#' d |> expl_fcts(starts_with("iv"))
#'
#' ## Negation of selection helper excludes all explanatory factors
#' d |> expl_fcts(!starts_with("iv"))
#'
#' ## In following three examples, each triplet should give identical results
#' ## Include all explanatory factors
#' d |> binom_contingency(dv)
#'
#' d |> binom_contingency(dv, !!!expl_fcts(d))
#'
#' d |> binom_contingency(dv, all_of(expl_fcts(d, .val = "character")))
#'
#' ## Include only iv and iv3
#' d |> binom_contingency(dv, iv, iv3)
#'
#' d |> binom_contingency(dv, !!!expl_fcts(d, iv, iv3))
#'
#' d |> binom_contingency(dv, all_of(expl_fcts(d, iv, iv3, .val = "character")))
#'
#' ## Exclude iv and iv3
#' d |> binom_contingency(dv, !c(iv, iv3))
#'
#' d |> binom_contingency(dv, !!!expl_fcts(d, !c(iv, iv3)))
#'
#' d |> binom_contingency(dv, all_of(expl_fcts(d, !c(iv, iv3), .val = "character")))
#'
#' ## Use with lapply, binom_contingency(), glm() and odds_ratio()
#' expl_fcts(d, .named = TRUE) |>
#'     lapply(\(x) binom_contingency(d, dv, !!x))
#'
#' expl_fcts(d, .named = TRUE) |>
#'     lapply(\(x)
#'         binom_contingency(d, dv, !!x) |>
#'         glm(cbind(pn, qn) ~ ., binomial, data = _)
#'     )
#'
#' expl_fcts(d, .named = TRUE) |>
#'     lapply(\(x)
#'         binom_contingency(d, dv, !!x, .drop_zero = TRUE) |>
#'         odds_ratio(.ind_var = !!x)
#'     )
#'
#' rm(d)

expl_fcts <- function(.data, ..., .named = FALSE, .val = c("syms", "data_syms", "character")) {

    .val <- match.arg(.val)

    if (...length())
        pos <- eval_select(expr(c(...) & chr_or_fct()), data = .data)
    else 
        pos <- eval_select(expr(chr_or_fct()), data = .data)

    efs <- names(pos)

    if(.named)
        efs <- set_names(efs)

    switch(.val,
        syms = syms(efs),
        data_syms = data_syms(efs),
        character = efs,
    )
}

