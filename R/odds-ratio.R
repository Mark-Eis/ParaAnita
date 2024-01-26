# ParaAnita R Package
# Mark Eisler Jan 2024
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# odds-ratio.R


# ========================================
#' @title
#' Odds Ratios, Standard Errors, Confidence Intervals and P-Values for Binomial GLMs
#'
#' @description
#' `odds_ratio()` calculates odds ratios and their profiled confidence intervals for \acronym{GLMs} and outputs these
#' together with the estimates of the regression coefficients, their standard errors and probabilities.
#'
#' @details
#' `odds_ratio()` is a generic function used to calculate odds ratios and their profiled confidence intervals for
#' univariable \acronym{GLMs} with a single categorical independent variable, or for multivariable \acronym{GLMs}, and
#' output these together with the estimates of the regression coefficients, their standard errors and probabilities.
#' The function invokes particular [`methods`][utils:: methods] which depend on the [`class`][base::class] of the first
#' argument.
#'
#' The S3 method for objects of class `"glm"` returned by [`glm()`][stats::glm] can be used with unvariable
#' \acronym{GLMs}, or with multivariable \acronym{GLMs} to calculate "adjusted" odds ratios. Optionally, if
#' `print_call = TRUE` the original call to [`glm()`][stats::glm] may be retrieved from the `"glm"` object supplied
#' as an argument and printed.
#'
#' Currently, the S3 methods for classes `"data.frame"` and `"binom_contingency"` can only be used with univariable
#' \acronym{GLMs}.
#'
#' If `.print_contr = TRUE` and any `factor` independent variables have a contrast [`attribute`][base::attributes] set,
#' the [`contrast matrix`][stats::contr.helmert] will be printed. Contrasts may be set conveniently for `factors` in
#' `data` using [`set_contrasts()`][set_contrasts], see examples.
#'
#' Confidence intervals for odds ratios are based on profile likelihood and calculated using
#' [`confint.glm()`][MASS::confint.glm]. The confidence level may be adjusted using `.level`, otherwise the default
#' value of \var{0.95} is used.
#'
#' @seealso [`binom_contingency()`][binom_contingency], [`contrast matrix`][stats::contr.helmert],
#'   [`contrasts()`][stats::contrasts], [`glm()`][stats::glm], [`set_contrasts()`][set_contrasts],
#'   [`summary.glm()`][stats::summary.glm]; [`Print_Methods`][Print_Methods] and [`print_all()`][print_all] for S3
#'   methods for printing objects of class `"odds_ratio"`.
#' @family odds-ratio
#'
#' @param object an object from which the odds ratios are to be calculated, which may be a [`binom_contingency`] table,
#'   a [`data frame`][base::data.frame] (or a data frame extension e.g., a [`tibble`][tibble::tibble-package]), or a
#'   [`glm`][stats::glm].
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @param .ind_var <[`data-masking`][rlang::args_data_masking]> quoted name of an independent variable, which may be
#'   either a character vector or factor.
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name(s) of the response variable(s) in the data
#'   representing the number of successes and failures respectively, see [`glm()`][stats::glm]; default
#'   `cbind(pn, qn)`.
#'
#' @param .level the confidence level required; default \var{0.95}.
#'
#' @param .print_call `logical`, whether or not to print the call for the \acronym{GLM}.
#'
#' @param .stat `logical`, whether or not to print `z` or `t` statistic for the \acronym{GLM};
#'   default `FALSE`.
#'
#' @param .print_contr `logical`. If `TRUE`, and `.ind_var` has a contrast attribute set, the contrast
#'   matrix will be printed; default `FALSE`.
#'
#' @return An object of classes `"odds_ratio"`, `"announce"`, inheriting from [`tibble`][tibble::tibble-package],
#'   and containing the following columns: -
#'
#' \item{parameter}{The names of the model parameters.}
#'
#' \item{estimate}{The estimate of the regression coefficient.}
#'
#' \item{se}{The standard error of the estimate.}
#'
#' \item{z (or t) value}{Optionally, the value of the z (or t) statistic for the estimate.}
#'
#' \item{p_val}{The p-value for the estimate.}
#'
#' \item{odds_ratio}{The odds ratio.}
#'
#' \item{ci}{The lower and upper confidence intervals for the odds ratio, by default at the 2.5% and 97.5% levels.}
#'
#' \item{sig}{Stars for statistical significance.}
#'
#' @keywords models regression
#' @export
#' @examples
#' ## Create simulated Bernoulli data
#' (d <- bernoulli_data())
#'
#' ## Invoking the S3 method for class "data.frame" and using the default
#' ## contrasts from options("contrasts")
#' ## — contrasts not printed 
#' d |> odds_ratio(.dep_var = dv, .ind_var = iv)
#'
#' ## Using the default contrasts from options("contrasts")
#' ## — adjust confidence level, contrasts are printed 
#' d |> odds_ratio(.dep_var = dv, .ind_var = iv, .level = 0.99, .print_contr = TRUE)
#'
#' ## Specifying treatment contrasts, with last level as base
#' ## — contrasts not printed
#' d |> set_contrasts(iv, base =  99L, contr = contr.treatment) |>
#'     odds_ratio(.dep_var = dv, .ind_var = iv)
#'
#' ## Specifying treatment contrasts, with last level as base
#' ## — contrasts printed
#' d |> set_contrasts(iv, base =  99L, contr = contr.treatment) |>
#'     odds_ratio(.dep_var = dv, .ind_var = iv, .print_contr = TRUE)
#'
#' ## Helmert contrasts specified
#' ## — contrasts printed
#' d |> set_contrasts(iv, contr = contr.helmert) |>
#'     odds_ratio(.dep_var = dv, .ind_var = iv, .print_contr = TRUE)
#'
#' # Set default unordered contrasts in options("contrasts") to Helmert
#' options("contrasts" =  c(unordered = "contr.helmert", ordered = "contr.poly"))
#' getOption("contrasts")
#'
#' ## Using the default, unordered Helmert contrasts
#' ## — contrasts printed
#' d |> odds_ratio(.dep_var = dv, .ind_var = iv, .print_contr = TRUE)
#'
#' ## Specify treatment contrasts
#' ## — contrasts printed
#' d |> set_contrasts(iv, contr = contr.treatment) |>
#'     odds_ratio(.dep_var = dv, .ind_var = iv, .print_contr = TRUE)
#'
#' # Restore default contrasts in options("contrasts")
#' options("contrasts" =  c(unordered = "contr.treatment", ordered = "contr.poly"))
#' options("contrasts")
#'
#' ## Invoking the S3 method for class "binom_contingency" 
#' d |> binom_contingency(dv, iv) |> odds_ratio(.ind_var = iv)
#' 
#' ## Create multivariable glm object and specify treatment contrasts
#' (d <- list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f"))
#' ) |> add_grps(binom_data(levels = 6), iv, .key = _))
#'
#' set_contr_treat(d, num_range("iv", 2:3)) <- c(1L, 2L)
#' get_contr_data(d)
#'
#' glm1 <- glm(cbind(pn, qn) ~ iv2 + iv3, family = binomial, data = d)
#'
#' glm1 |> summary()
#'
#' ## Invoking the S3 method for class "glm"
#' glm1 |> odds_ratio()
#'
#' glm1 |> odds_ratio(.print_call = FALSE, .stat = TRUE, .print_contr = TRUE)
#'
#' ## Compare S3 method for class "glm" to that for "data.frame"
#' ## — only possible for univariable analyses
#' (d <- binom_data())
#'
#' glm(cbind(pn, qn) ~ iv, family = binomial, data = d) |>
#'     odds_ratio()
#'
#' d |> odds_ratio(.ind_var = iv)  ## Warning about missing .dep_var
#'
#' ## Helmert contrasts given more easily readable names
#' d |> set_contrasts(iv) <- contr.helmert
#' helm_names(d$iv) <- c(":", "v")
#' d |> get_contrasts(iv)
#'
#' ## Add separator as last little tweak ;-)
#' contr_colpfx(d$iv) <- ": "
#'
#' glm(cbind(pn, qn) ~ iv, family = binomial, data = d) |>
#'     odds_ratio()
#'
#' d |> odds_ratio(.dep_var = cbind(pn, qn), .ind_var = iv)
#'
#' ## Printing lengthier output with print_all()
#' binom_data(26, 100) |>
#'     odds_ratio(.dep_var = cbind(pn, qn), .ind_var = iv, .print_contr = TRUE) |>
#'     print_all()
#'
#' rm(d, glm1)

odds_ratio <- function(object, ...)
    UseMethod("odds_ratio")

# ========================================
#  Odds Ratios and Confidence Intervals for a Binomial Contingency Table
#  S3method odds_ratio.binom_contingency()
#
#' @rdname odds_ratio
#' @export

odds_ratio.binom_contingency <- function(object, ..., .ind_var, .level = 0.95, .print_call = FALSE, .stat = FALSE,
    .print_contr = FALSE) {

    check_dots_empty()
    .ind_var <- enexpr(.ind_var)

    NextMethod(.dep_var = quote(cbind(pn, qn)))
}

# ========================================
# Odds Ratios And Confidence Intervals for a Data Frame
#
#' @rdname odds_ratio
#' @export

odds_ratio.data.frame <- function(object, ..., .dep_var, .ind_var, .level = 0.95, .print_call = FALSE,
    .stat = FALSE, .print_contr = FALSE) {

    check_dots_empty()

    if (!inherits(object, "binom_contingency")) {
        .ind_var <- enexpr(.ind_var)
        if(missing(.dep_var)) {
            warning("Missing '.dep_var' set to default: cbind(pn, qn)")
            .dep_var <- quote(cbind(pn, qn))
        } else
            .dep_var <- enexpr(.dep_var)
    }

    if (expr(!any(is.factor(!!.ind_var), is.character(!!.ind_var))) |> eval_tidy(data = object))
        stop("\targument .ind_var = ", as_name(.ind_var), " not of type factor or character vector")
    if (any(!is.numeric(.level), .level < 0, .level >= 1))
        stop("\n\targument \".level\" must be positive numeric less than 1")

    glm(inject(!!.dep_var ~ !!.ind_var), family = "binomial", data = object) |>
    odds_ratio(.level = .level, .print_call = .print_call, .stat = .stat, .print_contr = .print_contr)           
}

# ========================================
#  Default Method for Odds Ratios and Confidence Intervals for a GLM
#  S3method odds_ratio.default()
#
#' @rdname odds_ratio
#' @export

odds_ratio.glm <- function(object, ..., .level = 0.95, .print_call = TRUE, .stat = FALSE, .print_contr = FALSE) {

    .glm <- object
    stopifnot(family(.glm)$family %in% c("binomial", "quasibinomial", "poisson"))

    if (.print_call) cat("\nCall:  ", paste(deparse(.glm$call), sep = "\n", collapse = "\n"), 
        "\n\n", sep = "")

    object <- coef(summary(.glm)) |>
        as.data.frame() |>
        rownames_to_column(var = "parameter") |>
        as_tibble() |>
        rename(c(estimate = last_col(3), se = last_col(2), p_val = last_col())) |>
        mutate(
            across(last_col(1), \(x) if (.stat) x else NULL),
            odds_ratio = c(0, coef(.glm)[-1]) |> exp(),
            ci = rbind(c(NA, NA), confint(.glm, level = .level)[-1,]) |> exp(),
            sig = starsig(.data$p_val),
            across("estimate":"odds_ratio", zapsmall)
        )
    new_odds_ratio(object, ..., .glm = .glm, .print_contr = .print_contr)
}

new_odds_ratio <- function(object = data.frame("x"), ..., .glm = glm(0 ~ NULL), .print_contr = FALSE) {
    object <- announce(object, "Estimates and Odds Ratios")
    structure(object, class = c("odds_ratio", class(object)), ..., glm = .glm, print_contr = .print_contr)
}

