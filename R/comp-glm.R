# ParaAnita R Package
# Mark Eisler Jan 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# comp-glm


# ========================================
#' @title
#' Compare Series of Nested GLMs
#'
#' @description
#' Compare a series of nested Bernoulli or binomial \acronym{GLMs} by supplying data, a dependent variable and
#' a list of terms representing the right-hand side of each of a series of model formulae.
#'
#' @details
#' \code{comp_glm()} builds the formulas from the dependent variable and the formula right-hand side, calls
#' [`glm`][stats::glm], and saves the resulting objects of class \code{"glm"} as a list column in a
#' [`tibble`][tibble::tibble-package], together with model fit information obtained using
#' [`glance.glm`][broom::glance.glm] from the \pkg{\link[broom]{broom}} package. The output may be ordered by a
#' selected column, otherwise by default in descending order of Akaike's Information Criterion (\acronym{AIC}).
#'
#' \code{comp_glm()} may be used conveniently to compare a series of related binomial \acronym{GLMs} based on different
#' groupings of factors added to \code{.data} using [`add_grps`][add_grps].
#'
#' @seealso [`add_grps`][add_grps], [`formula`][stats::formula], [`glance.glm`][broom::glance.glm] and
#'   [`glm`][stats::glm].
#' @family comp_glm
#'
#' @param .data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]). 
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of the binary dependent variable to be used
#'   as the \acronym{LHS} of the model formula; should be numeric with values of \var{0} and \var{1}, or a two-column
#'   matrix with the columns giving the numbers of successes and failures e.g., \code{cbind(pn, qn)}.
#'
#' @param \dots <[`dynamic-dots`][rlang::dyn-dots]> the \acronym{RHS} of any number of model formulae to be compared,
#'   based on independent variables in `.data`.
#'
#' @param .family a description of the error distribution and link function to be used in the model. Can be a character
#'   string naming a family function, a family function or the result of a call to a family function; default
#'   \code{binomial}.
#'
#' @param .arrange_by <[`data-masking`][rlang::args_data_masking]> quoted name of a column for ordering results. Use
#'   [`desc`][dplyr::desc] to sort by a variable in descending order; default \code{desc(AIC)}.
#'
#' @note It is the user's responsibility to check models are suitably nested to ensure meaningful comparisons.
#'
#' @return
#' An object of classes `"comp_glm"`, `"announce"`, inheriting from [`tibble`][tibble::tibble-package] and hence
#'   `"data.frame"`, with columns including the \acronym{RHS} of the model \code{formula}, the \code{glm} object and
#'   eight further goodness-of-fit measures output by \code{\link[broom]{glance.glm}}, as follows: -
#'
#' \item{f_rhs}{The right-hand side of the formula as supplied in the \dots argument.}
#'
#' \item{.glm}{A list-column containing the glm model objects.}
#'
#' \item{AIC}{Akaike's Information Criterion.}
#'
#' \item{BIC}{Bayesian Information Criterion.}
#'
#' \item{deviance}{Deviance of the model.}
#'
#' \item{df.null}{Degrees of freedom for the null model.}
#'
#' \item{df.residual}{Residual degrees of freedom.}
#'
#' \item{logLik}{The log-likelihood of the model.}
#'
#' \item{nobs}{Number of observations.}
#'
#' \item{null.deviance}{Deviance of the null model.}
#'
#' @keywords models regression
#' @export 
#' @examples
#' ## Simulate Bernoulli data
#'
#' (d <- list(
#'         iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'         iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'         iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#'     ) |> add_grps(bernoulli_data(levels = 6), iv, .key = _))
#'
#' ## Models arranged in descending order of AIC by default.
#' d |> comp_glm(dv, iv, iv2, iv3, iv4)
#'
#' ## Arrange models by formula right-hand side 
#' (comps <- d |> comp_glm(dv, iv, iv2, iv3, iv4, .arrange_by = f_rhs))
#'
#' ## Inspect components of .glm list-column
#' lapply(comps$.glm, formula)
#'
#' lapply(comps$.glm, summary)
#'
#' ## Convert to binomial data using binom_contingency()
#' (d <- d |> binom_contingency(dv))
#'
#' (comps <- d |> comp_glm(cbind(pn, qn), iv, iv2, iv3, iv4))
#'
#' ## Inspect components of .glm list-column
#' lapply(comps$.glm, formula)
#'
#' lapply(comps$.glm, summary)
#'
#' rm(comps, d)

comp_glm <- function(.data, .dep_var, ..., .family = binomial, .arrange_by = desc(AIC)) {
    .dep_var <- enexpr(.dep_var)
    .f_rhs_ls <- enexprs(..., .named = TRUE)
    
    if (is.character(.family)) 
        .family <- get(.family, mode = "function", envir = parent.frame())
    if (is.function(.family)) 
        .family <- .family()
    if (is.null(.family$family)) {
        print(.family)
        stop("'family' not recognized")
    }
    # NB - Putting this line directly in tibble() thows error: 'object 'x' not found'
    glms <- lapply(.f_rhs_ls, \(x) glm(inject(!!.dep_var ~ !!x), .family, .data))
    tibble(
        f_rhs = names(.f_rhs_ls), 
        .glm = glms
    ) |> bind_cols(lapply(glms, glance) |> list_rbind()) |>
    arrange({{.arrange_by}}) |>
    new_comp_glm()
}

# ========================================
#  Constructor for Compare Nested GLMs
#  new_comp_glm()
#
# Not exported

new_comp_glm <- function(x = data.frame(NULL), prt_str = "Compare Nested GLMs", ...) {
    stopifnot(is.data.frame(x))
    x <- announce(x, prt_str)
    structure(x, class = c("comp_glm", class(x)), ...)
}


# ========================================
#' @title
#' Analyses of Deviance Summarising Fits of Univariable GLMs
#' 
#' @description
#' `univ_anova()` provides a succinct summary of the analyses of deviance for all univariable GLMs based on each
#' possible categorical independent variable in a Bernoulli or binomial data set.
#'
#' @details
#' \code{univ_anova()} uses [`add1()`][stats::add1] in the \pkg{\link[stats]{stats}} package to compare univariable
#' \acronym{GLMs} for each possible categorical independent variable (i.e., `"factor"` columns) in `.data` with
#' the null model. Other data types e.g. `"character"` vectors will be ignored and should be converted to
#' `"factor"` to be included in this analysis.
#' 
#' @seealso  [`add1()`][stats::add1], [`anova()`][stats::anova], [`anova.glm()`][stats::anova.glm], and
#'   [`glm()`][stats::glm].
#' @family comp_glm
#' 
#' @param data a data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]).
#'
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name of the binary dependent variable, which
#'   should be \code{numeric} with values of \var{0} and \var{1}, or a two-column matrix with the columns giving the 
#'   numbers of successes and failures e.g., \code{cbind(pn, qn)}.
#'
#' @param .family a family function; default \code{"binomial"}. (See [`family`][stats::family] for details of family
#'   functions.)
#'
#' @param .test a character string, (partially) matching one of \code{"none", "Chisq", "LRT", "Rao", "F"} or \code{"Cp"};
#'   default \code{"none"}.
#' 
#' @return
#' An object of class `c("univ_anova" "announce")`, inheriting from `"anova"` and `"data.frame"` as for `add1()`, summarising
#'   the differences between the fitted univariable \acronym{GLMs} and the null model.
#' 
#' @keywords models regression
#' @export
#' @export
#' @examples
#' d <- list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |> add_grps(bernoulli_data(levels = 6), iv, .key = _) |> print()
#'
#' d |> univ_anova(dv, .test = "LRT")
#'
#' d |> binom_contingency(dv, starts_with("iv")) |> print() |>
#'      univ_anova(cbind(pn, qn), .test = "LRT")
#'
#' rm(d)

univ_anova <- function(data, .dep_var, .family = binomial, .test = c("none", "Rao", "LRT", "Chisq", "F")) {
    .test = match.arg(.test)
    .family = match.fun(.family)
    .dep_var <- enexpr(.dep_var)

    nullmodel <- glm(inject(!!.dep_var ~ 1), .family, data)

    add1(
        object = nullmodel,
        scope = names(data)[map_lgl(data, is.factor)] |>
            paste(collapse = " + ") |>
            (\(.x) paste("~. +", .x ))() |>
            formula(),
        test = .test
    ) |> 
    new_univ_anova()
}

# ========================================
#  Constructor for univ_anova
#  new_univ_anova()
#
# Not exported

new_univ_anova <- function(x = anova(NULL), ...) {
    stopifnot(isa(x, c("anova", "data.frame")))
    x <- announce(x, "Univariable Analysis of Deviance")
    structure(x, class = c("univ_anova", class(x)), ...)
}


# ========================================
#' @title
#' List of Summary and Analysis of Deviance Objects for Related Univariable GLMs
#'
#' @description
#' `summanov()` provides a list of the summary and analysis of deviance objects for a series of related univariable
#' \acronym{GLMs} of data with a binary dependent variable or a (two-column) dependent variable of binomial proportions.
#'
#' @details
#' Variables in \code{.data} to be included (or excluded)  as independent variables in the list of \acronym{GLM}
#' analyses may be selected using the \code{\dots} argument with the<[`tidy-select`][dplyr::dplyr_tidy_select]> syntax
#' of package \pkg{\link[dplyr]{dplyr}}, including use of \strong{selection helpers}.
#'
#' The structure of the output list may be changed from a list of pairs into pair of lists conveniently using
#' [`list_transpose`][purrr::list_transpose]. The univariable \acronym{GLMs} may then be easily compared and likewise
#' the univariable \acronym{GLM} anovas (analysis of deviance).
#'
#' [`univ_anova`][univ_anova] provides a succinct summmary of the univariable analyses of deviance for all potential
#' categorical independent variables in \code{data}. [`anova_tbl`][anova_tbl] also provides a succinct summmary from
#' the list of anovas.
#'
#' @seealso [`anova.glm()`][stats::anova.glm], [`list_transpose()`][purrr::list_transpose], [`glm()`][stats::glm] and
#'   [`summary.glm()`][stats::summary.glm]; [`Print_Methods`][Print_Methods] for S3 method for printing objects of class
#'   `"summ_anov"`.
#' @family comp_glm
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> quoted name(s) of one or more factors or character vectors
#'   in \code{.data}, to be included (or excluded)  as independent variables in the list of \acronym{GLM} analyses.
#'
#' @param .test a character string, (partially) matching one of \code{"Chisq", "LRT", "Rao", "F"} or \code{"Cp"};
#'   default \code{"Chisq"}.
#'
#' @inheritParams comp_glm
#'
#' @return
#' A [`list`][base::list] of `summ_anov` objects of length equal to the number of factors or character vectors selected
#'   using the \code{\dots} arguments. A `summ_anov` object is simply a list with class `"summ_anov"`, comprising the
#'   following two elements: -
#'
#' \item{summary}{Summary of the generalised linear model fit given by [`summary.glm`][stats::summary.glm].}
#'
#' \item{anova}{Analysis of deviance for the generalised linear model fit given by [`anova.glm`][stats::anova.glm].}
#'
#' @keywords models regression
#' @export
#' @examples
#' ## Simulate Bernoulli data
#' (d <- list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |> add_grps(bernoulli_data(levels = 6), iv, .key = _))
#'
#' ## Binary dependent variable
#' d |> summanov(dv, starts_with("iv"))
#' d |> summanov(dv, starts_with("iv") & !iv2)
#'
#' ## Binomial proportions
#' (d <- d |> binom_contingency(dv, starts_with("iv")))
#'
#' (uva <- d |> summanov(cbind(pn, qn), num_range("iv", 2:4)))
#'
#' ## Change list of pairs into a pair of lists using {purrr} list_transpose()
#' list_transpose(uva)$summary
#'
#' list_transpose(uva)$anova
#'
#' rm(d, uva)

summanov <- function(data, .dep_var, ..., .family = binomial, .test = "Chisq") {
    .dep_var <- enexpr(.dep_var)
    eval_select(expr(c(...)), data) |>
        names() |>
        setNames(nm = _) |>
        lapply(\(x)
            inject(!!.dep_var ~ !!sym(x)) |>
            glm(.family, data) |>
            new_summ_anov(test = .test)
        ) |>
        announce("GLM Summary and Analysis of Deviance")
}

# ========================================
#  Constructor for GLM Summary and Anova List
#  new_summ_anov()
#
# Not exported

new_summ_anov <- function(x = glm(0~NULL), test = "Chisq") {
    stopifnot(inherits(x, "glm"))
    structure(
        list(
            summary = announce(summary(x), "GLM Summary"),
            anova = announce(anova(x, test = test), "GLM Anova")
        ),
        class = c("summ_anov", "list"))
}

# ========================================
#' @title
#' Create Tibble from List of Anovas
#'
#' @description
#' Create a tibble from a list of anovas that compare a model to the null model or that compare two nested models.
#'
#' @details
#' The anovas to be compared must all be of the same type i.e., all must be analyses of a single model object or
#' all must be comparisons of two models. If any of the models has more than one independent variable, the results
#' may be difficult to interpret and a warning will be given.
#'
#' \code{anova_tbl()} can be used to be easily and conveniently compare a list of anovas obtained with
#' [`summanov`][summanov] and  [`list_transpose`][purrr::list_transpose], see examples.
#'
#' @seealso [`anova.glm()`][stats::anova.glm] [`list_transpose()`][purrr::list_transpose], and
#'   [`tibble`][tibble::tibble-package].
#' @family comp_glm
#'
#' @param anova_ls a \code{\link{list}} of \code{anova} objects.
#' 
#' @return
#' An object of classes `"anova_tbl"`, `"announce"`, inheriting from [`tibble`][tibble::tibble-package], showing the
#'   table entries for each anova, one per line.
#' 
#' @keywords models regression
#' @export
#' @examples
#' ## Following on from summanov() examples… 
#' ## Simulate Bernoulli data
#' (d <- list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |> add_grps(binom_data(levels = 6), iv, .key = _))
#' 
#' ## Create list of GLM anovas using summanov()
#' (alist <- d |> summanov(cbind(pn, qn), starts_with("iv")) |>
#'     list_transpose() |> _$anova)
#' 
#' ## Tibble from list of GLM anovas 
#' alist |> anova_tbl()
#' 
#' ## Add GLM anova with two independent variables
#' alist$iv3_iv4 <- glm(cbind(pn, qn) ~ iv3 + iv4, family = binomial, data = d) |>
#'     anova()
#' 
#' ## Tibble from list with multivariable anova invokes warning
#' try(alist |> anova_tbl())
#' 
#' rm(d, alist)

anova_tbl <- function(anova_ls) {
    stopifnot(
        is.list(anova_ls),
        all(map_lgl(anova_ls, isa, c("announce", "anova", "data.frame"))),
        all(map_lgl(anova_ls[-1], \(x) identical(row.names(anova_ls[[1]])[1], row.names(x)[1])))
    )

    badnames <- lapply(anova_ls, nrow) > 2
    if (any(badnames))
        warning(
            paste(names(badnames)[badnames], collapse = ", "),
            if (length(badnames[badnames]) > 1) " have" else " has",
            " > 1 independent variable, interpret with caution"
        )

    duplex <- !identical(row.names(anova_ls[[1]])[1], "NULL")

    an_tbl <- anova_ls |> map(\(x)
        x[1, !(is.na(x[1,]))] |>
        as_tibble() |>
        rename_with(\(y) gsub("Resid.", if(duplex) "U+00A7" else "Null", y, fixed = TRUE)) |>
        bind_cols(x[2, ] |> as_tibble())  |>
        rename_with(\(y) gsub(if(duplex) "Resid." else "U+00A7", "M2_Resid.", y, fixed = TRUE)) |>
        rename_with(~ gsub("U+00A7", "M1_Resid.", .x, fixed = TRUE))) |>
    list_rbind(names_to = "Name")

    if ("Pr(>Chi)" %in% names(an_tbl))
        an_tbl <- mutate(an_tbl, sig = starsig(.data$`Pr(>Chi)`))

    new_anova_tbl(an_tbl, paste("Comparing", if (duplex) "two models" else "to null model"))
}

# ========================================
#  Constructor for an Analysis of Deviance Table
#  new_anova_tbl()
#
# Not exported

new_anova_tbl <- function(x = data.frame(NULL), subhead = "") {
    stopifnot(is.data.frame(x))
    prt_str <- paste0("Analysis of Deviance (", subhead, ")")
    x <- announce(x, prt_str)
    structure(x, class = c("anova_tbl", class(x)))
}

