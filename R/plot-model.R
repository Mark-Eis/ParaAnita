# ParaAnita R Package
# Mark Eisler Dec 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# plot-model


# ========================================
#' @title
#' Collate Data for Plotting Univariable GLM Predictions with Error Bars
#'
#' @description
#' `glm_plotdata()` outputs data collated suitably for easy creation of standardised plots with error bars representing
#' confidence intervals or standard errors, based on predictions from univariable general linear models (\acronym{GLM}s).
#'
#' @details
#' This function works with univariable \acronym{GLM}s of Bernoulli or binomial data having a dependent variable
#' expressed as numbers of successes and failures and a single independent variable with multiple levels and its output
#' may be plotted conveniently using the [`ggplot()`][ggplot2::ggplot] S3 method
#' [`ggplot.glm_plotdata()`][ggplot.glm_plotdata].
#'
#' `glm_plotdata()` allows exploration of proposed groupings of the levels of the independent variable, such as
#' obtained using [`add_grps()`][add_grps] or [`fct_collapse()`][forcats::fct_collapse], and will include both the
#' grouped and ungrouped levels in its output. In such cases, `.ind_var` should contain the groupings and the `.ungroup`
#' argument should name a column in `object`'s data containing the ungrouped levels, see examples. The grouped levels are
#' used as the independent variable in the \acronym{GLM} and are shown within the output object in the column `grouped`
#' while the ungrouped levels are shown in the column `level`. If the `.ungroup` is `NULL` (the default), levels of
#' `.ind_var` will appear in the column `level` and the `grouped` column in the output will contain [`NA`][base::NA].
#'
#' If `conf_level` is a value such as \var{0.95} (the default) or a similar value, `lower` and `upper`
#' values in the output delimit the prediction confidence intervals at that confidence level. If `conf_level` is
#' [`NA`][base::NA], then the `lower` and `upper` are the model predictions ±standard error.
#'
#' If `type = "link"`, then the linear predictors and their confidence intervals or ±standard errors are obtained.
#' If `type = "response"`, then the linear predictors and their confidence intervals or ±standard errors are
#' transformed back to the response scale using the link inverse function.
#'
#' @note Confidence intervals are calculated from the standard errors of the parameter estimates using the quantiles of
#'   the t distribution with \emph{n - 1} degrees of freedom, at the probability given by `conf_level`. These
#'   confidence intervals are generally more conservative i.e., a little wider, than those obtained by "profiling"
#'   (e.g., using [`confint.glm`][MASS::confint.glm]). If the `conf_level` argument is [`NA`][base::NA], standard
#'   error is shown rather than a confidence interval.
#'
#' @seealso  [`add_grps`][add_grps], [`binom_contingency`][binom_contingency], [`glm`][stats::glm],
#'   [`imap`][purrr::imap] and  [`tibble`][tibble::tibble-package].
#' @family plot_model
#'
#' @param object an object from which the data for plotting univariable GLM predictions are to be collated; may be a 
#'   [`binomial contingency table`][binom_contingency], a [`data frame`][base::data.frame] (or a data frame extension
#'   e.g., a [`tibble`][tibble::tibble-package]), or a [`glm`][stats::glm] as used by the default S3 method.
#'
#' @param \dots further arguments passed to or from other methods.
#' 
#' @param .dep_var <[`data-masking`][rlang::args_data_masking]> quoted name(s) of the response variable(s) in the data
#'   representing the number of successes and failures respectively, see [`glm()`][stats::glm]; default
#'   `cbind(pn, qn)`.
#'
#' @param .ind_var <[`data-masking`][rlang::args_data_masking]> quoted name of the independent variable.
#'
#' @param .ungroup <[`data-masking`][rlang::args_data_masking]> quoted name of the column containing the ungrouped levels
#'   of `.ind_var`, see details; default `NULL`.
#'
#' @param conf_level the confidence level required for the error bars; default \var{0.95}. If `NA`, error bars are
#'   standard error.
#'
#' @param type the type of prediction required. The default is on the scale of the linear predictors;
#'   the alternative `"response"` is on the scale of the response variable; default `"link"`.
#'
#' @return An object of class `"glm_plotdata"`, `"announce"`, inheriting from [`tibble`][tibble::tibble-package],
#'   with values on the linear predictor or response scale (depending on `type`) in columns as follows: -
#'
#' \item{level}{Level of the independent variable.}
#'
#' \item{grouped}{Grouped levels of the independent variable.}
#'
#' \item{n}{Number of observations.}
#'
#' \item{obs}{Observed values.}
#'
#' \item{pred}{Values predicted by the model.}
#'
#' \item{lower}{Lower extent of error bar.}
#'
#' \item{upper}{Upper extent of error bar.}
#'
#' It also has attributes `"conf_level"`, signifying the confidence level, `"subtitle"`, by default the name of the
#'  independant variable, and `"type"` (see argument `type`).  
#'
#' @keywords dplot
#' @export
#' @examples
#' (d <- binom_data())
#'
#' ## Ungrouped data
#' ## 95% Confidence interval by default
#' d |> glm_plotdata(.ind_var = iv)                     ## Linear predictor scale
#' d |> glm_plotdata(.ind_var = iv, type = "response")  ## Response scale
#'
#' ## Standard error
#' d |> glm_plotdata(.ind_var = iv, conf_level = NA)                     ## Linear predictor scale
#' d |> glm_plotdata(.ind_var = iv, conf_level = NA, type = "response")  ## Response scale
#'
#' ## Grouped data
#' (d <- list(iv2 = list(ab = c("a", "b"), cd = c("c", "d"))) |>
#'     add_grps(d, iv, .key = _))
#'
#' ## 95% Confidence interval by default
#' d |> glm_plotdata(.ind_var = iv2, .ungroup = iv)                     ## Linear predictor scale
#' d |> glm_plotdata(.ind_var = iv2, .ungroup = iv, type = "response")  ## Response scale
#'
#' ## Standard error on linear predictor scale
#' d |> glm_plotdata(.ind_var = iv2, .ungroup = iv, conf_level = NA)
#'
#' ## Standard error on response scale
#' d |> glm_plotdata(.ind_var = iv2, .ungroup = iv, conf_level = NA, type = "response")
#'
#' rm(d)

glm_plotdata <- function(object, ...)
    UseMethod("glm_plotdata")

# ========================================
#  Format Data for Plotting Univariable GLM Predictions with Error Bars for a Binomial Contingency Table
#  S3method glm_plotdata.binom_contingency()
#
#' @rdname glm_plotdata
#' @export

glm_plotdata.binom_contingency <- function(object, ..., .ind_var, .ungroup = NULL, conf_level = 0.95,
    type = c("link", "response")) {

    check_dots_empty()
    .ind_var <- enquo(.ind_var)
    .ungroup <- enquo(.ungroup)

    NextMethod()
}


# ========================================
#  Format Data for Plotting Univariable GLM Predictions with Error Bars for a Data Frame
#  S3method p glm_plotdata.data.frame()
#
#' @rdname glm_plotdata
#' @export

glm_plotdata.data.frame <- function(object, ..., .dep_var, .ind_var, .ungroup = NULL, conf_level = 0.95,
    type = c("link", "response")) {

    if(missing(.dep_var)) {
        pn <- qn <- NULL 
        .dep_var <- expr(cbind(pn, qn))
    } else
        .dep_var <- enquo(.dep_var)
    if (!inherits(object, "binom_contingency")) {
        .ind_var <- enquo(.ind_var)
       .ungroup <- enquo(.ungroup)
    }

    if (expr(!any(is.factor(!!.ind_var), is.character(!!.ind_var))) |> eval_tidy(data = object))
        stop("\targument .ind_var = ", as_name(.ind_var), " not of type factor or character vector")
    if (!is.na(conf_level) && any(!is.numeric(conf_level), conf_level < 0, conf_level >= 1))
        stop("\n\targument \"conf_level\" must be positive numeric less than 1")

    object <- new_formula(get_expr(.dep_var), get_expr(.ind_var), env = rlang::get_env(.ind_var)) |>
        glm(family = "binomial", data = object) |>
        structure(ungroup = .ungroup)

    NextMethod()      
}


# ========================================
#  Default Method for Format Data for Plotting Univariable GLM Predictions with Error Bars
#  S3method glm_plotdata.default()
#
#' @rdname glm_plotdata
#' @export

glm_plotdata.default <- function(object, ..., conf_level = 0.95, type = c("link", "response")) {

    type <- match.arg(type)
    stopifnot(
	    inherits(object, c("glm", "lm")),
	    	family(object)$family %in% c("binomial", "quasibinomial", "poisson")
    )
    if (length(formula(object)[[3]]) > 1)
	     stop("glm_plotdata() works only for univariable models: \"object\" has > 1 term.")
	
	dep_var <- object$formula[[2]]
	ind_var <- object$formula[[3]]
    ungrouped <- object %@% "ungroup"

    dispersion <- summary(object)$dispersion
    residual.scale <- as.vector(sqrt(dispersion))
    pred <- # linear predictors and se from lm, same as predict.glm()
        predict.lm(object, se.fit = TRUE, scale = residual.scale, type = "response")
    fit <- pred$fit
    ebar <- pred$se.fit
    if (!is.na(conf_level)) {
        .df <- sum(weights(object)) - 1
        ebar <- ebar * qt((1 + conf_level)/2, .df)
    }
    fit.lower <- pred$fit - ebar
    fit.upper <- pred$fit + ebar

    if (type == "response") {
        .linkinv = family(object)$linkinv
        fit <- .linkinv(fit)
        fit.lower <- .linkinv(fit.lower)
        fit.upper <- .linkinv(fit.upper)
    }

    pdta <- object$data |>
        mutate(
            level = !!ind_var,
            ungrouped = !!ungrouped,
            n = as.integer(!!dep_var %*% c(1, 1)),
            obs = as.numeric(
                if (type == "response")
                    !!dep_var %*% c(1, 0) / n
                else
                    log(!!dep_var %*% c(1, 0) / !!dep_var %*% c(0, 1))
            ),
            pred = fit,
            lower = fit.lower,
            upper = fit.upper,
            across("pred":"upper", unname),
            across("obs":"upper", zapsmall),
            .keep = "none"
        ) 

    {
        if ("ungrouped" %in% (pdta |> names()))
            pdta |>
            rename(tmp = "level") |>
            rename(level = ungrouped, grouped = "tmp")
        else
           pdta |> mutate(grouped = as.factor(NA))
    } |> relocate("grouped", .after = "level") |>
    new_glm_plotdata(conf_level = conf_level, subtitle = as_label(ind_var), type = type)
}


# ========================================
#  Constructor for glm_plotdata
#  new_glm_plotdata()
#
# Not exported

new_glm_plotdata <- function(x = data.frame(NULL), ..., conf_level = 0.95, subtitle = NULL,
    type = c("link", "response")) {

    stopifnot(inherits(x, c("tbl_df", "tbl", "data.frame")))
    x <- announce(x, "GLM Plot Data")
    structure(x, class = c("glm_plotdata", class(x)), ..., conf_level = conf_level, subtitle = subtitle, type = type)
}


# ========================================
#' @title
#' Data for Plotting Univariable GLM Predictions and Error Bars for Multiple Independent Variables
#'
#' @description
#' `glm_plotlist()` formats data for plotting univariable \acronym{GLM} predictions with error bars
#' for each of a number of independent variables.
#'
#' @details
#' `glm_plotlist()` invokes [`binom_contingency()`][binom_contingency] and [`glm_plotdata()`][glm_plotdata] to create
#' a `list` of `"glm_plotdata"` objects for plotting univariable \acronym{GLM} predictions with error bars for each
#' of a number of independent variables in `data`. Independent variables to be included are selected using the
#' \code{\dots} argument with the <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{\link[dplyr]{dplyr}},
#' including use of \dQuote{selection helpers}.
#'
#' Like [`glm_plotdata()`][glm_plotdata], `glm_plotlist()` allows exploration of proposed groupings of levels of
#' independent variables (e.g. as obtained using [`add_grps()`][add_grps] or [`fct_collapse()`][forcats::fct_collapse])
#' and inclusion of both grouped and ungrouped levels in the `"glm_plotdata"` objects comprising its output list. In
#' such cases, the `.ungroups` argument is used to provide a named `character vector` of the names of the corresponding
#' factors in `data` giving the grouped and ungrouped levels of the form `ungrouped_name = "grouped_name"`; levels not
#' otherwise mentioned will be left as is.
#'
#' The grouped levels are used as the independent variable in the \acronym{GLM} invoked by `glm_plotdata()` and are
#' output in the column `grouped` within the corresponding `"glm_plotdata"` object, while the ungrouped levels are shown
#' in the column `level`, see [`glm_plotdata()`][glm_plotdata].
#'
#' [`glm_plotlist()`][glm_plotlist] may be used in conjunction with package \pkg{\link[purrr]{purrr}}
#' [`map()`][purrr::map] to rapidly obtain multiple plots of univariable \acronym{GLM}s for a number of independent
#' variables.
#'
#' Levels of independent variables for which the observed values are all zero or all one are not included in the output,
#' although they are taken into consideration in calculating denominators in the case of grouped levels.
#'
#' @seealso [`add_grps`][add_grps], [`bind_rows`][dplyr::bind_rows] and [`binom_contingency`][binom_contingency]
#'   and [`fct_collapse`][forcats::fct_collapse].
#' @family plot_model
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> independent variables to be included in the plot data.
#'
#' @param .ungroups a named character vector of ungrouped levels of independent variables specified in `.ind_var`,
#'   see details; default `NULL`.
#'
#' @param .facet_by `NULL`, the default; or, if the output is to be combined into a single object to be used for a
#'   faceted plot, a `character vector` of length one used to name an additional column containing the names of the
#'   independant variables.
#'
#' @inheritParams glm_plotdata
#' @inheritParams binom_contingency
#'
#' @return
#' If the argument `.facet_by` is `NULL`, a `list` of `"glm_plotdata"` objects suitable for producing multiple plots
#'   using [`ggplot()`][ggplot2::ggplot]. Otherwise, a single `"glm_plotdata"` object with an additional column taking
#'   its name from `.facet_by` and containing the names of the independent variables.
#'
#' @keywords dplot
#' @export
#' @examples
#' # Coming soon!
#'

glm_plotlist <- function(data, .dep_var, ..., .ungroups = NULL, .conf_level = 0.95, .facet_by = NULL) {

    .dep_var = enquo(.dep_var)
    pos <- eval_select(expr(c(...)), data)
    if (any(!.ungroups %in% names(pos)))
        stop(".ungroups \"", paste0(.ungroups[!.ungroups %in% names(pos)], collapse = "\", \""), "\" not found in ...!")
    
    plist <- setNames(nm = names(pos)) |>
    replace(.ungroups, names(.ungroups)) |>
    imap(\(ugp, level) binom_contingency(.data = data, .dep_var = !!.dep_var, all_of(c(level, ugp))) |>
        glm_plotdata(
            .ind_var = !!sym(level),
            .ungroup = !!(if(identical(ugp, level)) expr(NULL) else sym(ugp)),
            conf_level = .conf_level,
            type = "response"
        ) |>
        filter(.data$obs > 0, .data$obs < 1)
    )
    if (!is.null(.facet_by)){
        plist <- bind_rows(plist, .id = .facet_by)
        plist %@% "facet_by" = .facet_by
        plist %@% "subtitle" = NULL
    }
    plist
}


# ========================================
#' @title
#' Format or Lookup Variable Names for Plot Titles
#'
#' @description
#' Vectorised labeller function used by `plot_model` for revising variable names for use as subtitles in
#' individual plots or as facet labels in faceted plots.
#'
#' @details
#' `var_labs` in package \pkg{\link[ParaAnita]{ParaAnita}} simply applies [`str_to_title`][stringr::str_to_title]
#' to its argument. The user may override this by providing their own vectorised [`labeller`][ggplot2::labeller]
#' function, see `labeller` under [`facet_wrap`][ggplot2::facet_wrap] and the example.
#'
#' @seealso [`as_labeller`][ggplot2::as_labeller], [`facet_wrap`][ggplot2::facet_wrap], [`labeller`][ggplot2::labeller].
#' @family plot_model
#'
#' @param labels character vector containing the names of the variables to be revised.
#'
#' @return A character vector containing the revised names.
#'
#' @export
#' @examples
#'
#' ## Default labeller
#' c("matthew", "mark", "luke", "john") |> var_labs()
#'
#' ygps <- c("year", "ygroup1", "ygroup2", "ygroup3", "ygroup4", "ygroup5", "ygroup6", "ygroup7")
#' mgps <- c("month", "season", "mgroup2", "mgroup3", "mgroup4", "mgroup5", "mgroup6")
#' demog <- c("gender", "age_group", "location", "breed")
#'
#' # Vectorised function to replace terse variable names with names suitable for labelling plots
#' var_labs <- as_labeller(
#'     c(
#'         c("Year", paste("Year Group", seq_along(ygps[-1]))) |> set_names(ygps),
#'         c("Month", "Season", paste("Month Group", seq_along(mgps[-1])[-1])) |> set_names(mgps),
#'         c("Animal Gender", "Age Group", "Geographic Location", "Cattle Breed") |> set_names(demog)
#'     )
#' )
#'
#' ygps |> var_labs()
#' mgps |> var_labs()
#' demog |> var_labs()
#'
#' rm(demog, mgps, var_labs, ygps)
#'
var_labs <- ggplot2::as_labeller(stringr::str_to_title)

# ========================================
#' @title
#' Plot Model Predictions with Error Bars for Univariable GLM
#'
#' @name Plot_Model
#' 
#' @description
#' S3 method to enable [`ggplot()`][ggplot2::ggplot] in package \pkg{\link[ggplot2]{ggplot2}} to plot `"glm_plotdata"`
#' objects ouptut by [`glm_plotdata()`][glm_plotdata].
#'
#' @details
#' This S3 method plots model predictions and error bars representing confidence intervals or standard errors for a 
#' univariable glm with a categorical independent variable, optionally allowing representation of groupings of levels of
#' the independent variable and faceting of a number of such plots.
#'
#' `ggplot.glm_plotdata()` recognises a `factor` or character column in `data` named `grouped` for plotting grouped
#' levels of an independent variable that are grouped within the underlying model. If levels are indeed grouped in the
#' model, the data bars will be plotted with colour-coded borders representing the groups, and the ungrouped observed
#' values contained in the `data` column `level` are plotted as symbols. If ungrouped levels are to be plotted, the
#' `grouped` column should only contain [`NA`][base::NA] values 
#'
#' A `character` column in `data` containing names of independent variables to be used for faceting may be identified by
#' setting an attribute `"facet_by"` in `data`. Names of variables to be used for faceting may be converted to more
#' informative facet labels for using a vectorised [`labeller()`][ggplot2::labeller] function, see `labeller` under
#' [`facet_wrap()`][ggplot2::facet_wrap].
#'
#' @seealso [`facet_wrap()`][ggplot2::facet_wrap], [`ggplot`][ggplot2::ggplot], [`labeller()`][ggplot2::labeller].
#' @family plot_model
#'
#' @param as_percent `logical`. If `TRUE`, the y-axis uses a percentage scale; default `FALSE`.
#'
#' @param rev_y `logical`. If `TRUE`, the direction of the y-axis is reversed, which may be useful
#'   when plotting linear predictors; default `FALSE`.
#'
#' @inheritParams glm_plotdata
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @inheritParams ggplot2::ggplot
#'
#' @return A [`ggplot`][ggplot2::ggplot] object.
#'
#' @keywords hplot
#' @export
#' @examples
#' ## Example uses randomly generated data; re-running may be worthwhile.  
#'
#' oldtheme <- theme_get()   ## Save ggplot defaults for later restoration
#'
#' ## Set ggplot defaults for pretty printing
#' theme_update(
#'   plot.title = element_text(color = "black", size = 20, hjust = 0.5),
#'   plot.subtitle = element_text(color = "black", size = 18, hjust = 0.5),
#'   axis.text.x = element_text(color = "black", size = 15),
#'   axis.text.y = element_text(color = "black", size = 15),
#'   axis.title.x = element_text(color = "black", size = 15),
#'   axis.title.y = element_text(color = "black", size = 15),
#'   strip.text.x = element_text(color = "black", size = 15),
#'   legend.position = "none"
#' )
#'
#' ## Create binomial data with groupings
#' (d <- list(iv2 = list(ab = c("a", "b"), cd = c("c", "d"))) |>
#'     add_grps(binom_data(), iv, .key = _))
#'
#' ## Set plot title and axis labels
#' plabs<- labs(
#'   x = "Level",
#'   y = "Linear predictor scale (logit)",
#'   title = "Example for ggplot.glm_plotdata()"
#' )
#'
#' ## Tweak to improve plot subtitles - see var_labs()
#' var_labs <- as_labeller(toupper)
#'
#' ## Ungrouped plot data on GLM linear predictor scale
#' (dp <- glm_plotdata(d, .ind_var = iv))
# #' (dp <- plotdata(d, .ind_var = iv))
#'
#' ## Plot model predictions and error bars with reversed y-axis
#' dp |> ggplot(rev_y = TRUE) + plabs
#'
#' ## Grouped plot data on GLM linear predictor scale
#' (dp <- glm_plotdata(d, .ind_var = iv2, .ungroup = iv))
# #' (dp <- plotdata(d, .ind_var = iv2, .ungroup = iv))
#'
#' ## Plot model predictions and error bars with reversed y-axis
#' dp |> ggplot(rev_y = TRUE) + plabs
#'
#' ## Revise y-axis title
#' plabs$y <- "Proportion Positive (%)"
#'
#' ## Ungrouped plot data on GLM reponse scale
#' (dp <- glm_plotdata(d, .ind_var = iv, type = "response"))
# #' (dp <- plotdata(d, .ind_var = iv, type = "response"))
#'
#' ## Plot model predictions and error bars
#' dp |> ggplot(as_percent = TRUE) + plabs
#'
#' ## Grouped plot data on GLM reponse scale
#' (dp <- glm_plotdata(d, .ind_var = iv2, .ungroup = iv, type = "response"))
# #' (dp <- plotdata(d, .ind_var = iv2, .ungroup = iv, type = "response"))
#'
#' ## Plot model predictions and error bars
#' dp |> ggplot(as_percent = TRUE) + plabs
#'
#' ## Override default subtitle
#' plabs$subtitle <- "Fascinating Results"
#' dp |> ggplot(as_percent = TRUE) + plabs
#'
#' theme_set(oldtheme)    ## Restore original ggplot defaults
#' rm(d, dp, oldtheme, plabs)


# ========================================
#  ggplot for glm_plotdata object
#  S3method ggplot.glm_plotdata()
#
#' @rdname plot_model
# #' @export


ggplot.glm_plotdata <- function(data = NULL, mapping = aes(), as_percent = FALSE, rev_y = FALSE, ...,
    environment = parent.frame()) {

    check_dots_empty()
    notgrp <- is.na(data$grouped)
    faceted <- !is.null(data %@% "facet_by")

    NextMethod(mapping = aes(.data$level, .data$pred)) +
    # geom_col(
        # colour = factor(ifelse(notgrp, 0, data$grouped)),
        # fill = "steelblue3",
        # linewidth = ifelse(notgrp, 0.5, 1)
    # ) +
    geom_col(
        aes(
            y = .data$pred,
           	colour = factor(ifelse(notgrp, 0, .data$grouped))
        ),
        fill = "steelblue3",
        linewidth = 1
    ) +
    geom_errorbar(
        aes(ymax = .data$upper, ymin = .data$lower),
        linewidth = if(faceted) 0.75 else 1,
        width = 0.2
    ) +
    geom_point(
        aes(y = .data$obs),
        colour = "black",
        fill = "springgreen2",
        size = ifelse(notgrp, 0, ifelse(faceted, 2, 3)),
        shape = "square filled",
        stroke = ifelse(notgrp, 0, ifelse(faceted, 0.25, 0.5))
    ) +
    theme(
        axis.text.x = element_text(
            color = "black",
            size = if(faceted) 12 else 15
        ),
        strip.text.x = element_text(color = "black", size = 12)
    ) +
    geom_text(
        aes(label = paste("n", "=", n, sep = ifelse(faceted, "", " ")), y = 0),
        size = if(faceted) 3.5 else 5,
        vjust = if(faceted) 1.4 else 1.75
    ) +
    labs(
        x = NULL,
        y = "Probability",
        title = "Model Predictions and CI"
    ) + {
        var_labs <- match.fun("var_labs")
        if (faceted) {
            facet_wrap(
                data_sym(data %@% "facet_by"),
                scales = "free",            # drops unused levels
                labeller = as_labeller(var_labs)
            )
        } else labs(subtitle = data %@% "subtitle" |> var_labs())
    } + {
        if (as_percent)
            scale_y_continuous(labels = function(x) paste0(x * 100, "%"))
    } + {
        if (rev_y)
            scale_y_reverse()
    }
}
