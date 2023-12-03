# ParaAnita R Package
# Mark Eisler Jun 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# set-contrasts.R


# ========================================
#' @title
#' Get and Set Contrasts Matrix for an Independent Variable in Data
#'
#' @description
#' \code{get_contrasts()} returns the \code{"contrasts"} attribute a selected factor within a data frame.
#'
#' \code{set_contrasts()} sets the \code{"contrasts"} attribute for a selected factor within a data frame;
#'
#' \code{set_contrasts()<-} is the replacement function form.
#'
#' @details
#' The \code{"contrasts"} attribute of \code{.f} may be set using either a numeric matrix or (the quoted name of) a
#' function which computes such matrices, supplied to \code{set_contrasts()} using the \code{contr} argument or the
#' \code{value} argument in the case of the replacement function form \code{set_contrasts()<-}. A suitable contrast matrix
#' may be obtained using a contrast function such as [`contr.helmert`][stats::contr.helmert],
#' [`contr.poly`][stats::contr.poly], [`contr.sum`][stats::contr.sum], [`contr.treatment`][stats::contr.treatment] or
#' [`contr.SAS`][stats::contr.SAS], or the (quoted) name of the function itself may be supplied. Additional arguments,
#' such as \code{base = }\var{x}, may be supplied to a contrast function using the \code{\dots} argument of
#' \code{set_contrasts()} or \code{set_contrasts()<-}.
#'
#' If a \code{base} argument is supplied when \code{contr = contr.treatment}, its value is capped to be no greater than
#' \code{nlevels(.f)}, hence it can be specified as a large integer (e.g., \var{99L}) to ensure the last level is the
#' reference level. This may be convenient when using \code{set_contrasts()} programmatically.
#'
#' If \code{NULL} is supplied as the \code{contr} or \code{value} argument, any existing \code{"contrasts"}
#' attribute will be removed from \code{.f}.
#'
#' @seealso [`contrast`][stats::contr.helmert], [`contrasts`][stats::contrasts], [`C`][stats::C],
#'   [`contr.helmert`][stats::contr.helmert], [`contr.poly`][stats::contr.poly], [`contr.sum`][stats::contr.sum],
#'   [`contr.treatment`][stats::contr.treatment] or [`contr.SAS`][stats::contr.SAS].
#' @family get-contrasts
#'
#' @param .f <[`data-masked`][rlang::args_data_masking]> quoted name of a \code{factor} in \code{data}.
#'
#' @inheritParams stats::C
#' @inheritParams stats::contrasts
#' @inheritParams expl_fcts
#'
#' @return A dataframe or [`tibble`][tibble::tibble-package] with the \code{"contrasts"} attribute set for \code{.f}.
#'
#' @keywords design regression array
#' @export
#' @examples
#' ## Create data frame with a factor iv
#' (d <- binom_data())
#'
#' ## set_contrasts()
#' d |> set_contrasts(iv, contr = contr.helmert) |> get_contrasts(iv)
#' d |> set_contrasts(iv, contr = contr.poly) |> get_contrasts(iv) |> zapsmall()
#' d |> set_contrasts(iv, contr = contr.sum) |> get_contrasts(iv)
#' d |> set_contrasts(iv, contr = contr.treatment) |> get_contrasts(iv)
#' d |> set_contrasts(iv, contr = contr.SAS) |> get_contrasts(iv)
#'
#' ## how.many argument
#' d |> set_contrasts(iv, 4, contr = contr.poly) |> get_contrasts(iv) |> zapsmall()
#' d |> set_contrasts(iv, 3, contr = contr.poly) |> get_contrasts(iv) |> zapsmall()
#' d |> set_contrasts(iv, 2, contr = contr.poly) |> get_contrasts(iv) |> zapsmall()
#' d |> set_contrasts(iv, 1, contr = contr.poly) |> get_contrasts(iv) |> zapsmall()
#'
#' ## base argument of contr.treatment
#' d |> set_contrasts(iv, base = 1, contr = contr.treatment) |> get_contrasts(iv)
#' d |> set_contrasts(iv, base = 2, contr = contr.treatment) |> get_contrasts(iv)
#' d |> set_contrasts(iv, base = 3, contr = contr.treatment) |> get_contrasts(iv)
#' d |> set_contrasts(iv, base = 4, contr = contr.treatment) |> get_contrasts(iv)
#' d |> set_contrasts(iv, base = 5, contr = contr.treatment) |> get_contrasts(iv)
#' 
#' ## Streamline with purrr::map()
#' d$iv |> levels() |> seq_along() |> set_names(\(x) paste0("base=", letters[seq_along(x)])) |>
#'   map(\(x) set_contrasts(d, iv, base = x, contr = contr.treatment) |>
#'   get_contrasts(iv))
#'
#' ## base argument of contr.treatment limited to nlevels(d$iv) 
#' d |> set_contrasts(iv, base = 99L, contr = contr.treatment) |> get_contrasts(iv)
#' 
#' ## Remove "contrasts" attribute using NULL
#' d |> set_contrasts(iv, contr = NULL) |> get_contrasts(iv)
#'
#' 
#' ## set_contrasts()<- replacement form
#' set_contrasts(d, iv) <- contr.helmert
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv) <- contr.poly
#' d |> get_contrasts(iv) |> zapsmall()
#' 
#' set_contrasts(d, iv) <- contr.sum
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv) <- contr.SAS
#' d |> get_contrasts(iv)
#'
#' ## how.many argument
#' set_contrasts(d, iv, 4) <- contr.poly
#' d |> get_contrasts(iv) |> zapsmall()
#' 
#' set_contrasts(d, iv, 3) <- contr.poly
#' d |> get_contrasts(iv) |> zapsmall()
#' 
#' set_contrasts(d, iv, 2) <- contr.poly
#' d |> get_contrasts(iv) |> zapsmall()
#' 
#' set_contrasts(d, iv, 1) <- contr.poly
#' d |> get_contrasts(iv) |> zapsmall()
#' 
#' ## base argument of contr.treatment
#' set_contrasts(d, iv, base = 1) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv, base = 2) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv, base = 3) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv, base = 4) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv, base = 5) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' set_contrasts(d, iv, base = 99L) <- contr.treatment
#' d |> get_contrasts(iv)
#' 
#' ## Naming contrasts matrix columns isn't always automatic
#' ## This is one way…
#' ct5 <- contr.SAS(5)
#' dimnames(ct5)
#' dimnames(ct5)[[2]] <- levels(d$iv)[-1]
#' dimnames(ct5)
#' 
#' d |> set_contrasts(iv) <- ct5
#' d |> get_contrasts(iv)
#' 
#' ## Set "contrasts" attribute for d$iv to Helmert contrasts matrix
#' set_contrasts(d, iv) <- contr.helmert
#' attr(d$iv, "contrasts")
#' d |> get_contrasts(iv)
#' ## get_contrasts() return value identical to that of contrasts() 
#' identical(contrasts(d$iv), d |> get_contrasts(iv))
#' 
#' ## Remove "contrasts" attribute 
#' set_contrasts(d, iv) <- NULL
#' ## get_contrasts() return value no longer identical to that of contrasts() 
#' identical(contrasts(d$iv), d |> get_contrasts(iv))
#' ## get_contrasts() shows NULL "contrasts" attribute… 
#' d |> get_contrasts(iv)
#' attr(d$iv, "contrasts")
#' ## …whereas contrasts() now shows default (unordered) contrasts from options("contrasts") 
#' contrasts(d$iv)
#' options("contrasts") 
#'
#' rm(d)

get_contrasts <- function(data, .f) {
    .f <- enquo(.f)
    if (!is.data.frame(data)) stop("\n\targument \"data\" not a data frame")
    if (eval_tidy(expr(!is.factor(!!.f)), data))
        stop("\targument ", as_name(.f), " not of type factor")
    eval_tidy(expr({{.f}} %@% contrasts), data)
}

# ========================================
#  Set Contrasts Matrix For An Independent Variable Within Data
#
#' @rdname get_contrasts
#' @export

set_contrasts <- function(data, .f, how.many = NULL, ..., contr) {
    .f <- enquo(.f)
    if (!is.data.frame(data)) stop("\n\targument \"data\" not a data frame")
    if (eval_tidy(expr(!is.factor(!!.f)), data))
        stop("\targument ", as_name(.f), " not of type factor")
    arg4 <- list2(...)
    if(!is.null(arg4[["base"]]))
        arg4[["base"]] <- arg4[["base"]] %:<% eval_tidy(expr(nlevels(!!.f)), data)
    data |> mutate(across(!!.f, \(x) C(x, contr, how.many, !!!arg4)))
}

# ========================================
#  Set Contrasts Matrix For An Independent Variable Within Data
#  Replacement function form
#
#' @rdname get_contrasts
#' @export

`set_contrasts<-` <- function(data, .f, how.many = NULL, ..., value) {
    .f <- enquo(.f)
    if (!is.data.frame(data)) stop("\n\targument \"data\" not a data frame")
    if (eval_tidy(expr(!is.factor(!!.f)), data))
        stop("\targument ", as_name(.f), " not of type factor")
    arg4 <- list2(...)
    if(!is.null(arg4[["base"]]))
        arg4[["base"]] <- arg4[["base"]] %:<% eval_tidy(expr(nlevels(!!.f)), data)
    data |> mutate(across(!!.f, \(x) C(x, value, how.many, !!!arg4)))
}


# ========================================
#' @title
#' Get and Set Treatment Contrasts for Independent Variables in Data
#'
#' @description
#' \code{get_contr_data()} shows the \code{"contrasts"} attributes of all or selected factors within a data frame.
#'
#' \code{set_contr_treat()} sets the \code{"contrasts"} attribute for selected factors within a data frame to a treatment
#' contrast matrix with individually specified baseline levels.
#'
#' \code{set_contr_treat()<-} is the replacement function form.
#'
#' @details
#' \code{get_contr_data()} prints the \code{"contrasts"} attributes of all or selected factors and returns \code{data}
#' invisibly.
#'
#' The \code{"contrasts"} attribute of \code{factors} selected with \code{\dots} are set using the contrast function
#' [`contr.treatment`][stats::contr.treatment] with baseline factor levels as specified numerically in the argument
#' \code{.baseline} or the \code{value} argument in the case of the replacement function form \code{set_contrasts()<-}.
#' If no \code{.baseline} argument is supplied, by default the first factor level is used as baseline.
#'
#' Factors in \code{.data} may be selected for getting and setting contrasts using the \code{\dots} argument with the
#' <[`tidy-select`][dplyr::dplyr_tidy_select]> syntax of package \pkg{dplyr}, including use of \strong{selection helpers}.
#'
#' The individual \code{.baseline} (or \code{.value}) argument values are capped to be no greater than
#' [`nlevels`][base::nlevels] for each of the corresponding factors selected in \code{...}. Hence, to ensure the last
#' level is the reference level, a baseline value can be specified as a large integer (e.g., \var{99}L), which may be
#' convenient when using \code{set_contr_treat()} programmatically.
#'
#' @seealso [`contrasts`][stats::contrasts], [`C`][stats::C], [`contr.treatment`][stats::contr.treatment].
#' @family get-contrasts
#'
#' @param \dots <[`tidy-select`][dplyr::dplyr_tidy_select]> The selection of one or more factors in \code{data} for
#'   getting or setting contrasts.
#'
#' @param .baseline a \code{numeric} vector of \code{length} equal to the number of contrasts to be set specifying which
#'   level is considered the baseline. 
#'
#' @param .verbose \code{logical}, whether to print "before and after" contrast matrices for factors in \code{data}.
#'
#' @param value \code{numeric}, see \code{.baseline} argument.
#'
#' @inheritParams set_contrasts
#' @inheritParams stats::C
#' @inheritParams stats::contrasts
#'
#' @return The original dataframe or [`tibble`][tibble::tibble-package] with the \code{"contrasts"} attributes set for
#'   selected \code{factors}.
#'
#' @keywords design regression array
#' @export
#' @examples
#' # Coming soon!
#'

get_contr_data <- function (data, ...) {
    pos <- eval_select(expr(where(is.factor)), data = data)
    if (...length())
        pos <- intersect(pos, eval_select(expr(c(...)), data = data))
    data[pos] |> map(\(x) x %@% contrasts) |>
    print() |> invisible()
}

# ========================================
# Set Treatment Contrasts For Independent Variables Within Data
#
#' @rdname get_contr_data
#' @export

set_contr_treat <- function (data, ..., .baseline = NULL, .verbose = TRUE) {
    if(.verbose) report(data, "before:")
    pos <- eval_select(expr(where(is.factor)), data = data)
    if (...length())
        pos <- intersect(pos, eval_select(expr(c(...)), data = data))
    efs <- syms(names(data[pos]))
    .baseline <- .baseline %||% rep(1L, length(efs))
    map2(efs, .baseline, \(ef, baseline) set_contrasts(data, !!ef, base = baseline) <<- contr.treatment)
    if(.verbose) report(data, "after:")
    data
}

# ========================================
# Set Treatment Contrasts For Independent Variables Within Data
#  Replacement function form
#
#' @rdname get_contr_data
#' @export

`set_contr_treat<-` <- function(data, ..., .verbose = FALSE, value)
    data |> set_contr_treat(..., .baseline = value, .verbose =.verbose)

# ========================================
# Unexported functions

# Infix minimum
`%:<%` <-  function (x, y) 
    min(x, y)

# Report contrasts
report <- function(data, cuando) {
    cat("\n_________________\nContrasts", cuando, "-\n")
    get_contr_data(data)
}
