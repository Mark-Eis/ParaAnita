# ParaAnita R Package
# Mark Eisler - Jun 2023
# For Binary and Binomial Data Analysis
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# factor-manip.R


# ========================================
#' @title
#' Add Factors to Data Based on Grouped Levels of an Existing Factor
#'
#' @description
#' Add new factors to data based on grouped levels of an existing factor, using a key compatible with
#' \code{\link[forcats]{fct_collapse}}.
#'
#' @details
#' The \code{.key} argument should be a series of named lists nested within an outer list. Each nested named list must
#' contain one or more named character vectors representing the new factor groupings. The nested lists  should be
#' structured for compatibility with \code{\link[forcats]{fct_collapse}} in the package \pkg{forcats}.
#'
#' \code{add_grps()} will add new, grouped factors to \code{.data}, one for each nested list and with the same name.
#' Levels are assigned to these new grouped factors using the name of whichever character vector, if any, contains the
#' old factor level. If none does, the original ungroupsed factor level is used.
#'
#' Various different groupings of a \code{factor} may be conveniently added to \code{.data} using \code{add_grps()} and
#' the corresponding series of related binomial \code{\link[stats]{glm}s} compared using \code{\link{comp_glm}}.
#'
#' @seealso \code{\link{comp_glm}}, \code{\link[forcats]{fct_collapse}}, \code{\link[base]{list}}.
#' @family factor-manip
#'
#' @param .fct the quoted name of an existing (ungrouped) \code{factor}.
#'
#' @param .key a \code{list} of nested, named lists representing the groupings, each containing a series of named
#'   character vectors.
#'
#' @param .sort \code{logical}, whether to sort levels of new factors; default \code{TRUE}.
#'
#' @inheritParams binom_contingency
#'
#' @return A data frame, or a data frame extension (e.g. a [`tibble`][tibble::tibble-package]), equivalent to \code{.data}
#'   with the additional grouped factor(s).
#'
#' @export
#' @examples
#' d <- binom_data(levels = 6) |> print()
#'
#' ## One grouped factor
#' grp_key <- list(g = c("a", "c", "e"), h = c("b", "d", "f"))
#'
#' d |> add_grps(iv, list(iv2 = grp_key))
#'
#' ## Several grouped factors
#' grp_key <- list(
#'     iv2 = grp_key,
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' )
#'
#' d |> add_grps(iv, grp_key)
#'
#' ## Cut out the middleman
#' list(
#'     iv2 = list(g = c("a", "c", "e"), h = c("b", "d", "f")),
#'     iv3 = list(i = c("a", "b", "c"), j = c("d", "e", "f")),
#'     iv4 = list(k = c("a", "b"), l = c("c", "d"), m = c("e", "f"))
#' ) |>
#' add_grps(d, iv, .key = _)
#'
#' ## Binomial data with month as explanatory variable, using dplyr and forcats package functions
#' (d <- binom_data(12, probs = rep_len(0.5, 12)) |>
#'     mutate(across(iv, \(x) fct_recode(x, !!!setNames(letters[1:12], month.abb)))) |>
#'     rename(month = "iv"))
#' 
#' ## Name three lists of different month groupings using purrr::map()
#' list(
#'     list(1:3, 4:6, 7:9, 10:12),
#'     list(1:4, 5:8, 9:12),
#'     list(c(1:3, 10:12), 4:9)
#' ) |>
#' map(\(x) map(x, \(y) month.abb[y])) |>
#' map(\(x) setNames(x, paste0("group", seq_along(x)))) |>
#' (\(x) setNames(x, paste0("months", seq_along(x))))() |>
#' add_grps(d, month, .key = _)        ## Add the new year groups to data
#' 
#' ## Example from fct_collapse() using gss_cat dataset from {forcats} package
#' \dontshow{
#'    if (!requireNamespace("forcats", quietly = TRUE)) 
#'        warning("package 'forcats' must be installed")
#'    try(gss_cat <- forcats::gss_cat)
#' }
#'
#' fct_count(gss_cat$partyid)
#'
#' grp_key <- list(
#'     partyid2 = list(
#'         missing = c("No answer", "Don't know"),
#'         other = "Other party",
#'         rep = c("Strong republican", "Not str republican"),
#'         ind = c("Ind,near rep", "Independent", "Ind,near dem"),
#'         dem = c("Not str democrat", "Strong democrat")
#'     )
#' )
#'
#' gss_cat |>
#'     add_grps(partyid, grp_key) |>
#'     _$partyid2 |> fct_count()
#'
#' gss_cat |>
#'     add_grps(partyid, grp_key, .sort = FALSE) |>
#'     _$partyid2 |> fct_count()
#'
#' \dontshow{
#'     rm(gss_cat)
#' }
#'
#' rm(grp_key, d)

add_grps <- function (data, .fct, .key, .sort = TRUE) {
    .fct = enquo(.fct)
    grp_fct_ls <- map(.key, ~ function(fct) call2(fct_collapse, !!!exprs({{ fct }}, !!!.x)) |> eval_tidy())

    data |> mutate(
        across(!!.fct, grp_fct_ls, .names = "{.fn}"),
        across(names(.key), \(f) fct_relevel(f, if (.sort) sort else NULL)),
        .after = !!.fct
    )
}

# ========================================
#' @title
#' Factor as Numeric
#'
#' @description
#' Transform a factor to approximately its original numeric values.
#'
#' @details
#' See \sQuote{Warning} section of \code{\link[base]{factor}}: \dQuote{In particular, \code{as.numeric}
#' applied to a factor is meaningless, and may happen by implicit coercion. To transform a factor
#' \code{f} to approximately its original numeric values, \code{as.numeric(levels(f))[f]} is
#' recommended and slightly more efficient than \code{as.numeric(as.character(f))}.}
#'
#' @seealso \code{\link[base]{factor}}
#' @family factor-manip
#'
#' @param f factor to be converted to numeric values
#'
#' @return Numeric
#'
#' @keywords category math
#' @export
#' @examples
#' f <- factor(2001:2020)
#'
#' f
#'
#' f |> as.numeric()	# Returns codes for factor levels, not what is required
#'
#' f |> fct_to_num()	# Returns approximate numeric values, as required
#'
#' rm(f)
#' 

fct_to_num <- function(f) as.numeric(levels(f))[f]
