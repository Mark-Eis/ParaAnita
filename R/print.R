# First R Package
# Mark Eisler Dec 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# print.R

# ========================================
#' @title
#' S3 Print Methods
#'
#' @name Print_Methods
#' 
#' @description
#' S3 methods to enable printing of [`announce`][announce], [`binom_contingency`][binom_contingency],
#' [`contingency_table`][contingency_table], [`odds_ratio`][odds_ratio] and [`summ_anov`][summanov] objects.
#'
#' @details
#' These print methods return their argument `x` invisibly (via [`invisible()`][base::invisible]).
#'
#' @seealso  [`print()`][base::print], [`print.tbl()`][tibble::print.tbl].
#' @family print
#'
#' @inheritParams base::print
#' @inheritParams tibble::print.tbl_df
#'
#' @return The argument `x`.
#'
#' @keywords print
#' @export
#' @examples
#'
#' ## print.announce() — print an 'announce' object
#' announce("x", lead = "Lorem ipsum dolor sit amet")
#'
#' (d <- bernoulli_data())
#'
#' ## print.binom_contingency() — print a 'binom_contingency' object
#' d |> binom_contingency(dv)
#'
#' (d2 <- tibble(
#'     iv = letters[1:4] |> sample(10, replace = TRUE) |> as.factor(),
#'     dv = c("Success", "Fail", "Borderline")  |> sample(10, replace = TRUE)
#'   ))
#'
#' ## print.contingency_table() — print a 'contingency_table' object
#' d2 |> contingency_table(dv)
#'
#' ## print.odds_ratio() — print an 'odds_ratio' object
#' d |> odds_ratio(.dep_var = dv, .ind_var = iv)
#'
#' ## print.summanov() — print a 'summanov' object
#' d |> summanov(dv, iv)
#'
#' rm(d, d2)


# ========================================
#  Print an Announce Object
#  S3method print.announce()
#
#' @rdname Print_Methods
#' @export

print.announce <- function(x, ...) {
	validate_announce(x)
    .lead <- x %@% "lead"
    cat(paste0(rep(c("_", "\n", .lead, ": -\n\n"), c(nchar(.lead) + 3, 1, 1, 1)), collapse = ""))
    x %@% "lead" <- NULL
    class(x) <- class(x)[-c(inherits(x, "announce", TRUE))]
    NextMethod()
    class(x) <- classlist(.Class)
    x %@% "lead" <- .lead
    invisible(x)
}

# ========================================
#  Recursive function to find deepest, nested "previous" attribute of .Class
#  Used by print.announce() to restore class(x) after NextMethod(); "announce" is
#  removed from class(x) before NextMethod() to avoid risk of pathological recursion. 
# 
#  classlist()
#
#  Not exported

classlist <- function(clist) {
    if (!is.null(clist %@% "previous"))
        clist <- classlist(clist %@% "previous")
    clist
}

# ========================================
#  Print Binomial Contingency Table with Confidence Level
#  S3method print.contingency_table()
#
#' @rdname Print_Methods
#' @export

print.binom_contingency <- function(x, width = NULL, ..., n = NULL, max_extra_cols = NULL, max_footer_lines = NULL) {
    NextMethod()
    .level <- x %@% "conf.level"
    if (!is.null(.level))
        cat("\tConfidence level", .level, "\n")
    invisible(x)
}

# ========================================
#  Print Contingency Table with Format String
#  S3method print.contingency_table()
#
#' @rdname Print_Methods
#' @export

print.contingency_table <- function(x, width = NULL, ..., n = NULL, max_extra_cols = NULL, max_footer_lines = NULL) {
    NextMethod()
}

contingency_table_names <- c(
    contingency_table = "Contingency Table",
    xcontingency_table = "Crossed Contingency Table",
    binom_contingency = "Binomial Contingency Table"
)

# ========================================
#  Print Odds Ratios and Confidence Intervals with Contrasts
#  S3method print.odds_ratio()
#
#' @rdname Print_Methods
#' @export

print.odds_ratio <- function(x, width = NULL, ..., n = NULL, max_extra_cols = NULL, max_footer_lines = NULL) {
    contr <- (x %@% glm)$contrasts
    has_contr <- !is.null(contr)
    NextMethod()
    if (x %@% "print_contr") {
        if (has_contr)
            announce(contr, "Contrasts")
        else
            announce(options("contrasts")[[1]], "Default contrasts used")
    } |> print()
    invisible(x)
}

# ========================================
#  Print GLM Summary and Anova List with Format String
#  S3method print.summ_anov()
#
#' @rdname Print_Methods
#' @export

print.summ_anov <- function(x, ...) {
    map(x, print)
    invisible(x)
}
