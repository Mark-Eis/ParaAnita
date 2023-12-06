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
#' S3 methods to enable printing of [`announce`][announce] and other objects.
#'
#' @details
#' To be completed...
#' These print methods return their argument `x` invisibly (via [`invisible()`][base::invisible]).
#'
#' @seealso  [`print()`][base::print].
#' @family print
#'
#' @inheritParams base::print
#'
#' @return The argument `x`.
#'
#' @keywords print
#' @export
# #' @examples


# ========================================
#  Print an Announce Object
#  S3method print.announce()
#
#' @rdname Print_Methods
#' @export

print.announce <- function(x, ...) {
	validate_announce(x)
    .lead <- x %@% lead
    cat(paste0(rep(c("_", "\n", .lead, ": -\n\n"), c(nchar(.lead) + 3, 1, 1, 1)), collapse = ""))
    x %@% lead <- NULL
    class(x) <- class(x)[-c(inherits(x, "announce", TRUE))]
    NextMethod()
    class(x) <- classlist(.Class)
    x %@% lead <- .lead
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
    if (!is.null(clist %@% previous))
        clist <- classlist(clist %@% previous)
    clist
}
