# First R Package
# Mark Eisler May 2024
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# print-lf.R


# ========================================
#' Pipe-Friendly Line Feeds and Printing
#'
#' @description
#' \code{lf()} outputs one or more line feeds during a piped sequence.
#'
#' @details
#' \code{print_lf()} prints an object in a piped sequence then outputs one or more line feeds.
#'
#' An object passed as argument in a piped sequence is printed and/or one or more line feeds are
#' output during a piped sequence using \code{\link[base]{cat}()}. This can be useful to separate
#' lines of printed output, see examples. 
#'
#' @seealso [`cat`][base::cat].
#' @family print
#'
#' @param x Object to be piped.
#'
#' @param n Number of line feeds; default \code{1}.
#'
#' @return Invisibly returns its first argument.
#'
#' @keywords print
#' @export
#' @examples
#' obj <- "Lorem ipsum dolor sit amet"
#' obj |> lf()               # line feed, object returned invisibly
#' obj |> lf(3)              # three line feeds, object returned invisibly
#' (obj |> lf(3))            # three line feeds, returned object rendered visible
#' obj |> lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#'
#' obj |> print() |> lf(3)   # line feeds are unexpectedly before printed output.
#'
#' ## Use print_lf() instead
#' obj |> print_lf()         # object printed with line feed and returned invisibly
#' obj |> print_lf(3)        # object printed with three line feeds and returned invisibly
#' (obj |> print_lf(3))      # Ditto, then rendered visible
#' obj |> print_lf(3) |> paste("consectetur adipiscing elit", sep = ", ")
#'
#' rm(obj)

lf <- function(x, n = 1) {
    linefeed(n)
    invisible(x)
}

# ========================================
#  Pipe-Friendly Print and Line Feeds
#' @rdname lf
#'
#' @export

print_lf <- function(x, n = 1) {
    print(x)
    lf(x, n)
}

# ========================================
#  Add Linefeeds to Printing
#  Not exported

linefeed <- function(n)
    if (!is.null(n)) {
        n <- as.integer(n[[1]])
        if (n < 0)
            n <- 0L
        cat(rep("\n", n))
    }

