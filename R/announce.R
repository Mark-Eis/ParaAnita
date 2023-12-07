# First R Package
# Mark Eisler Dec 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# announce.R


# ========================================
#  Constructor for an Announce Object
#  new_announce()
#
#  Not exported

new_announce <- function(object = vector(), lead = "Announce: -\n", ...) {
    stopifnot(!is.null(object))
    structure(object, class = c("announce", class(object)), lead = lead, ...)
}

# ========================================
#  Validator for an Announce Object
#  validate_announce()
#
#  Not exported, but probably should be…
# #' @export

validate_announce <- function(x) {
    lead <- x %@% lead

    if(!is.character(lead)) {
	    stop(
	        "\"lead\" attribute must be of type character",
		     call. = FALSE
	    )
    }

    if(!length(lead) || !nchar(lead)) {
	    stop(
	        "\"lead\" attribute cannot be empty",
		     call. = FALSE
	    )
    }

    x
}


# ========================================
#' Announce Class for Consistent Printing
#'
#' @description
#' Creates an object of class `"announce"` with a built-in title string used for printing.
#'
#' @details
#' \code{announce()} converts an object to class `"announce"`, inheriting from its existing class(es).
#'
#' @seealso [`cat()`][base::cat], [`print()`][base::print].
#' @family print
#'
#' @param object Object to be converted to `"announce"` class.
#'
#' @param lead a `character` string giving the title to be printed.
#'
#' @param \dots other named arguments to be forwarded to print methods of classes inherited from `object`.
#'
#' @return an object of class `"announce"` inheriting from class(es) of `object`.
#'
#' @keywords print
#' @export
#' @examples
#' announce()
#' (cpt <- announce("x", lead = "Lorem ipsum dolor sit amet"))
#' .class2(cpt)
#'
#' ## an Announce object, or one inheriting from announce, can be safely overwritten
#' (cpt <- announce(cpt, "Consectetur adipiscing elit"))
#' .class2(cpt)
#'
#' rm(cpt)

# ========================================
#  "Helper" Function for Constructing an Announce Object

announce <- function(object = vector(), lead = "Announce", ...) {
    lead <- as.character(lead)
    if (inherits(object, "announce"))
        class(object) <- class(object)[-c(inherits(object, "announce", TRUE))]
	validate_announce(new_announce(object, lead))
}
