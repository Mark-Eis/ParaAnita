# BitsnBobs R Package
# Mark Eisler - Dec 2023
# For general bits and bobs of code
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# contrast-names.R

# ========================================
#' Get and Set Contrast Matrix Column Names
#'
#' @description
#' Set and view the column names of contrasts associated with a factor.
#'
#' @details
#' \code{contr_colnames()} returns the current column names of the contrasts for a factor.
#'
#' \code{contr_colnames()<-} sets the column names of the contrasts for a factor.
#'
#' \code{cntr_pfx()<-} prefixes the current column names of the contrasts for a factor with the character or string
#' provided. This can be useful when factor levels are elided with the factor name as, for instance, in the printed
#' output of \code{\link[stats]{summary.glm}}.
#'
#' If contrasts are not set for \var{x}, both \code{contr_colnames()<-} and \code{cntr_pfx()<-} set the contrast attribute
#'   using the default function from \code{\link[base]{options}("contrasts")} before modifying the column names.
#'
#' @seealso \code{\link[stats]{contrast}}, \code{\link[stats]{contrasts}} and \code{\link[base]{factor}}.
#' @family contrast-names
#'
#' @param x a \code{factor} for which the contrast column headings are to be set or viewed.
#'
#' @param value a character vector containing the contrast column names, of length equal to the number of contrasts;
#'   or in the case of \code{contr_colpfx()<-} a character or string used to prefix the existing contrast column names.
#'
#' @export
#' @examples
#' (d <- data.frame(
#'   f = gl(5, 5, labels = LETTERS[1:5]),
#'   dv = sample(c(0,1), 25, replace = TRUE)
#' ))
#'
#' contrasts(d$f) <- contr.helmert
#' contrasts(d$f)
#' contr_colnames(d$f)
#' glm(dv ~ f, family = binomial, data = d) |> summary()
#'
#' contr_colnames(d$f) <- c("A v. B", "AB v. C", "ABC v. D", "ABCD v. E")
#' contr_colnames(d$f)
#' contrasts(d$f)
#' glm(dv ~ f, family = binomial, data = d) |> summary()
#'
#' contr_colpfx(d$f) <- ": "
#' contr_colnames(d$f)
#' glm(dv ~ f, family = binomial, data = d) |> summary()
#'
#' rm(d)
	
contr_colnames <- function(x)
	colnames(contrasts(x))

# ========================================
#  Set column names of contrasts associated with a factor.
#' @rdname contr_colnames
#' @export
	
`contr_colnames<-` <- function(x, value) {
	colnames(contrasts(x))<- value
	x
}

# ========================================
#  Prefix column names of contrasts associated with a factor.
#' @rdname contr_colnames
#' @export
	
`contr_colpfx<-` <- function(x, value) {
	colnames(contrasts(x))<- colnames(contrasts(x)) |> map_chr(~paste0(value, .x))
	x
}

# ========================================
#' Create and Set Names for Helmert Contrasts
#'
#' @description
#' Create and set column names for Helmert contrasts associated with a factor.
#'
#' @details
#'
#' Helmert contrasts for a factor contrast the second level with the first, the third with the average of the first two,
#' and so on. A contrasts matrix can be set as an attribute of a factor using the function
#' [`contrasts<-`][stats::contrasts], and using the contrasts function [`contr.helmert`][stats::contr.helmert] as an
#' argument i.e., \code{contrasts(f)<- contr.helmert}, will create and set an appropriate Helmert contrasts matrix
#' attribute. However the columns of the resulting Helmert contrasts matrix are unnamed. \code{helm_names()<-} creates
#' these column names based on the names the levels of the factor
#' \var{x}.
#'
#' If factor \var{x} does not have an associated Helmert contrast matrix, \code{helm_names()<-} will set one, giving a
#' message. The names of the levels of factor \var{x} need to be fairly short for function this to be helpful.
#'
#' The function \code{helm_names()} is an alias for \code{\link{contr_colnames}} and included simply for completeness
#' and to optimise help searches. 
#'
#' @seealso [`contrast`][stats::contr.helmert], [`contrasts`][stats::contrasts] and [`contr.helmert`][stats::contr.helmert].
#' @family contrast-names
#'
#' @param x a \code{factor} for which the contrast column headings are to be named.
#'
#' @param value a character vector or \code{\link{list}} of length \var{two} (any subsequent elements will be ignored),
#'   which are separators used in creating the headings; the first is a "within" separator and the second a "between"
#'   separator, see examples.
#'
#' @export
#' @examples
#' (f <- gl(5, 5, labels = LETTERS[1:5]))
#'
#' contrasts(f)<- contr.helmert
#' f
#' helm_names(f)
#'
#' helm_names(f)<- c(":", "v")
#' f
#' helm_names(f)
#'
#' contrasts(f)<- NULL
#' f
#'
#' helm_names(f)<- c("-", "vs.")
#' f
#' helm_names(f)
#'
#' rm(f)

# ========================================
#  Get column names of contrasts associated with a factor.
#  Alias for contr_colnames(),
	
helm_names <- function(x)
	contr_colnames(x)

# ========================================
#  Set column names of contrasts associated with a factor.
#' @rdname helm_names
#' @export

`helm_names<-` <- function(x, value) {
	
	if(!identical(contrasts(x) |> `dimnames<-`(NULL), contr.helmert(length(levels(x))) |> `dimnames<-`(NULL))) {
		message("Setting Helmert contrasts for factor x with `helm_names()<-`.")
		contrasts(x)<- contr.helmert
	}	
	len <- levels(x) |> length()
	contr_colnames(x)<-map2_chr(
			map2_chr(
				levels(x)[1],
				levels(x)[1:(len - 1)],
				~ifelse(identical(.x, .y), .x, paste0(.x, value[[1]], .y))
			),
			levels(x)[2:len],
			~paste(.x, value[[2]], .y)
		)
	x
}
