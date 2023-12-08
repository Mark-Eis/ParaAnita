# First R Package
# Mark Eisler Aug 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# simulate-data.R


# ========================================
#' @title
#' Simulated Bernoulli and  Binomial Proportion Data
#'
#' @name Simulate_Data
#'
#' @description
#' `bernoulli_data()` creates a simulated univariate Bernoulli data set having a dependent variable `dv` with values of
#' \var{0} and \var{1}, and an independent variable `iv` with levels represented by lower case letters.
#'
#' `binom_data()` creates a simulated univariate binomial proportion data set having a variable `pn` representing the
#' number of successes, a variable `qn` representing the number of failures, and an independent variable `iv` with
#' levels represented by lower case letters.
#'
#' @details
#' A random sample from a Bernoulli distribution is obtained for each level of the independent variable `iv`, at the
#' corresponding probability given in `probs`, using [`rbinom()`][stats::rbinom] with `size = 1`. The result is returned
#' as a [`tibble`][tibble::tibble-package] with two columns, `iv` representing the level of the independent variable and
#' `dv` representing the simulated data. The result may be easily converted into (simulated) proportion data and inspected
#' using [`binom_contingency()`][binom_contingency], see examples.
#'
#' A random sample from a binomial distribution of size `length` is obtained for each level of the independent variable
#' `iv`, at the corresponding probability given in `probs`, using [`rbinom()`][stats::rbinom] with `size = levels`. 
#'
#' `bernoulli_data()` and `binom_data()` are used for demonstrating and testing functions such as
#' [`contingency_table()`][contingency_table], [`binom_contingency()`][binom_contingency] and [`odds_ratio()`][odds_ratio].
#'
#' @seealso [`rbinom()`][stats::rbinom]
#' @family simulate_data
#'
#' @param levels numeric, the desired number of levels of the independent variable `iv`;  default \var{5}.
#'
#' @param length numeric, the desired number of simulated observations per level of the independent variable `iv`; 
#'   default \var{20}.
#'
#' @param probs a numeric vector of the same length as levels, representing the probabilities of success for each
#'   corresponding level; default `seq(0.5, 0.1, length.out = levels)`.
#'
#' @return
#' An object of class `"announce"` inheriting from [`tibble`][tibble::tibble-package] with column `iv` for the independant
#'   variable, and for `bernoulli_data()`, column `dv` representing the dependant variable; and for `binom_data()`, columns
#'   `pn` and `qn` representing the number of "successes" and "failures", as follows: -
#'
#' \item{`iv`}{a `factor` representing levels of the independant variable.}
#'
#' \item{`dv`}{an `integer` representing the value of the dependent variable.}
#'
#' \item{`pn`}{an `integer` representing  the number of successes.}
#'
#' \item{`qn`}{an `integer` representing  the number of failures.}
#'
#' @export
#' @examples
#' bernoulli_data()
#' bernoulli_data() |> binom_contingency(dv, iv)
#' bernoulli_data(probs = seq(0.4, 0, length.out = 5)) |> binom_contingency(dv, iv)
#'
#' binom_data()
#' binom_data(probs = seq(0.4, 0, length.out = 5))
#'
bernoulli_data <- function(levels = 5, length = 20, probs = seq(0.5, 0.1, length.out = levels)) {
    stopifnot(length(probs) == levels)
    tibble(
        iv = letters[1:levels] |> rep(each = length) |> as.factor(),
         dv = probs |> map(rbinom, n = length, size = 1) |>
            (\(.b) {.b[-1] |> walk(~ {.b[[1]] <<- c(.b[[1]], .x)}); .b[[1]]})()
    ) |>
    announce("Simulated Bernoulli Data") 
}

#' @export
#' @rdname Simulate_Data

binom_data <- function(levels = 5, length = 20L, probs = seq(0.5, 0.1, length.out = levels)) {
    stopifnot(length(probs) == levels)
    tibble(    
        iv = letters[1:levels] |> as.factor(),
        pn = rbinom(levels, length, probs),
        qn = length - .data$pn
    ) |>
    announce("Simulated Binomial Data") 
}

