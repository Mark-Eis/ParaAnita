# First R Package
# Mark Eisler Jan 2024
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# ParaAnita.R


# ========================================
#' @title
#' Chi-Squared or Fisher's Exact Test
#' 
#' @description
#' Test input data using Chi-squared or Fisher's exact test as appropriate. 
#'
#' @details
#' Uses \code{\link[stats]{chisq.test}()} to calculate expected values and then applies Chi-squared test if all
#' expected values are 5 or greater, otherwise applies \code{\link[stats]{fisher.test}()}.
#'
#' @seealso  \code{\link[stats]{chisq.test}}, \code{\link{contingency_table}}.
#' 
#' @param ... A vector, matrix, data frame or any valid input to \code{chisq.test}
#' 
#' @return A list containing the observed and expected values and the result of either \code{chisq.test()} or 
#' \code{fisher.test()}, as appropriate.
#' 
#' @export
#' @examples
#' (t <- bernoulli_data(2, 50, c(0.6, 0.4)) |>
#'   contingency_table(dv, iv, .rownames = TRUE))
#'
#' t |> chsqfish()
#'
#' (t <- bernoulli_data(3, 10, c(0.8, 0.5, 0.2)) |>
#'   contingency_table(dv, iv, .rownames = TRUE))
#'
#' t |> chsqfish()
#'
#' rm(t)

chsqfish <- function(...) {
    chsq <- chisq.test(...)

    structure(
        list(
            observed = chsq$observed,
            expected = chsq$expected,
            test = 
                if (min(chsq$expected) < 5)
                     chsq$observed |> fisher.test(simulate.p.value = T)
                else
                     chsq
        ),
        class = "chsqfish"
    )
}

# ========================================
#' @title
#' Stars for Statistical Significance
#'
#' @description
#' Stars for statistical significance with levels as usual in R. A vectorised function.
#' 
#' @param p A numeric vector of probabilities.
#' 
#' @return A `character vector`, length of \code{p}.
#' 
#' @export
#' @examples
#' (test_seq <- c(0.0003, 0.0010, 0.0032, 0.0100, 0.0316, 0.0500, 0.0631, 0.1000, 0.3162))
#' starsig(test_seq)
#' rbind(test_seq, as.character(starsig(test_seq)))
#' data.frame(val = test_seq, sig = starsig(test_seq))
#' 
#' rm(test_seq)

starsig <- function(p) {
    if (!is.numeric(p))
        stop("p must be numeric") 

    cut(p, c(0, 0.001, 0.01, 0.05, 0.1, 1), include.lowest = TRUE, right = FALSE) |>
    `levels<-`(c("***", "**", "*", ".", "NS"))
}

