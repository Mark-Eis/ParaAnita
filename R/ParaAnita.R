# First R Package
# Mark Eisler Dec 2023
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# ParaAnita.R


# ========================================
#' Remove Sequentially Numbered Objects from Workspace
#'
#' Remove a series of sequentially named objects from the workspace or from another specified
#' environment. For example, conveniently remove a series of sequentially numbered models.
#'
#' \code{rm_objects()} lists all objects in the workspace (or another specified environment) whose
#' names start with \code{basename}, then removes any in which \code{basename} is followed by
#' an element included in \code{suffixes}, and finally lists all remaining objects with names
#' matching \code{basename}.
#'
#' @seealso \code{\link[base]{ls}} and \code{\link[base]{rm}}.
#'
#' @param basename Common base name (quoted) of the series of objects.
#' @param suffixes A numeric or character vector representing the suffixes of the series of objects.
#' @param envir An environment from which to remove objects. Use \code{.GlobalEnv} for the workspace; default
#' \code{caller_env()}.
#'
#' @return A character vector of matching names remaining in the workspace or another specified
#' environment, returned invisibly.
#'
#' @keywords environment
#' @export
#' @examples
#'
#'  ## Create some sequentially numbered objects
#'  model1 <- model2 <- model3 <- model4 <- lm(1~1)
#'  ls(pattern = "model")
#'
#'  ## Remove three of them
#'  rm_objects(model, 1:3)
#'
#'  ## Create some sequentially named objects
#'  model_a <- model_b <- model_c <- model_d <- lm(1~1)
#'  ls(pattern = "model_")
#'
#'  ## Remove three of them
#'  rm_objects(model_, letters[1:3])
#'
#'  ## Use within a function
#'  (\() {                  ## Anonymous function, but doesn't have to be
#'    model1 <- model2 <- model3 <- model4 <- model5 <- lm(1~1)
#'    rm_objects(model, 1:5)
#'  })()
#'
#'  ls(pattern = "model")
#'
#'  rm_objects(model, c(4, "_d"))

rm_objects <- function(basename, suffixes, envir = rlang::caller_env()) {
    basename <- enquo(basename)
    intro <- paste0("Objects matching \"", as_name(basename), "\u2026\"")
    envirname <- rlang::env_name(envir)
    envstr <- paste("in", ifelse(identical(envirname, ""), "this", envirname), "environment:\n\t")
    objs <- expr(ls(envir, pattern = as_name(basename)))

    cat(intro, envstr, eval(objs), "\n")
    rm(list = map_chr(suffixes, ~ paste0(as_name(basename), .)), envir = envir)
    cat(intro, "remaining", envstr, eval(objs), "\n")
    invisible(eval(objs))
}

# ========================================
#' @title
#' Chi-Squared or Fisher's Exact Test
#' 
#' @description
#' Test input data using Chi-squared or Fisher's exact test as appropriate. 
#'
#' @details
#' Uses \code{\link[stats]{chisq.test}()} to calculate expected values and then applies Chi-squared test if all
#' expected values are 5 or greater, otherwise applies \code{\link[stats]{fisher.test}()}, and
#' \code{\link[stats]{fisher.test}}
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
#' @return A character vector, length of \code{p}.
#' 
#' @export
#' @examples
#' (test_seq <- round(10^seq(-4, 0, 0.5), 4))
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

