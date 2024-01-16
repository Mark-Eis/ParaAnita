# First R Package
# Mark Eisler Jan 2024
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# remove-objects.R


# ========================================
#' Remove Sequentially Numbered Objects from Workspace
#'
#' Remove a series of sequentially named objects from the workspace or from another specified
#' environment. For example, conveniently remove a series of sequentially numbered models.
#'
#' `rm_objects()` lists all objects in the workspace (or another specified environment) whose
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
#' @return A character vector of matching names remaining in the calling `environment`, usually the workspace unless
#'   `rm_objects()` was called within a function, or another specified environment, returned invisibly.
#'
#' @keywords environment
#' @export
#' @examples
#'  ## Note: running outside example() will be more informative
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

rm_objects <- function(basename, suffixes, envir = parent.frame()) {
    basename <- deparse(substitute(basename)) 
    intro <- paste0("Objects matching \"", as.symbol(basename), "…\"")
    envirname <- environmentName(envir)
    envstr <- paste("in", if (nchar(envirname)) envirname else "unnamed", "environment:\n\t")
    objs <- expr(ls(envir, pattern = as.symbol(basename)))

    cat(intro, "found", envstr, eval(objs), "\n")
    rm(list = map_chr(suffixes, ~paste0(as.symbol(basename), .)), envir = envir)
    cat(intro, "remaining", envstr, eval(objs), "\n")
    invisible(eval(objs))
}

