# First R Package
# Mark Eisler Feb 2024
# For Anita Rabaza
#
# Requires R version 4.2.0 (2022-04-22) -- "Vigorous Calisthenics" or later
#
# utils.R

# ========================================
# chr_or_fct
# Predicate function
# Not exported
chr_or_fct <- function()
    force(\(x) is.factor(x) | is.character(x))

# ========================================
# is_datapro
# Is value .data pronoun
# Not exported
is_datapro <- function(x) {
    if (class(x) == "call" && length(x) == 3 && length(x[[1]]) == 1) {
        fun <- match.fun(x[[1]])
	    all(
	        identical(fun, `$`) || identical(fun, `[[`),
	        x[[2]] == quote(.data)
	    )
    } else FALSE
}

# ========================================
# rm_datapro
# Remove .data pronoun
# Not exported
rm_datapro <- function(x) {
    if (is_datapro(x)) {
        fun <- match.fun(x[[1]])
        x <- x[[3]]
        if (identical(fun, `[[`))
            x <- as.symbol(x)
    }
    x
}
