## code to prepare `budworm` dataset goes here

## W.N. Venables and B.D. Ripley
## Modern Applied Statistics with S
## Fourth Edition 2002.
## DOI 10.1007/978-0-387-21706-2
## © 2002 Springer Science+Business Media New York
##
## 7. Generalized Linear Models p. 190

budworm <- data.frame(
	ldose = rep(0:5, 2),
	sex = factor(rep(c("M", "F"), c(6, 6))),
	numdead = c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
)

budworm$numalive <- 20 - budworm$numdead

usethis::use_data(budworm, overwrite = TRUE)
