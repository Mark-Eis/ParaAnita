# ParaAnita
### R Functions for Binary and Binomial Data Analysis

**Author:** Mark C. Eisler

**eMail:** Mark.Eisler@bristol.ac.uk

**ORCID** = [0000-0001-6843-3345](https://orcid.org/0000-0001-6843-3345)

## Installation

You can install the development version of ParaAnita from [GitHub](https://github.com/) with:
      
``` r
# install.packages("devtools")
devtools::install_github("Mark-Eis/ParaAnita")
```

### ParaAnita Package Description: –
Summarises binary (Bernoulli) and binomial proportion data in contingency tables with `contingency_table()`, `xcontingency_table()`, `binom_contingency()`.

Calculates odds ratios and their confidence intervals and associated probabilities with `odds_ratio()`.

Gets, sets or removes the contrasts attribute for selected categorical variables (`factors`) within data with `get_contrasts()`, `get_contr_data()`, `set_contrasts()`, `set_contrasts()<-`, `set_contr_treat()` and `set_contr_treat()<-`.

Gets, sets and manipulates contrast `names` with `contr_colnames()`, `contr_colnames()<-`, `contr_colpfx()<-`, `helm_names()` and `helm_names()<-`.

Compares related univariable models of data using various measures with `anova_tbl()`, `comp_glm()`, `summanov()` and `univ_anova()`.

Arranges data with standard errors and optional groupings of levels of selected categorical variables in a format convenient for plotting with `glm_plotlist()` and `glm_plotdata()`, and plots the data in individual or faceted plots with `ggplot.glm_plotdata()` and 
`var_labs()`.

Creates simulated Bernoulli and binomial proportion data sets with categorical explanatory variables with `bernoulli_data()` and `binom_data()`.

Provides various auxiliary functions to simplify these tasks with `add_grps()`, `announce()`, `chsqfish()`, `drop_null()`, `drop_zero()`, `expl_fcts()`, `fct_to_num()`, `good_levels()`, `lf()`, `print_all()`, `print_lf()`,  `rm_objects()` and `starsig()`.
