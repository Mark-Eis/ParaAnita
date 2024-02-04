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

The **ParaAnita R package** includes functions intended to address and simplify a number of issues commonly encountered during binary (Bernoulli) and binomial data analysis using generalised linear models. More specifically, *ParaAnita* does the following: – 

- Summarises binary and binomial proportion data in contingency tables with `contingency_table()`, `xcontingency_table()`, `binom_contingency()`.

- Calculates odds ratios and their confidence intervals and associated probabilities with `odds_ratio()`.

- Gets, sets or removes the contrasts attribute for selected categorical variables (`factors`) within data with `get_contrasts()`, `get_contr_data()`, `set_contrasts()`, [`set_contrasts<-()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.html), `set_contr_treat()` and [`set_contr_treat<-()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.html).

- Gets, sets and manipulates categorical variable contrast `names` with `contr_colnames()`, [`contr_colnames<-()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.html), [`contr_colpfx<-()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.html), `helm_names()` and [`helm_names<-()`](https://mark-eis.github.io/ParaAnita/reference/helm_names.html).

- Compares related generalised linear models using various measures with `anova_tbl()`, `comp_glm()`, `summanov()` and `univ_anova()`.

- Collates model results and standard errors, with optional grouping of levels of selected categorical variables, in a format convenient for plotting with `glm_plotlist()` and `glm_plotdata()`, and plots these in individual or faceted plots with `ggplot.glm_plotdata()` and `var_labs()`.

- Adds, modifies, removes or selects factors in data with `add_grps()`, `drop_null()`, `drop_zero()`, `expl_fcts()`, `fct_to_num()`, and `good_levels()`.

- Simulates Bernoulli and binomial proportion data sets with categorical explanatory variables with `bernoulli_data()` and `binom_data()`.

- Simplifies statistical analysis with `chsqfish()` and `starsig()`.

- Provides auxiliary print functions and prints objects derived from ParaAnita S3 methods with `announce()`, `lf()`, `print_all()` and `print_lf()`.

- Tidies up the R workspace with `rm_objects()`.

– Dataset: `budworm`
