# ParaAnita

### R Functions for Binary and Binomial Data Analysis

**Author:** Mark C. Eisler

**eMail:** <Mark.Eisler@bristol.ac.uk>

**ORCID** = [0000-0001-6843-3345](https://orcid.org/0000-0001-6843-3345)

## Installation

You can install the development version of ParaAnita from
[GitHub](https://github.com/) with:

``` r

# install.packages("pak")
pak::pkg_install("Mark-Eis/ParaAnita")
```

### ParaAnita Package Description: –

The **ParaAnita R package** includes functions intended to address and
simplify a number of issues commonly encountered during binary
(Bernoulli) and binomial data analysis using generalised linear models.
More specifically, *ParaAnita* does the following: –

- Creates contingency tables with
  [`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md),
  [`xcontingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md).

- Summarises binary (Bernoulli) and binomial proportion data in
  contingency tables with
  [`as_binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md),
  [`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md).

- Calculates odds ratios, their confidence intervals and associated
  probabilities with
  [`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md).

- Gets, sets or removes the contrasts attribute for selected categorical
  variables (`factors`) within data with
  [`get_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md),
  [`get_contr_data()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md),
  [`set_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md),
  [`set_contrasts<-()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.html),
  [`set_contr_treat()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md)
  and
  [`set_contr_treat<-()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.html).

- Gets, sets and manipulates categorical variable contrast `names` with
  [`contr_colnames()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md),
  [`contr_colnames<-()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.html),
  [`contr_colpfx<-()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.html),
  [`helm_names()`](https://mark-eis.github.io/ParaAnita/reference/helm_names.md)
  and
  [`helm_names<-()`](https://mark-eis.github.io/ParaAnita/reference/helm_names.html).

- Compares related generalised linear models using various measures with
  [`anova_tbl()`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md),
  [`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md),
  [`summanov()`](https://mark-eis.github.io/ParaAnita/reference/summanov.md)
  and
  [`univ_anova()`](https://mark-eis.github.io/ParaAnita/reference/univ_anova.md).

- Collates model results and standard errors, with optional grouping of
  levels of selected categorical variables, in a format convenient for
  plotting with
  [`glm_plotlist()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotlist.md)
  and
  [`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md),
  and plots these in individual or faceted plots with
  [`ggplot.glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md)
  and
  [`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md).

- Adds, modifies, removes or selects factors in data with
  [`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md),
  [`drop_null()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md),
  [`drop_zero()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md),
  [`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md),
  [`fct_to_num()`](https://mark-eis.github.io/ParaAnita/reference/fct_to_num.md),
  and
  [`good_levels()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md).

- Simulates Bernoulli and binomial proportion data sets with categorical
  explanatory variables with
  [`bernoulli_data()`](https://mark-eis.github.io/ParaAnita/reference/Simulate_Data.md)
  and
  [`binom_data()`](https://mark-eis.github.io/ParaAnita/reference/Simulate_Data.md).

- Simplifies statistical analysis with
  [`chsqfish()`](https://mark-eis.github.io/ParaAnita/reference/chsqfish.md)
  and
  [`starsig()`](https://mark-eis.github.io/ParaAnita/reference/starsig.md).

- Provides auxiliary print functions and prints objects derived from
  ParaAnita S3 methods with
  [`announce()`](https://mark-eis.github.io/ParaAnita/reference/announce.md),
  [`lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md),
  [`print_all()`](https://mark-eis.github.io/ParaAnita/reference/print_all.md)
  and
  [`print_lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md).

- Tidies up the R workspace with
  [`rm_objects()`](https://mark-eis.github.io/ParaAnita/reference/rm_objects.md).

- Includes the dataset:
  [`budworm`](https://mark-eis.github.io/ParaAnita/reference/budworm.html),
  from David Collett (1991). *Modelling Binary Data*. London: Chapman &
  Hall.
