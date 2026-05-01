# Package index

## Contingency Tables

Summarise binary and binomial proportion data in contingency tables.

- [`binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
  [`as_binom_contingency()`](https://mark-eis.github.io/ParaAnita/reference/binom_contingency.md)
  : Binomial Contingency Table for Data with a Binary Outcome
- [`contingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md)
  [`xcontingency_table()`](https://mark-eis.github.io/ParaAnita/reference/contingency_table.md)
  : Contingency Tables for Two or More Categorical Variables

## Compare Models

Compare related generalised linear models using anova and other
measures.

- [`anova_tbl()`](https://mark-eis.github.io/ParaAnita/reference/anova_tbl.md)
  : Create Tibble from List of Anovas
- [`comp_glm()`](https://mark-eis.github.io/ParaAnita/reference/comp_glm.md)
  : Compare Series of Nested GLMs
- [`summanov()`](https://mark-eis.github.io/ParaAnita/reference/summanov.md)
  : List of Summary and Analysis of Deviance Objects for Related
  Univariable GLMs
- [`univ_anova()`](https://mark-eis.github.io/ParaAnita/reference/univ_anova.md)
  : Analyses of Deviance Summarising Fits of Univariable GLMs

## Contrasts

### Contrast Attribute

Get, set or remove the contrasts attribute for selected categorical
variables.

- [`get_contr_data()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md)
  [`set_contr_treat()`](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md)
  [`` `set_contr_treat<-`() ``](https://mark-eis.github.io/ParaAnita/reference/get_contr_data.md)
  : Get and Set Treatment Contrasts for Independent Variables in Data
- [`get_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md)
  [`set_contrasts()`](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md)
  [`` `set_contrasts<-`() ``](https://mark-eis.github.io/ParaAnita/reference/get_contrasts.md)
  : Get and Set Contrasts Matrix for an Independent Variable in Data

### Contrast Names

Get, set and manipulate contrast names.

- [`contr_colnames()`](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md)
  [`` `contr_colnames<-`() ``](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md)
  [`` `contr_colpfx<-`() ``](https://mark-eis.github.io/ParaAnita/reference/contr_colnames.md)
  : Get and Set Contrast Matrix Column Names
- [`helm_names()`](https://mark-eis.github.io/ParaAnita/reference/helm_names.md)
  [`` `helm_names<-`() ``](https://mark-eis.github.io/ParaAnita/reference/helm_names.md)
  : Create and Set Names for Helmert Contrasts

## Factors

### Factor Levels

Enumerate, remove or select factors in data.

- [`good_levels()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md)
  [`drop_zero()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md)
  [`drop_null()`](https://mark-eis.github.io/ParaAnita/reference/good_levels.md)
  : Levels of Independent Variable where a Bernoulli Dependent Variable
  is Neither All Success Nor All Failure
- [`expl_fcts()`](https://mark-eis.github.io/ParaAnita/reference/expl_fcts.md)
  : Explanatory Factors in Data as List of Expressions
- [`levels_data()`](https://mark-eis.github.io/ParaAnita/reference/levels_data.md)
  [`nlevels_data()`](https://mark-eis.github.io/ParaAnita/reference/levels_data.md)
  : Levels of all Factors in Data

### Manipulate Factors

Group or transform factors in data.

- [`add_grps()`](https://mark-eis.github.io/ParaAnita/reference/add_grps.md)
  : Add Factors to Data Based on Grouped Levels of an Existing Factor
- [`fct_to_num()`](https://mark-eis.github.io/ParaAnita/reference/fct_to_num.md)
  : Factor as Numeric

## Odds Ratios

Calculate odds ratios, their confidence intervals and associated
probabilities.

- [`odds_ratio()`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md)
  [`formula(`*`<odds_ratio>`*`)`](https://mark-eis.github.io/ParaAnita/reference/odds_ratio.md)
  : Odds Ratios, Standard Errors, Confidence Intervals and P-Values for
  Binomial GLMs

## Plot Models

Plot model results and standard errors, with optional grouping of levels
of selected categorical variables.

- [`glm_plotlist()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotlist.md)
  : Data for Plotting Univariable GLM Predictions and Error Bars for
  Multiple Independent Variables
- [`glm_plotdata()`](https://mark-eis.github.io/ParaAnita/reference/glm_plotdata.md)
  : Collate Data for Plotting Univariable GLM Predictions with Error
  Bars
- [`ggplot(`*`<glm_plotdata>`*`)`](https://mark-eis.github.io/ParaAnita/reference/plot_model.md)
  : Plot Model Predictions with Error Bars for Univariable GLM
- [`var_labs()`](https://mark-eis.github.io/ParaAnita/reference/var_labs.md)
  : Format or Lookup Variable Names for Plot Titles

## Printing

Auxilliary print functions and printing of objects derived from
ParaAnita S3 methods.

- [`announce()`](https://mark-eis.github.io/ParaAnita/reference/announce.md)
  : Announce Class for Consistent Printing
- [`lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md)
  [`print_lf()`](https://mark-eis.github.io/ParaAnita/reference/lf.md) :
  Pipe-Friendly Line Feeds and Printing
- [`print_all()`](https://mark-eis.github.io/ParaAnita/reference/print_all.md)
  : Print All (or More) of an Object
- [`print(`*`<announce>`*`)`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
  [`print(`*`<binom_contingency>`*`)`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
  [`print(`*`<contingency_table>`*`)`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
  [`print(`*`<odds_ratio>`*`)`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
  [`print(`*`<summ_anov>`*`)`](https://mark-eis.github.io/ParaAnita/reference/Print_Methods.md)
  : S3 Print Methods

## Simulate Data

Simulate Bernoulli and binomial proportion data sets with categorical
explanatory variables.

- [`bernoulli_data()`](https://mark-eis.github.io/ParaAnita/reference/Simulate_Data.md)
  [`binom_data()`](https://mark-eis.github.io/ParaAnita/reference/Simulate_Data.md)
  : Simulated Bernoulli and Binomial Proportion Data

## Statistical Auxiliary Functions

Simplify statistical analysis with auxiliary functions.

- [`chsqfish()`](https://mark-eis.github.io/ParaAnita/reference/chsqfish.md)
  : Chi-Squared or Fisher's Exact Test
- [`starsig()`](https://mark-eis.github.io/ParaAnita/reference/starsig.md)
  : Stars for Statistical Significance

## Tidy up workspace

Clear the workspace of related objects such as similarly named models.

- [`rm_objects()`](https://mark-eis.github.io/ParaAnita/reference/rm_objects.md)
  : Remove Sequentially Numbered Objects from Workspace

## Data

Datasets provided in ParaAnita.

- [`budworm`](https://mark-eis.github.io/ParaAnita/reference/budworm.md)
  : Budworm Data
