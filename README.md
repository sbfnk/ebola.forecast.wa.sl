
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Assessing the performance of real-time epidemic forecasts: A case study of the 2013–16 Ebola epidemic

This repository contains the data and code for our preprint:

> Sebastian Funk, Anton Camacho, Adam J. Kucharski, Rachel Lowe,
> Rosalind M. Eggo and W. John Edmunds. Assessing the performance of
> real-time epidemic forecasts: A case study of the 2013-16 Ebola
> epidemic. bioRxiv 177451. Online at \<<https://doi.org/10.1101/177451>
> \>.

### How to download or install

You can download the compendium as a zip from from this URL:
</archive/master.zip>

Or you can install this compendium as an R package,
ebola.forecast.wa.sl, from GitHub with:

``` r
devtools::install_github("sbfnk/ebola.forecast.wa.sl")
```

### Included data sets

The package includes five data sets. One of them contains the data of
Ebola cases from Western Area that was used for the analysis. It can be
loaded with

``` r
data(ebola_wa)
```

The other four data sets contain Monte-Carlo samples from the
semi-mechanistic model used for forecasts, as well as the three null
models. The data sets are called `samples_semi_mechanistic`,
`samples_bsts`, `samples_deterministic` and `samples_deterministic`, and
they can be loaded with

``` r
data(samples)
```

The data sets from the null models can be re-created using

``` r
samples_bsts <- null_model_bsts()
samples_deterministic <- null_model_deterministic()
samples_unfocused <- null_model_unfocused()
```

### Table and figures

The table and figures in the manuscript can be re-created using

``` r
table1()
figure1()
figure2()
figure3()
figure4()
```
