# forcis 1.0.1

Answers to CRAN comments:

* `DESCRIPTION` file
  * Change package title
  * Single quote FORCIS database
  * Add authors and year in reference

* Examples
  * Replace `\dontrun{}` by `\donttest{}`
  * Store database in temporary directory

# forcis 1.0.0

Answers to editor and reviewer comments:

* BREAKING CHANGE
  * Remove the usage of the hidden dotfile `.forcis` - [[#87](https://github.com/FRBCesab/forcis/pull/87)]

* `DESCRIPTION` file
  * Add reviewer 2 as `rev` - [[#111](https://github.com/FRBCesab/forcis/issues/111)]
  * Add FRB-CESAB as `fnd` - [[#69](https://github.com/FRBCesab/forcis/issues/69)]
  * Set minimal version of R to 4.1.0 (pipe) - [[285c37](https://github.com/FRBCesab/forcis/commit/285c37af7ed2c0605bc88aebcca6bc72b790fe2c)]
  * Move `dplyr` from *Imports* to *Suggests* - [[#79](https://github.com/FRBCesab/forcis/issues/79)]

* R code
  * Refactor code of `read_*_data()` functions - [[#85](https://github.com/FRBCesab/forcis/pull/85)]
  * Homogeneize function outputs (`tibble`) - [[#82](https://github.com/FRBCesab/forcis/pull/82)]
  * Use the same prefix `forcis_` for global options - [[#81](https://github.com/FRBCesab/forcis/pull/81)]
  * Improve and standardize function examples - [[#99](https://github.com/FRBCesab/forcis/pull/99), [#103](https://github.com/FRBCesab/forcis/pull/103)]
  * Refactor `compute_*()` functions in base R - [[#79](https://github.com/FRBCesab/forcis/issues/79)]

* Documentation
  * Improve vignettes - [[#64](https://github.com/FRBCesab/forcis/issues/64), [#65](https://github.com/FRBCesab/forcis/issues/65), [#66](https://github.com/FRBCesab/forcis/issues/66), [#107](https://github.com/FRBCesab/forcis/issues/107), [#108](https://github.com/FRBCesab/forcis/issues/108), [#112](https://github.com/FRBCesab/forcis/issues/112)]
  * Add a statement of need in `README` - [[#84](https://github.com/FRBCesab/forcis/pull/84)]
  * Add two figures in `README` to better explain the package - [[#105](https://github.com/FRBCesab/forcis/pull/105), [#106](https://github.com/FRBCesab/forcis/pull/106)]

* Unit tests
  * Improve unit tests - [[#67](https://github.com/FRBCesab/forcis/issues/67), [#68](https://github.com/FRBCesab/forcis/issues/68), [#79](https://github.com/FRBCesab/forcis/pull/79), [#88](https://github.com/FRBCesab/forcis/pull/88), [#101](https://github.com/FRBCesab/forcis/pull/101), [#118](https://github.com/FRBCesab/forcis/pull/118)]

* Miscellaneous
  * Recommend to use [`air`](https://github.com/posit-dev/air) in `CONTRIBUTING.md` to automatically format code - [[#80](https://github.com/FRBCesab/forcis/pull/80)]
  * Add a favicon to the website - [[#115](https://github.com/FRBCesab/forcis/pull/115)]
  * Add a `dependabot.yml` - [[#90](https://github.com/FRBCesab/forcis/pull/90)]
  


# forcis 0.1.0

First stable release.

> N.B. This release has been submitted to [rOpenSci](https://github.com/ropensci/software-review/issues/660) for review.
