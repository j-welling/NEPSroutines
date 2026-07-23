
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NEPSroutines

<!-- badges: start -->

[![R-CMD-check](https://github.com/j-welling/NEPSroutines/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/j-welling/NEPSroutines/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

NEPSroutines is an R package that provides a collection of functions for
the scaling of NEPS competence data. It implements standardized scaling
routines for one- and two-parametric logistic models with binary and
polytomous responses.

## Features

- Data preparation and consistency checks
- Descriptive analyses
- Analysis of missing values (per person & per item)
- IRT analyses (1PL, 2PL, PCM, GPCM)
- DIF analyses
- Distractor analysis
- Dimensionality analysis
- Score creation (WLEs, sum scores, meta scores)
- SUF creation
- Quarto extension and functionalities for semi-automated technical
  reports

## Information for Users

### Installation

To make your work reproducible, install the latest official release of
the package. On the GitHub page of NEPSroutines, go to the panel
“Releases” on the right. There, you find the number of the latest
release (e.g., “Release v1.3.0”).

You can install NEPSroutines in two different ways:

#### TAR-Ball

Click on the page of the latest release. Download the source code in
TAR.GZ format. Install the TAR-Ball locally with:

``` r
install.packages(
  "PATH/TO/DIRECTORY/NEPSroutines-X.Y.Z.tar.gz",
  type = "source",
  repos = NULL
)
```

Replace PATH/TO/DIRECTORY with your local path and X.Y.Z with the actual
version number.

#### From GitHub

You can also install the newest version of NEPSroutines from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("LIfBi-Educational-Measurement/NEPSroutines@vX.Y.Z")
```

Replace X.Y.Z with the actual version number.

#### Load Package

Once installed, you can load the package like any other R package:

``` r
library(NEPSroutines)
```

### Getting Started

Scaling NEPS data using R in general and NEPSroutines in particular is
documented using **vignettes** within this package. Start with the
**introductory vignette** by using the function `open_guide()`.

## Information for Developers

If you want to contribute to the package, please read first the
DEVELOPMENT file in the inst/ folder.

You can install the development version of NEPSroutines from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("LIfBi-Educational-Measurement/NEPSroutines")
```
