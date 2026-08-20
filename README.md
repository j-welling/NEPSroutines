
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

You can install NEPSroutines in different ways:

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

#### Load the Package

Once installed, you can load the package like any other R package:

``` r
library(NEPSroutines)
```

### Getting Started

Scaling NEPS data using R in general and NEPSroutines in particular is
documented using **vignettes** within this package. Start with the
**introductory vignette** by using the function `open_guide()`. There
you will get acquainted with the documentation structure of the package
and learn how to access other vignettes.

### Example data and scripts

NEPSroutines provides example (simulated) data and scripts for users.
The vignette `example_data_and_scripts` gives an overview and explains
how to access them.

## Information for Developers

If you want to contribute to the package, please read first the
CONTRIBUTING file in the inst/ folder.

### Installation of Development Version

You can install the development version of NEPSroutines from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("LIfBi-Educational-Measurement/NEPSroutines")
```

### Installation from Local Clone

If you cloned the repository to your local computer, first switch to the
repository folder and to the branch you want to use. Then install the
package from the local source with vignettes:

``` r
install.packages("devtools")
devtools::install("PATH/TO/DIRECTORY/NEPSroutines", build_vignettes = TRUE)
```

Replace `PATH/TO/DIRECTORY` with the path to the folder that contains
the cloned `NEPSroutines` repository.

The argument `build_vignettes = TRUE` is required if you want to call
the vignettes after installing the package from a local clone.
