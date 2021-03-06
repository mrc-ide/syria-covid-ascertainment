
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.0.3-brightgreen.svg)](https://cran.r-project.org/)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](http://choosealicense.com/licenses/mit/)

## Research compendium for COVID-19 ascertainment report in Damascus

This is a working R compendium (think R package but for reproducible
analysis). The analysis directory contains R scripts used to generate
the results.

### Installation

    git clone https://github.com/mrc-ide/syria-covid-ascertainment.git
    cd syria-covid-ascertainment
    open syriaascertainment.Rproj
    devtools::install_deps()

### Overview

The structure within analysis is as follows:

    analysis/
        |
        ├── analysis.R            # analysis script
        ├── wafiat.R              # script used in wafiat analysis
        |
        ├── 02_09 /               # analysis outputs run for 2nd September
        ├── wafiat_analysis /     # analysis outputs for wafiat analysis
        |
        ├── graphics/             # figures generated via graphics
        |
        ├── data/
        │   ├── DO-NOT-EDIT-ANY-FILES-IN-HERE-BY-HAND
        │   ├── raw/       # data obtained from elsewhere
        │   └── derived/   # data generated during the analysis

The `src` directory contains the orderly tasks, which are used for much
of the simulation work with `squire`. The `run` directory contains a
bash script for running the orderly tasks in parallel.

### Compendium DOI:

<http://doi.org/10.5281/zenodo.4030018>

The files at the URL above will generate the results as found in the
publication. The files hosted at
github.com/mrc-ide/syria-covid-ascertainment are the development
versions and may have changed since the report was published

### Overview of contents

This repository is our research compendium for our analysis of
<https://mrc-ide.github.io/syria-covid-ascertainment/>. The compendium
contains all data, code, and text associated with the publication. The
`R` files in the `analysis` directory contain details of how all the
analyses reported in the paper were conducted, as well as instructions
on how to rerun the analysis to reproduce the results. The `data/`
directory in the `analysis/` directory contains all the raw and derived
data generated.

### The R package

This repository is organized as an R package. There are no/negligable R
functions exported in this package - the majority of the R code is in
the analysis directory. The R package structure is here to help manage
dependencies, to take advantage of continuous integration, and so we can
keep file and data management simple.

To download the package source as you see it on GitHub, for offline
browsing, use this line at the shell prompt (assuming you have Git
installed on your computer):

``` r
git clone https://github.com/mrc-ide/syria-covid-ascertainment.git
```

Once the download is complete, open the `syriacovidascertainment.Rproj`
in RStudio to begin working with the package and compendium files. We
will endeavour to keep all package dependencies required listed in the
DESCRIPTION. This has the advantage of allowing
`devtools::install_deps(".", upgrade = "never")` to install the required
R packages needed to run the code in this repository

### Licenses

Code: [MIT](http://opensource.org/licenses/MIT) year: 2020, copyright
holder: OJ Watson

Data: [CC-0](http://creativecommons.org/publicdomain/zero/1.0/)
attribution requested in reuse
