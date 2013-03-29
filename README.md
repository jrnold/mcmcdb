# mcmcdb

Stop wrangling and start analyzing your MCMC samples!

This **R** package was born out of my own frustrations with dealing
with MCMC samples.  I was wasting too much time wrangling the MCMC
samples in order to get what I needed for my analyses.

Sometimes the MCMC samples are needed in their flat form, e.g. in
order to calculate convergence statistics. Sometimes the samples are
needed in their original dimensions, e.g. in order to calculate the
predicted values or other functions of the parameters. Sometimes, the
samples need to be seperated by chain, e.g. convergence diagnostics.
Sometimes the samples needed to be pooled across chains,
e.g. calculating quantiles.

This package aims to do one thing: make storing, accessing, and
manipulating MCMC samples easier and faster.  It does not include
convergence statistics, or plotting functions; although it will make
applying such functions to samples easier.

It defines generic functions to access the MCMC samples in a common
manner, regardless of how they are stored. This makes is possible to
store samples in a variety of formats, while accessing them with the
same functions. Currently, this package only contains a class for
storing samples in memory. However, SQLite and other backends are
planned.

# Install

Use [devtools](https://github.com/hadley/devtools) to install **mcmcdb** from github.

```r
library(devtools)
install_github(c(r-"checker", "mcmcdb"), "jrnold")
```

# Usage


`McmcdbWide` objects can be created directly, however it is more
likely that MCMC samples are either written to disk or in an R object
of another format. The function `McmcdbWide` will create new
`McmcdbWide` from matrix, `mcmc`, and `mcmc.list` objects.  The
function `mcmcdb_wide_from_stan` will create a `McmcdbWide` object
from the csv files output by a Stan command line program.

