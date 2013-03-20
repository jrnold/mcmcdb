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
convergence statistics, or plotting functions; although it makes make
writing such functions easier.

It defines classes and some generic functions.  Classes are used to
abstract how the samples; this means it will be possible to store MCMC
samples in different formats, e.g. in memory, SQLite, HDF5, etc, while
still using the same functions to access the data. 

## Examples

```
library(mcmcdb)
```

# Install

Use [devtools](https://github.com/hadley/devtools) to install **mcmcdb** from github.

```
library(devtools)
install_github(c("DataFrameConstr", "mcmcdb"), "jrnold")
```

`line_samples` is a `McmcdbWide` object which is included in the
package for examples.

```
data(line_samples)
line_samples
```

The object include two parameter arrays `beta`, a vector of length 2,
and `tau`, a scalar (vector of length 1). And samples from two 




