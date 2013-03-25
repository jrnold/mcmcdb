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

`line_samples` is a `McmcdbWide` object which is included in the
package for examples.

```
data(line_samples)
line_samples
```

The object include two parameter arrays `beta`, a vector of length 2,
and `tau`, a scalar (vector of length 1). It has 2 chains, each with 
100 samples. 

# Methods

## Samples



## Extract

The idiomatic **R** extract operators are defined.  A single bracket
extracts a flat parameter. There are three flat parameters for this
object: `beta[1]`, `beta[2]` and `tau`. The names of the flat parameters,
and their associated parameter arrays can be found with,
```
mcmcdb_flatpars(line_samples)
```

To extract all samples for `beta.1` use,
```
str(line_samples["beta[1]"])
```

To extract the 1st iteration in chain 2 for `beta.1` use,
```
line_samples["beta[1]", 1, 2]
```

If `drop=FALSE`, then instead of a numeric vector, a data frame is
returned, 
```
summary(line_samples["beta[1]", drop=FALSE])
```

While the single bracket is used to extract by flat parameters, 
the double bracket is used to extract by parameter arrays.

To extract all samples of `beta`, 
```
str(line_samples[["beta"]])
```

To extract the first chain, iterations 1-10 of `beta`, 
```
str(line_samples[["beta", 1, 1:10]])
```

To return the values in a data frame instead of an array, use 
`drop=FALSE`,
```
line_samples[["beta", 1, 1:3, drop=FALSE]]
```

Use the dollar operator to extract all samples for a given parameter
array,
```
str(line_samples$beta)
```

# Install

Use [devtools](https://github.com/hadley/devtools) to install **mcmcdb** from github.

```
library(devtools)
install_github(c("DataFrameConstr", "mcmcdb"), "jrnold")
```

