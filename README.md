# Mcmcdb

**R** package for storing and accessing MCMC samples


## API

### Extract

```
x[i, j, k, ..., drop=TRUE]
```

Arguments 

- `i`: flat parameter names
- `j`: chains
- `k`: iterations
- `drop`: `logical` determines what data type is returned

Returns

If `drop=FALSE`, `data.frame` with columns

- `flatpar` : flat parameter name
- `chain_id` : chain 
- `iteration` : iteration
- `val` : parameter value

If `drop=TRUE`, `numeric` vector with the parameter values.

```
x[[i, j, k, ..., drop=TRUE]]
```

Arguments 

- `i`: parameter array names
- `j`: chains
- `k`: iterations
- `drop`: `logical` determines what data type is returned

Returns

If `drop=FALSE`, `data.frame` with columns

- `flatpar` : flat parameter name
- `pararray`: parameter array name
- `chain_id` : chain 
- `iteration` : iteration
- `value` : parameter value

If `drop=TRUE`, named `list` of `array` objects for each parameter
array.  Each array has dimensions (dimensions of parameter array,
chains * iterations).

```
x$i
```

Arguments

- `i`: parameter array name

Returns

`array` with dimensions (dimensions of parameter array, chains *
iterations).

### ply functions

#### mcmc_iply

Apply a function to each iteration. 
The iteration is in the same shape as the original data.

#### mcmc_fply

Apply a function to flat parameters. 

#### mcmc_aply

Apply a function to parameter arrays  ?? What shape

#### mcmc_cply

Apply a function to chains

### Accessors

- `mcmcdb_chains`: `data.frame`
- `mcmcdb_parameters`: `McmcParameters`
- `mcmcdb_pararrays`: `list`
- `mcmcdb_pardims`: `list`
- `mcmcdb_par_indices`: `list`
- `mcmcdb_flatpars`: `character`
- `mcmcdb_iters`: `data.frame`
- `mcmcdb_flatpar_chains`: `data.frame`
- `mcmcdb_metadata`: `list`

- `mcmcdb_data`:
- `mcmcdb_init`: 


# Notes

Relevant SQL reserved keywords, `iterate`, `chain`, `parameter`,
`parameters`, `value`.

- Classes are capitalized, and CamelCase
- Functions and methods are all lower and "_", and not "."
- Internal functions which will be used as an S4 method are named
  as if they were S3 methods.
- External classes and methods use "mcmcdb_" prefix.

