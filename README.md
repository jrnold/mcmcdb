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

- `parname` : flat parameter name
- `chain_id` : chain 
- `iteration` : iteration
- `value` : parameter value

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

- `parname` : flat parameter name
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

- `mcmciply`: apply to iterations
- `mcmcfply`: apply to flat parameters
- `mcmcaply`: apply to parameter arrays
- `mcmccply`: apply to chains


### Accessors

- `get_chains`: `data.frame`
- `get_parameters`: `McmcParameters`
- `get_chain_iters`: `data.frame`
- `get_par_chains`: `data.frame`
- `get_metadata`: `list`


# Notes

Relevant SQL reserved keywords, `iterate`, `chain`, `parameter`, `parameters`.
