mcmc_from_csv <- function(file, start=1, end=NULL, thin=1,  ...) {
    x <- as.matrix(read.csv(file, ...))
    if (is.null(end)) {
        end <- nrow(x)
    }
    mcmc(x, start=start, end=end, thin=thin)
}

mcmc_list_from_csv <- function(file, start=1, end=NULL, thin=1,  ...) {
    new("mcmc.list",
        mapply(mcmc_from_csv, file, start, end, thin))
}
