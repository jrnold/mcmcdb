#' @include package.R
#' @export read_stan_csv
NULL

## setClass("StanSamples",
##          contains = "matrix",
##          representation(stan_version_major = "integer",
##                         stan_version_minor = "integer",
##                         stan_version_patch = "integer",
##                         data = "character",
##                         init = "character",
##                         append_samples = "integer",
##                         save_warmup = "integer",
##                         # the seed can be larger than .Machine$integer.max
##                         seed = "character", 
##                         chain_id = "integer",
##                         iter = "integer",
##                         warmup = "integer",
##                         thin = "integer",
##                         nondiag_mass = "integer",
##                         equal_step_sizes = "integer",
##                         leapfrog_steps = "integer",
##                         max_treedepth = "integer",
##                         epsilon = "integer",
##                         epsilon_pm = "integer",
##                         delta = "numeric",
##                         gamma = "numeric",
##                         # Part of adaptation phase
##                         step_size = "numeric",
##                         step_size_multipliers = "numeric",
##                         cov_matrix = "matrix",
##                         # Per iteration 
##                         rejected = "logical",
##                         warmup = "logical",
##                         treedepth = "integer",
##                         stepsize = "numeric"
##                         ))

# For Stan 1.2.0 
parse_stan_header <- function(file) {
  lines <- readLines(file)
  
  header_classes <-
    c(stan_version_major = "integer",
      stan_version_minor = "integer",
      stan_version_patch = "integer",
      data = "character",
      init = "character",
      append_samples = "integer",
      save_warmup = "integer",
      # the seed can be larger than .Machine$integer.max
      seed = "character", 
      chain_id = "integer",
      iter = "integer",
      warmup = "integer",
      thin = "integer",
      nondiag_mass = "integer",
      equal_step_sizes = "integer",
      leapfrog_steps = "integer",
      max_treedepth = "integer",
      epsilon = "integer",
      epsilon_pm = "integer",
      delta = "numeric",
      gamma = "numeric",
      # Part of adaptation phase
      step_size = "numeric")
  
  comments <- lines[str_detect(lines, "^#")]

  header <- list()
  ## select lines where key=value
  eq_lines <- na.omit(str_match(comments, "# +(.*?)=(\\S+)"))
  for (i in 1:nrow(eq_lines)) {
    keyname <- gsub(" ", "_", eq_lines[i, 2])
    value <- as(eq_lines[i, 3], header_classes[keyname])
    header[[keyname]] <- value
  }
  
  ## Saved parameters can be longer than step size multipliers because
  ## they include generated quantities.
  parln <- lines[which(str_detect(lines[1:min(50, length(lines))], "^lp__,"))]
  header[["colnames"]] <- str_split(parln, ",")[[1]]

  ## Sample or Optimization
  if (!is.na(str_match(lines[1], "Samples"))) {
    header[["point_estimate"]] <- FALSE
  } else if (!is.na(str_match(lines[1], "Point Estimate"))) {
    header[["point_estimate"]] <- TRUE
  } else {
    print(sprintf("First line is unrecognized:\n%s", lines[1]))
    header[["point_estimate"]] <- NA
  }

  #Adaption method
  adaptation <- na.omit(str_match(comments,
                                  "\\((.*?)\\) adaptation finished"))[ , 2]
  if (length(adaptation)) {
    header[["adaptation_type"]] <- adaptation
    if (adaptation == "mcmc::nuts_nondiag") {
      cov_line <- which(str_detect(comments, "estimated covariance matrix:"))
      cov_sz <- str_count(comments[cov_line + 1], ",")
      cov_matrix <- matrix(NA_real_, cov_sz, cov_sz)
      for (i in seq_len(cov_sz)) {
        row <- str_sub(comments[cov_line + i], 2L, -2L)
        cov_matrix[i, ] <- as.numeric(str_split(row, ",")[[1]])
      }
      header[["covariance_matrix"]] <- cov_matrix
    } else if (adaptation == "mcmc::nuts_diag") {
      parameter_mass_pat <- "parameter step size multipliers:"
      parameter_mass_line <-
        comments[which(str_detect(comments, parameter_mass_pat)) + 1]
      step_size_multipliers <-
        as.numeric(str_split(str_sub(parameter_mass_line, 3), ",")[[1]])
      header[["step_size_multipliers"]] <- step_size_multipliers
    }
  } else {
    header[["adaptation_type"]] <- NA_character_
  }

  header
}


#' Read csv output from command line Stan
#'
#' Read csv files produced by Stan.
#'
#' This returns both the sample values and the metadata in the comments of the file.
#' This function has been tested for the output of Stan 1.2.0.
#'
#' @param file character. name of an output file produced by a STAN model.
#' 
#' @return A \code{data.frame} with attributes
#' \describe{
#' \item{\code{header}}{A \code{list} containing header information}
#' \item{\code{rejected}}{A \code{logical} vector indicating whether the iteration was
#' a rejected sample (for samples only)}
#' \item{\code{warmup}}{A \code{logical} vector indicating whether the iteration was in
#' the warmup.}
#' }
read_stan_csv <- function(file) {
  header <- parse_stan_header(file)
  pointest <- header[["point_estimate"]]
  colClasses <- "numeric"
  ## ncolumns <- length(header[["colnames"]])
  ## if (!pointest) {
  ##   colClasses <- c("numeric", "integer", "numeric", rep("numeric", ncolumns - 3))
  ## } else {
  ##   colClasses <- rep("numeric", ncolumns)
  ## }
  x <- as.matrix(read.csv(file, header=TRUE,
                          comment.char="#", colClasses=colClasses))
  niter <- nrow(x)
  if (!pointest) {
    attr(x, "rejected") <- unname(c(FALSE, apply(x[2:niter, ]
                                                 == x[1:(niter-1), ], 1, all)))
    if (header[["save_warmup"]]) {
      warmup <- header[["warmup"]]
      thin <- header[["thin"]]
      attr(x, "warmup") <- seq_len(niter) <= ceiling(warmup / thin)
    } else {
      attr(x, "warmup") <- rep(FALSE, niter)
    }
  } else {
    attr(x, "warmup") <- c(rep(seq_len(niter - 1), TRUE), FALSE)
  }
  attr(x, "header") <- header
  x
}

mcmcdb_wide_stan_one <- function(file) {
  samples <- read_stan_csv(file)
  header <- attr(samples, "header")
  rejected <- attr(samples, "rejected")
  warmup <- attr(samples, "warmup")
  # Drop non-parameter columns, but leave log-posterior
  treedepth <- samples[ , "treedepth__"]
  stepsize <- samples[ , "stepsize__"]
  samples <- samples[ , setdiff(colnames(samples), c("treedepth__", "stepsize__"))]
  chain_id <- header[["chain_id"]]
  ## CHAINS
  chains <- data.frame(chain_id = chain_id)
  ## flatpar_chains <- data.frame(chain_id = chain_id,
  ##                              flatpar = colnames(data))
  ## TODO : put step_size_multipliers and covariance in here
  for (i in seq_along(header)) {
    key <- names(header)[i]
    ## TODO: add cov_matrix to flatpar_chains
    if (key %in% c("colnames", "covariance_matrix")) {
      next
    }
    val <- header[[i]]
    chains[[key]] <- val
  }
  if ("step_size_multipliers" %in% names(header)) {
    flatpar_chains <- data.frame(chain_id = chain_id,
                                 flatpar = colnames(samples),
                                 step_size_multipliers = NA)
    step_sizes <- header[["step_size_multipliers"]]
    flatpar_chains[ 2:(1 + length(step_sizes)), "step_size_multipliers"] <-
      step_sizes
  } else if ("covariance_matrix" %in% names(header)) {
    ## flatpar_chains <- data.frame(chain_id = chain_id,
    ##                              flatpar = colnames(samples))
    ## cov_matrix <- header[["covariance_matrix"]]
    ## d <- dim(cov_matrix)[1]
    ## for (i in seq_along(colnames(samples)[2:(1 + d)])) {
    ##   flatpar_chains[[paste("cov", i, sep=".")]] <- 1
    ## }
    flatpar_chains <- NULL
  } else {
    flatpar_chains <- NULL
  }

  iters <- data.frame(chain_id = chain_id,
                      iter = seq_len(nrow(samples)),
                      treedepth = treedepth,
                      stepsize = stepsize,
                      rejected = rejected,
                      warmup = warmup)
  list(samples = samples, chains = chains, iters = iters,
       flatpar_chains = flatpar_chains)
}

#' Create McmdbWide from Stan csv
#'
#' Create McmdbWide from Stan csv
#'
#' This returns both the sample values and the metadata in the comments of the file.
#' This function has been tested for the output of Stan 1.2.0.
#'
#' @param file character. Name of an output file produced by a STAN model.
#' 
#' @return An object of class \code{"McmcdbWide"}
mcmcdb_wide_from_stan <- function(file) {
  data <- llply(file, mcmcdb_wide_stan_one)
  samples <- do.call(rbind, llply(data, `[[`, i = "samples"))
  chains <- do.call(rbind, llply(data, `[[`, i = "chains"))
  iters <- do.call(rbind, llply(data, `[[`, i = "iters"))
  if (!is.null(data[[1]][["flatpar_samples"]])) {
    flatpar_chains <- McmcdbFlatparChains(do.call(rbind,
                                                  llply(data, `[[`,
                                                        i = "flatpar_chains")))
  } else {
    flatpar_chains <- NULL
  }
  McmcdbWide(samples, chains = McmcdbChains(chains),
             iters = McmcdbIters(iters),
             parameters = mcmc_parparser_stan,
             flatpar_chains = flatpar_chains)
             
}
