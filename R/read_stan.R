#' @include package.R
#' @export read_stan_csv
NULL

#' @docType class
#' @title Class \code{StanSamples}
#'
#' @description Class representing samples generated
#' by the Stan command line program.
setClass("StanSamples",
         contains = "matrix",
         representation(stan_version_major = "integer",
                        stan_version_minor = "integer",
                        stan_version_patch = "integer",
                        data = "character",
                        init = "character",
                        append_samples = "logical",
                        save_warmup = "logical",
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
                        step_size = "numeric",
                        step_size_multipliers = "numeric",
                        cov_matrix = "matrix",
                        point_estimate = "logical",
                        adaptation_type = "character",
                        # Per iteration 
                        rejected = "logical",
                        is_warmup = "logical",
                        treedepth = "integer",
                        stepsize = "numeric"
                        ))

# For Stan 1.2.0 
parse_stan_header <- function(file) {
  lines <- readLines(file)
  comments <- lines[str_detect(lines, "^#")]
  header <- list()
  ## select lines where key=value
  eq_lines <- na.omit(str_match(comments, "# +(.*?)=(\\S+)"))
  for (i in 1:nrow(eq_lines)) {
    keyname <- gsub(" ", "_", eq_lines[i, 2])
    value <- eq_lines[i, 3]
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
#' @return An object of class \code{"\linkS4class{StanSamples}"}.
read_stan_csv <- function(file) {
  header <- parse_stan_header(file)
  colClasses <- "numeric"
  x <- as.matrix(read.csv(file, header=TRUE,
                          comment.char="#", colClasses=colClasses))
  treedepth <- as.integer(x[ , "treedepth__"])
  stepsize <- x[ , "stepsize__"]
  x <- new("StanSamples",
           x[ , !colnames(x) %in% c("treedepth__", "stepsize__")])
  x@treedepth <- treedepth
  x@stepsize <- stepsize
  niter <- nrow(x)
  slots <- getSlots(getClass("StanSamples"))
  for (i in seq_along(header)) {
    slotname <- names(header)[i]
    slotval <- header[[i]]
    if (slotname %in% names(slots)) {
      if (slotname %in% c("append_samples", "save_warmup")) {
        slotval <- as.logical(as.integer(slotval))
      } else {
        slotval <- as(slotval, slots[slotname])
      }
      slot(x, slotname) <- slotval
    } else {
      if (! slotname %in% "colnames") {
        warning(sprintf("%s found in csv header, but not in definition of StanSamples",
                        slotname))
      }
    }
  }
  if (!x@point_estimate) {
    slot(x, "rejected") <- unname(c(FALSE,
                                    apply(x[2:niter, ]
                                          == x[1:(niter-1), ], 1, all)))
    if (x@warmup) {
      slot(x, "is_warmup") <- seq_len(niter) <= ceiling(x@warmup / x@thin)
    } else {
      slot(x, "is_warmup") <- rep(FALSE, niter)
    }
  } else {
    slot(x, "warmup") <- c(rep(seq_len(niter - 1), TRUE), FALSE)
  }
  x
}

mcmcdb_wide_stan_one <- function(file) {
  samples <- read_stan_csv(file)
  chains <- data.frame(chain_id = samples@chain_id)
  exclude_slots <- c(".Data", "rejected", "is_warmup", "treedepth",
                     "stepsize")
  for (i in setdiff(slotNames(samples), exclude_slots)) {
    if (i %in% c("step_size_multipliers", "cov_matrix")) {
      chains[[i]] <- list(slot(samples, i))
    } else {
      chains[[i]] <- slot(samples, i)
    }
  }
  iters <- data.frame(chain_id = samples@chain_id,
                      iter = seq_len(nrow(samples)),
                      treedepth = samples@treedepth,
                      stepsize = samples@stepsize,
                      rejected = samples@rejected,
                      warmup = samples@warmup)
  list(samples = samples, chains = chains, iters = iters)
}

#' Create McmdbWide from Stan csv files
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
  McmcdbWide(samples, chains = McmcdbChains(chains),
             iters = McmcdbIters(iters),
             parameters = mcmc_parparser_stan)
}
