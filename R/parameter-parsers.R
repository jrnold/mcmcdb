expand.paste <- function (..., sep = "") {
    apply(expand.grid(...), 1, paste, collapse = sep)
}

parse_parameter_names_default <- function(x) {
    result <- data.frame(name=x,
                         index=rep("1", length(x)),
                         stringsAsFactors=FALSE)
    rownames(result) <- x
    result
}

parse_parameter_names_stan <- function(x) {
    result <- data.frame(str_split_fixed(x, fixed("."), n=2),
                          stringsAsFactors=FALSE)
    colnames(result) <- c("name", "index")
    rownames(result) <- x
    result$index <- str_replace_all(result[ , 2], fixed("."), fixed(","))
    result$index[result$index == ""] <- "1"
    result
}

parse_parameter_names_bugs <- function(x) {
    result <-
        data.frame(str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)],
                   stringsAsFactors=FALSE)
    colnames(result) <- c("name", "index")
    result$index[result$index == ""] <- "1"
    rownames(result) <- x
    result
}

zeros <- function(dim) {
    if (length(dim) <= 1) {
        numeric(dim)
    } else if (length(dim) == 2) {
        matrix(0, nrow=dim[1], ncol=dim[2])
    } else {
        array(0, dim)
    }
}

parsed_to_template <- function(x) {
    split_indices <- function(indices, n_dim) {
        str_split_fixed(indices, fixed(","), n_dim)
    }
    dlply(x, "name",
          function(y) {
              n_dim <- str_count(y$index[1], fixed(",")) + 1
              if (n_dim > 1) {
                  print(split_indices(y$index, n_dim))
                  y_dim <- apply(split_indices(y$index, n_dim),
                                 2, function(z) max(as.integer(z)))
              } else {
                  y_dim <- max(as.integer(y$index))
              }
              zeros(y_dim)
          })
}

parsed_to_parameter_slot <- function(x) {
    result <-
        alply(x, 1, function(y) {
            list(index=eval(parse(text=sprintf("c(%s)", y$index))),
                 parameter=y$name)
        })
    names(result) <- rownames(x)
    attributes(result)[c("split_type", "split_labels")] <- NULL
    result
}

preprocess_parameters <- function(x) {
    list(template=parsed_to_template(x),
         parameters=parsed_to_parameter_slot(x))
}
