row_map_at <- function(.tbl, .colname, .vars, .fun, ...) {

  colname <- rlang::enquo(.colname)

  coldata <- purrr::pmap(select(.tbl, !!! .vars), .fun, ...)

  coldata_lengths <- purrr::map_dbl(coldata, length)

  if(min(coldata_lengths) == 1 & max(coldata_lengths) == 1) {
    coldata <- unlist(coldata)
  }

  res <- dplyr::mutate(.tbl, !!colname := coldata)

  res

}


row_sum_at <- function(.tbl, .colname, .vars, ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, sum, ...)

  res

}


row_mean_at <- function(.tbl, .colname, .vars, ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, mean, ...)

  res

}

row_concatenate_at <- function(.tbl, .colname, .vars, sep = ", ", ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, utils$str_c_na_rm, sep = sep, ...)

  res

}

utils <- list(


  str_c_na_rm = function(..., sep = ", ") {

    x <- c(...)

    if(all(is.na(x))) {
      res <- NA_character_
    } else {
      res <- oaste0(x[!is.na(x)], collapse = sep)
    }

    res

  }

)

