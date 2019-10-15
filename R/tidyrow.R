#' Apply a function rowwise for selected variables in a table
#' and store the results in a new column
#'
#' @param .tbl A dataframe or tibble
#' @param .colname The name of the column to be added
#' @param .vars Columns to apply over
#' @param .fun Function to apply
#' @param ... Additional arguments to .fun
#'
#' @return
#' @export
#'
#' @examples
row_map_at <- function(.tbl, .colname, .vars, .fun, ...) {

  .colname <- enquo(.colname)

  coldata <- purrr::pmap(select(.tbl, !!! .vars), .fun, ...)

  coldata_lengths <- purrr::map_dbl(coldata, length)

  if(min(coldata_lengths) == 1 & max(coldata_lengths) == 1) {
    coldata <- unlist(coldata)
  }

  res <- .tbl %>%

    mutate(
      !!.colname := coldata
    )

  res

}


#' Sum tidyselected columns rowwise and store the result as a
#' column in the dataframe
#'
#' @param .tbl A dataframe or tibble
#' @param .colname The name of the column to be added
#' @param .vars Columns to apply over
#' @param ... Additional arguments to sum (e.g. na.rm = TRUE)
#'
#' @return
#' @export
#'
#' @examples
row_sum_at <- function(.tbl, .colname, .vars, ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, sum, ...)

  res

}


#' Average tidyselected columns rowwise and store the result as a
#' column in the dataframe
#'
#' @param .tbl A dataframe or tibble
#' @param .colname The name of the column to be added
#' @param .vars Columns to apply over
#' @param ... Additional arguments to mean (e.g. na.rm = TRUE)
#'
#' @return
#' @export
#'
#' @examples
row_mean_at <- function(.tbl, .colname, .vars, ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, mean, ...)

  res

}

#' Concatenate selected columns rowwise and store the result in a
#' new column in the dataframe
#'
#' @param .tbl A dataframe or tibble
#' @param .colname The name of the column to be added
#' @param .vars Columns to apply over
#' @param sep Separator for concatenation, defaults to ', '
#' @param ... Additional arguments to str_c
#'
#' @return
#' @export
#'
#' @examples
row_concatenate_at <- function(.tbl, .colname, .vars, sep = ", ", ...) {

  colname <- rlang::enquo(.colname)

  res <- row_map_at(.tbl, !! colname, .vars, utils$str_c_na_rm, sep = sep, ...)

  res

}

utils <- list(


  str_c_na_rm = function(..., sep = ", ") {

    x <- c(...)

    x[x == ""] <- NA_character_

    if(all(is.na(x))) {
      res <- NA_character_
    } else {
      res <- paste0(x[!is.na(x)], collapse = sep)
    }

    res

  }

)

tidy_print <- function(.tbl = quo(.data)) {

  print(.tbl)

  res <- 1

  res

}
