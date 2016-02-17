# Inspiration: https://github.com/hadley/readxl/blob/master/R/read_excel.R
# Rewrote read_excel() to read_excel2() with additional argument 'n_max' for number
# of rows to evaluate in function readxl:::xls_col_types and
# readxl:::xlsx_col_types()
# This is probably an unstable solution, since it calls internal functions from readxl.
# May or may not survive next update of readxl. Seems to work in version 0.1.0
#' Read xls and xlsx files.
#'
#' @description Import excel files into R similar to readxl::read_excel,
#'   but with all rows evaluated in xls_col_types and xlsx_col_types
#' @import readxl
#' @inheritParams readxl::read_excel
#' @param n_max Max number of rows to evalutate for internal function xls_col_types and xlsx_col_types. Defaults to all rows (1050000L)
#' @seealso \code{\link[readxl]{read_excel}}
#' @export

read_excel2 <- function(path, sheet = 1, col_names = TRUE, col_types = NULL,
                       na = "", skip = 0, n_max = 1050000L) {

  path <- readxl:::check_file(path)
  ext <- tolower(tools::file_ext(path))

  switch(readxl:::excel_format(path),
         xls =  read_xls2(path, sheet, col_names, col_types, na, skip, n_max),
         xlsx = read_xlsx2(path, sheet, col_names, col_types, na, skip, n_max)
  )
}

#' @keywords internal
read_xls2 <- function(path, sheet = 1, col_names = TRUE, col_types = NULL,
                     na = "", skip = 0, n_max = n_max) {

  sheet <- readxl:::standardise_sheet(sheet, readxl:::xls_sheets(path))

  has_col_names <- isTRUE(col_names)
  if (has_col_names) {
    col_names <- readxl:::xls_col_names(path, sheet, nskip = skip)
  } else if (readxl:::isFALSE(col_names)) {
    col_names <- paste0("X", seq_along(readxl:::xls_col_names(path, sheet)))
  }

  if (is.null(col_types)) {
    col_types <- readxl:::xls_col_types(
      path, sheet, na = na, nskip = skip, has_col_names = has_col_names, n = n_max
    )
  }

  readxl:::xls_cols(path, sheet, col_names = col_names, col_types = col_types,
                    na = na, nskip = skip + has_col_names)
}

#' @keywords internal
read_xlsx2 <- function(path, sheet = 1L, col_names = TRUE, col_types = NULL,
                       na = "", skip = 0, n_max = n_max) {
  path <- readxl:::check_file(path)
  sheet <-
    readxl:::standardise_sheet(sheet, readxl:::xlsx_sheets(path))

  if (is.null(col_types)) {
    col_types <-
      readxl:::xlsx_col_types(
        path = path, sheet = sheet, na = na, nskip = skip + isTRUE(col_names), n = n_max
      )
  }

  readxl:::read_xlsx_(path, sheet, col_names = col_names, col_types = col_types, na = na,
             nskip = skip)
}
