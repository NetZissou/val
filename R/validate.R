#' Validate input data with the provided schema
#'
#' @param data data to be validated
#' @param schema pre-specified schema
#' @param quiet TRUE when validate without console output, only return the invalid stack table
#'
#' @return a list with success indicator and stack of error messages
#' @export
#'
validate <- function(data, schema, quiet = FALSE) {

  result <-
    list(
      valid = TRUE,
      invalid_stack_tbl = NULL
    )

  # Validate column structures
  validate_col_structure(
    data = data,
    schema = schema,
    quiet = quiet
  )

  # Validate each observation
  result$invalid_stack_tbl <-
    validate_obs(
      data = data,
      schema = schema
    )

  n_invalid_obs <- nrow(result$invalid_stack_tbl)
  if (n_invalid_obs > 0) {

    result$valid <- FALSE

    if (!quiet) {
      cat(
        crayon::red(
          glue::glue(
            "{n} problems found when validating the data",
            n = n_invalid_obs
          )
        ), sep = "\n"
      )
    }
  } else {

    n_obs <- nrow(data)
    if (!quiet) {
      cat(
        crayon::green(
          glue::glue(
            "All {n} observations were successfully validated!",
            n = n_obs
          )
        ), sep = "\n"
      )
    }
  }

  return(result)
}

#' Validate certain cols of the input data with the provided schema
#'
#' @inheritParams validate
#' @param ... columns to be validated
#'
#' @returna a list with success indicator and stack of error messages
#' @export
validate_col <- function(data, ..., schema, quiet = FALSE) {

  data <- data %>%
    dplyr::select(
      ...
    )

  cols_data <- names(data)
  cols_schema <- purrr::map_chr(schema, "name")

  if (all(cols_data %in% cols_schema)) {

    # Filter Schema
    schema <-
      purrr::keep(
        .x = schema,
        .p = function(x, cols) {
          x$name %in% cols
        },
        cols = cols_data
      )

  } else {

    stop(
      glue::glue(
        "Columns {cols} not specified in the provided schema.",
        cols = setdiff(cols_data, cols_schema)
      )
    )
  }

  # Check if column is specified in the provided schema
  validate_result <- validate(data = data, schema = schema, quiet = quiet)
  return(validate_result)
}

#' Wrapper function for `validate`
#'
#' @description
#' This function will load all columns as characters and then use the `validate`` function to check if
#' the data could be parsed based on the specified schema
#'
#' @param file data file to be validated
#' @param schema pre-specified schema
#' @param quiet TRUE when validate without console output, only return the invalid stack table
#'
#' @return a list with success indicator and stack of error messages
#' @export
validate_file <- function(file, schema, quiet = FALSE) {

  raw_data <-
    readr::read_csv(
      file = file,
      col_types = list(.default = readr::col_character())
    )

  validate_result <- validate(data = raw_data, schema = schema, quiet = quiet)
  validate_result$file <- file

  return(
    validate_result
  )
}


#' Assit function for readr `col_types` parameters
#'
#' @param schema pre-specified schema
#'
#' @return list of col types
#' @export
generate_col_types <- function(schema) {

  purrr::map(
    .x = schema,
    .f = function(x) {
      switch (x$type,
              string = {col_type <- readr::col_character()},
              int = {col_type <- readr::col_integer()},
              double = {col_type <- readr::col_double()},
              boolean = {col_type <- readr::col_logical()},
              date_time = {
                order <- x$params$parse_order
                col_type <- readr::col_datetime(format = order)
              },
              date = {
                order <- x$params$parse_order
                col_type <- readr::col_date(format = order)
              }
      )

      return(col_type)
    }
  ) %>%
    purrr::set_names(
      purrr::map(schema, "name")
    )
}

#' Validate column structures
#'
#' @inheritParams validate
#'
#' @export
validate_col_structure <- function(data, schema, quiet = FALSE) {

  # ==================================== #
  # ---- Validate Column Structures ----
  # ==================================== #

  # 1) Check if all cols in data were specified in schema
  # 2) Check if all cols in schema were found in data

  cols_data <- names(data)
  cols_schema <- purrr::map_chr(schema, "name")

  if (setequal(cols_data, cols_schema)) {
    if (!quiet) {
      cat(
        crayon::green(
          glue::glue(
            "Incoming data structure is valid!"
          )
        ),
        sep = "\n"
      )
    }
  } else {
    if (!quiet) {
      # Report missing columns
      cat(
        crayon::red(
          glue::glue(
            "Missing column: {conflict_cols}",
            conflict_cols = setdiff(cols_schema, cols_data)
          )
        ), sep = "\n"
      )
      # Report un-specified columns
      cat(
        crayon::red(
          glue::glue(
            "Unspecified column: {conflict_cols}",
            conflict_cols = setdiff(cols_data, cols_schema)
          )
        ), sep = "\n"
      )
    }
    stop("Incoming data structure does not align with pre-specified schema")
  }
}



#' Validate each observation
#'
#' @inheritParams validate
#'
#' @export
validate_obs <- function(data, schema) {

  invalid_stack_tbl <- tibble::tibble(
    obs_index = integer(),
    invalid_col = character(),
    desc = character()
  )
  # Make sure columns are in the same order as the schema
  cols_schema <- purrr::map_chr(schema, "name")
  data <-
    data %>%
    dplyr::select(cols_schema)

  # Validate Observation {i}
  for (i in 1:nrow(data)) {
    obs <- data[i,]
    for (j in 1:ncol(data)) {
      obj <- obs[[j]]
      obj_schema <- schema[[j]]

      # Validate object {i,j}
      val_desc <- validate_object(obj, obj_schema)

      if (!is.null(val_desc)) {
        invalid_stack_tbl <-
          invalid_stack_tbl %>%
          dplyr::add_row(
            obs_index = i,
            invalid_col = cols_schema[j],
            desc = val_desc
          )
      }
    }
  }
  return(invalid_stack_tbl)
}

#' Validate the lowest object level
#'
#' @param object the cell object in the incoming data
#' @param object_schema the prespecified schema for this object
#'
#' @return a description if the object is invalid
validate_object <- function(object, object_schema) {

  desc <- NULL
  # 1) Check: Mandatory
  if (object_schema$mandatory & is.na(object)) {
    desc <- "Object is mandatory but the value is missing."
    return(desc)
  }

  # 2) Check: type
  # Skip the check if the object is NA
  if (!is.na(object)) {

    switch (object_schema$type,

            # ================ #
            # > Date time ----
            # ================ #
            date_time = {
              suppressWarnings(
                parsed_obj <-
                  readr::parse_datetime(
                    object,
                    format = object_schema$params$parse_order,
                    locale = readr::locale(tz = "US/Eastern")
                  )
              )

              # Fail to parse
              if (is.na(parsed_obj)) {
                desc <- "Unable to parse object into the prespecified datetime format."
                return(desc)
              }

              start_date <- lubridate::as_date(object_schema$params$start_date)
              if (object_schema$params$end_date == "now") {
                end_date <- Sys.Date()
              } else {
                end_date <-lubridate::as_date(object_schema$params$end_date)
              }

              # Out of bound
              if (!(parsed_obj >= start_date & parsed_obj <= end_date)) {
                desc <- "Object date is out of specified time boundary."
                return(desc)
              }
            },

            # =========== #
            # > Date ----
            # =========== #
            date = {
              suppressWarnings(
                parsed_obj <-
                  readr::parse_datetime(
                    object,
                    format = object_schema$params$parse_order,
                    locale = readr::locale(tz = "US/Eastern")
                  )
              )

              # Fail to parse
              if (is.na(parsed_obj)) {
                desc <- "Unable to parse object into the prespecified datetime format."
                return(desc)
              }

              start_date <- lubridate::as_date(object_schema$params$start_date)
              if (object_schema$params$end_date == "now") {
                end_date <- Sys.Date()
              } else {
                end_date <-lubridate::as_date(object_schema$params$end_date)
              }

              # Out of bound
              if (!(parsed_obj >= start_date & parsed_obj <= end_date)) {
                desc <- "Object date is out of specified time boundary."
                return(desc)
              }
            },

            # ============== #
            # > Integer ----
            # ============== #
            int = {
              suppressWarnings(
                parsed_obj <-
                  readr::parse_integer(object)[1]
              )

              # Fail to parse
              if (is.na(parsed_obj)) {
                desc <- "Unable to parse object into the prespecified integer format."
                return(desc)
              }

              if (!is.null(object_schema$params)) {

                min <- object_schema$params$min
                max <- object_schema$params$max

                if (!(parsed_obj >= min & parsed_obj <= max)) {
                  desc <- "Object integer is out of specified boundary."
                  return(desc)
                }
              }
            },

            # ============= #
            # > Double ----
            # ============= #
            double = {
              suppressWarnings(
                parsed_obj <-
                  readr::parse_double(object)[1]
              )

              # Fail to parse
              if (is.na(parsed_obj)) {
                desc <- "Unable to parse object into the prespecified double format."
                return(desc)
              }

              if (!is.null(object_schema$params)) {

                min <- object_schema$params$min
                max <- object_schema$params$max

                if (!(parsed_obj >= min & parsed_obj <= max)) {
                  desc <- "Object double is out of specified boundary."
                  return(desc)
                }
              }
            },

            # ============== #
            # > Boolean ----
            # ============== #
            boolean = {
              suppressWarnings(
                parsed_obj <-
                  readr::parse_logical(object)[1]
              )

              # Fail to parse
              if (is.na(parsed_obj)) {
                desc <- "Unable to parse object into the prespecified boolean format."
                return(desc)
              }
            },

            # ================ #
            # > Character ----
            # ================ #
            string = {
              if (!is.null(object_schema$params)) {

                allowable_values <- object_schema$params$elements
                if (!(object %in% allowable_values)) {
                  desc <- "Object value is not in the prespecified allowable values."
                  return(desc)
                }
              }
            }

    )
  }

  return(desc)
}

