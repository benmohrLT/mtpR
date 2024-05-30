#' Convert Well IDs to Row and Column Indices
#'
#' This function takes a dataframe and a column name containing well IDs (e.g., "A1", "B6", "AA1") and
#' returns the dataframe with two new columns for the row and column indices based on those well IDs.
#'
#' @param wells A dataframe containing a column with well IDs.
#' @param well_id A string representing the name of the column in `wells` with the well IDs.
#'
#' @return A tibble with two additional columns `Row` and `Column` for the numeric row and column indices.
#' @export
#'
#' @examples
#' wells_df <- data.frame(WellID = c("A1", "B6", "AA10", "AZ25"))
#' wells_df <- to_rows_columns(wells_df, "WellID")
#'

to_rows_columns <- function(wells, well_id) {

  # Validate input is correct
  if (!is.character(well_id) || length(well_id) != 1) {
    stop("well_id must be a single character string representing the column name.", call. = FALSE)
  }

  if (!is.data.frame(wells)) {
    stop("wells must be a dataframe or tibble.", call. = FALSE)
  }

  if (!all(grepl("^[A-Z]{1,2}\\d+$", wells[[well_id]]))) {
    stop("All well IDs must be in the format of one or two letters followed by digits.", call. = FALSE)
  }

  row_labels <- c(LETTERS, paste0(rep(LETTERS, each = 26), LETTERS))

  # Extract and convert well IDs to indices
  wells <- dplyr::mutate(wells,
                         Row = as.numeric(match(stringr::str_extract(toupper(.data[[well_id]]), "[A-Z]+"), row_labels)),
                         Column = as.numeric(stringr::str_extract(.data[[well_id]], "\\d+"))
  )

  # Handle NAs if any well IDs are out of range or incorrectly formatted
  if (any(is.na(wells$Row), is.na(wells$Column))) {
    stop("Some well IDs are incorrectly formatted or out of range.", call. = FALSE)
  }

  return(wells)
}

#' Renumber wells across multiple plates
#'
#' @param df Data frame containing plate well indexes.
#' @param well_index_column Name of the column with well indexes (as an unquoted string).
#' @param plate_size Number of wells per plate.
#'
#' @return Data frame with added `Plate.Index` indicating plate number
#'         and renumbered `Well.Index` within each plate.
#' @export
#' @importFrom dplyr mutate select across
#' @examples
#' reaction_data <- data.frame(Well.Index = 1:96)
#' updated_data <- add_plates(reaction_data, "Well.Index", 96)

add_plates <- function(df, well_index_column, plate_size) {
  if (!is.character(well_index_column) || length(well_index_column) != 1) {
    stop("well_index_column must be a single character string representing a column name.", call. = FALSE)
  }

  # Check if the well_index_column actually exists in the dataframe
  if (!well_index_column %in% names(df)) {
    stop("The specified well_index_column does not exist in the dataframe.", call. = FALSE)
  }

  df |>
    dplyr::mutate(
      Plate.Index = ceiling(.data[[well_index_column]] / plate_size),
      Well.Index = .data[[well_index_column]] - (Plate.Index - 1) * plate_size
    )
}

#' Replicate reactions across multiple plates
#'
#' This function takes a data frame of reactions and adds replicates for each reaction
#' based on the total number of plates. It adds an identifier for plate replicates.
#'
#' @param reactions A data frame containing reactions to be replicated across plates.
#' @param plate_replicate_number_total The total number of plate replicates.
#'
#' @return A data frame with reactions replicated across the specified number of plates,
#'         including a new identifier for each plate replicate.
#' @export
#' @importFrom tidyr uncount
#'
#' @examples
#' reactions_df <- data.frame(ReactionID = 1:10)
#' replicated_df <- add_plate_replicates(reactions_df, 3)
#'
add_plate_replicates <- function(reactions, plate_replicate_number_total) {
  stopifnot(is.data.frame(reactions), is.numeric(plate_replicate_number_total))

  reactions_with_replicates <- tidyr::uncount(reactions,
                                              weights = plate_replicate_number_total,
                                              .remove = FALSE,
                                              .id = "PlateReplicate.Number"
  )

  return(reactions_with_replicates)
}
