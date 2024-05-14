#' Replicate Reactions and Assign Unique Numeric Positions
#'
#' This function takes a vector of unique reaction identifiers and a number of replicates, and
#' assigns a unique numeric position to each reaction based on the specified ordering priority.
#'
#' @param reactions_vector A vector of unique reaction identifiers to be replicated.
#' @param num_replicates The number of times each reaction should be replicated.
#' @param priority The priority for positioning: "reaction" for reaction-first ordering,
#'                 or "replicate" for replicate-first ordering.
#' @param inter_spacing The numeric distance between the sets, either different reactions or replicates.
#' @param intra_spacing The numeric distance between replicates within a set.
#' @param start_position The starting numeric position for the first entry.
#'
#' @return A data frame with the columns 'reaction', 'replicate', and 'position'.
#'         Each row corresponds to a reaction replicate with a unique assigned position.
#'
#' @examples
#' data <- data.frame(rxn = 1:3)
#' data <- replicate_reactions(data, "rxn", num_replicates = 3, priority = "reaction",
#'                             inter_spacing = 10, intra_spacing = 1, start_position = 1)
#' data <- replicate_reactions(data, "rxn", num_replicates = 3, priority = "replicate",
#'                             inter_spacing = 5, intra_spacing = 1, start_position = 1)
#'
#' @export
replicate_reactions <- function(rxn_df,
                                reaction_col_name,
                                #reactions_vector,
                                num_replicates,
                                priority,
                                inter_spacing,
                                intra_spacing,
                                start_position) {

  if (!is.character(priority) || !priority %in% c("reaction", "replicate")) {
    stop("Priority must be either 'reaction' or 'replicate'", call. = FALSE)
  }
  if (!reaction_col_name %in% names(rxn_df)) {
    stop("The specified reaction column does not exist in the data frame", call. = FALSE)
  }

  reactions_vector <- rxn_df[[reaction_col_name]]

  # Initialize the dataframe with reactions and replicates
  df <- expand.grid(reaction = reactions_vector,
                    replicate = 1:num_replicates,
                    stringsAsFactors = FALSE)
  df <- df[order(df$reaction, df$replicate), ]

  # Create a position vector
  positions <- numeric(nrow(df))
  current_position <- start_position

  if (priority == "reaction") {
    # Sequentially assign positions based on reaction numbers
    for (r in unique(df$reaction)) {
      for (rep in 1:num_replicates) {
        positions[df$reaction == r & df$replicate == rep] <- current_position
        current_position <- current_position + intra_spacing
      }
      current_position <- current_position + inter_spacing - intra_spacing # Adjust for next reaction set
    }
  } else if (priority == "replicate") {
    # Sequentially assign positions based on replicates
    for (rep in 1:num_replicates) {
      for (r in unique(df$reaction)) {
        positions[df$reaction == r & df$replicate == rep] <- current_position
        current_position <- current_position + inter_spacing
      }
      current_position <- current_position + intra_spacing - inter_spacing # Adjust for next replicate set
    }
  }

  # Add the position information to the dataframe
  df$position <- positions

  # Add the input df information

  df <- dplyr::left_join(df, rxn_df, by = dplyr::join_by(reaction == {{reaction_col_name}}))
  names(df)[names(df) == "reaction"] <- reaction_col_name

  return(df)
}
