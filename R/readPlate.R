#' Read Plates
#'
#' @param dat A data frame containing plate shaped data with Column names and Row names based on plate shape
#' @param plate.size The size of plate for this set of wells
#'
#' @return A data frame with two columns (wells and values) representing the resulting data loaded in a long format
#' @examples
#' SamplePlateData <- readRDS(system.file("extdata", "SamplePlateShapedData.rds", package = "mtpR"))
#' readPlate(SamplePlateData, plate.size = 96)
#'
#' @export


readPlate <- function(dat, plate.size = 96){

# Evaluating plate size to know how to split
  mtpR::platesize_check(plate.size) -> plate.size.df

 # Check for row names
  if (stringr::str_detect((toString(dat[1])), "[[:alpha:]]*")) {
    print((dat[1]))
    # add to a counter to include the first row
    n <- 1
  } else {
    print("no headers")
    n <- 0
  }


 # Trying to appropriately pull and reassign the first column name
 # Based on whether it looks like things are named in the input
 # For now, require a strict input with the col names and lettered rows
  if (stringr::str_detect(string = colnames((dat[1])),
                          pattern = "[[:alpha:]]")
      ) {
    value.name <- toString(colnames(dat[1]))
  } else {
    value.name <- "values"
  }
  first.col.name <- toString(colnames(dat[1]))

  # print(value.name)
  # print(first.col.name)

  # Trying to keep the native column and data names where possible
  # Need to add more rigor here.
  dat.p <- dat |>
    tidyr::pivot_longer(
      cols = colnames(dat[(1+n):(plate.size.df$colmax+n)],
        ),
      values_to = "values",
      names_to = "well.name"
      ) |>
    dplyr::mutate(
      well.num = stringr::str_extract(well.name,
                                      pattern = "\\d+"),
      well = stringr::str_c((!!as.name(first.col.name)), well.num)
           ) |>
    # print() |>
    dplyr::select(-c(well.name,
                     !!as.name(first.col.name),
                     well.num,
                     )
                  ) |>
    dplyr::mutate(!!first.col.name := values) |>
    dplyr::select(-c(values))

  return(dat.p)
  }
