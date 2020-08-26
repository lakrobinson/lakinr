#' Read data from FARS csv file.
#'
#' @param filename A csv data file
#'
#' @return Outputs a data.frame from csv file. An error occurs if the file does not exist.
#'
#' @examples
#' \dontrun{
#' accident_2015 <- fars_read("accident_2015.csv.bz2")
#' }
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}


#' Make file name
#'
#' creates a name for the accident zipped csv.bz2 file based on the {year} argument.
#'
#' @param year numerical input to indicate the year of the data
#'
#' @return returns a file name based on the year
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS data files based on the provided years.
#'
#' @param years numerical inputs to indicate the years of the data
#'
#' @return The function uses the make_filename function and returns a data.frame based on the
#' multiple provided years under csv format. An error occurs if the input year does not exist.
#'
#' @seealso \code{\link{make_filename}}
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013:2015)
#' }
#'
#' @importFrom dplyr %>% mutate select
#'
#' @export
#'
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}


#' Summarize the number of observations by year
#'
#' Read multiple data files with different years from FARS and
#' summarise the number of observations by month and year.
#'
#' @param years numerical input to indicate the years of the data
#'
#' @return Output the data frame with monthly number of accidents (row) and selected years
#' (column).
#'
#' @seealso \code{\link{fars_read_years}}
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2014:2015)
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize %>%
#' @importFrom tidyr spread
#'
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}


#' Visualize the accidents in the US map
#'
#' Plot the accidents on the US map for a given state and year.
#'
#' @param state.num State number
#' @param year Selected year(s)
#'
#' @return \code{fars_map_state} Output a plot of accidents for the selected
#'   state and year. An error occurs if the state or year does not exist in the
#' data set.
#'
#' @seealso
#' \code{\link{make_filename}}
#' \code{\link{fars_read}}
#'
#' @examples
#' \dontrun{
#' fars_map_state(4, 2014)
#' fars_map_state(5, 2015)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @export

fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}
