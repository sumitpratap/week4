# Sumit Pratap Pradhan
# Coursera "Building R Packages"
# Week 2 Assigment -- Documenting Functions
# 27MAR2018

#' Read FARS Data.
#'
#' This function reads data from the US National Highway
#' Traffic Safety Administration's [Fatality Analysis
#' Reporting System](https://www.nhtsa.gov/Data/Fatality-
#' Analysis-Reporting-System-(FARS)), given a filename for the
#' data. It returns a tibble of the data. For this function to
#' work properly, a filename pointing to an existing file must
#' be given.
#'
#' \code{fars_read} reads FARS data in R envirnoment.
#'
#' @param 
#'		filename Name of file that contains FARS data
#'
#' @return This function returns a data frame containing the
#' 		FARS data. If an incorrect filename is entered the
#'		function will stop and print 'does not exist'
#'		message.
#'   
#' @examples
#' \dontrun{
#' fars_read(accident_2014.csv.bz2)
#' }
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


#' Make FARS Filename.
#'
#' This function takes a year as input and produces a FARS
#' filename. Enter values as per requirement 
#'
#' \code{make_filename} This will return FARS
#'		filename based on year passed.
#'
#' @param 
#'		year value of the year
#'
#' @return This function returns a a FARS filename. 
#'   
#' @examples
#' \dontrun{
#' 		make_filename(2013) 
#'		}
#' @export

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' Read FARS files for one or more years.
#'
#' This function takes a vector of years, pulls the FARS data #' for those years, and then produces a summary data frames. #' The summary data frames shows the number of observations
#' for each month/year combination for the extracted FARS
#' data.
#'
#' \code{fars_read_years} produces a list of data frames of
#' FARS data, based on input vector of years passed as
#' arguments.
#'
#' @param 
#'		years vector of years
#'
#' @return This function returns a list of tibbles, where each #' tibble contains containing the year and month from the
#' observations in the corresponding year's FARS data.
#'   
#' @importFrom magrittr "%>%"
#'
#' @examples
#' \dontrun{
#' fars_read_years(years = c(2013, 2014, 2015))
#' fars_read_years(years = 2013)
#'	}
#'
#' @export

fars_read_years <- function(years) {
        lapply(years, function(year) {

			MONTH<-NULL
		

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


#' Produce a Summary of FARS Files.
#'
#' This function takes a vector of years as input and produces #' a list of data frames corresponding to input given
#'
#' \code{fars_summarize_years} produces a summary tibble of
#' FARS years and months for given a vector of years
#'
#' @param 
#'		years vector of years
#'
#' @return This function returns tibble where the first column #' is the month,the second and following columns are the
#' requested years, and the rows for the year columns are the
#' number of FARS observations for that month/year
#' combination.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(years = c(2013, 2014, 2015))
#' fars_summarize_years(years = 2013)
#'	}
#'
#' @export

fars_summarize_years <- function(years) {
			
			MONTH<-NULL
			n<-NULL
			year<-NULL

        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>% 
                dplyr::group_by(year, MONTH) %>% 
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}


#' Map State Motor Vehicle Fatalities.
#'
#' This function takes a state number and a year, and draws
#' a state outline with dots to represent the location of
#' motor vehicle fatalities for that year.
#'
#' \code{fars_map_state} maps state motor vehicle fatalities #' given a year and state id number.
#'
#' @param 
#'		state.num Numerical code for US state.
#' @param 
#' 		year  value of year
#'
#' @return NULL
#'
#' @examples
#'   \dontrun{
#'     fars_map_state(48, 2013)
#'     fars_map_state(48, 2014)
#'     fars_map_state(6, 2013)
#'   }
#'
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)
	   STATE<-NULL
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
