\name{fars_map_state}
\alias{fars_map_state}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data to a state map.}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{An \code{intger} represents an identifier for a state.}

\item{year}{An \code{integer}, represents the year of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file.}
}
\value{
This function creates a plot of a state from \code{\link[maps]{map}} and plots points corresponding to the coordinates of recorded accidents.
}
\description{
This function plots individual \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} accidents' coordinates as points on a given state's map.
}
\details{
The function returns an error \code{invalid STATE number: state.num} if the provided number does not correspond to a STATE in the data of the given year. The function returns a message \code{no accidents to plot} if the number of accidents for the given year and state comination is 0.
Otherwise, it constructs a file name using the provided year, reads the data file and plots points of individual accidents on the STATE map.
}
\examples{
\dontrun{
fars_map_state(49, 2013)
}
}
