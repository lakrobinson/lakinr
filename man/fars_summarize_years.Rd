\name{fars_summarize_years}
\alias{fars_summarize_years}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{fars_read_years}
}
\value{
A \code{tibble} of the number of accidents per month for each year.
}
\description{
This function returns a summary of the number of accidents per month for each year of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data.
}
\details{
The function returns a numerical summary of the number of accidents per month for each given year.
}
\examples{
\dontrun{
years <- 2013:2015
fars_summarize_years(years)
}

}
