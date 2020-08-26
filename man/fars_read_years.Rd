\name{fars_read_years}
\alias{fars_read_years}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data files.}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A vector of \code{integr}s, or other types coerced to intgers, represent the years of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data files.}
}
%%  ~~function to do ... ~~
}
\description{
This is a  \code{readr_csv} function that reads a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file given a relevaant year(s).
}
\details{
The function returns a warning \code{invalid year: year} if the input year does not correspond to one of the files in the current working directory.
Otherwise, the function constructs a vecotr of file names using \code{\link{make_filename}} and reads them in a list using \code{\link{fars_read}}.
}
\value{
A \code{list} of \code{data.frame}s, each of its elements correspond to one of the given years.
}
\examples{
\dontrun{
# invalid input, year does not exist
fars_read_years(c(2013, 2014, 3000))

# valid input, years exist
fars_read_years(2013:2015)
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
