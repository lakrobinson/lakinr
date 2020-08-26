\name{make_filename}
\alias{make_filename}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
This function constructs a filename for a FARS data file, given a corresponding year.
}
\usage{
make_filename(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{year}{An \code{integer}, represents the year of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file.}
}
}

\value{
A \code{string} for a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file for the given year.
}
}

\examples{
\dontrun{
# input year as an intger
make_filename(2013)
}
}

