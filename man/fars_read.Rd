\name{fars_read}
\alias{fars_read}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file}
}

\description{
This is a \code{\link[readr]{read_csv}} function that reads a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file given its name.
}
\usage{
fars_read(x)
}

\arguments{
  \item{filename}{A string, such as returned by \code{\link{make_filename}}, representing the name of a \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data in the current working directory.}

}

\details{
The function returns an error \code{file does not exist} if a file with the name from the input exist in the current working
directory; otherwise, it returns a \code{data.fram}.
}

\value{
A \code{data.frame} of the \href{https://www.nhtsa.gov/research-data/fatality-analysis-reporting-system-fars}{FARS} data file.
}

\examples{
\dontrun{
# construct a file name using `make_filename`
file_name <- make_filenam(2013)
fars_read(file_name)

# supply file name explicitly
fars_read("accident_2013.csv.bz2")
}
}
