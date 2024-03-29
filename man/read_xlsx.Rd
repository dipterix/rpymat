% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-readxlsx.R
\name{read_xlsx}
\alias{read_xlsx}
\title{Read data frame from a \code{'xlsx'} file}
\usage{
read_xlsx(
  path,
  sheet = NULL,
  method = c("auto", "pandas", "readxl"),
  n_max = Inf,
  ...
)
}
\arguments{
\item{path}{\code{'xlsx'} file path}

\item{sheet}{either a character or an integer of which spread-sheet to read;
the number starts from \code{1}}

\item{method}{which method to use for reading the \code{'xlsx'} file;
choices are \code{'auto'} (automatically find proper method),
\code{'pandas'} (use \code{pandas.read_xlsx}), or \code{'readxl'} (use the
corresponding R package)}

\item{n_max}{maximum number of rows (excluding headers) to read}

\item{...}{passed to 'Python' function \code{pandas.read_xlsx} or
\code{readxl::read_excel}, depending on \code{method}}
}
\value{
A \code{\link{data.frame}} table
}
\description{
Tries to use \code{'readxl'} package or \code{'pandas'} to read data frame.
}
\examples{

\dontrun{

rpymat::read_xlsx("Book1.xlsx", sheet = 1)

rpymat::read_xlsx("Book1.xlsx", sheet = "sheet1")

}


}
