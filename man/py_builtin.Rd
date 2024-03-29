% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{py_builtin}
\alias{py_builtin}
\title{Get 'Python' built-in object}
\usage{
py_builtin(name, convert = FALSE)
}
\arguments{
\item{name}{object name}

\item{convert}{see \code{\link[reticulate]{import_builtins}}}
}
\value{
A python built-in object specified by \code{name}
}
\description{
Get 'Python' built-in object
}
\examples{

if(interactive() && dir.exists(env_path())) {


# ------ Basic case: use python `int` as an R function ---------
py_int <- py_builtin("int", convert = TRUE)

# a is an R object now
a <- py_int(9)
print(a)
class(a)

# ------ Use python `int` as a Python function -----------------
py_int2 <- py_builtin("int", convert = FALSE)

# b in a python object
b <- py_int2(9)

# There is no '[1] ' when printing
print(b)
class(b)

# convert to R object
py_to_r(b)



}

}
