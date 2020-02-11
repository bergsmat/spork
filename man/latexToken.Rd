% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spork-to-latex.R
\name{latexToken}
\alias{latexToken}
\title{Process Latex Token}
\usage{
latexToken(x, unrecognized, math, italics, ...)
}
\arguments{
\item{x}{character}

\item{unrecognized}{function to process unrecognized tokens}

\item{math}{whether to wrap in math environment}

\item{italics}{whether to use italics or not}

\item{...}{ignored arguments}
}
\value{
character
}
\description{
Pre-processes a latex token not recognized as
spork.  Escapes the common names for Greek letters
and escapes latex metacharacters.
}
\examples{
latexToken('foo')
latexToken('alpha')
latexToken('Alpha')
}
\seealso{
Other latex: 
\code{\link{as_latex.spork}()},
\code{\link{as_latex}()},
\code{\link{spork_to_latex}()}
}
\concept{latex}
\keyword{internal}