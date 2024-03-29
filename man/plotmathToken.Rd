% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmath.R
\name{plotmathToken}
\alias{plotmathToken}
\title{Process Plotmath Token}
\usage{
plotmathToken(
  x,
  conditional = getOption("plotmath_conditional_quote", FALSE),
  unescape = getOption("plotmath_unescape", TRUE),
  ...
)
}
\arguments{
\item{x}{(length-one) character}

\item{conditional}{if true, return good tokens (parseable) unmodified; see \code{\link{goodToken}}}

\item{unescape}{whether to escape (unrecognized) backslash}

\item{...}{ignored arguments}
}
\value{
plotmath
}
\description{
Processes a plotmath token. Escapes single-quotes and wraps in single-quotes.
Also maps 'varepsilon' to 'epsilon', since plotmath has only the latter;
likewise 'varrho' maps to 'rho' and 'varpi' maps to 'omega1'.
}
\examples{
plotmathToken("can't")
plotmathToken("\\\\", unescape = TRUE)
plotmathToken("\\\\", unescape = FALSE)
plotmathToken("\n", conditional = TRUE)
plotmathToken("\n", conditional = FALSE)
plotmathToken('alpha')
plotmathToken('Alpha')
}
\seealso{
Other plotmath: 
\code{\link{[.plotmath}()},
\code{\link{[[.plotmath}()},
\code{\link{as.expression.plotmath}()},
\code{\link{as.png.plotmath}()},
\code{\link{as_plotmath.greek}()},
\code{\link{as_plotmath.spar}()},
\code{\link{as_plotmath.spork}()},
\code{\link{as_plotmath}()},
\code{\link{concatenate.plotmath}()},
\code{\link{ggplot.plotmath}()},
\code{\link{goodToken}()}

Other interface: 
\code{\link{as.expression.plotmath}()},
\code{\link{as_html.spar}()},
\code{\link{as_html.spork}()},
\code{\link{as_latex.spar}()},
\code{\link{as_latex.spork}()},
\code{\link{as_plotmath.spar}()},
\code{\link{as_plotmath.spork}()},
\code{\link{as_previews.spork}()},
\code{\link{as_spork.character}()},
\code{\link{htmlToken}()},
\code{\link{latexToken}()}
}
\concept{interface}
\concept{plotmath}
