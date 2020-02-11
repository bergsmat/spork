#' Coerce to Spork
#'
#' Coerces to class 'spork'. Generic,
#' with method \code{\link{as_spork.character}}.
#' A spork is simple text expressing
#' arbitrarily nested subscripts (\code{x_y_z})
#' and superscripts (\code{x^y^z}). A dot
#' (\code{x^y._z}) explicitly terminates
#' a group. An asterisk (\code{*}) suggests
#' multiplication. Special characters
#' may be escaped with a backslash.
#' Convert to plotmath with\code{\link{as_plotmath}}
#' and to latex with \code{\link{as_latex}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @importFrom ggplot2 ggplot
#' @family spork
#' @return spork
#' @md
#' @examples
#' library(magrittr)
#' library(dplyr)
#' library(latexpdf)
#'
#' 'j^*' %>% as_spork %>% as_plotmath %>% as.character
#' as_spork %>%
#' as_plotmath #%>%
#' as_expression
#' data.frame(y=1:10, x=1:10) %>%
#' decorate("x: one joule (Omega) ~ 1 kg*m^2./s^2") %>%
#' mutate(
#'  x = structure(x, label = x %>% attr('label') %>%
#'  as_spork %>%
#'  as_plotmath %>%
#'  as.expression)
#' ) %>%
#'  ggplot(aes(x, y))
#'
#' data.frame(y=1:10, x=1:10) %>%
#' decorate("x: gravitational force - gamma (kg\\.m/s^2.)") %>%
#' mutate(x = structure(x, label = x %>% attr('label') %>%
#' as_spork %>%
#' as_plotmath %>%
#' as.expression)) %>%
#' ggplot(aes(x, y))
#'
#' path <- tempfile()
#' data.frame(
#'  stringsAsFactors = FALSE,
#'  spork = c(
#'    'one joule (Omega) ~ 1 kg*m^2./s^2',
#'    'gravitational force - gamma (kg\\.m/s^2.)'
#'  )
#') %>%
#'  transmute(
#'    latex = spork %>%
#'    as_spork %>%
#'    as_latex
#'  ) %>%
#'  as.pdf(reserve = FALSE, wider = -70)
as_spork <- function(x, ...)UseMethod('as_spork')

#' Coerce Character to Spork
#'
#' Coerces character to class 'spork'.
#' See description for \code{\link{as_spork}}.
#'
#' @param x character
#' @param ... ignored arguments
#' @export
#' @family spork
#' @return spork
#' @examples
#' as_spork('V_c./F')
as_spork.character <- function(x, ...){
  class(x) <- union('spork', class(x))
  x
}

#' Coerce Factor to Spork
#'
#' Coerces factor to class 'spork'
#' by converting to character and calling
#' \code{\link{as_spork}}.
#'
#' @param x factor
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family spork
#' @return spork
#' @examples
#' as_spork(as.factor('V_c./F'))
as_spork.factor <- function(x, ...)as_spork(as.character(x), ...)

#' Coerce to Plotmath
#'
#' Coerce to plotmath.  Generic, with method
#' \code{\link{as_plotmath.spork}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family plotmath
#' @return plotmath
#' @examples
#' example(as_plotmath.spork)
as_plotmath <- function(x, ...)UseMethod('as_plotmath')

#' Coerce to Latex
#'
#' Coerce to latex.  Generic, with method
#' \code{\link{as_latex.spork}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family latex
#' @return latex
#' @examples
#' example(as_latex.spork)
as_latex <- function(x, ...)UseMethod('as_latex')

#' Convert Spork to Plotmath
#'
#' Converts spork to plotmath. See '?plotmath'.
#' Vectorized version of \code{\link{spork_to_plotmath}}.
#'
#' @export
#' @param x spork
#' @param ... ignored
#' @return plotmath
#' @family plotmath
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_plotmath
#' 'AUC_ss' %>% as_plotmath
#' 'C_max_ss' %>% as_plotmath
#' 'var^eta_j' %>% as_plotmath
#' as_plotmath(as_spork('one joule (Omega) ~ 1 kg*m^2./s^2'))
as_plotmath.spork <- function(x, ...){
  y <- sapply(x, spork_to_plotmath , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('plotmath', class(y))
  y
}
#' Convert Spork to Latex
#'
#' Converts spork to latex.
#' Vectorized version of \code{\link{spork_to_latex}}.
#'
#' @export
#' @param x spork
#' @param ... ignored
#' @return latex
#' @family latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_spork(x)
#' as_latex(x)
#' as_latex(as_spork('gravitational force (kg\\.m/s^2.)'))
as_latex.spork <- function(x, ...){
  y <- sapply(x, spork_to_latex , USE.NAMES = F)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('latex', class(y))
  y
}

#' Coerce Plotmath to Expression
#'
#' Coerces plotmath to expression by parsing as text.
#' @export
#' @keywords internal
#' @family spork
#' @param x plotmath
#' @param ... ignored arguments
#' @return expression
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_spork(x)
#' x <- as_plotmath(x)
#' x
#' as.expression(x)[[4]]
#' as.expression(x[[4]])
#' class(as.expression(x))
#' lapply(as.expression(x), class)
#' as.expression(as_plotmath(as_spork('V_c./F')))
#' as.expression(as_plotmath(as_spork(character(0))))
#' library(magrittr)
#' 'gravitational force (kg\\.m/s^2.)' %>%
#'   as_spork %>%
#'   as_plotmath %>%
#'   as.expression -> label
#'   label

as.expression.plotmath <- function(x, ...)parse(text = x)

#' Subset Spork
#'
#' Subsets spork, retaining class.
#' @param x spork
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return spork
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_spork(x)
#' class(x)
#' class(x[1])
`[.spork` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('spork', class(y))
  y
}
#' Element-select Spork
#'
#' Element-selects spork, retaining class.
#' @param x spork
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return spork
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_spork(x)
#' class(x)
#' class(x[[1]])
`[[.spork` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('spork', class(y))
  y
}

#' Subset Plotmath
#'
#' Subsets plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_spork(x))
#' class(x)
#' class(x[1])
`[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}
#' Element-select Plotmath
#'
#' Element-selects plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return plotmath
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_plotmath(as_spork(x))
#' class(x)
#' class(x[[1]])
`[[.plotmath` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('plotmath', class(y))
  y
}

#' Subset Latex
#'
#' Subsets latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_spork(x))
#' class(x)
#' class(x[1])
`[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}
#' Element-select Latex
#'
#' Element-selects latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family util
#' @return latex
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_latex(as_spork(x))
#' class(x)
#' class(x[[1]])
`[[.latex` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('latex', class(y))
  y
}

#' Coerce Symbolic Units to Spork
#'
#' Coerces symbolic units to spork by coercing first
#' to unit_string.
#' @param x symbolic_units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family util
#' @return spork
#' @examples
#' library(units)
#' x <- as_units('kg.m/s^2')
#' names(attributes(x))
#' y <- attr(x,'units')
#' class(y)
#' as.character(y)
#' as.character(attr(x, 'units'))
#' as_spork(y)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork
#' 'kg.m^2/s^2' %>% as_units %>% attr('units') %>% as_spork(FALSE)
#' 'kg.m2 s-2' %>% as_units %>% attr('units') %>% as_spork(FALSE)

as_spork.symbolic_units <- function(x, canonical = TRUE, ...){
  y <- as_unit_string(x, canonical = canonical, ...)
  y <- as_spork(y, ...)
  y
}

#' Coerce Units to Spork
#'
#' Coerces units to spork by coercing first
#' to unit_string.
#' @param x units; see \code{\link[units]{as_units}}
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family spork
#' @return spork
#' @examples
#' library(units)
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_units %>% as_spork
#' 'kg.m2 s-2' %>% as_units %>% as_spork
#' 'kg.m^2/s^2' %>% as_units %>% as_spork(FALSE)
#' 'kg.m2 s-2' %>% as_units %>% as_spork(FALSE)
as_spork.units <- function(x, canonical = TRUE, ...){
  y <- as_unit_string(x, canonical = canonical, ...)
  y <- as_spork(y, ...)
  y
}

#' Coerce Unit String to Spork
#'
#' Coerces unit string to spork.  A literal dot
#' means different things in spork vs. units,
#' and there may be some other subtleties as well.
#' Unit string is character that \code{\link{is_parseable}}.
#' @param x unit_string
#' @param ... ignored arguments
#' @export
#' @keywords internal
#' @family spork
#' @return units
#' @examples
#' library(magrittr)
#' 'kg.m^2/s^2' %>% as_unit_string %>% as_spork
#' 'kg.m2 s-2' %>% as_unit_string %>% as_spork
as_spork.unit_string <- function(x, ...){
  stopifnot(all(is_parseable(x)))
  y <- gsub('\\.','*',x) # \u22c5 https://en.wikipedia.org/wiki/Interpunct
  y <- gsub('\\^([0-9])+','^\\1.',y) # canonical, all pos num follow ^
  y <- gsub('([a-zA-Z])([-0-9]+)', '\\1^\\2.',y) # non-canonical, unsigned or neg num follow char
  y <- as_spork(as.character(y))
  y
}

