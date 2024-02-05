#' Concatenate Something
#'
#' Concatenates something.
#' Generic, with methods
#' \code{\link{concatenate.character}},
#' \code{\link{concatenate.plotmath}}, and
#' \code{\link{concatenate.latex}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family concatenate
#' @return see methods
#' @examples
#' # see methods
concatenate <- function(x,...)UseMethod('concatenate')

#' Concatenate Character
#'
#' Concatenates Character. Collapses vector using separator.
#' @param x character
#' @param sep character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family concatenate
#' @family character
#' @return character
#' @examples
#' concatenate(letters)
concatenate.character <- function(x, sep = '', ...){
  stopifnot(is.character(sep))
  stopifnot(length(sep) == 1)
  res <- paste(x, collapse = sep)
  res
}

#' Concatenate Plotmath
#'
#' Concatenates plotmath. Collapses vector using separator.
#' @param x plotmath
#' @param sep character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family concatenate
#' @family plotmath
#' @return plotmath
#' @examples
#' concatenate(as_plotmath(as_spork(c('BMI_i','kg/m^2'))))
concatenate.plotmath <- function(x, sep = '*', ...){
  stopifnot(is.character(sep))
  stopifnot(length(sep) == 1)
  res <- paste(x, collapse = sep)
  class(res) <- union('plotmath', class(res))
  res
}

#' Concatenate Latex
#'
#' Concatenates latex. Collapses vector using separator.
#' @param x latex
#' @param sep character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family concatenate
#' @family latex
#' @return latex
#' @examples
#' concatenate(as_latex(as_spork(c('BMI_i','kg/m^2'))))
concatenate.latex <- function(x, sep = '{}',...){
  stopifnot(is.character(sep))
  stopifnot(length(sep) == 1)
  res <- paste(x, collapse = sep)
  class(res) <- union('latex', class(res))
  res
}
#' Concatenate NULL
#'
#' Concatenates null. Returns empty string.
#' @param x NULL
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family concatenate
#' @return character
#' @examples
#' concatenate(NULL)
concatenate.NULL <- function(x,...)''
