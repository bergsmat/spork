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
#' Convert to plotmath with \code{\link{as_plotmath}}
#' and to latex with \code{\link{as_latex}}.
#' Both plotmath and latex names of Greek
#' letters are supported; see \code{\link{as_previews.spork}}
#' and examples there for disambiguation.
#'
#' Experimental support is implemented for
#' newlines (\code{'\\n'}) but these
#' may be handled differently by
#' \code{\link{as_plotmath.spar}} and
#' \code{\link{as_latex.spar}}.
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
#' library(ggplot2)
#' label <- 'one joule (Omega) ~ 1 kg*m^2./s^2'
#' label <- as_spork(label)
#' label <- as_plotmath(label)
#' label <- as.expression(label)
#' x <- data.frame(y=1:10, x=1:10)
#' p <- ggplot(x, aes(x, y))
#' p$labels$x <- label
#'
#' print(p)

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
#' @family interface
#' @family character
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
#' @family factor
#' @return spork
#' @examples
#' as_spork(as.factor('V_c./F'))
as_spork.factor <- function(x, ...)as_spork(as.character(x), ...)

#' Subset Spork
#'
#' Subsets spork, retaining class.
#' @param x spork
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family spork
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
#' @family spork
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

#' Coerce Spork to List
#'
#' Coerces spork to list.  Each element inherits class.
#' Supports use of \code{\link{lapply}}.
#' @param x spork
#' @param ... ignored
#' @export
#' @keywords internal
#' @family spork
#' @return list of spork
#' @examples
#' x <- as_spork(letters[1:5])
#' lapply(x, class)
as.list.spork <- function(x, ...){
  y <- as.list(as.character(x))
  y <- lapply(y, as_spork)
  y
}
