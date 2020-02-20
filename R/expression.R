#' Coerce Plotmath to Expression
#'
#' Coerces plotmath to expression by parsing as text.
#' @export
#' @family expression
#' @family interface
#' @family plotmath
#' @keywords internal
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


