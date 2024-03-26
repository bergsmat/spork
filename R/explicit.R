#' Make Something Explicit
#'
#' Makes something explicit.
#' Generic, with method
#' \code{\link{explicit.spork}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family explicit
#' @return see methods
#' @examples
#' # see methods
explicit <- function(x,...)UseMethod('explicit')

#' Make Spork Explicit
#'
#' Makes spork explicit. For every section
#' as indicated by line breaks, it supplies
#' dots to close open scripts.
#' Explicit spork can be concatenated more easily.
#' One generally wants to avoid breaking lines
#' within active sub- and super scripts.
#' @param x character
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family explicit
#' @return spork
#' @examples
#' s <- c(
#'   'Work_out\\nx^y\\n',
#'   '\\nkg/m^2/S^2'
#' )
#' explicit(as_spork(s))
explicit.spork <- function(x, ...){
  
  y <- strsplit(x, '\\\\n')
  y <- lapply(y, .explicit)
  y <- lapply(y, paste, collapse = '\\n')
  tail <- grepl('\\\\n$', x)
  tail <- ifelse(tail, '\\n', '')
  y <- paste0(y, tail)
  y <- as_spork(y)
  y
}

.explicit <- function(x, ...){
  dots <- gsub('\\\\.','',x)
  dots <- gsub('[^.]', '', dots)
  dots <- nchar(dots)
  
  subs <- gsub('\\\\_','',x)
  subs <- gsub('[^_]', '', subs)
  subs <- nchar(subs)
  
  sups <- gsub('\\\\^','',x)
  sups <- gsub('[^^]', '', sups)
  sups <- nchar(sups)
  
  need <- subs + sups - dots
  need <- pmax(0, need)
  
  tail <- lapply(need, function(n){rep('.', n)})
  tail <- lapply(tail, paste, collapse = '')
  
  result <- paste0(x, tail)
  result
}
