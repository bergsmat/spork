#' Parse Spork.
#'
#' Parses spork.
#' Generic, with method \code{\link{as_spar.spork}}.
#' @param x object
#' @param ... passed arguments
#' @keywords internal
#' @export
#' @family generics
#' @family spar
#' @return see methods
#' @examples
#' # see methods
as_spar <- function(x, ...)UseMethod('as_spar')

#' Parse Spork
#'
#' Parses spork.  Converts length-one character
#' to vector of tokens.  Explicit tokens include
#' \code{*._^} and any of these escaped with
#' backslash, e.g. \code{'\\*'}.
#' Backslash-n is an explicit token (\code{'\\n'}).
#' One or more consecutive whitespace characters are a single token,
#' as are one or more consecutive octothorpes (\code{#}).
#' Any string of characters delimited by
#' one or more of the above is implicitly
#' a token as well.
#'
#' @param x length-one character using spork syntax
#' @param ... ignored arguments
#' @export
#' @keywords manip
#' @return spar (character vector)
#' @family spar
#' @family spork
#' @examples
#' as_spar(as_spork('one joule (Omega) ~ 1 kg*m^2./s^2'))

as_spar.spork <- function(x, ...){
  if(length(x) == 0) {
    out <- character(0)
    class(out) <- union('spar', class(out))
    return(out)
  }
  if(length(x) > 1)stop('expecting length-one character')
  if(x == ''){
    out <- ''
    class(out) <- union('spar', class(out))
    return(out)
  }
  input <- x
  output <- character(0)
  explicit <- c(
    '[\\][n]','\\s+','#+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  while(nchar(input)){
    m <- sapply(explicit, function(pattern)position(input, pattern))
    if(max(m) == -1){
      out <- c(output, input)
      class(out) <- union('spar', class(out))
      return(out)
    }
    m <- m[m != -1]
    m <- m[m == min(m)]
    stopifnot(length(m) == 1)
    p <- names(m)
    output <- c(
      output,
      before(input, p),
      this(input, p)
    )
    input <- after(input, p)
    if(identical(input, character(0))){
      input <- ''
    }
  }
  class(output) <- union('spar', class(output))
  return(output)
}

position <- function(x, what, fixed = FALSE)as.integer(regexpr(what, x, fixed = fixed))

this <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at == -1) return(character(0))
  len <- attr(at, 'match.length')
  last <- at + len - 1
  ths <- substr(x, start = at, stop = last)
  return(ths)
}
before <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at <= 1) return(character(0))
  bef <- substr(x, start = 0, stop = at - 1)
  return(bef)
}
after <- function(x, what, fixed = FALSE){
  at <- regexpr(what, x, fixed = fixed)
  if(at < 1) return(character(0))
  len <- attr(at, 'match.length')
  last <- at + len - 1
  if(last == nchar(x)) return(character(0))
  aft <- substr(x, start = last + 1, stop = nchar(x))
  return(aft)
}
