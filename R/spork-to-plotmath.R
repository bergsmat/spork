#' Convert One Spork to Plotmath
#'
#' Converts one spork to plotmath.
#' See description for \code{\link{as_spork}}.
#' By default, unrecognized tokens are returned
#' unmodified if they are parseable.
#' Otherwise, backslashes and single quotes are escaped,
#' and the result is wrapped in single quotes.
#' See \code{\link{plotmathToken}}.
#'
#' @export
#' @keywords internal
#' @return character
#' @family plotmath
#' @param x character
#' @param unrecognized function to process unrecognized tokens: default \code{\link{plotmathToken}}
#' @param ... ignored
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% spork_to_plotmath
#' 'AUC_ss' %>% spork_to_plotmath
#' 'C_max_ss' %>% spork_to_plotmath
#' 'var^eta_j' %>% spork_to_plotmath
spork_to_plotmath <- function(
  x,
  unrecognized = getOption('plotmath_unrecognized','plotmathToken'),
  ...
){
  # the plotmath of a spork is the sequential
  # combination of tokens.
  # tokens separated by _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Single quote is escaped and used for quoting.
  # Whitespace is quoted.
  # \\, \., \*, \_, and \^ are quoted.
  # unescaped '*' is promoted to %.%.
  # surviving tokens are processed by 'unrecognized'.

  x <- sporklet(x,...)
  closers <- character(0)
  active <- FALSE
  if(length(x)==0)return(x)
  if(identical(x, ''))return(x)
  base <- ''
  explicit <- c(
    '\\s+','#+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  for(token in x){
    m <- sapply(explicit, function(pattern)position(token, pattern))
    if(max(m) == -1){ # unrecognized token
      # pre-process
      fun <- match.fun(unrecognized)
      token <- fun(token, ...)
      if(active){
        base <- paste0(base, '*', token)
      }else{
        if(grepl('[]}]$',base)){ # not empty nest
          base <- paste0(base, '*', token)
          active <- TRUE
        }else{ # empty nest or start of line
          base <- paste0(base, token)
          active <- TRUE
        }
      }
    }
    if(max(m) != -1){ # recognized token
      m <- m[m != -1]
      m <- m[m == min(m)]
      stopifnot(length(m) == 1)
      p <- names(m)
      if(p == '\\s+'){
        token <- paste0("'",token,"'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '*', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '#+'){
        token <- paste0("'",token,"'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '*', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][*]'){
        token <- paste0("'*'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0("'.'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0("'_'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0("'^'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[*]'){
        token <- paste0("%.%")
        if(active){
          base <- paste0(base, token)
          active <- FALSE
        }else{
          base <- paste0(base, "''", token)
          active <- FALSE
        }
      }
      if(p == '[.]'){
        if(length(closers)){
          cl <- closers[[1]]
          closers <- closers[-1]
          if(grepl('%\\.%$',base)) base <- paste0(base, "''")
          if(active){
            base <- paste0(base, cl)
            active <- FALSE
          }else{ # not active
            if(grepl('[[{]$',base)){# empty nest ok
              base <- paste0(base, cl)
            }else{
              base <- paste0(base, cl )
            }
          }
        }
      }
      if(p == '[_]'){
        closers <- c(']', closers)
        if(active){
          base <- paste0(base, "[")
          active <- FALSE
        }else{
          if(!grepl('[]}]', base)){
            # must have something to subscript
            base <- paste0(base, "''[")
          }else{
            base <- paste0(base, "*''[")
          }
        }
      }
      if(p == '\\^'){
        closers <- c('}', closers)
        if(active){
          base <- paste0(base, "^{")
          active <- FALSE
        }else{
          if(!grepl('[]}]', base)){
            # must have something to superscript
            base <- paste0(base, "''^{")
          }else{
            base <- paste0(base, "*''^{")
          }
        }
      }
    }
  }
  # use of %.% can leave a dangling operator.
  # supply default rhs before closing
  # indeed, always check for %.% before appending close
  if(grepl('%\\.%$',base)) base <- paste0(base, "''")
  if(length(closers)){ # dump
    if(grepl('%\\.%$',base)) base <- paste0(base, "''")
    if(active){
      base <- paste0(base, paste(closers, collapse = ''))
    }else{
      if(grepl('[[{]',base)){
        # empty script ok
        base <- paste0(base, paste(closers, collapse = ''))
      }else{
        base <- paste0(base, paste(closers, collapse = ''))
      }
    }
  }
  return(base)
}

#' Test Whether Token is Parseable
#'
#' Tests whether token is Parseable
#' @param x length-one character
#' @param ... ignored arguments
#' @return logical
#' @export
#' @keywords internal
#' @family plotmath
#' @examples
#' goodToken('alpha')
#' goodToken('foo')
#' goodToken('\\$')
goodToken <- function(x,...){
  stopifnot(length(x) == 1)
  y <- try(silent = TRUE, parse(text = x))
  if(inherits(y, 'try-error'))return(FALSE)
  TRUE
}

#' Quote a Token
#'
#' Quotes a token. Escapes single-quotes and wraps in single-quotes.
#' Also maps 'varepsilon' to 'epsilon', since plotmath has only the latter;
#' likewise 'varrho' maps to 'rho' and 'varpi' maps to 'omega1'.
#' @param x (length-one) character
#' @param conditional if true, return good tokens (parseable) unmodified
#' @param unescape whether to escape (unrecognized) backslash
#' @param ... ignored arguments
#' @export
#' @family plotmath
#' @keywords internal
#' @return character
#' @examples
#' plotmathToken("can't")

plotmathToken <- function(
  x,
  conditional = getOption('plotmath_conditional_quote', TRUE),
  unescape = getOption('plotmath_unescape', TRUE),
  ...
){
  token <- x
  token <- gsub('\\bvarepsilon\\b','epsilon', token)
  token <- gsub('\\bvarrho\\b','rho', token)
  token <- gsub('\\bvarpi\\b','omega1', token)
  if(conditional){
    if(goodToken(token)){
      return(token)
    }
  }
  if(unescape) token <- gsub('[\\]','\\\\\\\\', token)
  token <- gsub("'","\\\\'",token)
  token <- paste0("'", token, "'")
  token
}


