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

#' Convert Greek to Plotmath
#' 
#' Converts Greek letter names to plotmath.
#' @param x greek
#' @param ... ignored
#' @family plotmath
#' @keywords internal
#' @export
#' @return plotmath
#' @examples
#' as_plotmath(greek())
as_plotmath.greek <- function(x, ...){
  x[x == 'varepsilon'] <- 'epsilon'
  x[x == 'varrho'] <- 'rho'
  x[x == 'varpi'] <- 'omega1'
  class(x) <- 'plotmath'
  x
}

#' Convert Default to Plotmath
#' 
#' Coerces to spork, then to plotmath.
#' @param x inherits character
#' @param ... passed arguments
#' @family plotmath
#' @keywords internal
#' @export
#' @return plotmath
#' @examples
#' as_plotmath('anything')
as_plotmath.default <- function(x, ...){
  x <- as_spork(x, ...)
  x <- as_plotmath(x, ...)
  x
}

#' Convert One Spork to Plotmath
#'
#' Converts one spork to plotmath.
#' See description for \code{\link{as_spork}}.
#' Unrecognized tokens are returned
#' unmodified by default.
#' Otherwise, backslashes and single quotes are escaped,
#' and the result is wrapped in single quotes.
#' See \code{\link{plotmathToken}}.
#'
#' Experimental support is implemented for
#' the sequence "backslash n" (\code{'\\n'}).
#' It tries to break the expression at the
#' point indicated, and stack the results.
#' Active subscripts and superscripts
#' are closed in advance, preventing
#' these from breaking across lines.
#'
#' @param x spar
#' @param unrecognized function to process unrecognized tokens
#' @param ... passed to \code{unrecognized}
#' @export
#' @family interface
#' @family plotmath
#' @family spar
#' @return character
#' @seealso plotmathToken
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_plotmath
#' 'AUC_ss' %>% as_plotmath
#' 'C_max_ss' %>% as_plotmath
#' 'var^eta_j' %>% as_plotmath
#' '& % $ # \\_ { } ~ \\^ \\' %>% as_plotmath
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_plotmath
#' 'one joule (`Omega`) ~ 1 kg*m^2./s^2' %>% as_plotmath
#' 'one joule (\\`Omega\\`) ~ 1 kg*m^2./s^2' %>% as_plotmath
as_plotmath.spar <- function(
  x,
  unrecognized = getOption('plotmath_unrecognized',spork::plotmathToken),
  ...
){
  # the plotmath of a spork is the sequential
  # combination of tokens.
  # tokens separated by _ or ^ or . which are non-printing
  # but trigger nesting or un-nesting.
  # Single quote is escaped and used for quoting.
  # Whitespace is quoted.
  # \\, \., \*, \_, and \^ are quoted.
  # unescaped '*' is promoted to %.%.
  # surviving tokens are processed by 'unrecognized'.

  #x <- sporklet(x,...)
  
  closers <- character(0)
  newlines <- character(0)
  any_atop <- FALSE
  active <- FALSE
  if(length(x)==0)return(structure(x, class = union('plotmath', class(x))))
  if(identical(x, ''))return(structure(x, class = union('plotmath', class(x))))
  base <- ''
  greek <- as.character(greek())
  ungreek <- paste0('`', greek, '`')
  greek <- paste0('\\b', greek, '\\b') # only at boundaries
  explicit <- c(
    '[\\][n]','\\s+','#+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^',
    greek, ungreek, '[\\][`]'
  )
  
  for(token in x){
    m <- sapply(explicit, function(pattern)position(token, pattern))
    if(max(m) == -1){ # unrecognized token
      # pre-process
      fun <- match.fun(unrecognized)
      #cat('as_plotmat.spar:', token, '->')
      token <- fun(token, ...)
      #cat(token, '\n')
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
      if(p == '[\\][n]'){
        # cannot split scripts across lines
        # \\n dumps the closer stack
        # sets active FALSE
        # wraps base in 'atop(' and ','
        # posts a newline of ')'
        # flushes newlines
        if(grepl('%\\.%$',base)) base <- paste0(base, "''")
        any_atop <- TRUE
        base <- paste0(
          # 'atop(',
          base,
          paste(closers,collapse = ''),
          #paste(newlines, collapse = ''),
          '),atop(textstyle('
        )
        active <- FALSE
        closers <- character(0)
        newlines <- paste0(newlines, ')')
      }
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
            # 0.2.6 consider '_Tau.iota' %>% as_plotmath
            # seems like next material, if any, should append.
            # but remaining active causes a bug for "a_b.^c"
            # which is rendered to "'a'['b']*''^{'c'}"
            # giving b and c vertically aligned.
            # the real need was the empty nest check for greek and `greek`
            # which have now been modeled on 'unrecognized' code block.
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
          if(!grepl('[]}]$', base)){
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
          if(!grepl('[]}]$', base)){
            # must have something to superscript
            base <- paste0(base, "''^{")
          }else{
            base <- paste0(base, "*''^{")
          }
        }
      }
      if(p %in% greek){ # modeled on 'unrecognized'
        token <- as_plotmath(as_greek(token))
        if(active){
          base <- paste0(base, '*', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '*', token)
            active = TRUE
          }else{
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p %in% ungreek){
        token <- gsub('`',"'",token)
        if(active){
          base <- paste0(base, '*', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '*', token)
            active = TRUE
          }else{
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][`]'){
        token <- paste0("'`'")
        if(active){
          base <- paste0(base, '*', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
    }
  }
  # use of %.% can leave a dangling operator.
  # supply default rhs before closing
  # indeed, always check for %.% before appending close
  if(grepl('%\\.%$',base)) base <- paste0(base, "''")
  if(length(closers)){ # dump
    #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
    if(active){
      base <- paste0(base, paste(closers, collapse = ''))
    }else{
      if(grepl('[[{]$',base)){
        # empty script ok
        base <- paste0(base, paste(closers, collapse = ''))
      }else{
        base <- paste0(base, paste(closers, collapse = ''))
      }
    }
  }
  # atop may have been invoked multiple times
  # dump the newline stack
  base <- paste0(base, paste(newlines, collapse = ''))
  # wrap in phantom atop 1
  if(any_atop)base <- paste0('atop(textstyle(),atop(textstyle(', base, ')))')
  class(base) <- union('plotmath', class(base))
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



#' Process Plotmath Token
#'
#' Processes a plotmath token. Escapes single-quotes and wraps in single-quotes.
#' Also maps 'varepsilon' to 'epsilon', since plotmath has only the latter;
#' likewise 'varrho' maps to 'rho' and 'varpi' maps to 'omega1'.
#' @param x (length-one) character
#' @param conditional if true, return good tokens (parseable) unmodified; see \code{\link{goodToken}}
#' @param unescape whether to escape (unrecognized) backslash
#' @param ... ignored arguments
#' @export
#' @family plotmath
#' @family interface
#' @return plotmath
#' @examples
#' plotmathToken("can't")
#' plotmathToken("\\", unescape = TRUE)
#' plotmathToken("\\", unescape = FALSE)
#' plotmathToken("\n", conditional = TRUE)
#' plotmathToken("\n", conditional = FALSE)
#' plotmathToken('alpha')
#' plotmathToken('Alpha')

plotmathToken <- function(
  x,
  conditional = getOption('plotmath_conditional_quote', FALSE),
  unescape = getOption('plotmath_unescape', TRUE),
  ...
){
  # greek should be parsed by as_spar()
  if(conditional){
    if(goodToken(x)){
      class(x) <- union('plotmath', class(x))
      return(x)
    }
  }

  if(unescape) x <- gsub('[\\]','\\\\\\\\', x)
  x <- gsub("'","\\\\'",x)
  x <- paste0("'", x, "'")
  class(x) <- union('plotmath', class(x))
  x
}

#' Convert Spork to Plotmath
#'
#' Converts spork to plotmath. See \code{\link[grDevices]{plotmath}}.
#' Vectorized version of \code{\link{as_plotmath.spar}}.
#'
#' @export
#' @param x spork
#' @param ... passed to \code{\link{as_plotmath.spar}}
#' @return plotmath
#' @family plotmath
#' @family spork
#' @family interface
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_plotmath
#' 'AUC_ss' %>% as_plotmath
#' 'C_max_ss' %>% as_plotmath
#' 'var^eta_j' %>% as_plotmath
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_plotmath
as_plotmath.spork <- function(x, ...){
  y <- lapply(x, as_spar, USE.NAMES = F, ...)
  y <- sapply(y, as_plotmath, USE.NAMES = F, ...)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('plotmath', class(y))
  y
}

#' Subset Plotmath
#'
#' Subsets plotmath, retaining class.
#' @param x plotmath
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family plotmath
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
#' @family plotmath
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
