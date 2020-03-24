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

#' Convert One Spork to Latex
#'
#' Converts one spork to latex.
#' See description for \code{\link{as_spork}}.
#' By default, unrecognized tokens are returned
#' literally.  However, Greek symbols and latex
#' metacharacters are escaped.
#' See \code{\link{latexToken}}.
#'
#' Experimental support is implemented for
#' the newline character (\code{'\\n'}).
#' Default behavior is to introduce literal
#' newline characters into the resulting
#' tex.  This may have no effect on the
#' typeset result. It may be possible
#' to achieve other effects by using
#' non-default values of helper arguments
#' and perhaps additional latex packages.
#'
#' @export
#' @family interface
#' @return latex
#' @family latex
#' @param x spar
#' @param newline value to replace \code{'\\n'}
#' @param unrecognized function to process unrecognized tokens: default \code{\link{latexToken}}
#' @param token_open,token_close these wrap text-like portions of the label; the defaults try to give upright characters (non-italic); also passed to \code{\link{latexToken}}
#' @param math_open,math_close these wrap math-like portions of the label;  the defaults try to give upright characters (non-italic) which may not work for Greek symbols; also passed to \code{\link{latexToken}}
#' @param label_open,label_close these wrap the entire label; defaults invoke traditional math mode
#' @param enforce_math whether to enforce math mode for nested expression: \code{\link{latexToken}}
#' @param ... passed to \code{unrecognized}; see \code{\link{latexToken}}
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_spork %>% as_latex
#' 'AUC_ss' %>% as_spork %>% as_latex
#' 'C_max_ss' %>% as_spork %>% as_latex
#' 'var^eta_j' %>% as_spork %>% as_latex
#' '& % $ # \\_ { } ~ \\^ \\' %>% as_spork %>% as_latex
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex

as_latex.spar <- function(
  x,
  newline = getOption('latex_newline','\n'),
  unrecognized = getOption('latex_unrecognized','latexToken'),
  token_open = getOption('latex_token_open', '\\textrm{'),
  token_close = getOption('latex_token_close','}'),
  math_open = getOption('latex_math_open', '\\mathrm{'),
  math_close = getOption('latex_math_close', '}'),
  label_open = getOption('latex_label_open', '$'),
  label_close = getOption('latex_label_close', '$'),
  enforce_math = getOption('latex_enforce_math',TRUE),
  ...
){
  # the latex of a spork is the sequential
  # combination of tokens.
  # tokens separated by _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Whitespace and recognized escapes are supplied literally.
  # unescaped '*' is promoted to \code{\cdot}.
  # surviving tokens are processed by 'unrecognized',
  # which escapes metacharacters and
  # names of Greek letters, but renders other
  # tokens literally.

  closers <- character(0)
  active <- FALSE
  if(length(x)==0)return(structure(x, class = union('latex', class(x))))
  if(identical(x, ''))return(structure(x, class = union('latex', class(x))))
  base <- ''
  explicit <- c(
    '[\\][n]', '\\s+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  for(token in x){
    m <- sapply(explicit, function(pattern)position(token, pattern))
    if(max(m) == -1){ # unrecognized token
      # pre-process
      fun <- match.fun(unrecognized)
      token <- fun(
        token,
        unrecognized = unrecognized,
        token_open = token_open,
        token_close = token_close,
        math_open = math_open,
        math_close = math_close,
        label_open = label_open,
        label_close = label_close,
        enforce_math = enforce_math,
        ...
      )
      if(active){
        base <- paste0(base, ' ', token)
      }else{
        if(grepl('[]}]$',base)){ # not empty nest
          base <- paste0(base, ' ', token)
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
          base <- paste0(base, newline)
      }
      if(p == '\\s+'){
        token <- paste0(token_open,token,token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, ' ', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][*]'){
        token <- paste0(token_open, '*', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0(token_open, '.', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0(token_open, '\\_', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0(token_open,"{\\textasciicircum}",token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[*]'){
        token <- paste0("{\\cdot}")
        if(active){
          base <- paste0(base, ' ', token)
          active <- FALSE
        }else{
          base <- paste0(base, ' ', token)
          active <- FALSE
        }
      }
      if(p == '[.]'){
        if(length(closers)){
          cl <- closers[[1]]
          closers <- closers[-1]
          #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
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
        closers <- c('}', closers)
        if(active){
          base <- paste0(base,"_{")
          active <- FALSE
        }else{
          if(!grepl('[]}]$', base)){
            # must have something to subscript
            base <- paste0(base, "~_{")
          }else{
            base <- paste0(base, "~_{")
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
            base <- paste0(base, "~^{")
          }else{
            base <- paste0(base, "~^{")
          }
        }
      }
    }
  }
  # use of %.% can leave a dangling operator.
  # supply default rhs before closing
  # indeed, always check for %.% before appending close
  #if(grepl('%\\.%$',base)) base <- paste0(base, "''")
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
  base <- paste0(math_open, base, math_close)
  base <- paste0(label_open, base, label_close) # enforce math environment
  return(base)
}

#' Process Latex Token
#'
#' Pre-processes a latex token not recognized as
#' spork.  Escapes the common names for Greek letters
#' and escapes latex metacharacters.
#'
#' @param x character
# @param unrecognized function to process unrecognized tokens
#' @param token_open,token_close these wrap the entire token (used once); by default the token is text-like
#' @param math_open,math_close these wrap math-like portions of the token;  the defaults try to give upright characters (non-italic) which may not work for Greek symbols
#' @param label_open,label_close these re-wrap math-like portions of the token if \code{enforce_math} is TRUE; defaults invoke traditional math mode
#' @param enforce_math whether to enforce math mode for nested expression
#' @param ... ignored arguments
#' @export
#' @family latex
#' @family interface
#' @return latex
#' @examples
#' latexToken('foo')
#' latexToken('alpha')
#' latexToken('Alpha')
latexToken <- function(
  x,
  #unrecognized = latexToken,
  token_open = getOption('latex_token_open', '\\textrm{'),
  token_close = getOption('latex_token_close','}'),
  math_open = getOption('latex_math_open', '\\mathrm{'),
  math_close = getOption('latex_math_close', '}'),
  label_open = getOption('latex_label_open', '$'),
  label_close = getOption('latex_label_close', '$'),
  enforce_math = getOption('latex_enforce_math',TRUE),
  ...
){
  special <- c(  '&',  '%',  '$',  '#',  '_',  '{',  '}','~',                '^',               '\\'             ) # special in latex
  replace <- c('\\&','\\%','\\$','\\#','\\_','\\{','\\}','${\\sim}$','{\\textasciicircum}','{\\textbackslash}')      # use in latex
  greek <- c(
    'alpha','beta','gamma','delta','epsilon','zeta',
    'eta','theta','iota','kappa','lambda','mu',
    'nu','xi','omicron','pi','rho','sigma','tau',
    'upsilon','phi','chi','psi','omega'
  )
  # https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
  names(greek) <- c(
    '\\alpha','\\beta','\\gamma','\\delta','\\epsilon','\\zeta',
    '\\eta','\\theta','\\iota','\\kappa','\\lambda','\\mu',
    '\\nu','\\xi',
    'o',
    '\\pi','\\rho','\\sigma','\\tau',
    '\\upsilon','\\phi','\\chi','\\psi','\\omega'
  )
  Greek <- c(
    'Alpha','Beta','Gamma','Delta','Epsilon','Zeta',
    'Eta','Theta','Iota','Kappa','Lambda','Mu',
    'Nu','Xi','Omicron','Pi','Rho','Sigma','Tau',
    'Upsilon','Phi','Chi','Psi','Omega'
  )
  names(Greek) <- c(
    'A','B','\\Gamma','\\Delta','E','Z',
    'H','\\Theta','I','K','\\Lambda','M',
    'N','\\Xi','O','\\Pi','P','\\Sigma','T',
    '\\Upsilon','\\Phi','X','\\Psi','\\Omega'
  )
  extra <- c(
    'Upsilon1','varepsilon','omega1',
    'theta1', 'phi1', 'sigma1',
    'vartheta','varphi','varsigma',
    'stigma', 'varrho','varpi'
  )

  names(extra) <- c(
    '\\Upsilon','\\varepsilon','\\varpi',
    '\\vartheta','\\varphi','\\varsigma',
    '\\vartheta','\\varphi','\\varsigma',
    '\\varsigma', '\\varrho','\\varpi'
  )
  escape <- function(x,pattern,replace)sub(
    fixed = TRUE,
    pattern,
    replace[match(pattern,replace)],
    x
  )


  ### specials
  input <- x
  output <- ''
  while(nchar(input)){
    m <- sapply(special, function(pattern)position(input, pattern, fixed = TRUE))
    if(max(m) == -1){ # no match
      output <- paste0(output,input)
      input <- ''
    }else{
      m <- m[m != -1] # remove nonmatch
      m <- m[m == min(m)] # take first match
      stopifnot(length(m) == 1)
      p <- names(m)
      bef <- before(input, p, fixed = TRUE)
      #ths <- this(input, p, fixed = TRUE)
      ths <- replace[match(p, special)]
      aft <- after(input, p, fixed = TRUE)
      output <- paste0(output, bef, ths)
      input <- after(input, p, fixed = TRUE)
      if(identical(input, character(0))){
        input <- ''
      }
    }
  }
  x <- output

  ### greek
  nms <- c(greek, Greek, extra)
  input <- x
  output <- ''
  while(nchar(input)){
    m <- sapply(nms, function(pattern)position(
      input,
      paste0('\\b',pattern,'\\b'),
      fixed = FALSE
    ))
    if(max(m) == -1){ # no match
      output <- paste0(output,input)
      input <- ''
    }else{
      m <- m[m != -1] # remove nonmatch
      m <- m[m == min(m)] # take first match
      stopifnot(length(m) == 1)
      p <- names(m)
      pattern <- nms[[p]]
      pattern <- paste0('\\b',pattern,'\\b')
      bef <- before(input, pattern, fixed = FALSE)
      # mathopen <- '\\'
      # mathclose <- '{}'
      # if(!italics){
      #   mathopen <- '\\mathrm{'
      #   mathclose <- '}'
      # }
      mathopen <- math_open
      mathclose <- math_close
      if(enforce_math){
        mathopen <- paste0(label_open,mathopen)
        mathclose <- paste0(mathclose, label_close)
      }

      ths <- paste0(mathopen, p, mathclose)
      #aft <- after(input, pattern, fixed = FALSE)
      output <- paste0(output, bef, ths)
      input <- after(input, pattern, fixed = FALSE)
      if(identical(input, character(0))){
        input <- ''
      }
    }
  }
  x <- output

  x <- paste0(token_open, x, token_close)
  class(x) <- union('latex', class(x))
  x
}

#' Convert Spork to Latex
#'
#' Converts spork to latex.
#' Vectorized version of \code{\link{as_latex.spar}}.
#'
#' @export
#' @param x spork
#' @param ... passed to \code{\link{as_latex.spar}}
#' @return latex
#' @family latex
#' @family spork
#' @family interface
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
  y <- lapply(x, as_spar, USE.NAMES = F, ...)
  y <- sapply(y, as_latex, USE.NAMES = F, ...)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('latex', class(y))
  y
}
#' Subset Latex
#'
#' Subsets latex, retaining class.
#' @param x latex
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family latex
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
#' @family latex
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



