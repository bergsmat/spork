#' Convert One Spork to Latex
#'
#' Converts one spork to latex.
#' See description for \code{\link{as_spork}}.
#' By default, unrecognized tokens are returned
#' literally.  However, Greek symbols and latex
#' metacharacters are escaped.
#' See \code{\link{latexToken}}.
#'
#' @export
#' @keywords internal
#' @return character
#' @family latex
#' @param x character
#' @param unrecognized function to process unrecognized tokens: default \code{\link{latexToken}}
#' @param italics whether to use italics or not (default: no)
#' @param math whether to wrap in math environment (default: yes)
#' @param ... passed arguments
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% spork_to_latex
#' 'AUC_ss' %>% spork_to_latex
#' 'C_max_ss' %>% spork_to_latex
#' 'var^eta_j' %>% spork_to_latex
#' '& % $ # \\_ { } ~ \\^ \\' %>% spork_to_latex
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% spork_to_latex

spork_to_latex <- function(
  x,
  unrecognized = getOption('latex_unrecognized','latexToken'),
  italics = FALSE,
  math = TRUE,
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

  x <- sporklet(x,...)
  closers <- character(0)
  active <- FALSE
  if(length(x)==0)return(x)
  if(identical(x, ''))return(x)
  base <- ''
  explicit <- c(
    '\\s+',
    '[*]','[.]','[_]','\\^',
    '[\\][*]','[\\][.]','[\\][_]','[\\]\\^'
  )
  for(token in x){
    m <- sapply(explicit, function(pattern)position(token, pattern))
    if(max(m) == -1){ # unrecognized token
      # pre-process
      fun <- match.fun(unrecognized)
      token <- fun(token, unrecognized = unrecognized, math = math, italics = italics, ...)
      if(active){
        base <- paste0(base, ' ', token)
      }else{
        if(grepl('[]}]$',base)){ # not empty nest
          base <- paste0(base, ' ', token)
          active <- TRUE
        }else{ # empty nest or start of line
          base <- paste0(base,' ', token)
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
        token <- paste0("\\textrm{",token,"}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, ' ', token)
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, ' ', token)
            active <- TRUE
          }
        }
      }
      if(p == '[\\][*]'){
        token <- paste0("\\textrm{*}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0("\\textrm{.}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0("\\textrm{\\_}")
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0("\\textrm{{\\textasciicircum}}")
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
          if(!grepl('[]}]', base)){
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
          if(!grepl('[]}]', base)){
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
      if(grepl('[[{]',base)){
        # empty script ok
        base <- paste0(base, paste(closers, collapse = ''))
      }else{
        base <- paste0(base, paste(closers, collapse = ''))
      }
    }
  }
  if(!italics) base <- paste0('\\mathrm{', base, '}')
  if(math) base <- paste0('$', base, '$') # enforce math environment
  return(base)
}

#' Process Latex Token
#'
#' Pre-processes a latex token not recognized as
#' spork.  Escapes the common names for Greek letters
#' and escapes latex metacharacters.
#'
#' @param x character
#' @param unrecognized function to process unrecognized tokens
#' @param italics whether to use italics or not
#' @param math whether to wrap in math environment
#' @param ... ignored arguments
#' @export
#' @family latex
#' @keywords internal
#' @return character
#' @examples
#' latexToken('foo')
#' latexToken('alpha')
#' latexToken('Alpha')
latexToken <- function(x, unrecognized = latexToken, math = TRUE, italics = FALSE, ...){
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
      mathopen <- '\\'
      mathclose <- '{}'
      if(!italics){
        mathopen <- '\\mathrm{'
        mathclose <- '}'
      }
      if(!math){
        warning('enforcing math mode for nested expression')
      }
      mathopen <- paste0('$',mathopen)
      mathclose <- paste0(mathclose, '$')
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

  x <- paste0('\\textrm{',x, '}')
  class(x) <- union('latex', class(x))
  x
}

