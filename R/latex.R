#' Convert Greek to Latex
#' 
#' Converts Greek letter names to latex.
#' By default, assumes latex package \code{upgreek} is available
#' so that lower case greek can be typeset upright, e.g. using 
#' backslash-upalpha instead of backslash-alpha. Do use the package
#' "upgreek" in your document preamble,
#' or else in your R environment set \code{options(spork_upgreek = FALSE)}.
#' 
#' @param x greek
#' @param ... ignored,
#' @param upgreek if TRUE, assume latex package \code{upgreek} is available
#' @family latex
#' @keywords internal
#' @export
#' @return latex
#' @examples
#' as_latex(greek())
as_latex.greek <- function(x, ..., upgreek = getOption('spork_upgreek', TRUE)){
  stopifnot(all(x %in% greek()))
  # https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols

    y <- x
  
  # general substitutions
  y[y == 'Upsilon1'] <- 'Upsilon'
  y[y == 'omega1'] <-  'varpi'
  y[y == 'theta1'] <-  'vartheta'
  y[y == 'phi1'] <-  'varphi'
  y[y == 'sigma1'] <-  'varsigma'
  y[y == 'stigma'] <-  'varsigma'
  
  # handle upgreek
  prefix <- '\\'
  stopifnot(length(upgreek) == 1, is.logical(upgreek))
  if(upgreek){
    prefix <- ifelse(grepl('[A-Z]', y), '\\Up', '\\up')
    y <- tolower(x) # see documentation for upgreek, e.g. \\Gamma -> \\Upgamma
  }
  y <- paste0(prefix, y)
  
  # Roman literals not covered by upgreek
  y[x == 'omicron'] <- 'o'
  y[x == 'Alpha'] <- 'A'
  y[x == 'Beta'] <- 'B'
  y[x == 'Epsilon'] <- 'E'
  y[x == 'Zeta'] <- 'Z'
  y[x == 'Eta'] <- 'H'
  y[x == 'Iota'] <- 'I'
  y[x == 'Kappa'] <- 'K'
  y[x == 'Mu'] <- 'M'
  y[x == 'Nu'] <- 'N'
  y[x == 'Omicron'] <- 'O'
  y[x == 'Rho'] <- 'P'
  y[x == 'Tau'] <- 'T'
  y[x == 'Chi'] <- 'X'
  
  class(y) <- c('latex','character')
  y
}


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
#' @param script_size three character values, one of which will be appended to token_open for unnested, nested, and multiply-nested contexts
#' @param ... passed to \code{unrecognized}; see \code{\link{latexToken}}
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_spork %>% as_latex
#' 'AUC_ss' %>% as_spork %>% as_latex
#' 'C_max_ss' %>% as_spork %>% as_latex
#' 'var^eta_j' %>% as_spork %>% as_latex
#' '& % $ # \\_ { } ~ \\^ \\' %>% as_spork %>% as_latex
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex
#' 'one joule (`Omega`) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex
#' 'one joule (\\`Omega\\`) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex

as_latex.spar <- function(
  x,
  newline = getOption('latex_newline','\n'),
  unrecognized = getOption('latex_unrecognized',spork::latexToken),
  token_open = getOption('latex_token_open', '\\textrm{'), ### was \\textrm
  token_close = getOption('latex_token_close','}'),
  math_open = getOption('latex_math_open', '\\mathrm{'),
  math_close = getOption('latex_math_close', '}'),
  label_open = getOption('latex_label_open', '\\('),
  label_close = getOption('latex_label_close', '\\)'),
  enforce_math = getOption('latex_enforce_math',TRUE),
  script_size = getOption('latex_script_size', c('','\\scriptsize ','\\tiny ')),
  ...
){
  # the latex of a spork is the sequential
  # combination of tokens.
  # Tokens _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Whitespace and recognized escapes are supplied literally.
  # unescaped '*' is promoted to \code{\cdot}.
  # surviving tokens are processed by 'unrecognized',
  # which escapes metacharacters and
  # names of Greek letters, but renders other
  # tokens literally.
  
  
  stopifnot(is.character(script_size), length(script_size) == 3)
  closers <- character(0)
  
  # appends appropriate script size to token_open
  tokenOpen <- function(token_open, closers, script_size){
    stopifnot(is.character(token_open), length(token_open) == 1)
    stopifnot(is.character(closers))
    stopifnot(is.character(script_size), length(script_size) == 3)
    level <- length(closers) + 1
    if(level > 3) level <- 3
    style <- script_size[[level]]
    x <- paste0(token_open, style)
    return(x)
  }
  
  active <- FALSE
  if(length(x)==0)return(structure(x, class = union('latex', class(x))))
  if(identical(x, ''))return(structure(x, class = union('latex', class(x))))
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
      token <- fun(
        token,
        unrecognized = unrecognized,
        token_open = tokenOpen(token_open, closers, script_size),
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
        token <- paste0(tokenOpen(token_open, closers, script_size),token,token_close)
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
      if(p == '#+'){
        token <- gsub('#','\\\\#',token)
        token <- paste0(tokenOpen(token_open, closers, script_size),token,token_close)
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
        token <- paste0(tokenOpen(token_open, closers, script_size), '*', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][.]'){
        token <- paste0(tokenOpen(token_open, closers, script_size), '.', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\][_]'){
        token <- paste0(tokenOpen(token_open, closers, script_size), '\\_', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0(tokenOpen(token_open, closers, script_size),"{\\textasciicircum}",token_close)
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
          base <- paste0(base,"{}_{")
          active <- FALSE
        }else{
          if(!grepl('[]}]$', base)){
            # must have something to subscript
            # https://en.wikipedia.org/wiki/Whitespace_character
            base <- paste0(base, "{}_{")  # 0.2.6 formerly ~_{ then \\,_{
          }else{
            base <- paste0(base, "{}_{")  # 0.2.6 formerly ~_{ then \\,_{
          }
        }
      }
      if(p == '\\^'){
        closers <- c('}', closers)
        if(active){
          base <- paste0(base, "{}^{")  # 0.3.0 was "^{"
          active <- FALSE
        }else{
          if(!grepl('[]}]$', base)){
            # must have something to superscript
            base <- paste0(base, "{}^{") # 0.2.6 formerly ~^{ then \\,{
          }else{
            base <- paste0(base, "{}^{") # 0.2.6 formerly ~^{ then \\,{
          }
        }
      }
      if(p %in% greek){
        # as of 0.2.6:
        # if we got here, the search term p
        # is literally one of spork greek vocabulary words.
        # These were formerly handled by latexToken,
        # but are now unreachable there since they are 
        # 'recognized' during parsing.
        # Calling as_latex() gives us the tex code
        # for the character of interest.
        # We reproduce here the openers and closers
        # (borrowed from latexToken)
        # that are needed to place these in math mode.
        # i.e.: \\alpha -> $\\mathrm{\\alpha}}$
        
        mathopen <- math_open
        mathclose <- math_close
        if(enforce_math){
          mathopen <- paste0(label_open,mathopen)
          mathclose <- paste0(mathclose, label_close)
        }

        token <- as_latex(as_greek(token))
        
        token <- paste0(
          tokenOpen(token_open, closers, script_size), 
            mathopen,
              token, 
            mathclose,
          token_close
        )
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p %in% ungreek){
        token <- gsub('`','',token)
        token <- paste0(tokenOpen(token_open, closers, script_size), token, token_close)
        if(active){
          base <- paste0(base, token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][`]'){
        token <- paste0(tokenOpen(token_open, closers, script_size), '`', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
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
  token_open = getOption('latex_token_open', '\\textrm{'), ### was \\textrm
  token_close = getOption('latex_token_close','}'),
  math_open = getOption('latex_math_open', '\\mathrm{'),
  math_close = getOption('latex_math_close', '}'),
  label_open = getOption('latex_label_open', '\\('),
  label_close = getOption('latex_label_close', '\\)'),
  enforce_math = getOption('latex_enforce_math',TRUE),
  ...
){
  special <- c(  '&',  '%',  '$',  '#',  '_',  '{',  '}','~', '^','\\') # special in latex
  replace <- c('\\&','\\%','\\$','\\#','\\_','\\{','\\}','\\({\\sim}\\)','{\\textasciicircum}','{\\textbackslash}')      # use in latex
  # greek <- c(
  #   'alpha','beta','gamma','delta',
  #   'epsilon','zeta','eta','theta',
  #   'iota','kappa','lambda','mu',
  #   'nu','xi','omicron','pi',
  #   'rho','sigma','tau','upsilon',
  #   'phi','chi','psi','omega'
  # )
  # # https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
  # names(greek) <- c(
  #   '\\alpha','\\beta','\\gamma','\\delta','\\epsilon','\\zeta',
  #   '\\eta','\\theta','\\iota','\\kappa','\\lambda','\\mu',
  #   '\\nu','\\xi',
  #   'o',
  #   '\\pi','\\rho','\\sigma','\\tau',
  #   '\\upsilon','\\phi','\\chi','\\psi','\\omega'
  # )
  # Greek <- c(
  #   'Alpha','Beta','Gamma','Delta',
  #   'Epsilon','Zeta','Eta','Theta',
  #   'Iota','Kappa','Lambda','Mu',
  #   'Nu','Xi','Omicron','Pi',
  #   'Rho','Sigma','Tau','Upsilon',
  #   'Phi','Chi','Psi','Omega'
  # )
  # names(Greek) <- c(
  #   'A','B','\\Gamma','\\Delta',
  #   'E','Z','H','\\Theta',
  #   'I','K','\\Lambda','M',
  #   'N','\\Xi','O','\\Pi',
  #   'P','\\Sigma','T','\\Upsilon',
  #   '\\Phi','X','\\Psi','\\Omega'
  # )
  # extra <- c( # these are things you can say in plotmath
  #   'Upsilon1','varepsilon','omega1',
  #   'theta1', 'phi1', 'sigma1',
  #   'vartheta','varphi','varsigma',
  #   'stigma', 'varrho','varpi'
  # )
  # 
  # names(extra) <- c( # these are things you can say in tex
  #   '\\Upsilon','\\varepsilon','\\varpi',
  #   '\\vartheta','\\varphi','\\varsigma',
  #   '\\vartheta','\\varphi','\\varsigma',
  #   '\\varsigma', '\\varrho','\\varpi'
  # )

  # escape <- function(x,pattern,replace)sub(
  #   fixed = TRUE,
  #   pattern,
  #   replace[match(pattern,replace)],
  #   x
  # )

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
  # nms <- c(greek, Greek, extra)
  nms <- greek()
  names(nms) <- as_latex(greek())
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
#'   '\\nAUC_ss',
#'   'C_max_ss\\n',
#'   'var^eta_j\\nrecords'
#' )
#' x <- as_spork(x)
#' writeLines(as_latex(x))
#' x <- as_spork('gravitational force\\n (kg\\.m/s^2.)')
#' explicit(x)
#' as_latex(x)
as_latex.spork <- function(x, ...){
  # as of 0.3.3, make spork explicit before converting to latex.
  # supports piece-wise math mode between linebreaks,
  # since linebreaks cannot occur in math mode.
  y <- explicit(x)
  y <- strsplit(y, '\\\\n')
  for(i in seq_along(y)){
    y[[i]] <- lapply(y[[i]], as_spar, USE.NAMES = F, ...)
    y[[i]] <- sapply(y[[i]], as_latex, USE.NAMES = F, ...)
  }
  y <- lapply(y, paste, collapse = ' \\\\ ')
  tail <- grepl('\\\\n$', x)
  tail <- ifelse(tail, ' \\\\ ', '')
  y <- paste0(y, tail)
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



