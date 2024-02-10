#' Convert Greek to Html
#' 
#' Converts Greek letter names to html.
#' @param x greek
#' @param ... ignored
#' @family html
#' @keywords internal
#' @export
#' @return html
#' @examples
#' as_html(greek())
as_html.greek <- function(x, ...){
  stopifnot(all(x %in% greek()))
  y <- paste0('&', x, ';') # construct default character entity reference
  y[x == 'Upsilon1']   <- '&upsih;'
  y[x == 'varepsilon'] <- '&epsilon;'
  y[x == 'omega1']     <- '&omega;'
  y[x == 'theta1']     <- '&thetasym;'
  y[x == 'phi1']       <- '&phi;'
  y[x == 'sigma1']     <- '&sigmaf;'
  y[x == 'vartheta']   <- '&thetasym;'
  y[x == 'varphi']     <- '&phi;'
  y[x == 'varsigma']   <- '&sigmaf;'
  y[x == 'stigma']     <- '&sigmaf;'
  y[x == 'varrho']     <- '&rho;'
  y[x == 'varpi']      <- '&piv;'

  class(y) <- c('html','character')
  y
}

#' Convert Default to Html
#' 
#' Coerces to spork, then to html.
#' @param x inherits character
#' @param ... passed arguments
#' @family html
#' @keywords internal
#' @export
#' @return html
#' @examples
#' as_html('anything')
as_html.default <- function(x, ...){
  x <- as_spork(x, ...)
  x <- as_html(x, ...)
  x
}

#' Coerce to Html
#'
#' Coerce to html.  Generic, with method
#' \code{\link{as_html.spork}}.
#'
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family html
#' @return html
#' @examples
#' example(as_html.spork)
as_html <- function(x, ...)UseMethod('as_html')

#' Convert One Spork to Html
#'
#' Converts one spork to html.
#' See description for \code{\link{as_spork}}.
#' By default, unrecognized tokens are returned
#' literally.  However, Greek symbols and html
#' metacharacters are escaped.
#' See \code{\link{htmlToken}}.
#'
#' Experimental support is implemented for
#' the newline character (\code{'\\n'}).
#' Default behavior is to introduce linebreaks
#' (<br/>) into the resulting
#' html.
#'
#' @export
#' @family interface
#' @return html
#' @family html
#' @param x spar
#' @param newline value to replace \code{'\\n'}
#' @param unrecognized function to process unrecognized tokens: default \code{\link{htmlToken}}
#' @param token_open,token_close these wrap text-like portions of the label; the defaults try to give upright characters (non-italic); also passed to \code{\link{htmlToken}}
#' @param math_open,math_close these wrap math-like portions of the label;  the defaults try to give upright characters (non-italic) which may not work for Greek symbols; also passed to \code{\link{htmlToken}}
#' @param label_open,label_close these wrap the entire label
# @param enforce_math whether to enforce math mode for nested expression: \code{\link{htmlToken}}
#' @param ... passed to \code{unrecognized}; see \code{\link{htmlToken}}
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_html
#' 'AUC_ss' %>% as_html
#' 'C_max_ss' %>% as_html
#' 'var^eta_j' %>% as_html
#' '& < % $ # \\_ { } ~ \\^ \\' %>% as_html
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_html
#' 'one joule (`Omega`) ~ 1 kg*m^2./s^2' %>% as_html
#' 'one joule (\\`Omega\\`) ~ 1 kg*m^2./s^2' %>% as_html

as_html.spar <- function(
  x,
  newline = getOption('html_newline','<br/>'),
  unrecognized = getOption('html_unrecognized',spork::htmlToken),
  token_open = getOption('html_token_open', ''),
  token_close = getOption('html_token_close',''),
  math_open = getOption('html_math_open', ''),
  math_close = getOption('html_math_close', ''),
  label_open = getOption('html_label_open', ''),
  label_close = getOption('html_label_close', ''),
  # enforce_math = getOption('html_enforce_math',TRUE),
  ...
){
  # the html of a spork is the sequential
  # combination of tokens.
  # Tokens _ or ^ or . are non-printing
  # but trigger nesting or un-nesting.
  # Whitespace and recognized escapes are supplied literally.
  # unescaped '*' is promoted to '&#183;'.
  # surviving tokens are processed by 'unrecognized',
  # which escapes metacharacters and
  # names of Greek letters, but renders other
  # tokens literally.

  closers <- character(0)
  active <- FALSE
  if(length(x)==0)return(structure(x, class = union('html', class(x))))
  if(identical(x, ''))return(structure(x, class = union('html', class(x))))
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
        token_open = token_open,
        token_close = token_close,
        math_open = math_open,
        math_close = math_close,
        label_open = label_open,
        label_close = label_close,
        #enforce_math = enforce_math,
        ...
      )
      if(active){
        base <- paste0(base, token)
      }else{
         if(grepl('[]}]$',base)){ # not empty nest for latex.  html equivalent?
          base <- paste0(base, token)
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
          base <- paste0(base, '', token) # changed from ' '; html seems less fussy than latex
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '', token) # changing here too
            active <- TRUE
          }else{ # empty nest or start of line
            base <- paste0(base, token)
            active <- TRUE
          }
        }
      }
      if(p == '#+'){
        token <- paste0(token_open,token,token_close)
        if(active){
          base <- paste0(base, '', token) # changed from ' '; html seems less fussy than latex
        }else{
          if(grepl('[]}]$',base)){ # not empty nest
            base <- paste0(base, '', token) # changing here too
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
        token <- paste0(token_open, '_', token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[\\]\\^'){
        token <- paste0(token_open,'^',token_close)
        if(active){
          base <- paste0(base, ' ', token)
        }else{
          base <- paste0(base, ' ', token)
          active <- TRUE
        }
      }
      if(p == '[*]'){
        token <- paste0("&#183;")
        if(active){
          base <- paste0(base, '', token)
          active <- FALSE
        }else{
          base <- paste0(base, '', token)
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
              base <- paste0(base, cl)
            }
          }
        }
      }
      if(p == '[_]'){
        closers <- c('</sub>', closers)
        if(active){
          base <- paste0(base,"<sub>")
          active <- FALSE
        }else{
          if(!grepl('[]}]$', base)){
            base <- paste0(base, "<sub>") # 0.2.6 removed &#160;
          }else{
            base <- paste0(base, "<sub>") # 0.2.6 removed &#160;
          }
        }
      }
      if(p == '\\^'){
        closers <- c('</sup>', closers)
        if(active){
          base <- paste0(base, "<sup>")
          active <- FALSE
        }else{
          if(!grepl('[]}]$', base)){
            base <- paste0(base, "<sup>") # 0.2.6 removed &#160;
          }else{
            base <- paste0(base, "<sup>") # 0.2.6 removed &#160;
          }
        }
      }
      if(p %in% greek){
        token <- html2xml(as_html(as_greek(token)))
        token <- paste0(token_open, token, token_close)
        if(active){
          base <- paste0(base, token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p %in% ungreek){
        token <- gsub('`','',token)
        token <- paste0(token_open, token, token_close)
        if(active){
          base <- paste0(base, token)
        }else{
          base <- paste0(base, token)
          active <- TRUE
        }
      }
      if(p == '[\\][`]'){
        token <- paste0(token_open, '`', token_close)
        if(active){
          base <- paste0(base, token)
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

#' Process Html Token
#'
#' Pre-processes a html token not recognized as
#' spork.  Escapes the common names for Greek letters
#' and escapes html metacharacters.
#'
#' @param x character
# @param unrecognized function to process unrecognized tokens
#' @param token_open,token_close these wrap the entire token (used once); by default the token is text-like
#' @param math_open,math_close these wrap math-like portions of the token;  the defaults try to give upright characters (non-italic) which may not work for Greek symbols
#' @param label_open,label_close these re-wrap math-like portions of the token
# @param enforce_math whether to enforce math mode for nested expression
#' @param ... ignored arguments
#' @export
#' @family html
#' @family interface
#' @return html
#' @examples
#' htmlToken('foo')
#' htmlToken('alpha')
#' htmlToken('Alpha')
htmlToken <- function(
  x,
  #unrecognized = htmlToken,
  token_open = getOption('html_token_open', ''),
  token_close = getOption('html_token_close',''),
  math_open = getOption('html_math_open', ''),
  math_close = getOption('html_math_close', ''),
  label_open = getOption('html_label_open', ''),
  label_close = getOption('html_label_close', ''),
  #enforce_math = getOption('html_enforce_math',TRUE),
  ...
){
  special <- c(  '&',  '<', '\\' )        # special in html
  replace <- c('&amp;','&lt;', '&#92;')      # use in html
  # greek <- c( # look for these
  #   'alpha','beta','gamma','delta',
  #   'epsilon','zeta', 'eta','theta',# no good match for arc epsilon in html
  #   'iota','kappa','lambda','mu',
  #   'nu','xi','omicron','pi',
  #   'rho','sigma','tau', 'upsilon',
  #   'phi','chi','psi','omega' # no regular phi in html
  # )
  # # https://www.overleaf.com/learn/latex/List_of_Greek_letters_and_math_symbols
  # # as of 0.2.5, address kableExtra bug 814 using html2xml
  # # https://github.com/haozhu233/kableExtra/issues/814 .
  # # https://www.thoughtco.com/html-codes-greek-characters-4062212
  # names(greek) <- html2xml(paste0('&', greek, ';')) # replace with these
  # 
  # Greek <- c( # look for these
  #   'Alpha','Beta','Gamma','Delta',
  #   'Epsilon','Zeta','Eta','Theta',
  #   'Iota','Kappa','Lambda','Mu',
  #   'Nu','Xi','Omicron','Pi',
  #   'Rho','Sigma','Tau','Upsilon',
  #   'Phi','Chi','Psi','Omega'
  # )
  # names(Greek) <- html2xml(paste0('&', Greek, ';')) # replace with these
  # 
  # texExtra <- c(
  #   'Upsilon', 'varepsilon','vartheta','varpi','varsigma','varrho'
  # )
  # plotmathExtra <- c(
  #   'theta1','phi1','sigma1','omega1','Upsilon1','stigma1'
  # )
  # # extra is a combination of texExtra and plotmathExtra
  # # spork supports union greek, Greek, extra
  # extra <- c( # look for these
  #   'Upsilon1','varepsilon','omega1',
  #   'theta1', 'phi1', 'sigma1',
  #   'vartheta','varphi','varsigma',
  #   'stigma', 'varrho','varpi' # no fancy rho in html
  # )
  # 
  # names(extra) <- html2xml(c( # replace with these
  #   '&upsih;','&epsilon;','&omega;',
  #   '&thetasym;','&phi;','&sigmaf;',
  #   '&thetasym;','&phi;','&sigmaf;',
  #   '&sigmaf;', '&rho;','&piv;'
  # ))

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
  names(nms) <- html2xml(as_html(greek()))
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
      # if(enforce_math){
      #   mathopen <- paste0(label_open,mathopen)
      #   mathclose <- paste0(mathclose, label_close)
      # }

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
  class(x) <- union('html', class(x))
  x
}

#' Convert Spork to Html
#'
#' Converts spork to html.
#' Vectorized version of \code{\link{as_html.spar}}.
#'
#' @export
#' @param x spork
#' @param ... passed to \code{\link{as_html.spar}}
#' @return html
#' @family html
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
#' as_html(x)
#' as_html(as_spork('gravitational force (kg\\.m/s^2.)'))
as_html.spork <- function(x, ...){
  y <- lapply(x, as_spar, USE.NAMES = F, ...)
  y <- sapply(y, as_html, USE.NAMES = F, ...)
  if(length(y) == 0) y <- character(0)
  class(y) <- union('html', class(y))
  y
}
#' Subset Html
#'
#' Subsets html, retaining class.
#' @param x html
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family html
#' @return html
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_html(as_spork(x))
#' class(x)
#' class(x[1])
`[.html` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('html', class(y))
  y
}

#' Element-select Html
#'
#' Element-selects html, retaining class.
#' @param x html
#' @param ... passed to next method
#' @export
#' @keywords internal
#' @family html
#' @return html
#' @examples
#' x <- c(
#'   'V_c./F',
#'   'AUC_ss',
#'   'C_max_ss',
#'   'var^eta_j'
#' )
#' x <- as_html(as_spork(x))
#' class(x)
#' class(x[[1]])
`[[.html` <- function(x, ...){
  y <- NextMethod()
  # contrasts and levels will have been handled
  class(y) <- union('html', class(y))
  y
}


#' Convert HTML Greek entity references to XML
#' 
#' Converts HTML Greek entity references to XML entity references.
#' This is necessary because of a kableExtra bug: 
#' https://github.com/haozhu233/kableExtra/issues/814 .
#' The mappings used here are from 
#' https://www.thoughtco.com/html-codes-greek-characters-4062212
#' 
#' @param x html character
#' @param ... ignored
#' @keywords internal
#' @family html
#' @return html
#' @examples
#' htmlToken('alpha')
#' htmlToken('Upsilon1')
#' htmlToken('vartheta')
#' htmlToken('stigma')
#' htmlToken('varrho')
#' htmlToken('varpi')
#' htmlToken('Upsilon1')
 
html2xml <- function(x, ...){
stopifnot(inherits(x,'character'))
html <- c(
'&Alpha;',
'&alpha;',
'&Beta;',
'&beta;',
'&Gamma;',
'&gamma;',
'&Delta;',
'&delta;',
'&Epsilon;',
'&epsilon;',
'&Zeta;',
'&zeta;',
'&Eta;',
'&eta;',
'&Theta;',
'&theta;',
'&Iota;',
'&iota;',
'&Kappa;',
'&kappa;',
'&Lambda;',
'&lambda;',
'&Mu;',
'&mu;',
'&Nu;',
'&nu;',
'&Xi;',
'&xi;',
'&Omicron;',
'&omicron;',
'&Pi;',
'&pi;',
'&Rho;',
'&rho;',
'&Sigma;',
'&sigma;',
'&sigmaf;',
'&Tau;',
'&tau;',
'&Upsilon;',
'&upsilon;',
'&Phi;',
'&phi;',
'&Chi;',
'&chi;',
'&Psi;',
'&psi;',
'&Omega;',
'&omega;',
'&upsih;',   # https://www.compart.com/en/unicode/U+03D2
'&thetasym;',
'&piv;'
)

xml <- c(
'&#913;',
'&#945;',
'&#914;',
'&#946;',
'&#915;',
'&#947;',
'&#916;',
'&#948;',
'&#917;',
'&#949;',
'&#918;',
'&#950;',
'&#919;',
'&#951;',
'&#920;',
'&#952;',
'&#921;',
'&#953;',
'&#922;',
'&#954;',
'&#923;',
'&#955;',
'&#924;',
'&#956;',
'&#925;',
'&#957;',
'&#926;',
'&#958;',
'&#927;',
'&#959;',
'&#928;',
'&#960;',
'&#929;',
'&#961;',
'&#931;',
'&#963;',
'&#962;',
'&#932;',
'&#964;',
'&#933;',
'&#965;',
'&#934;',
'&#966;',
'&#935;',
'&#967;',
'&#936;',
'&#968;',
'&#937;',
'&#969;', 
'&#978;',   # https://www.compart.com/en/unicode/U+03D2
'&#977;',   # https://en.wikipedia.org/wiki/Theta
            # https://www.compart.com/en/unicode/U+03D1
'&#982;'    # https://en.wikipedia.org/wiki/List_of_XML_and_HTML_character_entity_references
            # https://www.compart.com/en/unicode/U+03D6

)

stopifnot(length(html) == length(xml))
i <- match(x, table = html, nomatch = 0)
matched <- i != 0
x[matched] <- xml[i[matched]]
x

}

