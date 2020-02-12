globalVariables('label')
#' Preview Something
#'
#' Creates a preview.
#' Generic, with methods for latex and plotmath.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family preview
#' @return see methods
#' @examples
#' library(magrittr)
#' 'V_c./F' %>% as_spork %>% as_plotmath %>% as_preview
#' \donttest{
#' 'V_c./F' %>% as_spork %>% as_latex %>% as_preview
#' }
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_plotmath %>% as_preview
#' \donttest{
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex %>% as_preview
#' }
as_preview <- function(x, ...)UseMethod('as_preview')

#' Preview Spork as Latex
#'
#' Preview spork after conversion to latex.
#' Creates and displays a temporary png file, after
#' conversion from pdf using \code{\link[latexpdf]{ghostconvert}}.
#' @param x spork; see \code{\link{as_spork}}
#' @param wide nominal page width
#' @param long nominal page length
#' @param dir a working directory; see \code{\link[latexpdf]{as.pdf}}
#' @param gs_cmd ghostscript command; see \code{\link[latexpdf]{ghostconvert}}
# @param preamble passed to \code{\link[latexpdf]{as.document}}
#' @param prolog passed to \code{\link[latexpdf]{as.document}}
#' @param epilog passed to \code{\link[latexpdf]{as.document}}
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family preview
#' @keywords internal
#' @importFrom latexpdf as.png
#' @importFrom latexpdf as.pdf
#' @importFrom latexpdf ghostconvert
#' @importFrom png readPNG
#' @importFrom grid grid.raster
#' @return invisible filepath
#' @examples
#' \donttest{
#' library(magrittr)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>%
#' as_spork %>%
#' as_latex %>%
#' as_preview
#' }
as_preview.latex <- function(
  x,
  wide = 70,
  long = 20,
  stem = 'latex_preview',
  dir = tempdir(),
  gs_cmd = getOption('gs_cmd','mgs'),
  #preamble = '',
  prolog = '\\begin{center}',
  epilog = '\\end{center}',
  ...
){
  stopifnot(length(x) == 1)
  pdf <- as.pdf(
    x,
    stem = stem,
    dir = dir,
    wide = wide,
    long = long,
    #preamble = preamble,
    prolog = prolog,
    epilog = epilog,
    ...
  )
  png <- ghostconvert(pdf, gs_cmd = gs_cmd, ...)
  img <- readPNG(png)
  grid.raster(img)
  invisible(png)
}

#' Compare Previews
#'
#' Compare previews of something.
#' Generic, with method \code{\link{as_previews.spork}}.
#' @param x object
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @return see methods
#' @family preview
#' @examples
#' example(as_previews.spork)
as_previews <- function(x,...)UseMethod('as_previews')

#' Compare Previews of Spork
#'
#' Compares plotmath and latex previews of spork
#' Generates png for both, and overlays
#' latex above plotmath.
#'
#' @param x length-one spork
#' @param wide width in mm of the latex image
#' @param long length in mm of the latex image
#' @param width width (default: inches) of the plotmath image
#' @param height height (default: inches) of the plotmath image
#' @param ... passed arguments
#' @export
#' @importFrom grid grid.newpage
#' @return invisible list of filepaths
#' @family preview
#' @examples
#' library(magrittr)
#' specials <- '& % $ # \\_ { } ~ \\^ \\'
#' \donttest{
#' specials %>% as_spork %>% as_previews
#' specials %>% gsub(' ','',.) %>% as_spork %>% as_previews
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_previews
#'
#' # disambiguation for plotmath and latex (see \code{\link[grDevices]{plotmath}}):
#'
#' 'epsilon.varepsilon' %>% as_spork %>% as_previews
#' 'rho.varrho' %>% as_spork %>% as_previews
#' 'Upsilon.Upsilon1' %>% as_spork %>% as_previews
#' 'phi.phi1.varphi' %>% as_spork %>% as_previews
#' 'sigma.sigma1.varsigma.stigma' %>% as_spork %>% as_previews
#' 'theta.vartheta.theta1' %>% as_spork %>% as_previews
#' 'omega.omega1.pi.varpi' %>% as_spork %>% as_previews
#' }

as_previews.spork <- function(x, wide = 70, long = 20, width = 3, height = 1,...){
  stopifnot(length(x) == 1)
  stopifnot(inherits(x, 'character'))
  x <- as_spork(x)
  grid.newpage()
  a <- as_preview(as_plotmath(x), width = width, height = height,...)
  b <- as_preview(as_latex(x), wide = wide, long = long,...)
  invisible(list(plotmath = a, latex = b))
}
#' Preview Spork as Plotmath
#'
#' Preview spork after conversion to plotmath.
#' Creates and displays a temporary png file with
#' a parsed expression.
#' @param x spork; see \code{\link{as_spork}}
#' @param width passed to \code{\link{as.png.plotmath}}
#' @param height passed to \code{\link{as.png.plotmath}}
#' @param ... passed arguments
#' @export
#' @keywords internal
#' @family preview
#' @keywords internal
#' @importFrom grDevices png
#' @importFrom png readPNG
#' @importFrom grid grid.raster
#' @importFrom latexpdf as.png
#' @return invisible filepath
#' @examples
#' library(magrittr)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>%
#' as_spork %>%
#' as_plotmath %>%
#' as_preview
as_preview.plotmath <- function(x, stem = 'plotmath_preview',width = 3, height = 1, ...){
  stopifnot(length(x) == 1)
  file <- as.png(x, stem = stem, width = width, height = height, ...)
  img <- readPNG(file)
  grid.raster(img)
  invisible(file)
}

#' Plot Spork
#'
#' Render spork in a ggplot.
#' @param data length-one spork; see \code{\link{as_spork}}
#' @param mapping ignored
#' @param ... ignored arguments
#' @param environment ignored
#' @param blank whether to use a blank plot area
#' @export
#' @family preview
#' @keywords internal
#' @method ggplot spork
#' @importFrom ggplot2 ggplot
#' @return gg
#' @examples
#' library(magrittr)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% ggplot
ggplot.spork <- function(data, mapping = aes(), ..., environment = parent.frame(), blank = TRUE){
  x <- data
  stopifnot(length(x) == 1)
  y <- as_plotmath(x)
  ggplot(y, blank = blank, ...)
}

#' @export
ggplot2::ggplot

#' Plot Plotmath
#'
#' Render plotmath in a ggplot.
#' @param data length-one plotmath; see \code{\link{as_plotmath}}
#' @param mapping ignored
#' @param ... ignored arguments
#' @param environment ignored
#' @param blank whether to use a blank plot area
#' @export
#' @family preview
#' @keywords internal
#' @method ggplot plotmath
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 scale_x_continuous
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_blank
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @return gg
#' @examples
#' library(magrittr)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_plotmath %>% ggplot
ggplot.plotmath <- function(data, mapping= aes(), ..., environment = parent.frame(), blank = TRUE){
  x <- data
  stopifnot(length(x)==1)
  p <- ggplot(data.frame(x = 1,y = 1,label = (x)))
  p <- p + geom_text(aes(x = 1,y = 1,label = label), parse = TRUE)
  if(blank){
    p <- p +
      scale_x_continuous(expand=c(0,0)) +
      scale_y_continuous(expand=c(0,0))
    p <- p + theme(
      axis.line=element_blank(),axis.text.x=element_blank(),
      axis.text.y=element_blank(),axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),legend.position="none",
      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),plot.background=element_blank())
  }
  p
}

#' Convert Spork to PNG
#'
#' Converts spork to png.
#' @param x spork; see \code{\link{as_spork}}
#' @param filename path for image file
#' @param width width
#' @param height height
#' @param units units
#' @param res resolution
#' @param ... passed arguments
#' @export
#' @family preview
#' @keywords internal
#' @method as.png spork
#' @importFrom grDevices png
#' @importFrom grDevices dev.off
#' @importFrom latexpdf as.png
#' @return invisible filepath
#' @examples
#' library(magrittr)
#' library(latexpdf)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as.png -> file
#' file
as.png.spork <- function(x, filename = tempfile(), width = 3, height = 1, units = 'in', res = 150, ...){
  args <- list(...)
  if(length(args))args <- args[names(args) %in% names(formals(png))]
  args <- c(list(filename = filename, width = width, height = height, units = units, res = res), args)
  do.call(png, args)
  p <- ggplot(x, ...)
  print(p)
  dev.off()
  invisible(filename)
}
#' Convert Plotmath to PNG
#'
#' Converts plotmath to png.
#' @param x plotmath; see \code{\link{as_plotmath}}
#' @param filename path for image file
#' @param width width
#' @param height height
#' @param units units
#' @param res resolution
#' @param ... passed arguments
#' @export
#' @family preview
#' @keywords internal
#' @method as.png plotmath
#' @importFrom grDevices png
#' @importFrom latexpdf as.png
#' @return invisible filepath
#' @examples
#' library(magrittr)
#' library(latexpdf)
#' 'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_plotmath %>% as.png -> file
#' file
as.png.plotmath <- function(x, filename = tempfile(), width = 3, height = 1, units = 'in', res = 150, ...){
  args <- list(...)
  if(length(args))args <- args[names(args) %in% names(formals(png))]
  args <- c(list(filename = filename, width = width, height = height, units = units, res = res), args)
  do.call(png, args)
  p <- ggplot(x, ...)
  print(p)
  dev.off()
  invisible(filename)
}



