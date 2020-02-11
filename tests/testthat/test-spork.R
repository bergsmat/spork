
test_that('all valid spork print as axis label',{
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  x <- data.frame(y=1:10, x = 1:10)
  label <- 'one joule (Omega) ~ 1 kg*m^2./s^2' %>%
    as_spork %>%
    as_plotmath %>%
    as.expression
  attr(x, 'label') <- label
  expect_silent(ggplot(x,aes(x, y)))
  label <- 'gravitational force \\\\ (kg\\.m/s^2.)' %>%
    as_spork %>%
    as_plotmath %>%
    as.expression
  expect_silent(ggplot(x,aes(x, y)))

 expect_identical(
  'gravitational force  (kg\\.m/s^2.)' %>%
    as_spork %>%
    as_plotmath %>%
    as.character,
  "gravitational*' '*force*'  '*'(kg'*'.'*m/s^{2}*')'"
  )

  expect_identical(
   '1 joule^\\*. ~1 kg m^2./s^2' %>%
     as_spork %>%
     as_plotmath %>%
     as.character,
   "1*' '*joule^{'*'}*' '*~1*' '*kg*' '*m^{2}*'/s'^{2}"
  )
})

test_that('R reserved words survive in print.dg labels',{
  library(magrittr)
  library(dplyr)
  library(ggplot2)
  library(testthat)
  x <- data.frame(y=1:10, x = 1:10)
  label <- 'for NaN% joule^\\*. ~1 kg m^2./s^2. %' %>%
    as_spork %>%
    as_plotmath %>%
    as.expression
  attr(x, 'label') <- label
  expect_silent(ggplot(x,aes(x, y)))
})
test_that('as_latex is stable',{
  expect_identical(
    'Omega joule^\\*. ~1 kg*m^2./s^2' %>%
      as_spork %>%
      as_latex %>%
      as.character,
    "$\\mathrm{ \\textrm{$\\mathrm{\\Omega}$} \\textrm{ } \\textrm{joule}^{ \\textrm{*}} \\textrm{ } \\textrm{${\\sim}$1} \\textrm{ } \\textrm{kg} {\\cdot} \\textrm{m}^{ \\textrm{2}} \\textrm{/s}^{ \\textrm{2}}}$"
  )
  expect_identical(
    'gravitational force gamma (kg\\.m/s^2.)' %>%
      as_spork %>%
      as_latex %>%
      as.character,
    "$\\mathrm{ \\textrm{gravitational} \\textrm{ } \\textrm{force} \\textrm{ } \\textrm{$\\mathrm{\\gamma}$} \\textrm{ } \\textrm{(kg} \\textrm{.} \\textrm{m/s}^{ \\textrm{2}} \\textrm{)}}$"
  )

})

test_that('spork to plotmath is stable',{
  e <- c(
  '',
  '.',
  '^',
  '.^',
  '^.',
  'a',
  'a.',
  '.a',
  '1',
  '1.',
  '.1',
  'a^',
  '^a',
  '1^',
  '^1',

  'a.^',
  'a^.',
  '.a^',
  '.^a',
  '^a.',
  '^.a', #

  '1.^',
  '1^.',
  '.1^',
  '.^1',
  '^1.',
  '^.1', #


  'a1.^',
  'a1^.',
  '.a1^',
  '.^a1',
  '^a1.',
  '^.a1',

  '1a.^',
  '1a^.',
  '.1a^',
  '.^1a',
  '^1a.',
  '^.1a',

  '".^',
  '"^.',
  '."^',
  '.^"',
  '^".',
  '^."',

  "'.^",
  "'^.",
  ".'^",
  ".^'",
  "^'.",
  "^.'",

  "  ",
  "  xx",
  "xx  ",


  "  xx  ",
  "xx  xx",

  "\\\\",
  "\\*",
  "\\.",
  "\\_",

  "*",
  "a*b",
  "a * b",
  "a *b",
  "a\\*b",

  "a\\*b$",

  "H_b^A_1^c",
  "H_b^A_1^c.",
  "H_b^A_1^.c",
  "H_b^A_1.^c",
  "H_b^A_.1^c",
  "H_b^A._1^c",
  "H_b^.A_1^c",
  "H_b.^A_1^c",
  "H_.b^A_1^c",
  "H._b^A_1^c",
  ".H_b^A_1^c",

  "____.",
  "___.",
  "__.",
  "_.",
  ".",
  "^^^^.",
  "^^^.",
  "^^.",
  "^.",
  ".",

  "H_b^A_1^c.",
  "_ ",
  " _",
  " = ",
  " _ ",
  "^c.",
  " ^c.",
  "^ c.",
  "^c .",
  "^c. ",
  " ^ c . ",
  " H _ b ^ A _ 1 ^ c . ",
  " H]_]b ^]A]_]1]^]c].]"
)
f <- c(
  "",
  "",
  "''^{}",
  "''^{}",
  "''^{}",
  "a",
  "a",
  "a",
  "1",
  "1",
  "1",
  "a^{}",
  "''^{a}",
  "1^{}",
  "''^{1}",
  "a^{}",
  "a^{}",
  "a^{}",
  "''^{a}",
  "''^{a}",
  "''^{}*a",
  "1^{}",
  "1^{}",
  "1^{}",
  "''^{1}",
  "''^{1}",
  "''^{}*1",
  "a1^{}",
  "a1^{}",
  "a1^{}",
  "''^{a1}",
  "''^{a1}",
  "''^{}*a1",
  "'1a'^{}",
  "'1a'^{}",
  "'1a'^{}",
  "''^{'1a'}",
  "''^{'1a'}",
  "''^{}*'1a'",
  "'\"'^{}"  ,
  "'\"'^{}",
  "'\"'^{}",
  "''^{'\"'}",
  "''^{'\"'}",
  "''^{}*'\"'",
  "'\\''^{}",
  "'\\''^{}",
  "'\\''^{}",
  "''^{'\\''}",
  "''^{'\\''}",
  "''^{}*'\\''",
  "'  '",
  "'  '*xx",
  "xx*'  '",
  "'  '*xx*'  '",
  "xx*'  '*xx",
  "'\\\\\\\\'",
  "'*'",
  "'.'",
  "'_'",
  "''%.%''",
  "a%.%b",
  "a*' '%.%' '*b",
  "a*' '%.%b",
  "a*'*'*b",
  "a*'*'*'b$'",
  "H[b^{A[1^{c}]}]",
  "H[b^{A[1^{c}]}]",
  "H[b^{A[1^{}*c]}]",
  "H[b^{A[1]*''^{c}}]",
  "H[b^{A[]*1^{c}}]",
  "H[b^{A}*''[1^{c}]]",
  "H[b^{}*A[1^{c}]]",
  "H[b]*''^{A[1^{c}]}",
  "H[]*b^{A[1^{c}]}",
  "H[b^{A[1^{c}]}]",
  "H[b^{A[1^{c}]}]",
  "''[''[''[''[]]]]",
  "''[''[''[]]]",
  "''[''[]]",
  "''[]",
  "",
  "''^{''^{''^{''^{}}}}",
  "''^{''^{''^{}}}",
  "''^{''^{}}",
  "''^{}",
  "",
  "H[b^{A[1^{c}]}]",
  "''[' ']",
  "' '[]",
  "' '*'='*' '",
  "' '[' ']",
  "''^{c}",
  "' '^{c}",
  "''^{' '*c}",
  "''^{c*' '}",
  "''^{c}*' '",
  "' '^{' '*c*' '}*' '",
  "' '*H*' '[' '*b*' '^{' '*A*' '[' '*1*' '^{' '*c*' '}*' ']}]",
  "' '*'H]'[']b'*' '^{']A]'[']1]'^{']c]'}*']']}]"
)
g <- as_plotmath(as_spork(e)) %>% as.character
expect_identical(f, g)
})

test_that('extreme juxtapostion without escape succeeds',{
  library(magrittr)
  library(testthat)
  render <- . %>% as_spork %>% as_plotmath %>% as.expression
  expect_silent('^1' %>% render)
  expect_silent('^*' %>% render)
  expect_silent('*^' %>% render)
  expect_silent('* ^' %>% render)
  expect_silent(' *^' %>% render)
  expect_silent('*^ ' %>% render)
  expect_silent('^\\*' %>% render)
  expect_silent('\\^\\*' %>% render)
  expect_silent('\\*' %>% render)
  expect_silent('\\\\' %>% render)
  expect_silent('\\_' %>% render)
  expect_silent('\\.' %>% render)
  expect_silent('\\^' %>% render)
  expect_silent('\\*^' %>% render)
  expect_silent('\\*^' %>% render)
  expect_silent('^\\.' %>% render)
  expect_silent('^\\\\' %>% render)
  expect_silent('^\\^' %>% render)
  expect_silent('^\\_' %>% render)
  expect_silent('1 joule^\\*. ~1 kg m^2./s^2' %>% render)
  expect_silent('^\\*. ' %>% render)
})
test_that('arbitrary plotmath escapes succeed by default',{
  library(magrittr)
  library(testthat)
  render <- . %>% as_spork %>% as_plotmath %>% as.expression
  expect_silent(' $ \n \\$ \\\t \\\\$ \\\\\' \\\\\\$ \\\\\\\" \\\\\\\\$ ' %>% render)
  expect_silent(' % \n \\% \\\t \\\\% \\\\\' \\\\\\% \\\\\\\" \\\\\\\\% ' %>% render)
})

test_that('expressions render without error',{
  library(magrittr)
  library(testthat)
  render <- . %>% as_spork %>% as_plotmath %>% as.expression
expect_silent( '^*' %>% render )
expect_silent( '^\\*' %>% render )
expect_silent( '^\\*.' %>% render )
expect_silent( '^\\.' %>% render )
expect_silent( '^\\\\' %>% render )
expect_silent( '\\\\' %>% render )
expect_silent( '^\\^' %>% render )
expect_silent( '^\\_' %>% render )
expect_silent( '\\^' %>% render )
expect_silent( '^.a' %>% render )
expect_silent( '\\$' %>% render )
expect_true( spork_to_plotmath('\\$') %>% goodToken)
expect_false( spork_to_plotmath('\\$', unescape = FALSE) %>% goodToken)
expect_false(spork_to_plotmath('\\$', unrecognized = function(x,...)x) %>% goodToken)
options(plotmath_unrecognized = function(x,...)x)
expect_false(spork_to_plotmath('\\$') %>% goodToken)
options(plotmath_unrecognized = NULL)
expect_true(spork_to_plotmath('\\$') %>% goodToken)
expect_identical(plotmathToken("\\$"), "'\\\\$'")
expect_identical(plotmathToken("\\$", unescape = FALSE),  "'\\$'")
expect_true(plotmathToken("\\$") %>% goodToken)
options(plotmath_unescape = FALSE)
expect_false(plotmathToken("\\$") %>% goodToken)
options(plotmath_unescape = NULL)
expect_false(plotmathToken("\\$", unescape = FALSE) %>% goodToken)
expect_identical(plotmathToken('foo'), 'foo')
expect_identical(plotmathToken('foo',conditional = FALSE),"'foo'")
options('plotmath_conditional_quote' = FALSE)
expect_identical(plotmathToken('foo'), "'foo'")
options('plotmath_conditional_quote' = NULL)
expect_identical(plotmathToken('foo'),"foo")

})

test_that('as_previews is stable',{
  library(magrittr)
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_plotmath %>% as_preview
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex %>% as_preview
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_previews
  expect_silent('V_c./F' %>% as_spork %>%as_previews)
  expect_silent('AUC_ss' %>% as_spork %>%as_previews)
  expect_silent('C_max_ss' %>% as_spork %>%as_previews)
  expect_silent('var^eta_j' %>% as_spork %>%as_previews)
  expect_silent('gravitational force - gamma (kg\\.m/s^2.)'%>% as_spork %>%as_previews)
  expect_silent('C(t_j.) = C_0. * epsilon^-kt_j' %>% as_spork %>%as_previews)
  expect_silent('eta^eta' %>% as_spork %>%as_previews)
  expect_silent('Eta^Eta' %>% as_spork %>%as_previews)
  expect_silent('omicron' %>% as_spork %>%as_previews)

  expect_silent('Alpha^alpha' %>% as_spork %>%as_previews)
  expect_silent('Beta^beta' %>% as_spork %>%as_previews)
  expect_silent('Gamma^gamma' %>% as_spork %>%as_previews)
  expect_silent('Delta^delta' %>% as_spork %>%as_previews)
  expect_silent('Epsilon^epsilon' %>% as_spork %>%as_previews)
  expect_silent('Zeta^zeta' %>% as_spork %>%as_previews)
  expect_silent('Eta^eta' %>% as_spork %>%as_previews)
  expect_silent('Theta^theta' %>% as_spork %>%as_previews)
  expect_silent('Iota^iota' %>% as_spork %>%as_previews)
  expect_silent('Kappa^kappa' %>% as_spork %>%as_previews)
  expect_silent('Lambda^lambda' %>% as_spork %>%as_previews)
  expect_silent('Mu^mu' %>% as_spork %>%as_previews)
  expect_silent('Nu^nu' %>% as_spork %>%as_previews)
  expect_silent('Xi^xi' %>% as_spork %>%as_previews)
  expect_silent('Omicron^omicron' %>% as_spork %>%as_previews)
  expect_silent('Pi^pi' %>% as_spork %>%as_previews)
  expect_silent('Rho^rho' %>% as_spork %>%as_previews)
  expect_silent('Sigma^sigma' %>% as_spork %>%as_previews)
  expect_silent('Tau^tau' %>% as_spork %>%as_previews)
  expect_silent('Upsilon^upsilon' %>% as_spork %>%as_previews)
  expect_silent('Phi^phi' %>% as_spork %>%as_previews)
  expect_silent('Chi^chi' %>% as_spork %>%as_previews)
  expect_silent('Psi^psi' %>% as_spork %>%as_previews)
  expect_silent('Omega^omega' %>% as_spork %>%as_previews)
  'Tau.iota.mu.omicron.theta.epsilon.upsilon.sigma' %>%
    as_spork %>%as_previews
  'varsigma~Upsilon1~varrho' %>% structure(class = c('plotmath','character')) %>% as_preview
  '$\\sigma$ $\\varsigma$ $\\Upsilon$' %>% structure(class = c('latex','character')) %>% as_preview

})
