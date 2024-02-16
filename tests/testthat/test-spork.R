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
  'gravitational force (kg\\.m/s^2.)' %>%
    as_spork %>%
    as_plotmath %>%
    as.character,
  "'gravitational'*' '*'force'*' '*'(kg'*'.'*'m/s'^{'2'}*')'"
  )

  expect_identical(
   '1 joule^\\*. ~1 kg m^2./s^2' %>% 
     as_spork %>%
     as_plotmath %>%
     as.character,
   "'1'*' '*'joule'^{'*'}*' '*'~1'*' '*'kg'*' '*'m'^{'2'}*'/s'^{'2'}"
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
    "\\(\\mathrm{\\textrm{\\(\\mathrm{\\Upomega}\\)} \\textrm{ } \\textrm{joule}{}^{ \\textrm{\\scriptsize *}} \\textrm{ } \\textrm{\\({\\sim}\\)1} \\textrm{ } \\textrm{kg} {\\cdot} \\textrm{m}{}^{\\textrm{\\scriptsize 2}} \\textrm{/s}{}^{\\textrm{\\scriptsize 2}}}\\)"
  )
  expect_identical(
    'gravitational force gamma (kg\\.m/s^2.)' %>% 
      as_spork %>%
      as_latex %>%
      as.character,
    "\\(\\mathrm{\\textrm{gravitational} \\textrm{ } \\textrm{force} \\textrm{ } \\textrm{\\(\\mathrm{\\upgamma}\\)} \\textrm{ } \\textrm{(kg} \\textrm{.} \\textrm{m/s}{}^{\\textrm{\\scriptsize 2}} \\textrm{)}}\\)"
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
  "H_b^A_1.^c", # 70, changed at 0.2.6
  "H_b^A_.1^c",
  "H_b^A._1^c", # 72, changed at 0.2.6
  "H_b^.A_1^c",
  "H_b.^A_1^c", # 74, changed at 0.2.6
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
   "''"                                                                   
  , ""                                                                     
  , "''^{}"                                                                
  , "''^{}"                                                                
  , "''^{}"                                                                
  , "'a'"                                                                  
  , "'a'"                                                                  
  , "'a'"                                                                  
  , "'1'"                                                                  
  , "'1'"                                                                  
  , "'1'"                                                                  
  , "'a'^{}"                                                               
  , "''^{'a'}"                                                             
  , "'1'^{}"                                                               
  , "''^{'1'}"                                                             
  , "'a'^{}"                                                               
  , "'a'^{}"                                                               
  , "'a'^{}"                                                               
  , "''^{'a'}"                                                             
  , "''^{'a'}"                                                             
  , "''^{}*'a'"                                                            
  , "'1'^{}"                                                               
  , "'1'^{}"                                                               
  , "'1'^{}"                                                               
  , "''^{'1'}"                                                             
  , "''^{'1'}"                                                             
  , "''^{}*'1'"                                                            
  , "'a1'^{}"                                                              
  , "'a1'^{}"                                                              
  , "'a1'^{}"                                                              
  , "''^{'a1'}"                                                            
  , "''^{'a1'}"                                                            
  , "''^{}*'a1'"                                                           
  , "'1a'^{}"                                                              
  , "'1a'^{}"                                                              
  , "'1a'^{}"                                                              
  , "''^{'1a'}"                                                            
  , "''^{'1a'}"                                                            
  , "''^{}*'1a'"                                                           
  , "'\"'^{}"                                                              
  , "'\"'^{}"                                                              
  , "'\"'^{}"                                                              
  , "''^{'\"'}"                                                            
  , "''^{'\"'}"                                                            
  , "''^{}*'\"'"                                                           
  , "'\\''^{}"                                                             
  , "'\\''^{}"                                                             
  , "'\\''^{}"                                                             
  , "''^{'\\''}"                                                           
  , "''^{'\\''}"                                                           
  , "''^{}*'\\''"                                                          
  , "'  '"                                                                 
  , "'  '*'xx'"                                                            
  , "'xx'*'  '"                                                            
  , "'  '*'xx'*'  '"                                                       
  , "'xx'*'  '*'xx'"                                                       
  , "'\\\\\\\\'"                                                           
  , "'*'"                                                                  
  , "'.'"                                                                  
  , "'_'"                                                                  
  , "''%.%''"                                                              
  , "'a'%.%'b'"                                                            
  , "'a'*' '%.%' '*'b'"                                                    
  , "'a'*' '%.%'b'"                                                        
  , "'a'*'*'*'b'"                                                          
, "'a'*'*'*'b$'"                                                         
, "'H'['b'^{'A'['1'^{'c'}]}]"                                            
, "'H'['b'^{'A'['1'^{'c'}]}]"                                            
, "'H'['b'^{'A'['1'^{}*'c']}]"                                           

, "'H'['b'^{'A'['1']*''^{'c'}}]"                            
# "'H'['b'^{'A'['1']^{'c'}}]"# 70, changed at 0.2.6  

, "'H'['b'^{'A'[]*'1'^{'c'}}]"                                           

, "'H'['b'^{'A'}*''['1'^{'c'}]]"
# "'H'['b'^{'A'}['1'^{'c'}]]" # 72, changed at 0.2.6

, "'H'['b'^{}*'A'['1'^{'c'}]]"                                           

, "'H'['b']*''^{'A'['1'^{'c'}]}"                             
# "'H'['b']^{'A'['1'^{'c'}]}"# 74, changed at 0.2.6 

, "'H'[]*'b'^{'A'['1'^{'c'}]}"                                           
, "'H'['b'^{'A'['1'^{'c'}]}]"                                            
, "'H'['b'^{'A'['1'^{'c'}]}]"                                            
, "''[''[''[''[]]]]"                                                     
, "''[''[''[]]]"                                                         
, "''[''[]]"                                                             
, "''[]"                                                                 
, ""                                                                     
, "''^{''^{''^{''^{}}}}"                                                 
, "''^{''^{''^{}}}"                                                      
, "''^{''^{}}"                                                           
, "''^{}"                                                                
, ""                                                                     
, "'H'['b'^{'A'['1'^{'c'}]}]"                                            
, "''[' ']"                                                              
, "' '[]"                                                                
, "' '*'='*' '"                                                          
, "' '[' ']"                                                             
, "''^{'c'}"                                                             
, "' '^{'c'}"                                                            
, "''^{' '*'c'}"                                                         
, "''^{'c'*' '}"                                                         
, "''^{'c'}*' '"                                                         
, "' '^{' '*'c'*' '}*' '"                                                
, "' '*'H'*' '[' '*'b'*' '^{' '*'A'*' '[' '*'1'*' '^{' '*'c'*' '}*' ']}]"
, "' '*'H]'[']b'*' '^{']A]'[']1]'^{']c]'}*']']}]" 
)
g <- as_plotmath(as_spork(e)) %>% as.character
expect_identical(f, g)


structure("'H'['b'^{'A'}*''['1'^{'c'}]]" , class = 'plotmath') %>% as_preview()
structure("'H'['b'^{'A'}['1'^{'c'}]]", class = 'plotmath') %>% as_preview()

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
expect_true( as_plotmath(as_spork('\\$')) %>% goodToken)
expect_false( as_plotmath(as_spork('\\$'), unescape = FALSE) %>% goodToken)
expect_false(as_plotmath(as_spork('\\$'), unrecognized = function(x,...)x) %>% goodToken)
options(plotmath_unrecognized = function(x,...)x)
expect_false(as_plotmath(as_spork('\\$')) %>% goodToken)
options(plotmath_unrecognized = NULL)
expect_true(as_plotmath(as_spork('\\$')) %>% goodToken)
expect_identical(as.character(plotmathToken("\\$")), "'\\\\$'")
expect_identical(as.character(plotmathToken("\\$", unescape = FALSE)),  "'\\$'")
expect_true(plotmathToken("\\$") %>% goodToken)
options(plotmath_unescape = FALSE)
expect_false(plotmathToken("\\$") %>% goodToken)
options(plotmath_unescape = NULL)
expect_false(plotmathToken("\\$", unescape = FALSE) %>% goodToken)
expect_identical(as.character(plotmathToken('foo')), "'foo'")
expect_identical(as.character(plotmathToken('foo',conditional = FALSE)),"'foo'")
options('plotmath_conditional_quote' = FALSE)
expect_identical(as.character(plotmathToken('foo')), "'foo'")
options('plotmath_conditional_quote' = NULL)
expect_identical(as.character(plotmathToken('foo')),"'foo'")

})
test_that('greek is parsed only at word boundaries',{
  expect_identical(
    'alpha.beta' %>% as_spork %>% as_spar %>% as.character,
    c('alpha','.','beta')
  )
  expect_identical(
    'alphabeta' %>% as_spork %>% as_spar %>% as.character,
    c('alphabeta')
  )
})
test_that('latexToken() responds to top-level arguments',{
  library(magrittr)
  expect_identical(
    '\n' %>% as_spork %>% as_latex(math_open = '\\textrm{') %>% as.character,
    "\\(\\textrm{\\textrm{\n}}\\)"
  )
})
test_that('plotmathToken() responds to top-level arguments',{
  library(magrittr)
  expect_identical(
    '\\' %>% as_spork %>% as_plotmath(unescape = FALSE) %>% as.character,
    "'\\'"
  )
})
test_that('as_spar recognizes newline independently of other whitespace',{
  library(magrittr)
  expect_identical(
    ' \\n \t' %>% as_spork %>% as_spar %>% as.character,
    c(" ",  "\\n", " \t")
  )
})
test_that('as_plotmath handles arbitrary location of newline',{
  library(magrittr)
  render <- . %>% as_spork %>% as_plotmath %>% as.expression
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^2\n' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^\n2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s\n^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./\ns^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2.\n/s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2\n./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*\nm^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg\n*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 \nkg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1\n kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ \n1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) \n~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega)\n ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega\n) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (\nOmega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule \n(Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule\n (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one \njoule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('\none joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
})
test_that('newline renders sensibly as plotmath',{
  library(magrittr)
  render <- . %>% as_spork %>% as_plotmath %>% as_preview
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^2\n' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s^\n2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./s\n^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2./\ns^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2.\n/s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*m^2\n./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg*\nm^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 kg\n*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1 \nkg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ 1\n kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) ~ \n1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega) \n~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega)\n ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (Omega\n) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule (\nOmega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule \n(Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one joule\n (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('one \njoule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_silent('\none joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
})
test_that('latex handles multiple newline',{
  skip_on_cran()
  library(magrittr)
  render <- . %>% as_spork %>% as_latex %>% as_preview
  '1' %>% as_spork %>% as_latex %>% as_preview
  '1\n' %>% as_spork %>% as_latex %>% as_preview
  '1\n2' %>% as_spork %>% as_latex %>% as_preview
  '1\n2\n' %>% as_spork %>% as_latex %>% as_preview
  '1\n2\n3' %>% as_spork %>% as_latex %>% as_preview
  '1\n2\n3\n' %>% as_spork %>% as_latex %>% as_preview
  '1\n2\n3\n4' %>% as_spork %>% as_latex %>% as_preview
  '1\n2\n3\n4\n' %>% as_spork %>% as_latex %>% as_preview
  'one \njoule \n(Omega) ~\n 1 kg*m^2./s^2'%>% render
  expect_identical(
    '1\\n2\\n3\\n4\\n' %>% as_spork %>% as_latex %>% as.character,
    "\\(\\mathrm{\\textrm{1}\n \\textrm{2}\n \\textrm{3}\n \\textrm{4}\n}\\)"
  )
})
test_that('plotmath handles multiple newline',{
  skip_on_cran()
  library(magrittr)
  render <- . %>% as_spork %>% as_plotmath %>% as_preview
  '1' %>% render
  '1\n' %>% render
  '1\n2' %>% render
  '1\n2\n' %>% render
  '1\n2\n3' %>% render
  '1\n2\n3\n' %>% render
  '1\n2\n3\n4' %>% render
  '1\n2\n3\n4\n' %>% render
  'one \njoule \n(Omega) ~\n 1 kg*m^2./s^2'%>% render
  # as of 0.2.6, numerals are just text
  # expectation changes below, e.g. textstyle(1) becomes textstyle('1')
  expect_identical(
    '1\\n2\\n3\\n4\\n' %>% as_spork %>% as_plotmath %>% as.character,
    "atop(textstyle(),atop(textstyle('1'),atop(textstyle('2'),atop(textstyle('3'),atop(textstyle('4'),atop(textstyle()))))))"
  )
  
})
test_that('greek characters are properly isolated',{
  skip_on_cran()
  # check these manually
  'alpha' %>% as_spork %>% as_latex
  'alpha' %>% as_spork %>% as_html
  'alpha' %>% as_spork %>% as_plotmath
  'alpha' %>% as_previews(sleep = 0)  # ok
  '`alpha`' %>% as_previews()  # ok
  '\nalpha' %>% as_previews(sleep = 0)# ok
  'alpha\n' %>% as_previews(sleep = 0)# ok
  '.alpha' %>% as_previews(sleep = 0) # ok
  'alpha.' %>% as_previews(sleep = 0) # ok
  'a.lpha' %>% as_previews(sleep = 0) # ok
 
  '(alpha)' %>% as_previews(sleep = 0) # ok
  '"alpha"' %>% as_previews(sleep = 0) # ok
  '{alpha}' %>% as_previews(sleep = 0) # ok
  '[alpha]' %>% as_previews(sleep = 0) # ok

  '(2)' %>% as_previews(sleep = 0) # ok
  '"2"' %>% as_previews(sleep = 0) # ok
  "'2'" %>% as_previews(sleep = 0) # ok
  '{2}' %>% as_previews(sleep = 0) # ok
  '[2]' %>% as_previews(sleep = 0) # ok
  'v[2]' %>% as_previews(sleep = 0)# ok
  '|2|' %>% as_previews(sleep = 0) # ok
  '| alpha |' %>% as_previews(sleep = 0) # ok
  '( alpha )' %>% as_previews(sleep = 0) # ok
  '" alpha "' %>% as_previews(sleep = 0) # ok
  '{ alpha }' %>% as_previews(sleep = 0) # ok
  '[ alpha ]' %>% as_previews(sleep = 0) # ok
  
  'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_previews # ok
  '\\alpha' %>% as_previews(sleep = 0) # ok
  '\\ foo' %>% as_previews(sleep = 0) # html shows no slash

  '\\alpha' %>% as_spork %>% as_spar
  '\\alpha' %>% as_spork %>% as_spar %>% as_latex
  '\\alpha' %>% as_spork %>% as_spar %>% as_plotmath
  '\\alpha' %>% as_spork %>% as_spar %>% as_html
  
  'foo##bar' %>% as_spork %>% as_spar
  'foo##bar' %>% as_spork %>% as_spar %>% as_latex
  'foo##bar' %>% as_spork %>% as_spar %>% as_plotmath
  'foo##bar' %>% as_spork %>% as_spar %>% as_html
  'foo##bar' %>% as_previews
  
  'kg*m' %>% as_spork %>% as_plotmath
  
}) # check these manually
test_that('latex is newline-tolerant by default',{
  skip_on_cran()
  library(testthat)
  library(magrittr)
  render <- . %>% as_spork %>% as_latex %>% as_preview
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./s^2\n' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./s^\n2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./s\n^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2./\ns^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2.\n/s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*m^2\n./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg*\nm^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 kg\n*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1 \nkg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ 1\n kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) ~ \n1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega) \n~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega)\n ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (Omega\n) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule (\nOmega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule \n(Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one joule\n (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('one \njoule (Omega) ~ 1 kg*m^2./s^2' %>% render)
  expect_no_error('\none joule (Omega) ~ 1 kg*m^2./s^2' %>% render)
}) # long running
test_that('as_previews is stable',{
  skip_on_cran()
  library(magrittr)
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_plotmath %>% as_preview
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex %>% as_preview
  #'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_previews
  expect_no_error('V_c./F' %>% as_spork %>%as_previews)
  expect_no_error('AUC_ss' %>% as_spork %>%as_previews)
  expect_no_error('C_max_ss' %>% as_spork %>%as_previews)
  expect_no_error('var^eta_j' %>% as_spork %>%as_previews)
  
  expect_no_error('gravitational force - gamma (kg * m/s^2.)'%>% as_spork %>%as_previews)
  expect_no_error('C(t_j.) = C_0. * epsilon^-kt_j' %>% as_spork %>%as_previews)
  expect_no_error('eta^eta' %>% as_spork %>%as_previews)
  expect_no_error('Eta^Eta' %>% as_spork %>%as_previews)
  expect_no_error('omicron' %>% as_spork %>%as_previews)

  expect_no_error('Alpha^alpha' %>% as_spork %>%as_previews)
  expect_no_error('Beta^beta' %>% as_spork %>%as_previews)
  expect_no_error('Gamma^gamma' %>% as_spork %>%as_previews)
  expect_no_error('Delta^delta' %>% as_spork %>%as_previews)
  expect_no_error('Epsilon^epsilon' %>% as_spork %>%as_previews)
  expect_no_error('Zeta^zeta' %>% as_spork %>%as_previews)
  expect_no_error('Eta^eta' %>% as_spork %>%as_previews)
  expect_no_error('Theta^theta' %>% as_spork %>%as_previews)
  expect_no_error('Iota^iota' %>% as_spork %>%as_previews)
  expect_no_error('Kappa^kappa' %>% as_spork %>%as_previews)
  expect_no_error('Lambda^lambda' %>% as_spork %>%as_previews)
  expect_no_error('Mu^mu' %>% as_spork %>%as_previews)
  expect_no_error('Nu^nu' %>% as_spork %>%as_previews)
  expect_no_error('Xi^xi' %>% as_spork %>%as_previews)
  expect_no_error('Omicron^omicron' %>% as_spork %>%as_previews)
  expect_no_error('Pi^pi' %>% as_spork %>%as_previews)
  expect_no_error('Rho^rho' %>% as_spork %>%as_previews)
  expect_no_error('Sigma^sigma' %>% as_spork %>%as_previews)
  expect_no_error('Tau^tau' %>% as_spork %>%as_previews)
  expect_no_error('Upsilon^upsilon' %>% as_spork %>%as_previews)
  expect_no_error('Phi^phi' %>% as_spork %>%as_previews)
  expect_no_error('Chi^chi' %>% as_spork %>%as_previews)
  expect_no_error('Psi^psi' %>% as_spork %>%as_previews)
  expect_no_error('Omega^omega' %>% as_spork %>%as_previews)
  'varsigma~Upsilon1~rho' %>% structure(class = c('plotmath','character')) %>% as_preview
  '$\\sigma$ $\\varsigma$ $\\Upsilon$' %>% structure(class = c('latex','character')) %>% as_preview  
  'Tau.iota.mu.omicron.theta.epsilon.upsilon.sigma' %>% as_previews ### italics!
  'tau iota mu omicron' %>% as_previews()
  '_tau.iota' %>% as_previews
  '_tau.`iota`' %>% as_previews
}) # long running
test_that('backslash-n has effect', {
  skip_on_cran()
  # test manually
  '1\\n2\\n3' %>% as_previews
  '1\\n2\\n3' %>% as_spork %>% as_latex %>% as_preview
  
})
test_that('sub/super are written smaller', {
  # test manually
  skip_on_cran()
  'AUC_ss_tau' %>% as_previews
  'a^b^c' %>% as_previews
  '1^2^3' %>% as_previews
  'M_2' %>% as_previews
  'AUC_SS' %>% as_previews
  '1_2_3_4' %>% as_previews
  '1_2_3._4' %>% as_previews
  '1_2._3_4' %>% as_previews
  '1^2^3^4' %>% as_previews
  '1_2^3_4' %>% as_previews
  '1^2_3^4' %>% as_previews
  'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_spork %>% as_latex
  'one joule (Omega) ~ 1 kg*m^2./s^2' %>% as_previews
  
   expect_identical(
    'Hb^a_1^c' %>% as_spork %>% as_latex %>% as.character,
    "\\(\\mathrm{\\textrm{Hb}{}^{\\textrm{\\scriptsize a}{}_{\\textrm{\\tiny 1}{}^{\\textrm{\\tiny c}}}}}\\)"
  )
  
})
