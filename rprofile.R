## ٩(´ᗜ`)و (´-`) .｡oO (the script for .Rprofile, 2021-03-24) |████████████████████████|


## Install vital packages if necessary == (2022-05-19) ========================
pkgs_must <- c('bindrcpp', 'changepoint', 'devtools', 'ellipse', 'extrafont', 'formattable', 'hablar', 'logKDE', 'minerva', 'minpack.lm', 'naturalsort',
               'pracma', 'psych', 'robustbase', 'rrcov', 'scico', 'tibbletime', 'tidyverse', 'VGAM', 'viridis')
pkgs_lack <- !pkgs_must %in% rownames(utils::installed.packages())
if (sum(pkgs_lack) > 0) {
  for (i in seq_along(which(pkgs_lack))) {
    cat(paste0('\n    trying to install ', pkgs_must[pkgs_lack], '...\n\n'))
    utils::install.packages(pkgs_must[pkgs_lack][i], dependencies = T)
  }
}


## Import while suppressing startup messages from defaultPackages and then invoke favorite packages == (2021-03-23) ========================
## https://stat.ethz.ch/pipermail/r-help/2013-July/356878.html
skip_messages <- function(packs, ...) {
  setHook(packageEvent(pkgname = packs, event = 'onLoad'), function(...) sink(file(tempfile(), 'w'), type = 'message'))
  setHook(packageEvent(pkgname = packs, event = 'attach'), function(...) sink(file = NULL, type = 'message'), action = 'append')
}
for (packs in c('tidyverse')) skip_messages(packs)  # Suppress annoying messages when calling

pkgs <- c('hablar', 'lubridate', 'readxl', 'tidyverse')  # Confirm called packages by search()
options(defaultPackages = c(getOption('defaultPackages'), pkgs))  # Invoke


## Fix the size of graphics device == (2021-03-22) ========================
setHook(packageEvent(pkgname = 'grDevices', event = 'onLoad'), function(...) {    
  if (Sys.info()['sysname'] == 'Darwin') grDevices::quartz.options(width = 4.5, height = 3.3)  # for Mac
  if (Sys.info()['sysname'] == 'Linux') options(repr.plot.width = 4.5, repr.plot.height = 3.3)  # for JupyterLab; confirm by options()$bitmapType
  if (Sys.info()['sysname'] == 'Windows') grDevices::windows.options(width = 4.5, height = 3.3)  # for Windows
})


## Fix the default parameters for a graphical device == (2021-03-24) ========================
## https://stackoverflow.com/questions/48839319
setHook(packageEvent(pkgname = 'grDevices', event = 'onLoad'), function(...) {
  f_device <- getOption('device')
  newDev <- function(...) {
    f_device(...) 
    graphics::par(
      mgp = c(2, 0.2, 0), ann = T, xaxs = 'i', yaxs = 'i', col = 'grey13', col.axis = 'grey13', fg = 'grey13', ps = 13, lwd = 1.3,
      mar = c(2.4, 4, 0.5, 1), tcl = 0.25, cex.axis = 1, las = 1,
      family = c('Avenir Next', 'sans', 'Yu Gothic')[which(c('Darwin', 'Linux', 'Windows') %in% Sys.info()['sysname'])]  # Noto Sans CJK JP
    )
  }
  options(device = newDev)
})


## Rewrite other parameters of defalut values == (2021-08-17) ========================
## https://stackoverflow.com/questions/39620669/source-script-to-separate-environment-in-r-not-the-global-environment
## https://coolbutuseless.bitbucket.io/2018/04/11/changing-the-default-arguments-to-a-function/
.nya0env <- new.env()  # Naming initial dot works out not to show your own functions on the gloval environment

Sys.setenv(TZ = 'Asia/Tokyo')  # Confirm by Sys.timezone()

library('grDevices')  # formals(package_name::function) is error
formals(cairo_pdf)[c('width', 'height', 'pointsize', 'onefile', 'bg')] <- list(4.5, 3.3, 13, TRUE, 'transparent')
library('graphics')
formals(axis)[c('col.ticks', 'lwd.ticks', 'lwd')] <- list('grey13', 1.3, 0)
library('minpack.lm', quietly = T)
formals(nlsLM)$control <- nls.lm.control(maxiter = 1024, nprint = 0)  # nprint = 1 denotes to show results
formals(source)$chdir <- TRUE  # if (R.Version()$major < 4) TRUE else FALSE
formals(unlist)$use.names <- FALSE


## Access permission names == (2021-08-17) ========================
researcher_names <- c('y-nishino', 'c-nakagawa', '')  # '' is assigned to Mac & Ubuntu
production_names <- c('Microtrac', 't-hayakawa')
yourname <- Sys.getenv('USERNAME')


## Calling basic script == (2022-05-19) ========================
if (Sys.info()['sysname'] == 'Darwin') {  # for Mac
  sys.source(file.path('~/Library/Mobile Documents/com~apple~CloudDocs/R_script', '0_startup.R'), envir = .nya0env, chdir = F)
} else {  # for Windows or JupyterLab in Ubuntu
  library('tidyverse')
  library('utils', quietly = T)  # to use pipe
  get_source <- function(url_no = 1, file_no = 1) {
    git_url <- file.path('https://github.com', c(
                         'Nyu3/template_R/blob/master',
                         'Nyu3/psd_R/blob/master',
                         'readqadiv13/tuning_PSD/blob/main'
               ))[url_no]
    git_file <- c('0_startup.R', 'PSD_archive.R', 'PSD_archive_generator.R', 'PSD_simulator.R')[file_no]
    filePATH <- paste0(file.path(git_url, git_file), '?raw=TRUE')
    script <- readLines(filePATH, encoding = 'UTF-8')
    eval(parse(text = script), envir = .nya0env)
  }

  if (any(grepl(yourname, researcher_names))) {  # for heavy users (Win & JupyterLab)
    purrr::walk2(c(1,2,2,2), 1:4, ~ get_source(url_no = .x, file_no = .y))
  } else if (yourname %in% production_names) {  # for light & PSD Win-users
    purrr::walk2(c(1,2,2,2), 1:4, ~ get_source(url_no = .x, file_no = .y))  # c(3,3,3,3) --> readqadiv13/...
  } else {  # for light Win-users
    get_source()  # Only use of '0_startup.R'
  }
  remove('get_source')
}
attach(.nya0env)  # Confirm by ls('.nya0env') and search()


## Font registeration == (2021-03-24) ========================
## https://ill-identified.hatenablog.com/entry/2020/10/03/200618
## https://taken.jp/font-family-name-english-japanese.html
if (.Platform$'OS.type' == 'windows') windowsFonts(`Yu Gothic` = windowsFont('Yu Gothic'))


## Move to casual space == (2021-08-10) ========================
if (Sys.info()['sysname'] == 'Darwin') setwd('~/Desktop')  # The directory anywhere you click ~.R file gives priority to this command
if (Sys.info()['sysname'] == 'Windows') {
  if (file.exists(file.path(Sys.getenv('USERPROFILE'), 'OneDrive/デスクトップ'))) {
    setwd(file.path(Sys.getenv('USERPROFILE'), 'OneDrive/デスクトップ'))
  } else {
    setwd(file.path(Sys.getenv('USERPROFILE'), 'Desktop'))
  }
}


## Hint message & delete objects == (2022-05-18) ========================
tips <- "
   plt.(iris[4:5])
   plt.(iris[-5], legePos = c(0.01, 0.99), lty = 1)
   dens.(iris[4:5], cum = F)
   crp.(iris[2:3])
   hist.(iris[2:3], col = c('slateblue', 'coral2'), bin = 0.1, name = c('A', 'B'), overlay = T)
   corp.(iris[3:4])
   box2.(iris, rot = 20, pareto = T, cut = T)
   box2.(diamonds[1:1000, 1:3], mark = 'color')
   box2.(id2y.(diamonds[1:1000, 1:3]))
   box2.(case2.(us_rent_income[5], div = 100), col = 0)
   box2.(time2.(economics[1:50, ], div = 'year'))
   barp.(iris, xyChange = T, rot = 25)
   barp.(iris, cum = T, xyChange = T)
   sp.(iris, col = 3)
   stats.(iris)
   pie.(iris[41:120,5], percent = T)
   html.(starwars)
...\n"

if (interactive()) if (!yourname %in% production_names) cat(tips)

remove(list = c('packs', 'pkgs', 'pkgs_lack', 'pkgs_must','production_names', 'researcher_names', 'skip_messages', 'tips', 'yourname'))

## END ##
