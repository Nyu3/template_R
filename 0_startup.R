## ٩(´ᗜ`)و (´-`) .｡oO (Common function, 2018-12-03)


## General parameters == (2022-11-05) ========================
gp. <- function(...) {
  # skipMess.('easypackages'::libraries(c('hablar', 'lubridate', 'readxl', 'tidyverse'))))
  # if (dev.list() > 0) dev.new(width = 3 * (1+ sqrt(5))/2, height = 3)  # 4.5, 3.3
  # Ubuntu  Avenir  Open Sans Light  Noto Sans CJK JP
  if (Sys.info()['sysname'] == 'Darwin') grDevices::quartz.options(width = 4.5, height = 3.3)  # for Mac
  if (Sys.info()['sysname'] == 'Linux') options(repr.plot.width = 4.5, repr.plot.height = 3.3)  # for JupyterLab; confirm by options()$bitmapType
  if (Sys.info()['sysname'] == 'windows') grDevices::windows.options(width = 4.5, height = 3.3)  # for Windows
  par(ann = T, cex.axis = 1, col = 'grey13', col.axis = 'grey13', col.lab = 'grey13', col.main = 'grey13', col.sub = 'grey13', fg = 'grey13',
      family = c('Avenir Next', 'sans', 'Yu Gothic')[which(c('Darwin', 'Linux', 'Windows') %in% Sys.info()['sysname'])],  # Noto Sans CJK JP
      las = 1, lwd = 1.3, mar = c(2.5, 4, 0.5, 1), mfrow = c(1, 1), mgp = c(1.5, 0.2, 0), oma = c(0, 0, 0, 0),
      ps = 13, tcl = 0.25, xaxs = 'r', yaxs = 'r'
  )
  formals(axis)[c('col.ticks', 'lwd.ticks', 'lwd')] <- list('grey13', 1.3, 0)
}


## Auto install == (2024-07-05) ========================
query_lib. <- function(package_name, ...) {
  tmp <- deparse(substitute(package_name)) %>% gsub('\"', '', .)  # variable or string
  tmp <- eval(substitute(alist(...))) %>%
         map_chr(~ deparse(.x) %>% gsub('\"', '', .)) %>%
         c(tmp, .)
  new_list <- utils::installed.packages() %>% rownames() %>% {tmp[!tmp %in% .]} %>% as.list()
  if (length(new_list) != 0) {
    purrr::walk(new_list, function(x) {
      cat('\n    trying to install', x, '...\n\n')
      install.packages(x, dependencies = T, repos = 'https://ftp.yz.yamagata-u.ac.jp/pub/cran/')
    })
  }
# query_lib.(hablar, vroom, xyz)  # mix of variable & not applicable library name
# query_lib.(formattable, 'scico')  # mix of variable & string
}

## Return strings even you write it as an object == (2024-07-19) ========================
lazy_name. <- function(x, ...) {  # NSE (Non Standard Estimation)
  tmp <- substitute(x)
  if (is.null(tmp) || exists(tmp)) {
    return(NULL)
  } else {
    return(as.character(tmp))
  }
# lazy_name.(abc)  lazy_name.('abc')  lazy_name.(NULL)  nyal <- NULL; lazy_name.(nyal)
}


## Rename duplicated names == (2024-05-22) ========================
make.unique2 <- function(x, sep = '') {
  x2 <- dplyr::case_when(is.na(x) ~ 'NA', TRUE ~ x)
  tmp <- ave(x2, x2, FUN = function(y) if (length(y) > 1) str_c(y, 1:length(y), sep = sep) else y)
  return(tmp)
}


## Skip warning messages == (2020-10-24) ========================
skipMess. <- function(x) suppressPackageStartupMessages(suppressWarnings(suppressMessages(invisible(x))))


## Path control == (2022-03-08) ========================
setwd. <- function(desktop = F, ...) {  # Needed to copy a file path on the console in advance
  if (desktop == TRUE) {
    setwd('~/Desktop')
    return(cat('Get back to the Desktop.\n\n'))
  }
  chr <- pp.()
  if (Sys.getenv('OS') == '') {  # for Mac & Ubuntu
    chr2 <- if (str_detect(chr, pattern = 'csv$|CSV$|DAT$|xls$|xlsx|R$')) dirname(chr) else chr
    setwd(chr2)
  } else {
    if (!str_detect(chr, pattern = '\\\\')) stop('Not available file path...\n\n', call. = F)
    chr2 <- gsub('\\\\', '\\/', chr)
    setwd(chr2)
  }
}  # setwd.()


## Lightly vroom() for csv == (2023-03-03) ========================
vroom. <- function(file = NULL, skip = 0, coln = T, ...) {
  query_lib.(hablar, vroom)
  file <- lazy_name.(file)

  if (!is.null(file)) {
    filen <- if (length(file) == 1) file else str_c(file, collapse = '???')
    File <- list.files(pattern = 'csv|CSV') %>% str_subset(., pattern = fixed(filen))
  } else {
    File <- dir(pattern = '\\.csv|\\.CSV') %>%
            str_subset(., pattern = fixed('$'), negate = T) %>% 
            choice.(., note = 'Target File(s)')
  }
  enc <- map_chr(File, ~ try(readr::guess_encoding(.), silent = T) %>%  # for a tibble: 0 x 0
                         {if ('try-error' %in% class(.)) 'ASCII' else .[[1,1]]} %>%
                         {dplyr::case_when(str_detect(., 'ASCII|Shift_JIS|windows') ~ 'cp932',
                                    str_detect(., 'UTF-8') ~ 'utf8',
                                    TRUE ~ 'unknown')
                         }
         )
  ## start reading
  if (length(File) == 1) {
    out <- skipMess.(vroom::vroom(File, locale = locale(encoding = enc), skip = skip, delim = ',', col_names = T))
    ## to find the reading data has column names or  not
    if (names(out) %>% map(., hablar::retype) %>% map_lgl(is.numeric) %>% any()) {  # column names like 'abc', '123', '456'
      out <- skipMess.(vroom::vroom(File, locale = locale(encoding = enc), skip = skip, delim = ',', col_names = F))
    }
    if (any(is_time.(out))) out <- hablar::retype(out)  # vroom or read_csv is expected to be smart...
  } else {
    dL <- map2(File, enc, ~ skipMess.(vroom::vroom(.x, locale = locale(encoding = .y), delim = ',', col_names = coln)) %>%
                            mutate(file = .x) %>% relocate(file) %>% {if (any(is_time.(.))) hablar::retype(.) else .}
          ) %>% {.[map_dbl(., nrow) > 0]}  # to delete a tibble: 0 x ...
    if (length(dL) == 0) stop('Empty file(s) ...\n\n', call. = F)

    ## bind brother sheets
    tmp <- matrix(NA, length(dL), length(dL))
    for (i in seq_along(dL)) {  # setequal() needs to fix the criterion, so you must seek every criteria by matrix
      tmp[i, ] <- map(dL, names) %>% map2_lgl(.[i], ., ~ setequal(.x, .y))
    }
    groupL <- which(colSums(tmp) != 1)
    aloneL <- which(colSums(tmp) == 1)
    if (length(groupL) != 0 && length(aloneL) != 0) {  # some lists contains brother
      out <- c(dL[groupL] %>% bind_rows %>% list, dL[aloneL])
    } else if (length(groupL) != 0 && length(aloneL) == 0) {  # all lists are brother; return a tibble
      out <- dL[groupL] %>% bind_rows
    } else if (length(groupL) == 0 && length(aloneL) != 0) {  # every list is a stranger
      out <- if (length(aloneL) == 1) dL[[1]] %>% select(!file) else dL
    }
  }
  return(out)
}


## Reading data == (2023-03-03) ========================
getData. <- function(path = NULL, file = NULL, timeSort = F, timeFactor = NULL, filetype = NULL, ...) {
  query_lib.(hablar)
  if (!is.null(path)) {
    oldDir <- getwd()
    if (str_detect(path, '\\.csv|\\.CSV|\\.xls|\\.XLS|\\.xlsx') %>% any()) {
      file <- basename(path)
      setwd(dirname(path))
    } else {
      setwd(path)
    }
  }
  File <<- file %||% {  # You can use the file name later in case of using write.()
             filetype %||% '\\.csv|\\.CSV|\\.xls|\\.XLS|\\.xlsx' %>%
             dir(pattern = .) %>%
             str_subset(., pattern = fixed('$'), negate = T) %>% {
               if (length(.) == 0) {
                 stop('No file available...\n\n', call. = F)
               } else {
                 choice.(., note = 'Target File(s)')
               }
             }
           }
  if (length(File) == 0) stop('No data file in this directory...\n\n', call. = F)
  if (str_detect(File, pattern = '\\.csv|\\.CSV') %>% all()) {  # Don't use fixed() while using |
    d <- vroom.(File)
  } else if (str_detect(File, pattern = '\\.xls|\\.XLS|\\.xlsx') %>% all()) {
    sht <- map(File, excel_sheets) %>% set_names(File)  # To mutate(sheet = ~), map_dfr() is not used
    dL <- list()
    for(i in seq_along(File)) {
      dL <- map2(File[i], sht[[i]], ~ read_excel(.x, sheet = .y) %>%
                                      mutate(file = .x, sheet = .y) %>%
                                      relocate(file, sheet)
            ) %>%
            {.[map_dbl(., nrow) > 0]} %>%
            c(dL, .)
    }
    if (length(File) == 1) dL <- map(dL, ~ select(., !file))

    ## bind brother sheets
    tmp <- matrix(NA, length(dL), length(dL))
    for (i in seq_along(dL)) {  # setequal() needs to fix the criterion, so you must seek every criteria by matrix
      tmp[i, ] <- map(dL, names) %>% map2_lgl(.[i], ., ~ setequal(.x, .y))
    }
    groupL <- which(colSums(tmp) != 1)
    aloneL <- which(colSums(tmp) == 1)
    if (length(groupL) != 0 && length(aloneL) != 0) {  # some lists contains brother
      d <- c(dL[groupL] %>% bind_rows %>% list, dL[aloneL])
    } else if (length(groupL) != 0 && length(aloneL) == 0) {  # all lists are brother; return a tibble
      d <- dL[groupL] %>% bind_rows
    } else if (length(groupL) == 0 && length(aloneL) != 0) {  # every list is a stranger
      d <- if (length(aloneL) == 1) dL[[1]] %>% select(!sheet) else dL
    }
  }
  ## cleaning
  clean_data <- function(d) {
    tmp1 <- d %>%
            dplyr::filter(rowSums(is.na(.)) != ncol(.)) %>%
            select_if(colSums(is.na(.)) != nrow(.)) %>%
            dt2time.(., timeSort, timeFactor) %>%
            mutate_if(is.character, correctChr.)

    tmp2 <- skipMess.(try(hablar::retype(tmp1), silent = T))  # hablar::retype(1e+9)  hablar::retype(1e+10)
    out <- if ('try-error' %in% class(tmp2)) tmp1 else tmp2
    return(out)
  }
  d <- {if (is.data.frame(d)) clean_data(d) else map(d, clean_data)} %>%
       {if (length(.) == 1) .[[1]] else .}  # just one sheet --> tibble

  if (!is.null(path)) setwd(oldDir)
  return(d)
}


## Set arguments in the function which you're copying == (2023-12-11) ========================
lazy_args. <- function(...) {
  chrs <- pp.(vectorize = T) %>% str_split('\n')
  lazy_do <- function(char, ...) {
    ## Delete 'name <- function(' part
    if(str_detect(char, 'function')) char <- str_locate(char, 'function\\(') %>% .[2] %>% {str_sub(char, . +1, str_length(char))}
    ## Delete comment out
    if (str_detect(char, '#')) char <- str_locate(char, '#') %>% .[2] %>% {str_sub(char, 1, . -1)}
    ## Replace colon to semicolon
    chr2 <- gsub('(^chr|^vec,|^x,|^xy,|^a|^b|^\\.d|^d,|^df,|^dt,|^dL,|^dn,)', '', char) %>%
          # gsub(',', ';', .) %>%
            gsub('\\{|\\}|\\.\\.\\.)', '', .) %>%  # remove {} ...)
            str_squish()  # remove space on the both ends
  # semicolon_locate <- str_locate_all(chr2, 'c\\([^;]{1,10};') %>% {.[[1]][, 2]}  # chless than 10 length except ;
  # if (length(semicolon_locate) != 0) for (i in semicolon_locate) str_sub(chr2, start = i, end = i) <- ','  # overwrite
    chr3 <- str_split(chr2, ',') %>% flatten_chr() %>% .[!. %in% '']
    if (length(chr3) == 0) return(NULL)
    if (str_count(chr3[length(chr3)], '\\)') == 1) chr3[length(chr3)] <- gsub('\\)', '', chr3[length(chr3)])  # abc = T) --> abc = T
    tf <- str_detect(chr3, '=')  # the 1st always starts TRUE
    chr4 <- chr3[1]
    ctr <- 1
    for (i in 2:length(chr3)) {
      if ((tf[i - 1] == T && tf[i] == T) || (tf[i - 1] == F && tf[i] == T)) {
        ctr <- ctr + 1
        chr4[ctr] <- chr3[i]
      }
      if ((tf[i - 1] == T && tf[i] == F) || (tf[i - 1] == F && tf[i] == F)) {
        chr4[ctr] <- str_c(chr4[ctr], ',', chr3[i])
      }
    }
    chr5 <- str_flatten(chr4, collapse = ';')
    eval(parse(text = chr5), envir = .GlobalEnv)
  }
  walk(chrs, lazy_do)
}


## Find whether quasi-time format or not == (2024-10-09) ========================
lazy_any. <- function(x, Fun = is.na, ...) {
  for (i in seq_along(x)) {
    if (!is.na(x[i])) {
      if (Fun(x[i]) && tryReturn.(date(x[i])) %>% {!is.na(.)}) {
        return(TRUE)
      } else {
        return(FALSE)
      }
      break
    }
    if (i == length(x) && !Fun(x[i])) return(FALSE)
  }
}

is_time. <- function(x, ...) if (is.list(x)) map_lgl(x, ~ is.POSIXct(.) | is.Date(.)) else is.POSIXct(x) | is.Date(x)

is_quasi_time. <- function(x, ...) {
  if (all(is.na(x)) || !is.character(x)) return(FALSE)
  x <- x[!is.na(x)]
  tz_TF <- {stringr::str_count(x, ':') == 2 & str_detect(x, 'UTC|JST|T|Z')} %>% any()
  digit_TF <- {str_detect(x, '[:digit:]') & !str_detect(x, '[:upper:]|[:lower:]') & !is.na(x)} %>% all()  # No alphabet
  chr1_TF <- {str_count(x, '/') == 2 | str_count(x, '-') == 2 & !str_count (x, '/|-') > 2 & !is.na(x)} %>% any()
  chr2_TF <- if (all(str_count(x, '-') != 2 | str_count(x, ' ') == 1)) TRUE else map_lgl(str_split(x, '-'), ~ .[1] %>% {str_length(.) == 4}) %>% any()
  return(!is.POSIXct(x) & !is.Date(x) & (tz_TF | (digit_TF & chr1_TF & chr2_TF)))
# map_lgl(c('2019/1/8', '2019/1/8 12:34', '2019/1/8 12:34:56', '2019-11-14', '2019-11-14 12:34', '2019-11-14 12:34:56', '2023-08-15 11:45:00 JST', '123456-01-2', '2023-08-02T00:00:00Z'), is_quasi_time.)
}

is_quasi_period. <- function(x, ...) {
  if (all(is.na(x)) || !is.character(x)) return(FALSE)
  if (x[!is.na(x)] %>% {all(str_count(., ':') %in% 1:2 & str_detect(., '/', negate = T) & str_detect(., '[:digit:]'))}) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}  # c('0:00', '24:00', '123:45', NA, '1:30:00')


## Time style conversion in the tibble level == (2024-04-25) ========================
dt2time. <- function(d, timeSort = F, timeFactor = NULL, ...) {  # Use this by getData.() & pp.()
  query_lib.(lubridate)
  if (map_lgl(d, ~ is_quasi_time.(.) || is_quasi_period.(.)) %>% any() %>% `!`) return(d)
  if (is.data.frame(d) && nrow(d) == 0) return(d)  # Safety net for pp.() when copying a mere cell as vector
  ## Time style conversion in the vector level
  chr2time <- function(x) {
    colons <- str_count(x, ':') %>% max(na.rm = T)  # shit data like 2023/8/11 01:21  2023/8/11 01:22:00, so use median not max
    colon2 <- str_count(x, '::') %>% max(na.rm = T)  # a design like a::b be removed
    if (colon2 > 0) return(x)
    if (is_quasi_time.(x)) {
      x <- gsub('  ', ' ', x)  # cannot convert '2023/8/13  01:21'
      if (colons == 0) {
        timeVec <- parse_date_time2(x, orders = 'Ymd', tz = 'Asia/Tokyo')
      }else if (colons == 1) {
        timeVec <- dplyr::case_when(str_count(x, ':') == 0 ~ str_c(x, ':00'), TRUE ~ x) %>% parse_date_time2(orders = 'YmdHM', tz = 'Asia/Tokyo')
      } else {
        timeVec <- dplyr::case_when(str_count(x, ':') == 0 ~ str_c(x, ':00:00'), str_count(x, ':') == 1 ~ str_c(x, ':00'), TRUE ~ x) %>%
                   parse_date_time2(orders = 'YmdHMOS', tz = 'Asia/Tokyo')  # 'HMOS' reacts millisec as well as sec
      }
      return(timeVec)
    } else if (is_quasi_period.(x)) {  # Make <Period> vector
      timeVec <- x %>% {
                   if (colons == 1) {
                     hm(., quiet = T)
                   } else if (colons == 2) {
                     map_chr(x, function(y) if (!is.na(y) && str_count(y, ':') == 1) str_c(y, ':00') else y) %>% hms(., quiet = T)
                   }
                 }
      return(timeVec)
    } else {
      return(x)  # Return x as raw
    }
  }  # END of chr2time()

  ## Then, time converting to the tibble; if dt has no time possibility, it will return dt with no change
  if (is.atomic(d)) return(chr2time(d))
  d <- d %>% mutate_if(~ is_quasi_time.(.) || is_quasi_period.(.), ~ chr2time(.))

  ## New colulmn and sorting with time series to distinguish the favorite from time columns
  if (timeSort == TRUE) {
     timeColN <- map_lgl(d, ~ is_time.(.)) %>% names(d)[.]
     if (!is.null(timeFactor)) {
       d <- d[[timeFactor]] %>% {if (is.null(.)) NA else .} %>% mutate(d, Time = .)  # Create 'Time' new column
     } else {
       if (length(timeColN) == 1) d <- select_if(d, ~ is_time.(.)) %>% pull() %>% mutate(d, Time = .)
       if (length(timeColN) > 1) d <- choice.(timeColN, note = 'TIME factor') %>% d[[.]] %>% mutate(d, Time = .)
     }
     if (d$Time %>% {length(.[is.na(.)]) / length(.) > 0.15}) d <- dplyr::filter(d, !is.na(Time))  # Too many NA is crap (15 %)
     d <- arrange(d, Time)  # Ascending sort in time vector
  }
  return(d)
# dt2time.(tibble(time = c('2023/8/11 00:00', '2023/8/11 01:21:00')))
}


## Powerful copy & paste == (2024-05-22) ========================
pp. <- function(n = 1, vectorize = F, ...) {  # n: instruct a row limit of column names {0, 1, 2, ...}
  query_lib.(hablar, stringdist)
  clip <- suppressWarnings(readr::clipboard()) %>%
          {.%||% stop('It\'s not a readable text ...\n\n', call. = F)} %>%
          gsub('#DIV/0!|#NAME\\?|#N/A|NA\\?|#NULL!|#NUM!|#REF!|#VALUE!', 'NA', .) %>%  # never apply 'NA' & NULL'
          gsub('Exported data\n\n', '', .)  # for a copy by 'DT' html table
  rowcol <- sum(str_count(clip, '\n') +1) %>% {c(., sum(str_count(clip, '\t')) / . +1)}

  if (vectorize == TRUE) return(str_split(clip, '\n') %>% unlist())  # for lazy_args.(); map.(clip, ~ str_count(., ' ')) %>% {. > 2} %>% any --> TRUE
  if (!is.na(rowcol[1]) && rowcol[1] == 1) {  # When copying just text, one cell or one row, you'll get an atomic vector
    vec <- str_split(clip, '\t')[[1]]
    if (suppressWarnings(sapply(vec, as.numeric)) %>% anyNA()) return(vec)  # When the copied vector is not all numeric
    else return(as.numeric(vec))
  }
  d <- read_delim(clip, col_names = F, show_col_types = F, delim = '\t') %>%  # I couldn't understand that a new line generates here...
       dplyr::filter(rowSums(is.na(.)) != ncol(.))

  ## Find which row the real column name has
  if (is.null(n)) {
    ## auto estimation of which row is header
    tmp <- pmin(nrow(d), 20) %>% {d[seq(.), ]} # %>% tidyr::fill(names(d), .direction = 'downup')  # Check 20 rows with any blank filled
    row123 <- vector()
    for (i in seq(3)) {  # similar pattern will be 0 ~ 1
      row123[i] <- tmp %>%
                   map_df(~ stringdist::stringdist(.[-i], .[i], weight = c(d = 1, i = 1, s = 0.1, t = 1), method = 'osa')) %>%
                   unlist() %>%
                   mean(na.rm = T)
    }
    n <- row123 %>% {if (any(.[!is.na(.)] >= 1)) which.max(.) else 0}  # if all is similar then return 0 (no column names)
  }
  is.wholenumber <- function(x, tol = .Machine$double.eps ^0.5) abs(x - round(x)) < tol  # is.integer(1) is FALSE !
  if (n < 0 || !is.wholenumber(n)) {
    stop('Input n as integer {0, 1, 2, ...}\n\n', call. = F)
  } else if (n >= 1) {
    bind_colnames <- d[seq(n), ] %>% map_chr(~ .[!is.na(.)] %>% str_flatten(collapse = '_'))
    d <- d[-seq(n), ] %>% set_names(bind_colnames)
  }

  ## make the data clean
  d <- d %>%
       set_names(names(d) %>%
                 gsub('NA', 'X', .) %>%
                 dplyr::if_else(nchar(.) == 0, 'X', .) %>%
                 make.unique2()
       ) %>%
       dt2time.(timeSort = F) %>%
       mutate_if(is.character, correctChr.) %>%
       hablar::retype() %>%
       select_if(colSums(is.na(.)) != nrow(.))
  return(d)
}  # END of pp.()


## Turn Sdorpion csv data into tidy one == (2024-10-21) ========================
pk. <- function(excel = T, ...) {
  tmp <- pp.()
  if (!any(str_detect(names(tmp), 'フェレ径'))) stop('Be sure to copy Sdorpion data...\n\n', call. = F)
  if (!any(str_detect(names(tmp), 'タグ1'))) {  # source from Grafana
    tmp2 <- tmp[-1] %>%
            mutate(粒度 = gsub('月', '-', 粒度) %>% gsub('日', '', .))
  } else {  # source from raw_data in Scorpion
    ## calibration (ruler length by pixel = 104, real vs SEM ratio by 0.1 mm scale = 0.9486)
    cali <- function(magnification) (9954 * magnification ^ (-0.9994) - 0.007076) / 104 * 0.9486

    ## remove lot or remark info from tag1~4 column; eg) ['123456-07' 'G13' '12345' '1300]
    chr_trim <- function(x, get_chr = T) {
      tmpx <- gsub("\\[\'|\\'\\]", '', x) %>%
              str_split("\\' \\'") %>% {
                if (get_chr == TRUE) {  # chr: type, size; eg) 6-12, 140/170
                  map_chr(., ~ .x %>% {
                    .[str_detect(., '-') | str_detect(., '/') | str_detect(., '\\#') | str_detect(., '[:alpha:]')][1]
                  })
                } else {  # num: ID, magnification
                  map_chr(., ~ .x %>% {.[!str_detect(., '-') & !str_detect(., '[:alpha:]')][1]})
                }
              }
      return(tmpx)
    }

    tmp2 <- tmp[-1, ] %>%
            mutate(
              タグ1 = chr_trim(タグ1, T),
              タグ2 = chr_trim(タグ2, T),
              タグ3 = chr_trim(タグ3, F),
              タグ4 = chr_trim(タグ4, F),
            ) %>%
            set_names(unlist(tmp[1, ])) %>%
            mutate(砥粒度 = str_c(tag1, ' (', tag2, ')'), 備考 = NA_character_) %>%
            rename(砥粒名 := tag1, 粒度 := tag2, 粒度分布ID := tag3, SEM倍率 := tag4, 測定日 := date, lot := lot_no) %>%
            relocate(測定日, .before = 砥粒名) %>%
            relocate(備考, 砥粒度, .after = lot) %>%
            hablar::retype() %>%
            mutate(
              面積 = area * cali(SEM倍率) ^ 2,
              包絡面積 = convex_area * cali(SEM倍率) ^ 2,,
              矩形面積 = boundingbox_area * cali(SEM倍率) ^ 2,,
              周囲長 = perimeter * cali(SEM倍率),
              包絡長 = convex_perimeter * cali(SEM倍率),
              矩形短辺 = l_minor * cali(SEM倍率),
              矩形長辺 = l_major * cali(SEM倍率),
              粒径 = sqrt(矩形短辺 ^2 + 矩形長辺 ^2),
              平均軸径 = (矩形短辺 + 矩形長辺) / 2,
              水平フェレ径 = feret_diameter_w * cali(SEM倍率),
              鉛直フェレ径 = feret_diameter_h * cali(SEM倍率),
              等価円周 = circle_equivalent_perimeter * cali(SEM倍率),
              円相当径 = circle_equivalent_diameter * cali(SEM倍率),
              最小外接円 = diameter_out * cali(SEM倍率),
              最大内接円 = diameter_in * cali(SEM倍率),
              フェレ径比 = feret_diameter_ratio,
              アスペクト比 = aspect_ratio,
              圧縮度 = compactness,
              円径比 = 最大内接円 / 最小外接円,  # sphericity
              針状度 = 1 -exp(-3 *abs(1 -圧縮度) *(1 -円径比) /アスペクト比),  # (圧縮度 / アスペクト比) / 2,  # acicularity
              円形度 = circularity,
              円形度2 = circularity2,
              円形度3 = circularity3,
              円磨度 = roundness,
              包絡度 = convexity,
              面積包絡度 = solidity,
              矩形度 = rectangularity,
              頂点数 = apex_n,
              角度_平均 = angle_mean,
              角度_標準偏差 = angle_sd,
              角度_最小 = angle_min,
              角度_最大 = angle_max,
              角度_mmr = 角度_最小 / 角度_最大,
              ## 1 - exp(-((角度_最大 + 角度_最大) / 2 / 角度_標準偏差) / 頂点数)  # (角度_平均 / 角度_標準偏差) / 頂点数
              角度_変動係数 = 1 / (1 +exp(-3 *((角度_最小 +角度_最大) /2 /角度_標準偏差) /頂点数)) ^3,
              ギザ度 = (包絡度 / 面積包絡度) * (円磨度 / 円形度) -0.5,  # edge roughness
              内接モコ度 = (pi * 最大内接円) / 周囲長,  # aka., roughness
              外接モコ度 = 周囲長 / (pi * 最小外接円),
              内面モコ度 = (pi * 最大内接円 ^2 / 4) / 周囲長,
              外面モコ度 = 周囲長 / (pi * 最小外接円 ^2 / 4),
              ## Vertex Boundary Angularity; not an abbraviation of beaver
              VBA = (1 - 針状度) * 角度_変動係数 * 外接モコ度,
              ギアmodule = pi *(最小外接円 + 最大内接円) /2 /頂点数,  # pi*円相当径/頂点数  pi*包絡長/頂点数
              ギア突出量 = (最小外接円 - 最大内接円) /2,  # (周囲長 - 包絡長) /頂点数  円相当径
              ギア元幅 = ギア突出量 *(1 + 2 /tan((1 -(角度_最小 +角度_最大) /2 /180) *pi)),  # 角度_平均
              ## ルイスの式: 歯先に働く力 = 歯型係数*歯元応力*基準円*歯幅 ~ k*基準円*歯幅 = k*(π*モジュール)*歯幅 ~ モジュール*歯幅
              MVP = log10(ギアmodule * ギア元幅)  # モジュール*歯幅; Momentum of vertex points
            # 座標 = apex_xy
            ) %>%
            relocate(VBA, MVP, 粒径, .before = 面積) %>%
            select(-area:-apex_xy)
  }

  ## summarise function
  transdata <- function(tmpx) {
    ## skip the columns, 砥粒度, ID, lot
    m1 <- summarise_all(tmpx, ~ mean.(.)) %>% mutate(stats = 'mean', .before = VBA)
    m2 <- summarise_all(tmpx, ~ mean.(., trim = 0.2)) %>% mutate(stats = 'trim_mean', .before = VBA)
    m3 <- summarise_all(tmpx, ~ prop_mean.(.)) %>% mutate(stats = 'proportional_mean', .before = VBA)
    m4 <- summarise_all(tmpx, ~ geo_mean.(.)) %>% mutate(stats = 'geometric_mean', .before = VBA)
    m5 <- summarise_all(tmpx, ~ hl_mean.(.)) %>% mutate(stats = 'Hodeges-Lehmann_estimator', .before = VBA)
    m6 <- summarise_all(tmpx, ~ median.(.)) %>% mutate(stats = 'median', .before = VBA)
  # m7 <- summarise_all(tmpx, ~ har_mean.(.)) %>% mutate(stats = 'harmonic_mean', .before = VBA)
  # p5 <- summarise_all(tmpx, ~ percentile.(., 0.05)) %>% mutate(stats = 'p5', .before = VBA)
  # p50 <- summarise_all(tmpx, ~ percentile.(., 0.5)) %>% mutate(stats = 'p50', .before = VBA)
  # p95 <- summarise_all(tmpx, ~ percentile.(., 0.95)) %>% mutate(stats = 'p95', .before = VBA)
    ## transpose the data
    out <- bind_rows(m1, m2, m3, m4, m5, m6)  # %>% t.()
    return(out)
  }

  ## separate data into IDs & shape data
  ## lot variation
  tmp_lot <- tmp2 %>% select(-c(砥粒名, 粒度, SEM倍率, 測定日, 備考)) %>% group_by(砥粒度, lot, 粒度分布ID)
  ids <- tmp_lot %>% tally() %>% ungroup()
  dats <- tmp_lot %>% group_map(~ transdata(.))
  out_lot <- bind_rows(
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'mean')),
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'trim_mean')),
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'proportional_mean')),
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'geometric_mean')),
             # bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'harmonic_mean')),
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'Hodeges-Lehmann_estimator')),
               bind_cols(ids, bind_rows(dats) %>% dplyr::filter(stats == 'median'))
             ) %>%
             mutate(砥粒名 = str_split_i(砥粒度, ' \\(', i = 1), .after = 砥粒度) %>%
             mutate(粒度 = str_split_i(砥粒度, ' \\(', i = 2) %>% gsub('\\)', '', .), .after = 砥粒名) %>%
             relocate(砥粒度, .before = VBA) %>%
             arrange(砥粒度, lot)

  ## type variation
  tmp_type <- tmp2 %>% select(-c(砥粒名, 粒度, SEM倍率, 測定日, 備考, lot, 粒度分布ID)) %>% group_by(砥粒度)
  id2 <- tmp_type %>% tally() %>% ungroup()
  dat2 <- tmp_type %>% group_map(~ transdata(.))
  out_type <- bind_rows(
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'mean')),
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'trim_mean')),
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'proportional_mean')),
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'geometric_mean')),
              # bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'harmonic_mean')),
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'Hodeges-Lehmann_estimator')),
                bind_cols(id2, bind_rows(dat2) %>% dplyr::filter(stats == 'median'))
              ) %>%
              mutate(砥粒名 = str_split_i(砥粒度, ' \\(', i = 1), .after = 砥粒度) %>%
              mutate(粒度 = str_split_i(砥粒度, ' \\(', i = 2) %>% gsub('\\)', '', .), .after = 砥粒名) %>%
              arrange(砥粒度)

  ## summary
  if (excel == TRUE) {
    write2.(list(生データ = tmp2, ロット集計 = out_lot, 砥粒度de集計 = out_type))
    cat('\n    Shape factor data has been created on your Desktop...\n\n')
  }
  return(tmp2)  # raw data
}


## Pick Sdorpion csv data in your folder and make them together tidy one == (2024-07-17) ========================
pk2. <- function(excel = T, ...) {
  ## Interactive input for the magnification of SEM image
  repeat {
    numb <- readline('\n    SEM倍率を入力してください  \n>>> ')
    if (zenk.(numb) %>% {skipMess.(as.numeric(.))} %>% {!is.na(.)}) {
      numb <- zenk.(numb) %>% as.numeric(numb)
      if (numb %% 10 != 0) cat('\n   Your input is not proper...\n\n')
      if (numb > 10 && numb %% 10 == 0) break
    }
  }
  cat(str_dup('=', times = 50), '\n')

  cat('\n   さきの倍率と同じcsvを選択してください  \n>>> ')
  tmp <- skipMess.(vroom.()) %>%
         .[!str_detect(names(.), '\\.')] %>%
         mutate(
           倍率 = numb,
           面積 = area / (倍率 / 100) ^ 2,
           包絡面積 = convex_area / (倍率 / 100) ^ 2,
           矩形面積 = NA_real_,
           周囲長 = perimeter / (倍率 / 100),
           包絡長 = convex_perimeter / (倍率 / 100),
           矩形短辺 = l_minor / (倍率 / 100),
           矩形長辺 = l_major / (倍率 / 100),
           平均軸径 = (矩形短辺 + 矩形長辺) / 2,
           水平フェレ径 = feret_diameter_w / (倍率 / 100),
           鉛直フェレ径 = feret_diameter_h / (倍率 / 100),
           等価円周 = circle_equivalent_perimeter / (倍率 / 100),
           円相当径 = circle_equivalent_diameter / (倍率 / 100),
           最大内接円 = NA_real_,
           最小外接円 = NA_real_,
           フェレ径比 = pmin(水平フェレ径 / 鉛直フェレ径, 鉛直フェレ径 / 水平フェレ径),
           アスペクト比 = aspect_ratio,
           圧縮度 = compactness,
           円径比 = NA_real_,  # sphericity
           針状度 = NA_real_,  # acicularity
           円形度 = circularity,
           円形度2 = NA_real_,
           円形度3 = NA_real_,
           円磨度 = roundness,
           包絡度 = convexity,
           面積包絡度 = solidity,
           矩形度 = rectangularity,
           頂点数 = NA_real_,
           角度_平均 = NA_real_,
           角度_標準偏差 = NA_real_,
           角度_最小 = NA_real_,
           角度_最大 = NA_real_,
           角度_mmr = NA_real_,
           ## 1 - exp(-((角度_最大 + 角度_最大) / 2 / 角度_標準偏差) / 頂点数)  # (角度_平均 / 角度_標準偏差) / 頂点数
           角度_変動係数 = NA_real_,
           ギザ度 =  NA_real_,  # edge roughness
           内接モコ度 = NA_real_,  # aka., roughness
           外接モコ度 = NA_real_,
           内面モコ度 = NA_real_,
           外面モコ度 = NA_real_,
           ## Vertex Boundary Angularity; not an abbraviation of beaver
           VBA = NA_real_,
           ギアmodule = NA_real_,
           ギア突出量 = NA_real_,
           ギア元幅 = NA_real_,
           ## ルイスの式: 歯先に働く力 = 歯型係数*歯元応力*基準円*歯幅 ~ k*基準円*歯幅 = k*(π*モジュール)*歯幅 ~ モジュール*歯幅
           MVP = NA_real_  # モジュール*歯幅; Momentum of vertex points
         # 座標 = apex_xy
         ) %>%
         relocate(VBA, MVP, .before = 面積) %>%
         select(-area:-circularity)

  if (excel == TRUE) write2.(tmp)
  return(tmp)
}


## Transpose data frame into tibble == (2024-02-22) ========================
t. <- function(d, ...) {
  tmp <- d %>% select(where(~!is.numeric(.)), everything()) %>% t()
  col1 <- tibble(!!rlang::sym(rownames(tmp)[1]) := rownames(tmp)[-1])
  col2 <- as_tibble(tmp[-1, ]) %>% set_names(tmp[1, ])
  out <- bind_cols(col1, col2) %>% hablar::retype()
  return(out)
# t.(diamonds[1:3, -3:-4])
}


## Transform list data to tibble == (2023-09-06) ========================
list2tibble. <- function(dL, ord = F, ...) {
  query_lib.(naturalsort)
  if (is.atomic(dL)) dL <- as_tibble(dL) %>% set_names('y')
  if (is.data.frame(dL)) return(as_tibble(dL))  # safety net; dL is already tibble
  dL <- dL[!sapply(dL, is.null)]  # delete NULL

  if (map.(dL, ~ class(.)) %>% {!'data.frame' %in% .}) {  # Each nested data is atomic ('numeric')
    tmp <- which(length.(dL) == 0)  # In case of logical(0)
    if(length(tmp) != 0) dL[tmp] <- NA
    if (is.null(names(dL))) names(dL) <- str_c('X', seq_along(dL))
    dL <- map2(dL, names(dL), ~ enframe(.x) %>% .[, -1] %>% set_names(.y))
  } else {
    dL <- map(dL, ~ as_tibble(., .name_repair = 'minimal'))  # In case that the list has a data.frame (x = ... , y = ...)
    if (is.null(names(dL))) names(dL) <- str_c('L', seq_along(dL))  # Note: dL is all consited of tibble for now
  }

  max_row <- map_dbl(dL, nrow) %>% max(., na.rm = T)
  dL2 <- map(dL, function(nya) nya[1: max_row, ])  # You must make every length of the list the same before using bind_cols()
  name2 <- map(dL, names) %>% unlist() %>% make.unique2()
  d <- skipMess.(bind_cols(dL2)) %>% set_names(name2)

  ## Re-order
  if (is.numeric(ord)) {
    if (length(ord) != ncol(dt)) stop('ord does NOT match the length of the data...\n\n', call. = F)
    d <- d %>% select(ord)
  } else if (ord == TRUE) {
    d <- d %>% select(naturalsort::naturalorder(names(d)))
  }

  ## fill the black (only when a column has one value)
  lonely_col <- d %>% select_if(~ is.character(.) | is.factor(.) | is.logical(.) | is_time.(.)) %>%
                map_dbl(~ is.na(.) %>% sum()) %>% .[. == (nrow(d) -1)] %>% names()
  out <- if (length(lonely_col) == 0) d else fill(d, all_of(lonely_col))
  return(out)
# list2tibble.(list('nya', 1:5, 6:7))  list2tibble.(par()) %>% html.()
}


## Transform any data to list == (2021-08-21) ========================
dLformer. <- function(d, ord = F, ...) {  # ord = T/F, or desirable order like c(3, 4, 1, 2) accoding to the list's names
  query_lib.(naturalsort)
  if (is.atomic(d)){
    dL <- tibble(d) %>% list()
  } else if ('data.frame' %in% class(d)) {
    d <- mutate_if(d, ~ is.factor(.), ~ as.character(.))
    numTF <- map_lgl(d, ~ is.numeric(.))
    chrTF <- map_lgl(d, ~ is.character(.))
    timeTF <- map_lgl(d, ~ is_time.(.))
    if (all(numTF)) {  # [y1, y2, ...]
      dL <- as.list(d)%>% map(~.[!is.na(.)])
    } else {
      if (sum(numTF) == 1 && sum(chrTF) == 1) {  # [ID, y]
        dL <- split(d, d[chrTF]) %>%
              map(~ select_if(., ~ is.numeric(.)) %>%
              .[!is.na(.)])  # dLformer.(iris[4:5])
      } else if (sum(numTF) == 1 && sum(timeTF) == 1) {  # [time, y]
        abbre_time <- function(x) {  # ex.) humidity, temperature, yield, monthly report, ...
          aT <- if (delta.(x, 'year') >= 3) format(x, '%Y年') else if (delta.(x, 'month') >= 4) format(x, '%B') else format(x, '%H時')
          return(aT)
        }
        d <- mutate_if(d, ~ is_time.(.), ~ abbre_time(.))
        dL <- d[timeTF] %>%
              pull() %>%
              naturalsort::naturalorder() %>%
              d[., ] %>%
              split(., .[timeTF]) 
      } else {  # [ID1, ID2, ..., y1, y2, ...] --> [y1, y2, ...]
        dL <- d[numTF] %>% as.list()
      }
    }
  } else {  # In case of list
    dL <- d
  }

  ## Re-order
  if (is.numeric(ord)) {
    if (length(ord) != length(dL)) stop('ord does NOT match the length of the data...\n\n', call. = F)
    dL <- dL[ord]
  } else if (ord == TRUE) {
    dL <- dL[naturalsort::naturalorder(names(dL))]
  }
  return(dL)
# dLformer.(iris[1])  dLformer.(iris[4:5])  dLformer.(iris[3:5])
}


## Extract xy from dt as list == (2023-03-03) ========================
xyL. <- function(d, type = 'xy', fix = 1, ...) {  # change data.frame into a set of data.frame as a list
  ## xy: [x1,y1, x2,y2, ...] --> [x1,y1], [x2,y2], ...
  ## yy: [y1,y2, ...] --> [seq,y1], [seq,y2], ...
  ## xyy: [x, y1,y2, ...] --> [x,y1], [x,y2], ...
  if (!is.data.frame(d)) stop('Make data a data.frame...\n\n', call. = F)

  if (type %in% c('xy', 'xyxy')) {  # [x1,y1, x2,y2, ...] --> [x1,y1], [x2,y2]
    if (ncol(d) %% 2 != 0) stop('Make data column number even...\n\n', call. = F)
    evens <- seq(ncol(d) /2)
    out <- map(evens, ~ d[c(2 *. -1, 2 *.)], as_tibble)
    names(out) <- names(d[2 *evens -1]) %>% make.unique2()
  } else if (type %in% c('y', 'yy', 'yyy', 'yyyy')) {
    out <- id2y.(d) %>%
           select_if(is.numeric) %>%
           map(~ .[!is.na(.)] %>% tibble(x = seq_along(.), y = .))
  } else if (type %in% c('xyy', 'xyyy')) {  # [x, y1,y2, ...] --> [x,y1], [x,y2]
    fix <- abs(fix)  # In case of careless pointing negative value like -5
    out <- map(seq_along(d)[-fix], ~ d[c(fix, .)]) %>% set_names(names(d)[-fix])
  }
  return(out)
# xyL.(psd[-1], 'xy') %>% plt.
# xyL.(iris, 'yy')  # choose y for ID pairing
# xyL.(iris[-5], 'yy')
# xyL.(iris, 'xyy', fix = 5) %>% walk(., box2.)
}


## Transform [ID1,ID2,y] <--> [ID1, y1.ID2, y2.ID2, y3.ID2, ...] == (2022-05-09) ========================
id2y. <- function(d, ...) {  # ID: crush, y: vacuum
  tab_col <- map_lgl(d, ~ is.character(.) | is.factor(.)) %>% names(d)[.]
  num_col <- map_lgl(d, ~ is.numeric(.)) %>% names(d)[.]

  if (length(tab_col) == 0 || length(num_col) == 0) return(d)  # [y1,y2, ...] or [ID1,ID2, ...]
  if (length(tab_col) *length(num_col) != 0) {  # [ID,y] or [ID1,ID2,y1,y2, ...]
    tab_one <- choice.(tab_col, note = 'the [ID] to be crushed into factor Y', chr = T, one = T)
    num_one <- choice.(num_col, note = 'the [y] to be vacuumed by the new Y', chr = T, one = T)
    out <- d %>%
           rowid_to_column('nya') %>%
           pivot_wider(names_from = all_of(tab_one), values_from = all_of(num_one)) %>%
           select(!nya)
    return(out)
  }
# id2y.(iris[4:5])  id2y.(diamonds[1:3] %>% sample_n(1000)) %>% box2.
}


## Split ID data into list in a tidy way == (2023-11-15) ========================
split2. <- function(d, nest = F, ...) {
  tabTF <- map_lgl(d, ~ is.character(.) | is.factor(.))
  numTF <- map_lgl(d, ~ is.numeric(.))
  tab_col <- names(d)[tabTF] %>% choice.(note = 'Factor', chr = T, one = T)
  if (sum(numTF) == 0) return(d)  # only [ID1, ID2, ...]
  if (nest == T && sum(tabTF) == 0) out <- d %>% nest(data = everything()) %>% ungroup()  # [y1, y2, ...]
  if (nest == T && sum(tabTF) != 0) out <- d %>% group_by(across(all_of(tab_col))) %>% nest() %>% ungroup()  # [ID1, ID2, y1, y2, ...]
  if (nest == F && sum(tabTF) == 0) return(d)
  if (nest == F && sum(tabTF) != 0) out <- split(d, d[tab_col]) %>% map(~ select(., !all_of(tab_col)))
  return(out)
# split2.(iris, nest = T)  split2.(us_rent_income)
}


## Split a data into more / less case == (2021-08-17) ========================
case2. <- function(d, div = NULL, percentage = T, ...) {
  if ('list' %in% class(d) || is.null(div)) return(d)
  if (is.atomic(d)) d <- tibble(d)
  if (is.data.frame(d) && ncol(d) > 1) stop('Make the data with ONE column...\n\n', call. = F)
  d <- d %>% dplyr::filter(rowSums(is.na(.)) != ncol(.))
  label <- dplyr::case_when(d < div ~ str_c('x < ', div), TRUE ~ str_c('x ≥ ', div))
  if (percentage == TRUE) {
    per_chr <- formatC(100 *table(label) /length(label), format = 'f', digits = 1) %>% str_c(., '%')
    label <- dplyr::case_when(str_detect(label, '<') ~ str_c(label, '\n(', per_chr[1], ')'), TRUE ~ str_c(label, '\n(', per_chr[2], ')'))
  }
  out <- d %>% mutate(label)  # case_when in mutate() needs real column name ('d <' is incorrect)
  names(out)[1] <- names(d) %||% 'data'
  return(out)
# case2.(iris[3], div = 3) %>% box2.()
}


## Change a data into time cases; year, month, min, sec, ... , or time length == (2023-08-13) ========================
time2. <- function(d, div = NULL, origin = F, ...) {  # data form: [time, y1, y2, ...]
  query_lib.(naturalsort, lubridate)
  if (map_lgl(d, is_time.) %>% sum == 0) return(d)
  time_col <- names(d)[map_lgl(d, is_time.)]
  d <- d %>% arrange(across(time_col[1]))
  time_scale <- vector('numeric', 6) %>% set_names(c('year', 'month', 'day', 'hour', 'min', 'sec'))

  for (i in seq_along(time_col)) {
    ## auto scaling of time unit
    if (is.null(div)) {
      tmp <- d[time_col[i]] %>% pull() %>% {min(., na.rm = T) %--% max(., na.rm = T)}
      for (j in seq_along(time_scale)) time_scale[j] <- time_length(tmp, unit = names(time_scale)[j])
      div0 <- whichNear.(time_scale, 50) %>% names(time_scale)[.]  # guess a better scale
      if (origin == TRUE) {
        div <- div0
      } else if (div0 == 'year') {
        if (time_scale['year'] < 1 || time_scale['year'] > 2) {  # hard to see 24 months levels in the graph
          div <- div0
        } else {
          div <- 'ym'
        }
      } else {
        div <- div0
      }
    }
    ## time length calculation
    if (origin == TRUE) {
      d <- d %>%
           mutate(!!time_col[i] := {min(get(time_col[i])) %--% get(time_col[i])} %>% time_length(unit = div)) %>%
           rename('{time_col[i]}_{div}' := time_col[i])
      if (i == length(time_col)) return(d)
    } else {  # Note: if the data type is <date>, hour/min/sec will be 0
      d <- if (div %in% c('ym', 'my')) {
             d %>% mutate(!!time_col[i] := {str_c(year(get(time_col[i])), '-', month(get(time_col[i])))} %>% as.factor)
           } else if (div %in% c('year', 'years')) {
             d %>% mutate(!!time_col[i] := year(get(time_col[i])) %>% as.factor)
           } else if (div %in% c('month', 'months')) {
             d %>% mutate(!!time_col[i] := month(get(time_col[i]), label = T, abbr = F))
           } else if (div %in% c('day', 'days')) {
           # d %>% mutate(!!time_col[i] := day(get(time_col[i])) %>% as.factor)
           # d %>% mutate(!!time_col[i] := str_c(as.numeric(month(get(time_col[i]))), '月', as.numeric(day(get(time_col[i]))), '日'))
             d %>% mutate(!!time_col[i] := as.Date(get(time_col[i])))
           } else if (div %in% c('hour', 'hours', 'hr', 'hrs')) {
             d %>% mutate(!!time_col[i] := hour(get(time_col[i])) %>% as.factor)
           } else if (div %in% c('min', 'minute', 'minutes')) {
	         d %>% mutate(!!time_col[i] := minute(get(time_col[i])) %>% as.factor)
           } else if (div %in% c('sec', 'second', 'seconds')) {
             d %>% mutate(!!time_col[i] := second(get(time_col[i])) %>% as.factor)
           }
      if (i == 1) d <- naturalsort::naturalorder(d[[time_col[i]]]) %>% d[., ] %>% rename('{time_col[i]}_{div}' := time_col[i])
    }
  }  # End of for()
  return(d)
# time2.(economics)
# time2.(economics, div = 'day', origin = T)
}


## HTML table == (2022-10-11) ========================
html. <- function(d, ...) {
  query_lib.(DT)
  if (is.atomic(d) && !is.null(names(d))) d <- as.list(d) %>% list2tibble.()  # Case with a vector with names created by sapply()
  num2chr <- function(num) {
    if (!is.numeric(num)) return(num)
    Digits <- gsub('\\.', '', num) %>% gsub('^0', '', .) %>% str_length()
    Digits <- rep(3, length(num))  # mm unit allows 0.001; '%.3f'
    Digits[num >= 100] <- 0  # 100.000 looks bothersome, so change it to 100
    Digits[num %>% near(., as.integer(.))] <- 0  # In case of integer
    chr <- vector(mode = 'character', length = length(num))
    for (i in seq_along(num)) chr[i] <- sprintf(str_c('%.', Digits[i], 'f'), num[i])
    return(chr)
  }
  d <- d %>% mutate_if(~ is.numeric(.), ~ num2chr(.))  # All data is character
# d[is.na(d)] <- ''  # Printing blank instead of NA
  DT::datatable(d, rownames = T, filter = 'top', extensions = c('Buttons'),
                options = list(
                  pageLength = 100, lengthMenu = c(10, 20, 50),
                  autoWidth = T, scrollX = T, scrollY = '400px', scrollCollapse = T,
                  dom = 'Blfrtip', buttons = c('copy', 'excel', 'colvis')
                )
  )
# html.(starwars)
}


## Quick table == (2023-08-21) ========================
table. <- function(d, ...) {
  if (is.atomic(d)) d <- as_tibble(d)
  if (ncol(d) == 1) {
    out <- d[[1]] %>% table() %>% sort(decreasing = T)
  } else if (any(sapply(d, is.numeric))) {
    out <- d %>% map(~ table(.) %>% sort(decreasing = T))
  } else if (all(!sapply(d, is.numeric))) {  # consited of non-numeric xyz --> the least unique column be spread into z1,z2,... for count
    n_col <- d %>% select_if(~ !is.numeric(.)) %>% sapply(., n_distinct) %>% sort(decreasing = T) %>% names()
    tmp <- d[n_col] %>% table() %>% as_tibble() %>% spread(last(n_col), n)
    num_col <- select_if(tmp, is.numeric) %>% names()
    out <- tmp %>% mutate(total = rowSums(select(., !!num_col))) %>% filter(total != 0) %>% arrange(desc(total))
  }
  return(out)
# table.(diamonds[2:4])  table.(starwars[4])
}


## Quick check for basic statistics == (2022-06-28) ========================
stats. <- function(d, transpose = F, split = F, ...) {
  if (is.atomic(d)) {
    vecN <- substitute(d) %>% as.character() %>% {if (length(.) == 1) . else .[2]}  # Confirm; substitute(iris[[1]]) %>% as.character
    d <- as_tibble(d) %>% set_names(vecN)
  } else if (is.list(d)) {
    d <- list2tibble.(d) %>% select_if(~ n_distinct(.) > 1)  # No column with the same value
  }
  d <- d %>% select_if(~ !is.list(.) & !is_time.(.))
  if (map_lgl(d, ~ is.character(.) | is.factor(.)) %>% all()) stop('Confirm data contents...\n\n', call. = F)

  stats_names <- c('Avg', 'Proportional avg', 'Geometric avg', 'Harmonic avg', 'Median', 'X2.5', 'X97.5', 'Min', 'Max',
                   'SD', 'Robust deviation', 'Skewness', 'Kurtosis',
                   'Range', 'IQR', 'Min-max ratio', 'CV', 'RMS', 'RMSE', 'MSLE', 'MAE', 'Declining distribution index',
                   'Index of balance', 'Gini coefficient', 'Hurst exponent', 'Crest factor', 'Specral flatness', 'Mean crossing rate',
                   'Total', 'Number')  # 'Regular sd', 'Regular skew', 'Regular kurt', 
  mini_stats <- function(d, transpose, ...) {  # min2.(dt, na = T)  max2.(dt, na = T)
    stats_summary <- d %>% {
      bind_rows(
        mean.(.),
        prop_mean.(.),  # propotional mean
        geo_mean.(.),  # geometric mean
        har_mean.(.),  # harmonic mean
        median.(.),
        percentile.(., probs = 0.025),
        percentile.(., probs = 0.975),
        min.(.),
        max.(.),
        sd.(.),
      # sd_reg.(.),
        sd2.(.),  # robust deviation
        skew.(.),  # 3rd power moment
      # skew_reg.(.),
        kurt.(.),  # 4th power moment
      # kurt_reg.(.),
        delta.(.),
        iqr.(.),
        mmr.(.),  # min-max ratio
        cv.(.),  # coefficient of variance
        rms.(.),  # root mean square
        rmse_stats.(.),  # root mean squared error
        msle.(.),  # mean squared logarithmic error
        mae.(.),  # mean absolute error
        ddi.(.),  # declining distribution index
        balance.(.),  # index of balance
        gini.(.),  # Gini coefficient
        hurst.(.),  # Hurst exponent
        cf.(.),  # crest factor
        sfm.(.),  # spectrum flatness measure
        mcr.(., zero = F),  # mean crossing rate
        sum.(.),
        length.(.)
      )} %>% mutate(stats = stats_names) %>% relocate(stats)
    if (transpose == TRUE) {
      stats_summary <- stats_summary %>%
                       pivot_longer(cols= -1) %>%
                       pivot_wider(names_from = stats, values_from = value) %>%
                       rename(var_names := name)
    }
    return(stats_summary)
  }
  tab_col <- d %>% select_if(~ is.character(.) | is.factor(.)) %>% names()
  if (split == TRUE && length(tab_col) == 1 && unique(d[tab_col]) %>% nrow(.) > 1) {
    out <- d %>% split(.[[tab_col]]) %>% map(~ mini_stats(., transpose))
  } else {
    out <- mini_stats(d, transpose)
  }
  return(out)
# stats.(iris) %>% html.()  stats.(iris, transpose = T)  stats.(iris, transpose = T, split = T)
# psych::describe(iris)
}


## summary for counting & one function == (2023-10-16) ========================
smry. <- function(d, f = 'mean', name = 'n', div = NULL, ...) {
  query_lib.(naturalsort)
  d <- list2tibble.(d)
  d <- d %>% select_if(~ !is.list(.)) %>% time2.(div = div)
  f0 <- f %>% gsub('ave|avg|average|mean|mean\\.', 'mean.', .) %>%
              gsub('sd|sd\\.|std|stdev', 'sd.', .) %>%
              gsub('quantile|quantile\\.|parcent|percent|percentile|percentile\\.', 'percentile.', .)
  if (str_detect(f0, '\\(x\\)|\\(x,')) f0 <- str_c('function(x) ', f0) %>% {eval(parse(text = .))}

  tab_col <- map_lgl(d, ~ is.character(.) | is.factor(.)) %>%
             names(d)[.] %>%
             choice.(note = 'What\'s [ID] ?', chr = T, one = F)

  if (is.null(tab_col)) {  # [y1,y2,...]
    out <- d %>% select_if(is.numeric) %>%
           pivot_longer(cols = everything()) %>%
           group_by(name) %>%
           summarise_all(.funs = list(
             n = length., sum = sum., median = median., avg = mean., std = sd., robust_std = sd2., range = delta., max = max., min = min.,
             skewness = skew., kurtosis = kurt., cv = cv., p5 = p5., p50 = p50., p95 = p95.)
           )
    return(out)
  }

  tmp1 <- d %>% count(across(all_of(tab_col)), name = name) %>%
          .[naturalsort::naturalorder(.[[1]]), ]
  if (map_lgl(d, is.numeric) %>% sum() == 1) {
    tmp2 <- d %>% group_by(across(all_of(tab_col))) %>% select_if(is.numeric) %>% {
              if (f != 'mean') {
                summarise_all(., .funs = list(
                  n = length., sum = sum., median = median., avg = mean., std = sd., robust_std = sd2., range = delta., max = max., min = min.,
                  skewness = skew., kurtosis = kurt., cv = cv., p5 = p5., p50 = p50., p95 = p95., tmp = f0)
                ) %>%
                rename(!!f := tmp)
              } else {
                summarise_all(., .funs = list(
                  n = length., sum = sum., median = median., avg = mean., std = sd., robust_std = sd2., range = delta., max = max., min = min.,
                  skewness = skew., kurtosis = kurt., cv = cv., p5 = p5., p50 = p50., p95 = p95.)
                )
              }
            }
  } else {
    tmp2 <- d %>% group_by(across(all_of(tab_col))) %>% select_if(is.numeric) %>%
            summarise_all(.funs = f0)
    cat('Casting, by = \"', f, '\"\n', sep = '')
  }

  out <- left_join(tmp1, tmp2)
  return(out)
# d<-sample(seq(87),1000,T)%>%starwars[.,]; smry.(d, f = 'percentile.(x, 0.5)/mean(x)')  smry.(iris)  smry.(iris[1:4])
}


## Search for the nearest number of which the target vector is almost equal to the reference value == (2021-08-21) ============
whichNear. <- function(vec, ref, back = F, value = F, ...) {
  if (is.numeric(c(vec, ref))) {
    out <- map_dbl(ref, ~ which(abs(vec -.) == min.(abs(vec -.))) %>% nth(., ifelse(back, -1, 1)))
    if (value == TRUE) out <- vec[out]
  } else {
    out <- map(ref, ~ str_which(vec, .) %>% {if (length(.) == 0) NA else .})  # whichNear.(names(iris), c('Sepal|Width', 'Length'))
  }
  return(out)
}


## Select just value you want according to a condition (sorry for misleading name); Note ref = sizeVec in length == (2021-08-21) ======
whichSize. <- function(vec, ref, mirror, ...) whichNear.(vec, ref) %>% mirror[.]


## Japanese or not for label & legend == (2023-09-27) ========================
jL. <- function(chr, ...) {  # 'systemfonts'::system_fonts()$family %>% unique() %>% sort()
  os_type <- which(c('Darwin', 'Linux', 'Windows') %in% Sys.info()['sysname'])
  if (str_detect(class(chr), 'character|factor') %>% any()) {
    if (!exists('chr') || is.null(chr) || anyNA(chr)) return(c('Avenir Next', 'sans')[os_type])
    tf <- str_detect(chr, pattern = '\\p{Hiragana}|\\p{Katakana}|\\p{Han}')
    if (any.(tf)) {
      c('HiraginoSans-W3', 'Noto Sans CJK JP', 'Yu Gothic')[os_type]  # Meiryo
    } else {
      c('Avenir Next', 'Noto Sans CJK JP', 'sans')[os_type]  # Ubuntu  Open Sans Light
    }
  } else {  # in case of 'expression()'
    return(c('Avenir Next', 'Noto Sans CJK JP', 'sans')[os_type])
  }
# mtext(~, family = jL.(c(Xlab, Ylab)))
}


## Correct abnormal Jap characters == (2022-06-17) ========================
hankana2zenkana. <- function(chr, ...) {
  if (str_detect(chr, '\\p{Hiragana}|\\p{Katakana}|\\p{Han}', negate = T) %>% any.()) return(chr)
  ## Converting semi-dakuten
  han <- c('ｶﾞ','ｷﾞ','ｸﾞ','ｹﾞ','ｺﾞ','ｻﾞ','ｼﾞ','ｽﾞ','ｾﾞ','ｿﾞ','ﾀﾞ','ﾁﾞ','ﾂﾞ','ﾃﾞ','ﾄﾞ','ﾊﾞ','ﾋﾞ','ﾌﾞ','ﾍﾞ','ﾎﾞ','ﾊﾟ','ﾋﾟ','ﾌﾟ','ﾍﾟ','ﾎﾟ')
  zen <- c('ガ','ギ','グ','ゲ','ゴ','ザ','ジ','ズ','ゼ','ゾ','ダ','ヂ','ヅ','デ','ド','バ','ビ','ブ','ベ','ボ','パ','ピ','プ','ペ','ポ')
  for (i in seq_along(zen)) {
    tmp_tf <- str_detect(chr, han[i]) %>% replace_na(replace = FALSE)
    chr[tmp_tf] <- gsub(han[i], zen[i], chr[tmp_tf])
  }
  ## Converting 1bite character
  out <- chartr('ｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝ｡｢｣､･ｦｧｨｩｪｫｬｭｮｯｰ',
                'アイウエオカキクケコサシスセソタチツテトナニヌネノハヒフヘホマミムメモヤユヨラリルレロワヲン。「」、・ヲァィゥェォャュョッー', chr)
  return(out)
}


## Zenkaku Japanese & English convert for Mac/Win == (2023-03-03) ========================
zenk. <- function(chr, ...) {
  query_lib.(stringi)
  ng_chr <- c('\r\n', '　', '  ', '－', '―') # never use '\n' for box2.() and '' which is applied for all strings
  ok_chr <- c('', ' ', ' ', '-', '-')
  for (i in seq_along(ng_chr)) {
    tmp_tf <- str_detect(chr, ng_chr[i]) %>% replace_na(replace = FALSE)
    chr[tmp_tf] <- gsub(ng_chr[i], ok_chr[i], chr[tmp_tf])
  }

  out <- chr %>% {if (any.(validUTF8(.)) || Sys.getenv('OS') != '') . else iconv(., 'utf8', 'cp932')} %>%
         hankana2zenkana.() %>%
         stringi::stri_trans_nfkc()  # Zenkaku alphabet to hankaku one
  return(out)
}


## Reshape text by cutting space & common characters == (2024-10-22) ========================
correctChr. <- function(chr, ...) {
  if (str_detect(chr, '\\p{Hiragana}|\\p{Katakana}|\\p{Han}', negate = T) %>% any.()) {  # only for alphabet or number
    tf1 <- str_detect(chr, '%') %>% replace_na(replace = FALSE)
    tf2 <- chr %>% {str_detect(., '[:digit:]') & str_detect(., ',') & !str_detect(., '-')} %>% replace_na(replace = FALSE)
    if (any.(tf1)) chr[tf1] <- chr[tf1] %>% as.vector() %>% parse_number(na = c('NA', '-')) %>% {. /100}  # '12.3%'
    if (any.(tf2)) chr[tf2] <- chr[tf2] %>% as.vector() %>% gsub(',', '', .)  # "123,456,789", or "\1,000"
  }
  out <- str_replace_all(chr, '#DIV/0!|#NAME\\?|\\bNA\\b|#N/A|NA\\?|\\bNULL\\b|#NULL!|#NUM!|#REF!|#VALUE!', NA_character_) %>%
         zenk.(chr)  # \\b means boundary
  return(out)
}


## Reshape text by cutting space & common characters == (2020-02-07) ========================
neatChr. <- function(chr, ...) {  # c('nya :: A', 'nya :: B') --> c('A', 'B')
  ## Delete space character
  regularChar <- c('(', ')', '[', ']', '$', '^', '?', '*', '+', '{', '}', '|', '\'')  # Replace the regular chr that cannot use str_detect()
  for (i in seq_along(regularChar)) chr <- gsub(regularChar[i], ' ', chr, fixed = T) %>% str_trim(., side = 'both')
  if (length(chr) == 1) return(chr)
  ## Search for common characters and delete them.
  len <- min.(str_length(chr))
  ctr <- NULL
  for (i in 2:len) {  # Why it starts 2 is; if a string is 'A' and it's removed, then the string after cutting will be ''.
    tenta <- str_trunc(chr, width = i, ellipsis = '') %>% unique(.)
    if (length(tenta) == 1 && str_detect(tenta, '[:alpha:]') %>% all(.)) ctr <- i
  }
  if (!is.null(ctr)) chr <- str_replace(chr, pattern = substr(chr[1], 1, ctr), replacement = '')
  return(chr)
}


## Interactive filter == (2024-07-17) ========================
choice. <- function(factors, note = NULL, chr = T, one = F, ...) {
  ## chr = T returns the text in the choice (F returns the number of choice)
  factors <- unlist(factors)  # In case of X x 1 tibble
  if (length(factors) == 0) return(NULL)
  if (length(factors) == 1) return(factors)

  ## interactive display
  if (one == TRUE) {
    tmp <- rep(NA_character_, length(factors))
    for (i in seq_along(tmp)) tmp[i] <- str_c('   [', i, ']', ifelse(i < 10, '  ', ' '), factors[i], '\n')
  } else {
    tmp <- rep(NA_character_, length(factors) + 1)
    for (i in seq_along(tmp)) {
      if (i == 1) tmp[i] <- str_c('   [0]  ALL\n')
      if (i > 1) tmp[i] <- str_c('   [', i - 1, ']', ifelse(i < 10, '  ', ' '), factors[i - 1], '\n')
    }
  }
  cat('\n', tmp)

  ## to get text or number
  if (one == TRUE) note2 <- note %||% 'Select No.'
  if (one == FALSE) note2 <- {note %||% 'Select No.'} %>% str_c(., '\n    Input like 1 2 3  or  1,2,3  or  1.2.3  or  1:3')

  repeat {
    numb <- readline(str_c('    ', note2, '\n>>> '))
    numb <- gsub(',|\\.|\\*|/|;| |  ', '_', numb) %>%
            str_split('_') %>%
            unlist() %>%
            {.[!. %in% '']} %>%
            correctChr.() %>%
            map.(~ eval(parse(text = .))) %>%
            unique() %>%
            .[!is.na(.)]  # To gurantee your input as numeric

    ## safe guard for wrong input
    if (all(numb == 0)) numb <- seq_along(factors); break
    if (all(numb >= 1) && one == TRUE && length(numb) == 1) break
    if (all(numb >= 1) && one == FALSE && length(numb) >= 1) numb <- numb + 1; break
  }
  numb <- numb[numb <= length(factors)]  # safe guard for overshoot input
  out <- if (chr == TRUE) factors[numb] else numb  # text or its number

  str_c('|', str_dup(stringi::stri_unescape_unicode('\\u2588'), 24), '|\n') %>% cat()
  return(out)
# choice.(LETTERS[1:2], note = 'Blood type', one = T)
# choice.(names(iris), note = 'xy data', chr = F)
}


## Plot range for plot.window() & axisFun.() == (2023-09-20) ========================
pr. <- function(d, XYlims = NA, expand_ratio = 0.035, ...) {
  if ('list' %in% class(d)) d <- unlist(d)
  if (is.atomic(d)) d <- as_tibble(d)
  if (ncol(d) > 1) d <- d[1]
  if (is.null(XYlims)) XYlims <- c(NA, NA)
  if (length(XYlims) == 1) XYlims <- c(XYlims, NA)
  xyR <- dplyr::coalesce(XYlims, range.(d))  # you can convert even dttm
  expand_direction <- ifelse(is.na(XYlims), 1, 0) *c(-1, 1)  # expand the range if pointed value --> 1, NA --> 0
  XYlim2 <- xyR +diff(xyR) *expand_direction *expand_ratio
  return(XYlim2)
# pr.(economics[1], XYlims = NA, 0.013)
}


## Axis value == (2020-02-07) ========================
halfSeq. <- function(vec, ...) vec[-1] -diff(vec) /2  # Solution of bn = (an+1 - an)/2
axisFun. <- function(XYlims, n = 5, ...) pretty(XYlims, n = n) %>% list(mainTicks = ., subTicks = halfSeq.(.))


## Cyclic number if it's over range for the interactive input == (2024-07-03) ========================
n_cyc. <- function(num, n_max, len_max = NULL, ...) {
  if (all(is.na(num))) return(n_max)
  num_changer <- function(x) {
    x <- parse_number(as.character(x)) %>%  # cleaning
         {. %% n_max} %>%
         ifelse(. != 0, ., n_max)  # just in case of x = n_max or 0
    return(x)
  }
  num <- num_changer(num)
  if (!is.null(len_max)) num <- rep(num, len_max) %>% .[1:len_max]
  return(num)
# n_cyc.('12', 5)  n_cyc.(4:7, 5)  n_cyc.(c(1,NA,9), 5, len_max = 6)
}


## Number complement if the input is short of all numbers for the argument 'sel' of plt.() == (2023-07-30) ========================
n_comp. <- function(nums, n_max, ...) {
  nums <- map_dbl(nums[!is.na(nums)], ~ .x %% n_max %>% ifelse(. != 0, ., n_max))
  n_seq <- seq(n_max)
  out <- c(nums, n_seq[!n_seq %in% nums])
  return(out)
# n_comp.(13, 5)  n_comp.(3:1, 5)  n_comp.(c(1, NA, 9), 5)
}

    
## Creat translucent color == (2022-05-09) ========================
col_tr. <- function(color, tr, ...) {
  color <- color %>% ifelse(is.na(.) | . == 0, '#FFFFFF00', .)
  return(adjustcolor(color, alpha.f = tr))  # rgb(t(col2rgb(color) /255), max = 1, alpha = tr)
}


## Color gradient with Y values == (2022-11-04) ========================
colGra. <- function(d, color, ColorSteps = 13, ...) {
  vec <- flatten_dbl(d) %>% .[!is.na(.)]
  color2 <- c(col_tr.(color, tr = 0.75), 'grey80', col_tr.(color, tr = 0.6))  # sapply(vec, abs) %>% max(.)
  kingMax <- abs(vec) %>% max  # NOTE: the 2nd para. of the following findInterval() are not {min, max} but {-max, +max}
  out <- colorRampPalette(color2)(ColorSteps)[findInterval(vec, seq(-kingMax, +kingMax, length.out = ColorSteps))]
  return(out)
# plot(iris[1:2], col = colGra.(iris[2], c('red', 'blue')))
}


## Auto color assignment == (2023-10-30) ========================
color2. <- function(col = NULL, len = NULL, ...) {
  query_lib.(RColorBrewer, scico, viridis, viridisLite)
  color_base <- c('grey13', 'firebrick1', 'deepskyblue4', 'antiquewhite3', 'sienna3', 'palevioletred3', 'seagreen4', 'dodgerblue3',
                  'darkorange2', 'maroon4', 'hotpink2', 'peachpuff2', 'lightsalmon3', 'tomato2', 'deeppink3', 'slateblue2',
                  'deepskyblue4', 'darkseagreen3')
  ## Cleaning
  color_chr_clean <- function (col) {
    chr_clean <- function(x) {
      if (is.na(x)) return('grey13')  # '#FFFFFF00'
      if (all(x %in% colors())) return(x)  # regular color names
      if (all(str_detect(x, '#') & (str_length(x) == 7 | str_length(x) == 9))) return(x)  # 7: raw, 9: translucent
      if (str_detect(x, pattern = '[:digit:]')) {  # number input you like
        x <- if (x == '0') 'grey13' else n_cyc.(x, n_max = length(color_base), len_max = 1) %>% color_base[.]  # 0 == '0' --> TRUE
        return(x)
      } else {
        return('grey13')  # wrong spell or NA
      }
    }
    return(map_chr(col, chr_clean))
  # c('red', 'black', 0, 'abc', NA) %>% color_chr_clean
  }

  ## Arrange
  if (is.null(col) && is.null(len)) {  # color2.()
    out <- color_base
  } else if (!is.null(col) && is.null(len)) {  # color2.(col = c('red', 'black', 0, 'abc', NA))
    out <- color_chr_clean(col)
  } else if (!is.null(col) && !is.null(len)) {  # color2.(col = c('grey35', 'blue3'), len = ncol(iris))
    out <- color_chr_clean(col) %>% rep(., times = len) %>% {.[1:len]}
  } else if (is.null(col) && !is.null(len)) {  # Auto assignment according to data
    if (len == 1) {
      rand <- floor(runif(1, min = 1, max = length(color_base) + 1))
      out <- color_base[rand]
    } else if (len == 2) {
      rand <- floor(runif(1, min = 1, max = length(color_base)))
      out <- c('grey24', color_base[-1][rand])
    } else if (len == 3) {  # color2.(len = ncol(iris[1:3]))
      color3s <- list(
                   color_base[1:3],
                   c('#003356', '#f3981c', '#e3e1d6'),  # navy, orange, accent
                   c('#bdc6ca', '#32495f', '#889291'),  # gray base, sub, accent
                   c('#99af87', '#4b4936', '#4a6b44'),  # green base (wasabi)
                   c('#ee7d50', '#68666c', '#d2ddde')  # rocky red, cloud grey, mouse grey
                 )
      rand <- floor(runif(1, min = 1, max = 1 + length(color3s)))
      out <- color3s[[rand]]
    } else if (len < 6) {
      out <- viridisLite::viridis(len +1, option = 'A')[-(len +1)]
    } else {
    # NOTE: name conflict of map() due to loading 'maps' package by use of 'pals'
    # col_v <- len %>% c(pals::tol.rainbow(.), pals::linearl(.), pals::cubicl(.))[floor(runif(1, min = 1, max = 3 +1))]
    # col_v <- c('A','B','C','D')[floor(runif(1, min = 1, max = 4 +1))] %>% {viridisLite::viridis(len, option = .)} %>% rev()
      ocean_pal <- colorRampPalette(c('#003000','#005E8C','#0096C8','#45BCBB','#8AE2AE','#BCF8B9'))
      land_pal <- colorRampPalette(c('#467832','#887438','#B19D48','#DBC758','#FAE769','#FCED93'))

      rand <- floor(runif(1, min = 1, max = dplyr::case_when(len <= 11 ~ 10, len <= length(color_base) ~ 11, TRUE ~ 9)))

      out <- if (rand == 1) viridisLite::viridis(len +1, option = 'A')[-(len +1)] else
             if (rand == 2) viridisLite::viridis(len +1, option = 'D')[-(len +1)] else
             if (rand == 3) viridisLite::viridis(len +1, option = 'E')[-(len +1)] else
             if (rand == 4) ocean_pal(len) else
             if (rand == 5) c(ocean_pal(len %/% 2), land_pal(len -len %/% 2)) else
             if (rand == 6) scico::scico(len, palette = 'cork') else
             if (rand == 7) scico::scico(len, palette = 'tokyo') else
             if (rand == 8) scico::scico(len, palette = 'turku') else
             if (rand == 9) RColorBrewer::brewer.pal(len, 'Spectral') else
             if (rand == 10) color_base[1:len]
    }
  }
  return(out)
}  # END of color2.()


## Halo around text == (2020-02-07) ========================
## https://stackoverflow.com/questions/25631216/r-plots-is-there-any-way-to-draw-border-shadow-or-buffer-around-text-labels
haloText. <- function(x, y, labels, cex, col = 'grey13', bg = 'white', theta = seq(0, 2 *pi, length.out = 36), r = 0.05, ...) {
  purrr::invoke_map(function(i) text(x +r *strwidth('A') *cos(i), y +r *strheight('A') *sin(i), labels, cex = cex, col = bg), as.list(theta))
  text(x, y, labels, cex = cex *0.95, col = col)  # Then draw actual text
}


## Optimum position of y-axis label == (2023-11-15) ========================
yPos. <- function(ylim2, ...) {
  ## strwidth('0')/strwidth('.') = 1, strwidth('0')/strwidth('.') = 2.230769 
  len <- axisFun.(ylim2, n = 6)[[1]] %>% {.[between(., ylim2[1], ylim2[2])]}  # fix 9.3 when it's over "10000"
# out <- len %>% {str_count(., '\\.') *1 +str_count(., '[:digit:]') *2.230769} %>% max(.) %>% dplyr::if_else(.<9,.,9.3) %>% {2.6179 -0.1873 *.}
  out <- len %>% {str_count(., '\\.') *1 +str_count(., '[:digit:]') *2.230769} %>% max(.) %>% dplyr::if_else(.<9,.,9.3) %>% {2.6179 -0.1729 *.}
  return(out)
}


## Positioning legend == (2021-08-22) ========================
legeX. <- function(ratio, ...) if (is.null(ratio) || is.na(ratio)) NULL else par('usr')[1] +diff(par('usr')[1:2]) *ratio
legeY. <- function(ratio, ...) if (is.null(ratio) || is.na(ratio)) NULL else par('usr')[3] +diff(par('usr')[3:4]) *ratio


## Easy legend == (2023-10-23) ========================
legen2. <- function(name, legePos = NULL, col = NULL, lty = NULL, pch = NULL, cex = NULL, inv = NULL, ...) {
  if (is.null(name) || 0 %in% name) return() else name <- as.character(name)  # no factor
  par(family = jL.(name))
  if (is.null(cex)) {
    Cex <- c(0.1, 1)  # Starters in the loop range
    for (stepW in c(0.1, 0.01, 1e-03)) {
      Cex <- Cex %>% {seq(.[1], .[2], by = stepW)}  # Make the range narrow gradually
      rss <- rep(NA_real_, length(Cex))
      for (i in seq_along(rss)) {
        tmp <- legend('topright', name, cex = Cex[i], plot = F)
        rss[i] <- (legeX.(0.42) -tmp$text$x[1]) ^2 + (legeX.(1 -0.42) -tmp$rect$w) ^2
      }
      Cex <- interval2.(rss, valley = T) %>% Cex[.]
      if (length(Cex) == 1) break
    }
    cex <- mean(Cex)
  }
  inv <- inv %||% {-0.5 *cex +1.5}
  col <- col %||% color2.(len = length(name))
  tmp <- legend('topright', name, cex = cex, plot = F)
  legeX <- legeX.(legePos[1]) %||% {2 *tmp[['rect']]$left -tmp[['text']]$x[1]}
  legeY <- legeY.(legePos[2]) %||% legeY.(0.975)

  legend(x = legeX, y = legeY, legend = name,
         cex = cex, x.intersp = 0.65, y.intersp = inv, lty = lty, pch = pch, horiz = F,
         box.lty = 0, lwd = 0.9, seg.len = 1.3, col = col, text.col = col, bg = col_tr.(0, 0.6))
  par(family = ifelse(Sys.getenv('OS') == '' & Sys.getenv('USER') != '', 'Avenir Next', 'sans'))
# plt.(iris, name = 0); legen2.(name = str_flatten(letters) %>% str_sub(., 1, 26) %>% rep(., 10))
}


## Now is the time == (2022-10-26) ========================
today2. <- function(chr = NULL, ...) {
  tmp <- today(tz = 'Asia/Tokyo') %>% gsub('-', '', .) %>% str_sub(3, 8)
  if (is.null(chr)) return(tmp)
  else return(str_c(tmp, '_', chr))
}
now2. <- function(...) now(tz = 'Asia/Tokyo') %>% gsub('-|:', '', .) %>% gsub (' ', '-', .) %>% str_sub(3, 13)


## Save graphics == (2024-09-11) ========================
save. <- function(name = NULL, type = 'png', wh = dev.size(), ...) {
  saveN <- name %||% now2.()
  if (as.character(substitute(type)) %in% c('jpg', 'jpeg', 'j')) {
    dev.copy(jpeg, file = str_c(saveN, '.jpg'), units = 'in', width = wh[1], height = wh[2], res = 150)
    dev.off()
  }
  if (as.character(substitute(type)) %in% c('png', 'p')) {
    dev.copy(png, file = str_c(saveN, '.png'), units = 'in', width = wh[1], height = wh[2], res = 350)
    dev.off()
  }
}
save2. <- function(name = NULL, wh = c(4.5, 3.3), ...) {
  saveN <- name %||% now2.()
  if (names(dev.cur()) == 'cario_pdf') dev.off()
  tryPDF <- function(...) {
    try(skipMess.(cairo_pdf(str_c(saveN, '.pdf'), width = wh[1], height = wh[2], bg = 'transparent', onefile = T)), silent = T)
  }
  if (class(tryPDF()) == 'try-error') {
    if (names(dev.cur()) != 'null device') dev.off()
    str_c('Do close \"', saveN, '.pdf\" on your application !!\n\n') %>% stop(., call. = F)
  } else {
    dev.off(); tryPDF()
  }
  gp.()
}


## Write list data to csv/xlsx file == (2023-12-11) ========================
write. <- function(.d, name = NULL, enc = NULL, ...) {
  if ('list' %in% class(.d)) .d <- list2tibble.(.d)
  name <- {name %||% now2.()} %>% {if (str_detect(., '\\.csv')) . else str_c (., '.csv')}
  if (!is.null(enc)) {
    enc <- if (str_detect(enc, 'c|cp|CP|cp932|CP932|cp-932')) 'cp932' else 'utf8'
  }
  write.csv(.d, name, row.names = F, na = '', fileEncoding = enc %||% ifelse(Sys.info()['sysname'] == 'windows', 'cp932', 'utf8'))
}
write2. <- function(.d, name = NULL, sheet = NULL, ...) {
  query_lib.(writexl)
  if (!'list' %in% class(.d)) {
    dL <- .d %>% mutate_if(is_time., ~ force_tz(.x, tzone = 'Asia/Tokyo')) %>% list()  # ~ str_c(.x, ' JST')
  } else {
    dL <- .d %>% map(function(x) x %>% mutate_if(is_time., ~ force_tz(.x, tzone = 'Asia/Tokyo')))
  }
  names(dL) <- names(dL) %||% sheet %||% str_c('#', seq_along(dL))
  writexl::write_xlsx(dL, path = str_c(name %||% now2.(), '.xlsx'), format_headers = F)
# openxlsx::write.xlsx(dL, file = str_c(name %||% now2.(), '.xlsx'), overwrite = T)
# write2.(list(A=iris, B=mtcars, C=chickwts, D=quakes), name = 'nya')
}


## Fitting by GAM (Generized Additive Model) - GCV (Generized Cross Validation) == (2023-09-07) ========================
gamXY. <- function(x, y = NULL, mdlGet = F, boost = F, n.boost = NULL, xlim = c(NA, NA), y0over = F, ...) {
  if (is.data.frame(x) && ncol(x) == 2) def.(c('x', 'y'), list(x[[1]], x[[2]]))
  x <- x[order(x)]
  y <- y[order(x)]
  ## Small data length cannot allow to use knot style
  gam_knots <- function(knots) try(mgcv::gam(y ~ s(x, k = knots, bs = 'cr'), method = 'REML'), silent = T)

  if (y0over == TRUE) {
    for (i in 15:30) if (all(fitted(gam_knots(i)) >= -1e-05)) break
    mdl <- gam_knots(i)  # plt.(list(tibble(x,y), tibble(x,fitted(mdl))))
  } else {
    mdl <- if (class(gam_knots(15))[1] == 'try-error') gam_knots(5) else gam_knots(15)
  }

  if (mdlGet == TRUE) return(minGCV)

  if (boost == TRUE || !is.null(n.boost)) {
    qx_len <- n.boost %||% whichSize.(ref = length(x), vec = c(50, 500), c(130, 888))
    qx <- if (anyNA(xlim) == TRUE) seq(min.(x), max.(x), length = qx_len) else seq(xlim[1], xlim[2], length = qx_len)
    xyFit <- tibble(x = qx, y = predict(mdl, newdata = tibble(x = qx)) %>% as.numeric())
  } else if (boost == FALSE) {
    ## GAM model always ignores NA and needs arranged. Even a plot with NA looks vacant, the fitted line shows proper position.
    y[!is.na(y)] <- fitted(mdl)  # Use NA info of original y and express unnatural appearance of vacant data
    xyFit <- tibble(x = x, y = y)             
  }
  return(xyFit)
# gamXY.(iris[1:2], n.boost = 200, xlim = c(5, 7))
}


## Finding curve intersection; different (x, y) version;  not so accurate if df has not so many data points == (2020-01-23) ==
## just vector but accurate analysis;  https://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
## another Ref;  https://stackoverflow.com/questions/31574382/intersection-between-density-plots-of-multiple-groups
intersectX. <- function(df1, df2, ...) {
  query_lib.(pracma)
  if (is.null(ncol(df1)) || is.null(ncol(df2))) {    #  for vector
    df1 <- data.frame(x = seq_along(df1), y = df1)
    df2 <- data.frame(x = seq_along(df2), y = df2)
  }
  s1 <- rep(NA_real_, nrow(df1))
  for (i in seq_along(s1)) {
    s1[i] <- whichNear.(vec = df2[[1]], ref = df1[[i, 1]]) %>% {abs(df1[[i, 2]] - df2[[., 2]])}
  }
  s1 <- scale(s1) %>% as.vector()
  fp <- pracma::findpeaks(-s1, nups = 10, ndowns = 10)  # '-' is to search for valleys, nups & ndowns are stricts of successive peaks
  # plot(s1); points(-fp[, 1] ~ fp[, 2], pch = 19, cex = 0.8, col = 'tomato2')  # Valleys analysis needs '-fp[, 1]' for plot.
  crossX <- rep(NA_real_, nrow(fp))  # Variation filter > 0.01
  for (i in seq_along(crossX)) crossX[i] <- fp[i, 2] %>% {abs(s1[. +1] -s1[.]) /((. +1) -.)} %>% {ifelse(. > 0.01, fp[i, 2], NA)}
  out <- crossX[!is.na(crossX)] %>% df1[., 1]
  return(out)
}  # plt.(list(df1, df2)); abline(v = intersectX.(df1, df2))


## Frame of plot == (2024-04-25) ========================
plot_frame. <- function(xy = NULL, grid = F, xlim2 = NULL, ylim2 = NULL, tcl = par('tcl'), padj = -0.1, rot = 0, cexlab = NULL,
                        bty = c('o', 'l')[1], xlab = NULL, ylab = NULL, yline = NULL, ...) {
  ## if you know xtype is 'num' in advance, xy = NULL is the shortcut
  xtype <- if (!is.null(xy)) {
    dplyr::case_when(sapply(xy[1], is.numeric) ~ 'num',
              sapply(xy[1], is_time.) ~ 'time',
              map_lgl(xy[1], ~ is.character(.x) | is.factor(.x)) ~ 'chr'
    )
  } else {
    'num'
  }
  if (grid == TRUE) {
    abline(v = if (xtype %in% c('num', 'time')) sort(unlist(axisFun.(xlim2, n=5))) else seq(nrow(xy)),
           h = sort(unlist(axisFun.(ylim2, n=6))),
           col = 'grey98'
    )
  }

  xcexaxis <- ifelse(yPos.(xlim2) > 0.9, 1, 0.9)
  ycexaxis <- ifelse(yPos.(ylim2) > 0.9, 1, 0.9)
  if (xtype == 'num') {
    for (i in 1:2) {
      axis(1, at = axisFun.(xlim2, n = 5)[[i]], labels = (i == 1), cex.axis = xcexaxis, padj = padj, tcl = tcl / i, lend = 'butt')
      if (bty == 'o') axis(3, at = axisFun.(xlim2, n = 5)[[i]], labels = F, tcl = tcl / i, lend = 'butt')
      axis(2, at = axisFun.(ylim2, n = 6)[[i]], labels = (i == 1), cex.axis = ycexaxis, tcl = tcl / i, lend = 'butt')
      if (bty == 'o') axis(4, at = axisFun.(ylim2, n = 6)[[i]], labels = F, tcl = tcl / i, lend = 'butt')
    }
  }

  if (xtype == 'time') {
    for (i in 1:2) {
      axis(1, at = unlist(axisFun.(xlim2, n = 5)[i]), labels = F, tcl = tcl / i, lend = 'butt')
      if (bty == 'o') axis(3, at = unlist(axisFun.(xlim2, n = 5)[i]), labels = F, tcl = tcl / i, lend = 'butt')
      axis(2, at = axisFun.(ylim2, n = 6)[[i]], labels = (i == 1), cex.axis = ycexaxis, tcl = tcl / i,, lend = 'butt')
      if (bty == 'o') axis(4, at = axisFun.(ylim2, n = 6)[[i]], labels = F, tcl = tcl / i, lend = 'butt')
    }
    ## x-label
    item_name <- axisFun.(xlim2, n = 5)[[1]] %>% gsub(' JST', '', .) %>% .[-c(1, length(.))]
    yPos <- par('usr')[3] -0.025 *delta.(par('usr')[3:4]) *whichSize.(ref = length(item_name), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.6))
    nameLen <- stringi::stri_numbytes(item_name) %>% max.()  # Count including multi bytes char and space
    rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.9, 0.8, 0.7)) %>%
               {. *whichSize.(ref = length(item_name), vec = c(8, 15, 35, 60, 100), c(1, 0.8, 0.75, 0.7, 0.4))}
    text(x = unlist(axisFun.(xlim2, n = 5)[1]) %>% .[-c(1, length(.))],
         y = yPos,
         labels = item_name,
         srt = rot,  xpd = T, adj = 0.5 + c(sin(rot /180 *pi), cos(rot /180 *pi)), cex = cexlab %||% rot_cex, family = jL.(item_name)
    )
  }

  if (xtype == 'chr') {
    for (i in 1:2) {
      if (i == 1) axis(1, at = seq(nrow(xy)), labels = F, tcl = - tcl / i, lend = 'butt')
      axis(2, at = axisFun.(ylim2, n = 6)[[i]], labels = (i == 1), cex.axis = ycexaxis, tcl = tcl / i,, lend = 'butt')
      if (bty == 'o') axis(4, at = axisFun.(ylim2, n = 6)[[i]], labels = F, tcl = tcl / i, lend = 'butt')
    }
    ## x-label
    item_name <- make.unique2(xy[[1]])
    yPos <- par('usr')[3] -0.035 *delta.(par('usr')[3:4]) *whichSize.(ref = length(item_name), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.06))
    nameLen <- stringi::stri_numbytes(item_name) %>% max.()  # Count including multi bytes char and space
    rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.9, 0.8, 0.7)) %>%
               {. *whichSize.(ref = length(item_name), vec = c(8, 15, 35, 60, 100), c(1, 0.8, 0.75, 0.7, 0.4))}
    text(x = seq(nrow(xy)),
         y = yPos,
         labels = item_name,
         srt = rot,  xpd = T, adj = 0.35 + c(sin(rot /180 *pi), cos(rot /180 *pi)), cex = cexlab %||% rot_cex, family = jL.(item_name)
    )
  }

  box(bty = bty)
  xlabcex <- whichSize.(ref = nchar(xlab), vec = c(15, 35, 50), c(1, 0.8, 0.5))
  ylabcex <- whichSize.(ref = nchar(ylab), vec = c(15, 35, 50), c(1, 0.8, 0.5))
  yline2 <- if (is.null(yline)) par('mar')[2] - yPos.(ylim2) else par('mar')[2] * (yline - 0.15) 
  mtext(xlab, side = 1, las = 1, cex = xlabcex, family = jL.(xlab), line = par('mar')[1] -1.00)
  mtext(ylab, side = 2, las = 3, cex = ylabcex, family = jL.(ylab), line = yline2)
# plt.(us_rent_income[c(2,5)], ylim = c(0, NA), type = 'h', lwd = 5, rot = 25, ylab = 'moe')
}


## Quick plot == (2024-07-10) ========================
plt. <- function(d, datatype = c('xy', 'yy', 'xyy')[1], trend = F, sel = NULL, xlim = NA, ylim = NA, name = NULL,
                 type = c('l', 'p', 'pp', 'b', 'bb', 'h', 's')[1], col = NULL, col_flag = NULL, lty = NULL, lwd = NULL, pch = NULL,
                 add = 1, grid = T, rot = 0, cexlab = NULL, xlab = NULL, ylab = NULL, yline = NULL, legePos = NULL, PDF = T, multi = F)
{  # sel is the order you want, item is to make item plot

  ## organize raw data & resolve nest type
  nest_get <- function(d, sel = NULL, type = 'l', col = NULL, lty = NULL, lwd = NULL, pch = NULL) {
    ## vector
    if (is.atomic(d)) {
      if (is.numeric(d)) {
        d <- d[!is.na(d)] %>% tibble(x = seq_along(.), y = .)
        out <- tibble(lengend = 'vector', data = list(d), color = color2.(col, len = 1), color_flag = list(color))
      } else {
        stop('Do not use a vector like [character].\n\n', call. = F)
      }
    }

    ## list
    if ('list' %in% class(d)) {  # [[x1,y1], [x2,y2], ...]
       tmp_tf <- d[!sapply(d, is.null)] %>%
                 map_lgl(~ all(sapply(., is.numeric)) && ncol(.) == 2) %>%
                 all()
      if (tmp_tf == TRUE) {
        out <- tibble(legend = name %||% names(d) %||% str_c('data', seq(d)), data = d, color = color2.(col, len = length(d)), color_flag = list(color))
      } else {
        stop('Make the list data all [numeric].\n\n', call. = F)
      }
    }

    ## data.frame
    if ('data.frame' %in% class(d)) {
      if (sum(map_lgl(d, is.numeric)) == 0) stop('Do not use a dataframe consited of all [character].\n\n', call. = F)
      if (ncol(d) == 1) d <- d %>% rowid_to_column('x')  # [y] -->  [seq,y]

      ## data type: x as num/chr/fac/time, y as num
      d <- time2.(d, div = 'day') %>%
           mutate_if(is.character, as.factor) %>%  # chr --> fac
           relocate(where(~ !is.numeric(.)))  # reorder column [y1,y2, x,...] --> [x, y1,y2,...]
      t_names <- names(d)[sapply(d, is_time.)] %>% choice.(one = T, note = 'Time factor')  # time
      x_names <- names(d)[sapply(d, is.factor)] %>%
                 choice.(note = 'Choose less than TWO X factors')  # 2 factor columns are available
      x_name2 <- map_dbl(d[c(t_names, x_names)], n_distinct) %>% which.max() %>% names()
      flag2 <- c(t_names, x_names) %>%
               .[!. == x_name2] %>% {
                 if (length(.) == 0) NULL
                 else if (length(.) == 1) .
                 else if (length(.) > 1) choice.(., one = T, chr = T, note = 'Choose FLAG factors')
               }
      y_names <- names(d)[sapply(d, is.numeric)]  # numeric

      ## color assignment for flag column, if there
      if (!is.null(flag2)) {  # keep outside color_flag() because of common color assignment
        major_factor <- table(d[[flag2]]) %>% which.max() %>% names()
        minor_factor <- unique(d[[flag2]]) %>% .[. != major_factor & !is.na(.)]
        rand <- floor(runif(length(minor_factor), min = 1, max = length(color2.())))
        minor_colors <- col_flag %||% color2.()[rand] %>% set_names(minor_factor)
      }
      color_flag <- function(basic_color) {
        if (is.null(flag2)) return(basic_color)
        all_colors <- c(set_names(basic_color, major_factor), minor_colors)
        tmp0 <- as.character(d[[flag2]]) %>% all_colors[.] %>% {dplyr::case_when(is.na(.) ~ set_names('grey85', 'unknown'), TRUE ~ .)}
        return(tmp0)
      }

      ## sd-whiskers trend; [x,y] --> [x,mean,lower,upper]
      sd_whiskers <- function(d_xy) {  # x-y trend with duplicated x made slim
        tmp0 <- if (map_dbl(d_xy, n_distinct)[names(d_xy)[!sapply(d_xy, is.numeric)]] != nrow(d)) {
                  d_xy %>%
                  group_by_if(~ !is.numeric(.)) %>%
                  summarise_all(list(
                    mean = ~ mean.(.),
                    lower = ~ dplyr::if_else(sd.(.) == 0, NA_real_, mean.(.) - sd.(.)),
                    upper = ~ dplyr::if_else(sd.(.) == 0, NA_real_, mean.(.) + sd.(.))
                  )) %>%
                  select_if(colSums(is.na(.)) != nrow(.))
                } else {
                  d_xy
                }
        return(tmp0)
      }

      ## single trend line even with several colors
      ## eg. [x(x1,x2),y1,y2, ...] --> [[seq,y1_x1],[seq,y1_x2], [[seq,y2_x1],[seq,y2_x2], ...]; 4 lines
      ## d_xyy <- tibble(flag=rep(c('old','new'),each=5), y=1:10, y2=21:30)
      single_trend <- function(d_xyy) {
        tmp0 <- d_xyy %>%
                rowid_to_column() %>%
                pivot_wider(names_from = names(d_xyy)[!sapply(d_xyy, is.numeric)], values_from = names(d_xyy)[sapply(d_xyy, is.numeric)]) %>%
                xyL.(type = 'xyy')
        return(tmp0)
      }
      
      ## all numeric: [x1,y1, x2,y2, ...], [x,y1,y2, ...], [y1,y2, ...]
      ## d <- iris[-5]
      if (is.null(x_name2)) {
        if (length(y_names) %% 2 == 0) {  # even number of columns
          # bothersome because this pattern is default
          # if (ncol(d) > 2) cat('---- A reminder: you can choice other datatype.\n     Try \'xy\', \'yy\', or \'xyy\' as datatype.\n')
          # if (ncol(d) == 2) datatype <- choice.(c('scatter: x,y', 'trend: y1,y2'), chr = F, one = T, note = 'your data type') %>% c('xy', 'yy')[.]
          # datatype <- choice.(c('x1,y1,x2,y2,...', 'x,y1,y2,...', 'y1,y2,...'), chr=F, one=T, note='your data type') %>% c('xy', 'yy', 'xyy')[.]
          datatype <- datatype
        } else {  # odd case
          datatype <- if (datatype != 'xy') choice.(c('y1,y2,...', 'x, y1,y2,...'), chr = F, one = T, note = 'your data type') %>% c('yy', 'xyy')[.]
        }
        tmp <- xyL.(d, datatype)
      }

      ## factor (character) case
      ## A1) d <- tibble(time=seq(as.POSIXct('2023-10-26'),as.POSIXct('2024-01-21'),by='1 hour'), temp=23+cumsum(rnorm(length(time),0,0.1)), humid=53+cumsum(rnorm(length(time),3,0.1))) %>% time2.(.,'day')
      ## A2) d <- diamonds %>% select(color, y)
      ## A3) d <- tibble(chr=map.(LETTERS[1:15],~str_c(.,1:10)), abc=iris[[1]], xyz=iris[[2]])
      ## A4) d <- tibble(flag=c('OK','NG',rep('OK',8)), trend=letters[1:10], y=21:30)
      ## B1) d <- tibble(flag=rep(c('old','new'),each=5), y=1:10, y2=21:30)
      ## B2) d <- iris[4:5]
      ## C1) d <- iris

      ## [(flag), x,y]
      ##  L select less than two x
      ##    L trend (whisker) type
      ##      L with/without flag
      ##    L split type
      ##      L x$[y1] --> single trend
      ##      L x$[y1,y2]
      ##      L x$[y1,y2,y3,...] --> select y1, y2
      if (!is.null(x_name2)) {
        ## avoid NAs from reduced to one NA
        ## ? [2024-06-21] mutate(!!x_name2 := make.unique2(get(x_name2)) %>% as.factor())
      # d <- d %>% mutate(!!x_name2 := make.unique2(x_name2) %>% as.factor())  # 'A1',NA,NA,'A2' --> 'A1','NA1','NA2','A2'

        if (n_distinct(d[x_name2]) > 5) {  # trend (with/without flag)
          tmp <- d %>%
                 select(-all_of(flag2)) %>%
                 xyL.(type = 'xyy') %>%
                 map(~ sd_whiskers(.))  # A1~A4) several-levels trend with/without whiskers
        } else {
          if (ncol(d) == 2) {
            tmp <- d %>% single_trend()  # B1~B2) keep single trend line with flag X
          } else {
            tmp <- d %>% select(c(all_of(x_name2), choice.(y_names, note = 'Two numeric'))) %>% split2.()
          }  # C1) divide it from a few of levels to x-y plot
        }
      }

      out <- tmp %>% tibble(legend = names(.), data = ., color = color2.(col, len = length(.)), color_flag = map(color, color_flag))
    }  # END of data.frame case

    ## other settings
    if (!is.null(sel) && is.numeric(sel) && any(sel > 0)) out <- out[sel, ]  # reorder if you want
    if (is.numeric(type)) type <- c('l', 'p', 'pp', 'b', 'bb', 'h', 's')[n_cyc.(type[1], 7)]
    out <- out %>%
           mutate(
             type = type,
             lty = if (type[1] %in% c('p', 'pp')) 0 else lty %||% 1,
             lwd = lwd %||% whichSize.(ref = nrow(out), vec = c(5, 10, 25, 50), mirror = c(1.5, 1.1, 0.8, 0.35)),
             pch_point = if (type[1] %in% c('p', 'b')) 1 else if (type[1] %in% c('pp', 'bb')) 19 else NA,
             pch_legend = if (anyNA(pch) && type[1] %in% c('p', 'pp', 'b', 'bb')) NA else pch_point,
             cex_point = map_dbl(out$data, function(x) ifelse(is.null(x), 0, nrow(x))) %>% {dplyr::case_when(. > 1000 ~ 0.8, . == 0 ~ 0, TRUE ~ 1)}
           )
    return(out)

  # nest_get(iris, sel = 2:1)
  # nest_get(iris[1:2])
  # nest_get(list(a=iris[1:2], b=iris[3:4]))
  }

  dn <- nest_get(d, sel, type, col, lty, lwd, pch)
  xlim2 <- pr.(map(dn$data, ~ .x[1] %>% range.()) %>% purrr::reduce(base::c), xlim, expand_ratio = 0.035)  # considered resolve time vector
  ylim2 <- pr.(map(dn$data, ~ .x[-1] %>% unlist() %>% range.()) %>% unlist(), ylim, expand_ratio = 0.12)  # numeric easily resoved by unlist()

  ## (add = 0) just prepare empty camvas
  ## (add = 1) normal plotting
  ## (add = 2) add lines or points only
  if (add != 2) {  # ex) plt.(d, add = 0); polygon (~); plt.(d, add = 2)
    par(xaxs = 'i', yaxs = 'i', ann = F)
    plot.new()
    plot.window(xlim = xlim2, ylim = ylim2)
    plot_frame.(xy = dn$data[[1]], grid = grid, xlim2 = xlim2, ylim2 = ylim2, tcl = par('tcl'), padj = -0.1, rot = rot, cexlab = cexlab,
                bty = 'o', xlab = xlab, ylab = ylab, yline = yline)
    if (add == 0) return(cat('\n'))
  }

  for (i in seq(nrow(dn))) {
    if (is.null(dn$data[[i]])) next
    x <- dn$data[[i]][1] %>% {if (map_lgl(., ~ is.character(.x) | is.factor(.x))) seq(nrow(.)) else unlist(.)}
    y <- dn$data[[i]][[2]]
    error_bar <- function() {
      if (ncol(dn$data[[i]]) == 2) {
        return()
      } else {
        dn$data[[i]][[1]] <- x  # alternative number for factor
        arrow_len <- ifelse(delta.(ylim2) /100 > delta.(y), 0, 0.03)  # if range of arrows is much tighter than ylim, the error message comes
        dn$data[[i]] %>%
        na.omit() %>%
        {arrows(.[[1]], .[['lower']], .[[1]], .[['upper']], angle = 90, code = 3, length = arrow_len, lwd = 0.8, col = dn$color[i])}
      }
    }

    if (dn$type[i] == 'l') {
      error_bar()
      lines(x, y, lty = dn$lty[i], col = col_tr.(dn$color[i], tr = 0.8), lwd = lwd)
    } else if (dn$type[i] %in% c('p', 'pp')) {
      points(x, y, pch = dn$pch_point[i], col = col_tr.(dn$color_flag[[i]], tr = ifelse(type == 'p', 1.3, 0.9)), lwd = dn$lwd[i] /2, cex = dn$cex_point[i])
    } else if (dn$type[i] %in% c('b', 'bb')) {
      error_bar()
      lines(x, y, lty = dn$lty[i], col = col_tr.(dn$color[i], tr = 0.8), lwd = dn$lwd[i])
      points(x, y, pch = dn$pch_point[i], col = col_tr.(dn$color_flag[[i]], tr = ifelse(type == 'b', 1.3, 0.9)), lwd = dn$lwd[i] /2, cex = dn$cex_point[i])
    } else if (dn$type[i] == 'h') {
      lines(x, y, lty = dn$lty[i], col = col_tr.(dn$color[i], tr = 0.8), lwd = dn$lwd[i] * 3, type = 'h', lend = 'butt')
    } else if (dn$type[i] == 's') {
      lines(x, y, lty = dn$lty[i], col = col_tr.(dn$color[i], tr = 0.8), lwd = dn$lwd[i], type = 's')
    }
  }
  if (nrow(dn) != 1 && !0 %in% name) {  # No legend is needed for one line at least. Or name = 0 returns no legend
    legen2.(name %||% dn$legend, legePos, dn$color, dn$lty, dn$pch_legend)
  }
  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  if (multi == FALSE) gp.()  # par(mfrow=c(2,1)) or layout(matrix(c(1,1,1,2),nrow=4,ncol=1,byrow=T)), not to use gp.()
# plt.(iris[1:2], type = 'pp')
# plt.(iris, type = 5, rot = 15)
# plt.(iris[-5], type = 'pp', legePos = c(0.03, 0.99))
# plt.(psd[2:3], ylim = c(0, NA))
# plt.(economics[-3], rot = 60)  # time vector
# plt.(tibble(chr=map.(LETTERS[1:15], ~ str_c(., 1:10)), abc=iris[[1]], xyz=iris[[2]]), rot = 60)
# plt.(us_rent_income[c(2,5)], ylim = c(0, NA), type = 'h', lwd = 3, rot = 25, ylab = 'moe (90% margin of error)')  # item-bar
# plt.(tibble(flag=c('OK','NG',rep('OK',8)), trend=letters[1:10], y=c(3,1,4,7,5,2,9,6,8,10)), type = 'bb', col_flag='red')  # event
}


## Kernel Density Estimation plot == (2024-07-17) ========================
dens. <- function(d, bw = 1, ord = F, sel = NULL, xlim = NA, ylim = NA, name = NULL, col = NULL, lty = 1, lwd = NULL, grid = T,
                  xlab = NULL, ylab = NULL, legePos = NULL, cum = F, write = F, ...) {
  ## convert a vector to kde
  query_lib.(logKDE)
  kde_xy <- function(vec) {
    vec <- vec[!is.na(vec)]
    BW <- list('nrd0',
               'Sj-ste',  # Sheather-Jones
               (4/3) ^(1/5) *sd(vec) *length(vec) ^(-1/5),  # Silverman's rule of thumb is sensitive to outliers
               # https://www.jstage.jst.go.jp/article/transinf/E102.D/1/E102.D_2018EDP7177/_pdf
               1.06 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *2 +1)),  # More robust sd
               1.08 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *4 +1)),
               1.08 *median(abs(vec -median(vec))) /0.6745 *length(vec) ^(-1 /(2 *6 +1))
          ) %>%
          .[[n_cyc.(ceiling(bw), 6)]]  # ceiling() is a hack in case of square density analysis

    ## Min adjustment
    Dens <- density(vec, na.rm = T, bw = BW, n = 1300)
    if (length(xlim) == 2 && !is.na(xlim[1])) {  # the left limit fixed like xlim = c(0, NA), c(1, NA), ...
      if (min(Dens$x) < xlim[1]) {
        mins <- vector()
        icut <- seq(1, 0.4, by = -0.005)
        for (i in seq_along(icut)) {  # Sometimes warning; 'Auto-range choice cut-off at 0'
          mins[i] <- skipMess.(logKDE::logdensity(vec, na.rm = T, bw = BW, n = 1300, adjust = icut[i])) %>%
                     .$x %>%
                     min(.)  # bw <- adjust *bw
        }
        adji <- whichNear.(ref = xlim[1], vec = mins) %>% icut[.]
        Dens <- skipMess.(logKDE::logdensity(vec, na.rm = T, bw = BW, n = 1300, adjust = adji))
      }
    } else {
      if (min(vec) >= 0 && any.(Dens$x < 0)) {
        for (i in seq(1, 0.5, by = -0.005)) {
          Dens <- density(vec, na.rm = T, bw = BW, n = 1300, adjust = i)  # bw <- adjust *bw
          if (all(Dens$x >= 0)) break
        }
      }
    }
    return(tibble(x = Dens$x, y = Dens$y))
  }  # END of kde_xy()
  
  ## transformation
  dL <- dLformer.(d, ord) %>% map(kde_xy)
  if (between(bw, 0, 1) == TRUE) {  # make the density ^ bw to highlight max channel
    bw2 <- function(x) {x[, 2] <- x[, 1] * x[, 2] ^ bw; return(x)}
    dL <- map(dL, bw2)
  }
  if (cum == TRUE) dL <- map(dL, cdf.)

  ## draw
  if (is.na(ylim[1]) == TRUE) {
    ymin_hook <- lapply(dL, '[', 2) %>% unlist() %>% delta.()
    ylim <- if (ymin_hook > 1) c(log10(1 / ymin_hook) / 3, NA) else ymin = c(0, NA)
    if (bw < 1) ylab <- ylab %||% str_c('Probability density (', round(bw, 2), ' powered moment)')
  }
  plt.(dL, sel=sel, xlim=xlim, ylim=ylim, name=name, col=col, lty=lty, lwd=lwd, grid=grid, xlab=xlab, ylab=ylab, legePos=legePos)

  ## p-th percentile
  out <- map(dL, ~ cdf.(., p = c(0.1, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.9) /100)) %>%  # percentile
         bind_rows(., map(dL, ~ mean.(.$x))) %>%  # mean
         bind_rows(., map(dL, ~ sd.(.$x))) %>%  # sd
         bind_rows(., map(dL, ~ sd.(.$x) / mean.(.$x))) %>%  # cv
         bind_cols(percentile = c(str_c('p', c(0.1, 1, 5, 10, 25, 50, 75, 90, 95, 99, 99.9)), 'mean', 'sd', 'cv'), .)  # name
  print(out)

  ## write out
  if (write == TRUE) write2.(dL)

# dens.(iris[4:5], cum = T)
# dens.(iris[4], 0.5)
}


## Cumulative ratio plot == (2024-01-10) ========================
crp. <- function(d, ord = F, sel = NULL, xlim = NA, ylim = c(-0.01, 1.05), name = NULL, col = NULL, grid = T,
                 lwd = NULL, xlab = NULL, ylab = NULL, legePos = c(0.05, 0.98), px = NULL, py = NULL, ext = F, ...) {

  dL <- dLformer.(d) %>% {if (is.atomic(.[[1]])) . else stop('Only available for [ID,y] or [y1,y2, ...]', call. = F)}
  minmax <- unlist(dL) %>% range(., na.rm = T) %>% {. +delta.(.) *c(-1, +1) *0.03}

  ## color
  col2 <- color2.(col, len = length(dL)) %>% set_names(names(dL))

  if (!exists('pLL_nu_lam')) {
    suppressMessages(devtools::source_url('https://github.com/Nyu3/psd_R/blob/master/PSD_archive.R?raw=TRUE'))
  }
  pLL <- pLL_lam_al_be_ga_de
  ## Beta Marshall-Olkin Weibull (betaMaolWei)
  z <- function(x,lam,de) 1 -exp(-(x /lam) ^de)
  h <- function(x,lam,be,de) z(x,lam,de) /(be +(1 -be) *z(x,lam,de))
  f <- function(x,lam,al,be,ga,de) incbeta(h(x,lam,be,de), al, ga)  # Fx <- ... cannot work
  ## Exponentiated generalized extended Gompertz
  # f <- function(x,lam,al,be,ga,de) (1 -(1 -(1 -exp(-lam *de *(exp(x /lam) -1))) ^al) ^be) ^ga

  quant <- function(vec) {
    vec <- {if (!is.atomic(vec)) vec[[1]] else vec} %>% .[!is.na(.)] %>% sort()# %>% unique()
    d_cum <- tibble(x = vec, y = cumsum(vec) /sum(vec))
    mdl <- lazy_call.(x = d_cum, y = NULL, pLL, f, ext = T, y1 = 0, y2 = 0)
    if (ext == TRUE) {  # extended full x range
      qx <- seq(minmax[1], minmax[2], length = 200)
      qxy <- tibble(x = qx, y = predict(mdl$model, newdata = tibble(x = qx)) %>% {dplyr::case_when(. < 0 ~ 0, . > 1 ~ 1, TRUE ~ .)})
    } else {
      qxy <- mdl$xy
    }
    return(tesL = list(d_cum, qxy, mdl$model))
  }
  dL123 <- map(dL, quant)
  dL_raw <- dL123 %>% map(~ .[[1]])
  dL_qxy <- dL123 %>% map(~ .[[2]])
  dL_res <- dL123 %>% map(~ .[[3]])
  for (i in seq_along(dL_res)) {
    cat('< N =', i, '>\n')
    cat('Model := Beta Marshall-Olkin Weibull;\n')  # Exponentiated generalized extended Gompertz
    cat('y = \n')  # (1 -exp(al *lam *(1 -exp(x /lam)))) ^be
    dL_res[[i]] %>% print(.)
    cat(str_dup('=', 38), '\n')
  }
  plt.(c(dL_raw, dL_qxy), xlim=xlim, ylim=ylim, add=0, grid=grid, xlab=xlab, ylab=ylab)
  plt.(dL_qxy, sel=sel, type='l', col=col_tr.(col2, tr = 1.0), add=2, lty=1, lwd=lwd, legePos=legePos)
  plt.(dL_raw, sel=sel, type='s', col=col_tr.(col2, tr = 0.5), add=2, lty=1, lwd=lwd, name=0)
  if (!is.null(px)) {
    out <- map(dL_qxy, function(nya) whichNear.(nya[[1]], px) %>% nya[[., 2]]) #%>% bind_rows(.)
    cat('\n    When px =', px, ',\n')
    print(out)
  }
  if (!is.null(py)) {
    out <- map(dL_qxy, function(nya) whichNear.(nya[[2]], py) %>% nya[[., 1]]) #%>% bind_rows(.)
    cat('\n    When py =', py, ',\n')
    print(out)
  }
# crp.(iris[2:3], ext = T)
}


## Histograms plot == (2024-07-17) ========================
hist. <- function(d, ord = F, bin = 'st', freq = T, xlim = NA, ylim = c(0, NA), col = NULL,
                  xlab = NULL, ylab = NULL, yline = NULL, legePos = NULL, name = NULL, plot = T, overlay = T, ...) {
  ## Cut data range by xlim
  ## Make the bin width (if vec is only integer, the ticks are positioned in the center of each bar)
  whatBreak <- function(vec) {
    out <- if (bin %in% c('St', 'st', 'Sturges') || !is.numeric(bin) && all(abs(vec) <= 1)) {
             'Sturges'
           } else if (bin %in% c('Sc', 'sc', 'Scott')) {
             'Scott'
           } else if (all(vec %% 1 == 0, na.rm = T) && max(vec, na.rm = T) < 30) {
             (min(vec, na.rm = T) -0.5) : (max(vec, na.rm = T) +0.5)  # dL <- floor(runif(100, 0, 30))
           } else {
             seq(floor(min(vec, na.rm = T)) -bin, ceiling(max(vec, na.rm = T)) +bin, by = bin)  # error if the interval is small than the range
           }
    return(out)
  }
  dL <- dLformer.(d, ord) %>% map(~ .[!is.na(.)])  # list of vectors, not xy.
  ## NOTE: Screening the range you wanna observe        
  if (length(xlim) > 1 && anyNA(xlim)) {  # When xlim = c(0, NA)
    dL <- map(dL, ~ . %>% {if (is.na(xlim [1])) .[. <= xlim[2]] else .[. >= xlim[1]]})
  } else if (length(xlim) > 1 && !anyNA(xlim)) {  # When xlim = c(0, 100)
    dL <- map(dL, ~ .[. >= xlim[1] & . <= xlim[2]])
  }

  if (plot == FALSE) {
    for (i in seq_along(dL)) {
      hist_data <- hist(dL[[i]], breaks = whatBreak(dL[[i]]), plot = FALSE)
      if (freq == TRUE) {
        tmp <- hist_data %>% {tibble(x = .$mids, y = .$counts)} %>% list(.)
      } else {
        tmp <- hist_data %>% {tibble(x = .$mids, y = .$density)} %>% list(.)
      }
      ten <- if (i == 1) tmp else c(ten, tenta)
      if (i == length(dL) && length(dL) == 1) return(ten[[1]])
      if (i == length(dL) && length(dL) > 1) return(ten)
    }
  }

  color <- color2.(col, len = length(dL))
  xlim2 <- pr.(dL, xlim, 0.02)
  tmp <- vector()
  for (i in seq_along(dL)) {
    tmp[i] <- dL[[i]] %>% {hist(., breaks = whatBreak(.), plot = F)} %>% {if (freq) .$counts else .$density} %>% max(.)
    if (i == length(dL)) Ymax <- max.(tmp)
  }
  ylim2 <- pr.(ifelse(is.na(ylim[2]), Ymax, ylim[2]), ylim, 0.08)
  ylab <- if (freq == TRUE) ylab %||% 'Frequency' else if (!freq) ylab %||% 'Density'

  par(xaxs = 'i', yaxs = 'i', mgp = c(0, 0.4, 0), ann = F)
  if (overlay == FALSE) {
    for (i in seq_along(dL)) {
      hist(dL[[i]], ann = F, axes = F, freq = freq, xlim = xlim2, ylim = ylim2, col = col_tr.(color[i], 0.75), breaks = whatBreak(dL[[i]]))
      plot_frame.(xlim2=xlim2, ylim2=ylim2, tcl=-par('tcl'), padj=-0.2, bty='l', xlab=xlab %||% name %||% names(dL)[i], ylab=ylab, yline=yline)
    }
  } else if (overlay == TRUE) {
    for (i in seq_along(dL)) {
      hist(dL[[i]], ann = F, axes = F, freq = freq, xlim = xlim2, ylim = ylim2, col = col_tr.(color[i], 0.75), breaks = whatBreak(dL[[i]]))
      par(new = if (length(dL) == 1) F else (if (i != length(dL)) T else F))
    }
    plot_frame.(xlim2=xlim2, ylim2=ylim2, tcl=-par('tcl'), padj=-0.2, bty='l', xlab=xlab, ylab=ylab, yline=yline)

    if ((length(dL) != 1 || !is.null(name)) && !0 %in% name) {  # No legend is needed for one line at least. Or name = 0 returns no legend
      name <- name %||% names(dL) %||% str_c('#', seq_along(dL))  # Auto assignment
      legen2.(name, legePos, col_tr.(color, 0.75))
    }
  }
  gp.()
  if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
# hist.(iris[2:3], col = c('slateblue', 'coral2'), bin = 0.1, name = c('A', 'B'), overlay = T)
}


## Pie chart for ratio == (2024-01-10) ========================
pie. <- function(d, col = NULL, cex = 0.85, percent = F, digit = 1, cent_name = NULL,  ...) {
  if (is.data.frame(d)) {  # Only use categorical data
    tab_cols <- map_lgl(d, ~ is.character(.) | is.factor(.))
    num_cols <- map_lgl(d, ~ is.numeric(.))
    if (sum(tab_cols) == 1 && sum(num_cols) == 0) {  # [ID]
      vec <- d[tab_cols] %>% table %>% sort(., decreasing = T)
    } else if (sum(tab_cols) == 1 && sum(num_cols) == 1) { # [ID, y]
      vec <- d %>% group_by_if(tab_cols) %>%
             summarise(sum = sum(get(names(d)[num_cols]))) %>%
             pull(sum, get(names(d)[tab_cols]))
    } else {
      stop('The data must include categorical data...\n\n', call. = F)
    }
  } else {
    stop('The data must include categorical vector or data.frame...\n\n', call. = F)
  }

  ## plot
  par(xaxs = 'i', yaxs = 'i', mar = c(1, 2, 1, 2), ann = F)
  color <- color2.(col, len = length(vec)) %>% col_tr.(tr = 0.8)
  vals <- if (percent == TRUE) {vec /sum(vec) *100} %>% round(., digit) else as.numeric(vec)
  names(vec) <- names(vec) %>% str_c(., '\n(', vals, ifelse(percent, '%)\n', ')\n'))
  pie(vec, col = color, border = 'grey13', clockwise = T, init.angle = 90, radius = 0.8, cex = cex)
  par(new = T)
  pie(vec, label = '', col = 'white', border = 'white', radius = 0.5)
  Theta <- seq(- pi, pi, length = 350)
  lines(0.5 *cos(Theta), 0.5 *sin(Theta), col = 'grey13')
  cent_name <- cent_name %||% names(d)[tab_cols]
  text(0, 0, labels = str_c(cent_name, '\n(', sum(vec), ')'), family = jL.(cent_name))
  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()
# pie.(iris[5])
# pie.(iris[41:120,5], percent = T)
}


## Linear correlation plot == (2023-10-27) ========================
## NOTE.1  Regression analysis is strictly applicable to cause (x) and effect (y; random variable) on the assumption that x has NO error...
## NOTE.2  Correlation analysis sees BOTH x & y as random variables; thus don't use linear regression and prob. ellipse at the same time...
## Trivia.1  You'll see the cross points on the line and ellipse can draw y-axis parallel lines as tangent
## Trivia.2  The longer axis of the ellipse is corresponding to the axis of principal component analysis
corp. <- function(d, xlim = NULL, ylim = NULL, xlab = NULL, ylab = NULL, col = 4, legePos = NULL, x_lr = NULL, li = F, el = T, fix = F,
                  yline = NULL, lege = T, PDF = T, ...) {
  ## owful settings...
  query_lib.(ellipse, robustbase)
  d <- list2tibble.(d) %>% select_if(is.numeric) %>% clean2.(na0 = F) %>% .[complete.cases(.), ]  # Omit the row including any NA
  if (nrow(d) == 0) stop('No available data...\n\n', call. = F)
  if (!ncol(d) %in% c(2, 3)) {
    d <- choice.(names(d), note = '[x,y] or [x,y,label] data', chr = F) %>% d[.]
  }
  def.(c('x', 'y'), list(d[[1]], d[[2]]))

  if (is.character(col)) stop('Don\'t use color name like \'blue\'.  Use numbers 1 to 6\n\n', call. = F)
  colpal <- n_cyc.(col, 6, len_max = 1) %>% c('Greys', 'Blues', 'Oranges', 'Purples', 'Reds', 'Greens')[.]

  ## elliplot::ellipseplot(iris[c(5, 1)], iris[c(5, 2)])
  mdl0 <- robustbase::lmrob(y ~x -1, na.action = na.exclude, setting = 'KS2014')  # robust::lmRob(y ~ x -1, na.action = na.exclude)
  mdl1 <- robustbase::lmrob(y ~x +1, na.action = na.exclude, setting = 'KS2014')  # robust::lmRob(y ~ x +1, na.action = na.exclude)
  mdlNum <- map_dbl(list(mdl0, mdl1), ~ summary(.) %>% .$sigma) %>% which.min()
  mdl <- list(mdl0, mdl1)[[mdlNum]]  # Choose better
  Coef <- list(c(0, coef(mdl0)), coef(mdl1))[[mdlNum]] %>% set_names(NULL)
  # Cor <- robust::covRob(d, corr = T)$cov[1, 2]  # No robust, including outliers
  # Cnt <- robust::covRob(d, corr = T)$center  # If Coef[2] ~ +/-0.01 and shows strong Cor, don't care because it's 1to1 relationship
  # Cor <- if (nrow(d) > 13) robustbase::covMcd(d, cor = T)$cor[1, 2] else cor(d)[1, 2]  # covMcd results are different for small data
  # Cor <- skipMess.(minerva::mine(d[[1]], d[[2]])$MIC)  # Non-linear (Maximum Information Coefficient)
  # Cor <- skipMess.(minerva::mine(d[[1]], d[[2]], normalization = T)$TIC)  # (Total Information Coefficient)
  Cor <- cor.test(d[[1]], d[[2]], method = 'spearman', exact = F)$estimate  # spearman > pearson > kendall

  Cnt <- try(robustbase::covMcd(d, cor = T, alpha = 0.75)$center, silent = T)
  if ('try-error' %in% class(Cnt)) Cnt <- c(mean.(x), Coef[1] +Coef[2] *mean.(x))

  ## Legend position 1, 2, 3, 4 as quadrant
  text_pos <- function(...) {
    xyPos <- {Cnt -c(mean(par('usr')[1:2]), mean(par('usr')[3:4]))} %>% {. >= c(0, 0)}
    def.(c('xpos', 'ypos', 'yScMin', 'yScMax'), list(xyPos[1], xyPos[2], par('usr')[3], par('usr')[4]))
    cnt_pos <- if (xpos && ypos) 1 else if (!xpos && ypos) 2 else if (!xpos && !ypos) 3 else if (xpos && !ypos) 4
    out <- list(
             if (Coef[2] >= 0 && Coef[1] < yScMin) 2 else if (Coef[2] >= 0 && Coef[1] >= yScMin) 4 else if (Coef[2] < 0) 3,
             if (Coef[2] >= 0) 4 else if (Coef[2] < 0 && Coef[1] < yScMax) 1 else if (Coef[2] < 0 && Coef[1] >= yScMax) 3,
             if (Coef[2] >= 0 && Coef[1] < yScMin) 2 else if (Coef[2] >= 0 && Coef[1] >= yScMin) 4 else if (Coef[2] < 0) 1,
             if (Coef[2] >= 0) 2 else if (Coef[2] < 0 && Coef[1] < yScMax) 1 else if (Coef[2] < 0 && Coef[1] >= yScMax) 3
           )[[cnt_pos]]
    return(out)
  }
  ## Legend equation
  text3 <- function(text_pos) {
    Show <- function(x) ifelse(any.(Cnt > 10), 1, 2) %>% {sprintf(str_c('%.', ., 'f'), x)}
    Text1 <- bquote('(' *bar(italic(x)) *',' ~bar(italic(y)) *')' == '(' *.(Show(mean.(x))) *','  ~.(Show(mean.(y))) *')')
    Text2 <- Coef[1] %>% {c(. > 0, . < 0, . == 0)} %>% which() %>%
             list(
               bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) + .(sprintf('%.2f', Coef[1])) *phantom(')')),
               bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) ~ .(sprintf('%.2f', Coef[1])) *phantom(')')),
               bquote(hat(italic(y)) == .(sprintf('%.2f', Coef[2])) *italic(x) *phantom(')'))
             )[[.]]
    Text3 <- bquote(italic(R)[adj] ^2 == .(sprintf('%.2f', summary(mdl)$adj.r.squared)))
    State <- function(x) if (abs(x) < 0.25) 'none' else if (abs(x) < 0.50) 'weak' else if (abs(x) < 0.75) 'moderate' else 'strong'
    Text4 <- bquote(italic(r)[Spearman] == .(sprintf('%.2f', Cor)) ~ (.(State(Cor))))  # No 'Perfect' due to robust cut
    Text5 <- if (cor.test(x, y, method = 'spearman', exact = F)$'p.value' > 0.004) {
               bquote('p-value' == .(sprintf('%.2f', cor.test(x, y)$p.value)))
             } else {
               bquote('p-value ~ 0.00')
             }
    ## Set proper text postion
    lege4 <- list(c(0.98, 1.00), c(0.05, 1.00), c(0.05, 0.35), c(0.98, 0.35))
    if (is.null(legePos)) legePos <- lege4[[text_pos]]
    ## Note: no co-existence
    text_num <- if (li && el) 1:3 else if (li && !el) 1:3 else if (!li && el) c(1, 4, 5) else if (!li && !el) 1
    for (i in seq_along(text_num)) {
      text(legeX.(legePos[1]), legeY.(legePos[2] -0.08 *i), adj = ifelse(legePos[1] < 0.5, 0, 1), cex = 0.8, col = '#22222295',
           label = list(Text1, Text2, Text3, Text4, Text5)[[text_num[i]]]
         # family = ifelse(Sys.getenv('OS') == '', 'CenturySchoolbook', 'Yu Mincho Light')
      )
    }
  }  # END of text3()

  ## Drawing
  xlim2 <- if (!is.null(xlim)) {
               pr.(x, xlim, 0.13)
           } else {
             if (between(min.(x) /min.(y), 0.9, 1.1) & between(max.(x) /max.(y), 0.9, 1.1) ) {  # When x&y differ slightly; keep the same scale
               pr.(c(x, y), NA, 0.13)
             } else {
               pr.(x, NA, 0.13)  # When x & y differ largely; change proper scale to see easily
             }
           }
  ylim2 <- if (!is.null(ylim)) {
               pr.(y, ylim, 0.13)
           } else {
             if (between(min.(x) /min.(y), 0.9, 1.1) & between(max.(x) /max.(y), 0.9, 1.1) ) {  # When x&y differ slightly; keep the same scale
               pr.(c(x, y), NA, 0.13)
             } else {
               pr.(y, NA, 0.13)
             }
           }

  if (fix == TRUE) {  # Wnen the same scale on both x and y-axis is desired
    def.(c('xlim2', 'ylim2'), list(range(xlim2, ylim2), range(xlim2, ylim2)))
  }

  ## Linear fitting
  draw_linearFit <- function(...) {
    qx <- seq(xlim2[1], xlim2[2], length.out = 100)
    qfit <- predict(mdl, se.fit = T, newdata = tibble(x = qx))$fit
    qse <- predict(mdl, se.fit = T, newdata = tibble(x = qx))$se
    ucl <- qfit +1.96 *qse  # 90, 95, 99% CI: 1.65, 1.96, 2.58
    lcl <- qfit -1.96 *qse
    polygonX <- sort(qx) %>% {c(., rev(.))}
    polygonY <- order(qx) %>% {c(ucl[.], lcl[rev(.)])}
    polygon(polygonX, polygonY, border = NA, col = col_tr.('grey95', tr = 0.8))
    abline(mdl, col = '#22222222', lwd = 3.5)  # KS2014 line
    if (!is.null(x_lr) & length(x_lr) == 2) {  # y-range of 95% CI corresponding to your interested x-range
      whichNear.(qx, x_lr) %>% {c(min(lcl[.]), max(ucl[.]))} %>% set_names(c('(Min.95%CI)', '(Max.95%CI)')) %>% print()
    }
  }
  ## http://friendly.github.io/heplots/reference/covEllipses.html
  ## heplots::covEllipses(d, col=col_tr.('grey35', 0.8), lwd=1, level=0.95, labels='', center.pch='', method='mcd', add=T)  # 'mve'
  draw_ellipse <- function(...) {  # Minimum Covariance Determinant (MCD)
    tmp <- try(robustbase::covMcd(d, alpha = 0.75)$cov, silent = T)
    elli95 <- {if (nrow(d) > 13 && !'try-error' %in% class(tmp)) tmp else cov(d)} %>%
              ellipse::ellipse(., centre = Cnt, level = 0.95, npoints = 200) %>% as_tibble()
    lines(elli95, col = col_tr.('black', 0.35), lwd = 1.0)
    if (!is.null(x_lr) && length(x_lr) == 2) {  # y-range of 95% CI corresponding to your interested x-range
      el1 <- elli95[1:100, ]
      el2 <- elli95[101:200, ]
      el1_range <- whichNear.(el1[[1]], x_lr) %>% el1[., 2] %>% range()
      el2_range <- whichNear.(el2[[1]], x_lr) %>% el2[., 2] %>% range()
      range(el1_range, el2_range) %>% set_names(c('(Min.95%CI)', '(Max.95%CI)')) %>% print()
    }
  }
  ## plot
  par(xaxs = 'i', yaxs = 'i', mgp = c(0, 0.4, 0), ann = F)
  plot.new()
  plot.window(xlim = xlim2, ylim = ylim2)
  color <- if (nrow(d) >= 20) {
    densCols(x, y, colramp = colorRampPalette(c('grey90', RColorBrewer::brewer.pal(9, colpal))))
  } else {  # Sigle coloring for small data
    str_sub(colpal, end = -2) %>% tolower() %>% col_tr.(., 0.5)  # Remove the last 's' like 'Greys' --> 'Grey' --> 'grey'
  }
  if (li == TRUE) draw_linearFit()
#  if (li == FALSE && el == TRUE) draw_ellipse()  # Modified ecllipse
  if (el == TRUE) draw_ellipse()  # Modified ecllipse
  if (nrow(d) >= 20) {
    points(x, y, pch = 19, lwd = 0.95, cex = 1.3, col = color)
  } else {
    points(x, y, pch = 21, lwd = 1.1, cex = 1.2, bg = color); points(x, y, pch = 21, lwd = 0.7, cex = 0.75)
  }
  plot_frame.(xlim2=xlim2, ylim2=ylim2, tcl=-par('tcl'), padj=-0.2, bty='l', xlab=xlab, ylab=ylab, yline=yline)
#  if (lege == TRUE) {
#    textP <- text_pos()
#    text3(textP)
#  }
  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()  # Get back to the default fear of using mar
# corp.(iris[3:4])
# corp.(iris[c(1, 4)], x_lr = c(5, 7))
}


## Clustering by probability ellipse == (2024-09-11) ========================
## [x,y,ID] preferable
ellip. <- function(d, trim = c(0, 1), sel = NULL, xlim = NA, ylim = NA, el = T, name = NULL, col = 1:6, xlab = NULL, ylab = NULL, yline = NULL, legePos = NULL, fix = F, PDF = T, ...) {
  query_lib.(ellipse, robustbase)
  ## data nesting
  d <- list2tibble.(d) %>%
       clean2.(na0 = F) %>%
       split2.(nest = T) %>%
       mutate(data = map(data, ~ trim.(., cuts = trim))) %>%
       dplyr::filter(map_dbl(.$data, nrow) > 0)

  if (map_lgl(d, ~ is.list(.)) %>% any() %>% `!`) stop('Use some data like [x,y,ID] ...\n\n', call. = F)

  ## data alignment
  num_cols <- select_if(d, is.list) %>% unnest(data) %>% ncol()
  if (num_cols == 1) d <- d %>% mutate(data = map(data, ~ rowid_to_column(., var = 'ID')))
  if (num_cols > 2) cat('CAUTION: only 1st & 2nd numeric data are used.\n')
  if (!is.null(sel)) d <- n_cyc.(sel, n_max = nrow(d), len_max = nrow(d)) %>% d[., ]

  ## color
  if (is.character(col)) stop('Don\'t use color name like \'blue\'.  Use numbers 1 to 6\n\n', call. = F)
  d <- d %>% mutate(colpal = n_cyc.(col, 6, len_max = nrow(d)) %>%
       c('Blues', 'Greys', 'Oranges', 'Reds', 'Purples', 'Greens')[.])  # for brewer.pal()
  
  ## labels
  xylabs <- select(d, data) %>% {.[[1]][[1]]} %>% names()
  xlab <- xlab %||% xylabs[1]
  ylab <- ylab %||% xylabs[2]
  name <- name %||% d[[1]]
  
  ## get ellipse info
  make_elli <- function(xy) {  # Minimum Covariance Determinant (MCD)
    if (nrow(xy) < 4) return(tibble(x = NA_real_, y = NA_real_) %>% set_names(names(xy)))
    def.(c('x', 'y'), list(xy[[1]], xy[[2]]))
    Cnt <- try(robustbase::covMcd(xy, cor = T, alpha = 0.75)$center, silent = T)
    tmp <- try(robustbase::covMcd(xy, alpha = 0.75)$cov, silent = T)
    elli95 <- {if (nrow(xy) > 13 && !'try-error' %in% class(tmp)) tmp else cov(xy)} %>%
              ellipse::ellipse(., centre = Cnt, level = 0.90, npoints = 200) %>%
              as_tibble()
    return(elli95)
  }
  d <- skipMess.(d %>% mutate(ellipse = map(data, ~ make_elli(.))))
  
  ## plot range
  if (el == TRUE) {
    tmp <- d %>% select(ellipse) %>% unnest(everything())  #  ellipse > data range
  } else {
    tmp <- d %>% select(data) %>% unnest(everything())
  }
  def.(c('x0', 'y0'), list(tmp[[1]], tmp[[2]]))
  if (between(min.(x0) /min.(y0), 0.9, 1.1) & between(max.(x0) /max.(y0), 0.9, 1.1) ) {  # When x & y differ slightly; keep the same scale
    def.(c('xlim2', 'ylim2'), list(pr.(c(x0, y0), xlim, 0.13), pr.(c(x0, y0), ylim, 0.13)))
  } else {  # When x & y differ largely; change proper scale to see easily
    def.(c('xlim2', 'ylim2'), list(pr.(x0, xlim, 0.13), pr.(y0, ylim, 0.13)))
  }
  if (fix == TRUE) {  # Wnen the same scale on both x and y-axis is desired
    def.(c('xlim2', 'ylim2'), list(range(xlim2, ylim2), range(xlim2, ylim2)))
  }
  
  ## plot
  par(xaxs = 'i', yaxs = 'i', mgp = c(0, 0.4, 0), ann = F)
  plot.new()
  plot.window(xlim = xlim2, ylim = ylim2)
  for (i in seq(nrow(d))) {
    xy <- d %>% select(data) %>% .[[1]] %>% .[[i]]
    if (el == TRUE) {
      color <- if (nrow(xy) == 1) {
                 RColorBrewer::brewer.pal(7, d$colpal[i])[1]
               } else {
                 densCols(xy, colramp = colorRampPalette(c(RColorBrewer::brewer.pal(7, d$colpal[i]))))
               }
    } else {
      color <- str_sub(d$colpal, end = -2)[i] %>% col_tr.(., 0.5)
    }
    points(xy, pch = 19, lwd = 0.95, cex = 1.3, col = color)
  }
  if (el == TRUE) {
    for (i in seq(nrow(d))) {
      els <- d %>% select(ellipse) %>% .[[1]] %>% .[[i]]
      polygon.(els, col = str_sub(d$colpal[i], end = -2) %>% tolower() %>% col_tr.(., 0.35))
    }
  }
  plot_frame.(xlim2=xlim2, ylim2=ylim2, tcl=-par('tcl'), padj=-0.2, bty='l', xlab=xlab, ylab=ylab, yline=yline)
  if (nrow(d) > 1) {
    legen2.(name = name, legePos = legePos, col = str_sub(d$colpal, end = -2) %>% tolower() %>% col_tr.(., 0.7), lty = 0, cex = 0.85)
  }
  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()  # Get back to the default fear of using mar
# ellip.(iris)
# ellip.(iris[4:5])
# ellip.(iris[3:5])
}


## Transformed a tibble for boxplot2. == (2023-10-16) ========================
box2nest. <- function(d, time_div = NULL, ...) {
  ## select data
  if (is.atomic(d)) d <- tibble(x = d)
  d <- list2tibble.(d) %>%
       time2.(div = time_div) %>%
       mutate_if(is.character, as.factor)
  tab_col <- d %>% select_if(is.factor) %>% names()
  num_col <- d %>% select_if(is.numeric) %>% names()  # columns length in the graph
  d <- d %>% select(!!tab_col, !!num_col)
  if(length(num_col) == 0) stop('The data does NOT include any numeric data...\n\n', call. = F)


  ## make a nested tibble for the combination matrix
  ## [y1,y2, ...]
  if (length(tab_col) == 0) {
    out <- tibble(row_title = NA_character_, tab = NA_character_, factors = NA_character_,
                  value = tibble(level = names(d), value = as.list(d)) %>% list())
  }

  ## [ID, y1,y2, ...]
  if (length(tab_col) == 1) {
    for (i in seq_along(num_col)) {
      tmp <- d %>%
             select(all_of(tab_col), all_of(num_col[i])) %>%
             group_nest(!!tab_col := get(tab_col)) %>%
             mutate(!!num_col[i] := map(data, ~ pull(.))) %>%
             select(!data) %>%
             list()
      out <- if (i == 1) {
               tibble(row_title = NA_character_, tab = NA_character_, factors = NA_character_, !!num_col[i] := tmp)
             } else {
               tibble(!!num_col[i] := tmp) %>% bind_cols(out, .)
             }
    }
  }

  ## [ID1,ID2, ... , y1,y2, ...]
  if (length(tab_col) > 1) {
    ## combination of levels (see in the row direction)
    ## tab_col = c('A','B','C') --> comb = matrix(c('A','B','C'), c('B','A','C'), c('C','A','B')) %>% as_tibble()
    for (i in seq_along(tab_col)) {
      if (i == 1) comb <- skipMess.(as_tibble(t(tab_col[unique(c(i, seq_along(tab_col)))]), .name_repair = 'unique'))
      if (i > 1)  comb <- skipMess.(as_tibble(t(tab_col[unique(c(i, seq_along(tab_col)))]), .name_repair = 'unique')) %>% bind_rows(comb, .)
      if (i == length(tab_col)) comb <- make.unique(rep('tab', length(tab_col)), sep = '') %>% set_names(comb, .)
    }

    ## tmp1 has tab's factors & other levels for only y1: [tab, factors, level1 for y1, level2 for y1, level3 for y1, ...]
    tmp1 <- map(d[tab_col] %>% c(), ~ unique(.) %>% as.character(.) %>% tibble(factors = .)) %>%
            map2(., 1:nrow(comb), ~ bind_cols(.x, comb[rep(.y, nrow(.x)), ]) %>% relocate(tab)) %>%
            bind_rows()

    ## increased levels for y2, y3, ... [tab, factors,  lv1(y1),lv2(y1),lv3(y1),  lv1(y2),lv2(y2),lv3(y2), ...]
    num_col2 <- rep(num_col, each = length(tab_col) - 1) %>% make.unique2(., sep = '')

    if (length(num_col) > 1) {  # [ID1,ID2,..., y1,y2,...]
      for (i in 1:(length(num_col) - 1)) {
        if (i == 1) tmp2 <- skipMess.(bind_cols(tmp1, select(tmp1, matches('[0-9]'))))  # pick up columns of tab2, tab3, ... 
        if (i > 1)  tmp2 <- skipMess.(bind_cols(tmp2, select(tmp1, matches('[0-9]'))))  # repeat num_col by the number of tab_col
        if (i == length(num_col) - 1) names(tmp2) <- c('tab', 'factors', num_col2)
      }
    } else if (length(num_col) == 1) {  # [ID1,ID2,...,y]
        tmp2 <- tmp1 %>% set_names(c('tab', 'factors', num_col2))
    }

    ## make dummy tmp2 named tmp3 which has list type, and input boxplot(fac~y) data as list
    tmp3 <- tmp2 %>% mutate_at(3:ncol(tmp2), as.list)
    for (i in 1:nrow(tmp2)) for (j in 3:ncol(tmp2)) {
      tab_ij <- tmp2[[i, j]]
      num_j <- gsub('\\d+$', '', names(tmp2)[j])
      tmp3[[i, j]] <- d %>%
                      dplyr::filter(get(tmp2[[i, 'tab']]) == tmp2[[i, 'factors']]) %>%
                      select(all_of(tab_ij), all_of(num_j)) %>%
                      group_nest(!!tab_ij := get(tab_ij)) %>%
                      mutate(!!names(tmp2)[j] := map(data, ~ pull(.))) %>%
                      select(!data) %>%
                      list()
    }
    out <- tmp3 %>% mutate(row_title = str_c('(', tab, ')\n', factors)) %>% relocate(row_title)
  }
  return(out)
# diamonds[4:5] %>% box2nest.       # [ID,y]
# diamonds[c(1,3,5)] %>% box2nest.  # [ID,y1,y2]
# diamonds[1:3] %>% box2nest.       # [ID1,ID2, y]
# diamonds[c(1:3,5)] %>% box2nest.  # [ID1,ID2, y1,y2]
# diamonds[1:5] %>% box2nest.       # [ID1,ID2,ID3, y1,y2]
}


## base function for box2.() == (2023-09-13) ========================
box1plot. <- function(yL, type = 'half', col, xlab = NULL, ylab = NULL, ylim = NA,
                      wid = 0.65, jit  = T, val = T,  n = F, rot = 0, cex_xname = 0.8, digit = NULL, ...) {
  ## yL <- c(iris[4:5])
  ## Scale of half or full boxplot
  if (type == 'full' || type == 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(0, wid *0.6, wid, wid))
  if (type != 'full' && type != 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(wid /2, wid /2, wid, 0))

  ## plot & color data
  xPos <- 2 *seq(length(yL)) -1  # NA is not omitted yet
  bgcolorL <- map2(as.list(col), yL, ~ dplyr::if_else(.x == '#FFFFFF00', 'grey13', .x) %>% rep(times = length(.y)))

  ## fivenum() is agreed with quantile() if vec is odd, but if even, fivenum() is a bit wider than quantile()
  ## Moreover, some cases make wrong whiskers that have no points more or less than 95th or 5th by quantile()
  c1 <- map_dbl(yL, ~ pmax(fivenum(.x)[2] -1.5 *IQR(.x, na.rm = T), min(.x)))  # fivenum() & min() don't need na.rm=T
  c2 <- map_dbl(yL, ~ fivenum(.)[2])
  c3 <- map_dbl(yL, ~ fivenum(.)[3])
  c4 <- map_dbl(yL, ~ fivenum(.)[4])
  c5 <- map_dbl(yL, ~ pmin(fivenum(.x)[4] +1.5 *IQR(.x, na.rm = T), max(.x)))

  ## for loop is needed because outlier isn't always just one ...
  points_outliers <- function(...) {
    CX <- map_dbl(yL, n_distinct) %>% max() %>% whichSize.(ref = ., vec = c(500, 100, 13, 4), c(0.2, 0.35, 0.7, 0.8))
    for (i in seq_along(yL)) {
      d_strip <- tibble(x = xPos[i], y = yL[[i]]) %>%
                 mutate(pch = dplyr::case_when(y >= c1[i] & y <= c5[i] ~ 21, TRUE ~ 4),
                        col = dplyr::case_when(pch == 21 ~ col_tr.(bgcolorL[[i]], 0.55), TRUE ~ col_tr.('grey13', 0.8)),
                        bg = bgcolorL[[i]]
                 ) %>%
                 dplyr::filter(!is.na(y))

      if (jit == TRUE) {
        for (ipch in c(21, 4)) {
          d_strip %>%
          dplyr::filter(pch == ipch) %>%
          {stripchart(.$y, at = .$x[1] +AT, vertical = T, method = 'jitter', jitter = jitW, add = T, lwd = 0.35, cex = CX,
                     col = .$col, bg = .$bg, pch = ipch)}
        }
      } else {
        Yout <- d_strip %>% dplyr::filter(y < c1[i] | y > c5[i])
        if (nrow(Yout) != 0) points(Yout$x, Yout$y, lwd = 0.3, cex = CX)
      }
    }
  }
  ## Scale of half or full boxplot
  box_whiskers <- function(...) {
    col_grey <- col_tr.('grey13', 0.95)
        rect(xPos -leftW,   c2, xPos +rightW,   c4, border = 0, col = col_tr.(col, 0.55))
    segments(xPos -leftW,   c3, xPos +rightW,   c3, col = col_grey, lwd = 3, lend = 'butt')
        rect(xPos -leftW,   c2, xPos +rightW,   c4, border = col_grey)
    segments(xPos -leftW/2, c1, xPos +rightW/2, c1, col = col_grey)
    segments(xPos -leftW/2, c5, xPos +rightW/2, c5, col = col_grey)
    segments(xPos,          c1, xPos,           c2, col = col_grey, lty = 'dashed')
    segments(xPos,          c4, xPos,           c5, col = col_grey, lty = 'dashed')
  }
  ## Show values
  textFun <- function(...) {
    Digit <- if (!is.null(digit) && !is.integer(digit)) digit else {
               if (delta.(unlist(yL)) < 1 && all(na.omit(median.(yL) < 1))) 3 else {
                 whichSize.(ref = delta.(unlist(yL)), vec = c(50, 5, 1), mirror = c(0, 1, 2))
               }
             }
    tCex <- whichSize.(ref = length(yL), vec = c(4, 13, 30), mirror = c(0.7, 0.6, 0.5))
    dD <- map(yL, quantile, probs = c(0, 0.5, 1), na.rm = T) %>% data.frame()
    for (i in 1:ncol(dD)) {
      if (!is.na(dD[1, i]) && round(dD[2, i] -dD[1, i], Digit +1) < 0.7 *10 ^(-Digit)) dD[1, i] <- NA
      if (!is.na(dD[3, i]) && round(dD[3, i] -dD[2, i], Digit +1) < 0.7 *10 ^(-Digit)) dD[3, i] <- NA
    }
    # haloText.(xPos+AT, dD[2,], labels=sprintf(str_c('%.',Digit,'f'), dD[2,]), cex=tCex*1.5)  # too slow ...
    text(xPos+AT, dD[2,], labels=sprintf(str_c('%.',Digit,'f'), dD[2,]), col ='white', cex=tCex*1.5*1.05, adj=dplyr::if_else(AT!=0,0.1,0.5))  # alternative haloText.
    text(xPos+AT, dD[2,], labels=sprintf(str_c('%.',Digit,'f'), dD[2,]), col ='grey13', cex=tCex*1.5*0.95, adj=dplyr::if_else(AT!= 0,0.1,0.5))
    text(xPos+AT, dD[1,], labels=sprintf(str_c('%.',Digit,'f'), dD[1,]), col ='grey70', cex=tCex, adj =c(0.5, 1.8))
    text(xPos+AT, dD[3,], labels=sprintf(str_c('%.',Digit,'f'), dD[3,]), col ='grey70', cex=tCex, adj =c(0.5, -1.0))
  }  # END of textFun()

  ## base plot
  par(xaxs = 'i', yaxs = 'i', mgp = c(0, 0.4, 0), ann = F)
  if (length(yL) > 30) par(mar = c(4, 3.3, 0.1, 1.0))
  xlim2 <- c(-1, 2 *length(yL) +1) +wid *c(1, -1)
  ylim2 <- pr.(yL, ylim, 0.07)  # NOTE: text of Max or Min is not shown by 0.05
  xlab <- xlab %||% ''
  ylab <- ylab %||% ''
  plot.new()
  plot.window(xlim = xlim2, ylim = ylim2)
  abline(v = par('usr')[1], h = par('usr')[3], col = 'grey13', lwd = 2 *par('lwd'))  # needed twice normal lwd
  axis(1, at = xPos, labels = F, lwd.ticks = par('lwd'), tcl = -par('tcl'), cex.axis = 1, lend = 'butt')
  walk(1:2, ~ axis(2, at =axisFun.(ylim2, n=6)[[.]], labels =(.==1), lwd.ticks =par('lwd'), tcl =-par('tcl')/.,
                   cex.axis=ifelse(yPos.(ylim2)>0.9,1,0.9), lend ='butt')
  )
  mtext(xlab, side = 1, las = 1, cex = whichSize.(ref = nchar(xlab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(xlab),
        line = par('mar')[1] -1.00)
  mtext(ylab, side = 2, las = 3, cex = whichSize.(ref = nchar(ylab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(ylab),
        line = par('mar')[2] -yPos.(ylim2))

  ## x-label
  xname <- names(yL) %||% str_c('#', seq_along(yL)) %>%
           if (n == F) . else str_c(., '\n(N=', map_dbl(yL, length),')') %>%
           gsub('\\n', '\n', ., fixed = T) %>%
           correctChr.()
  if (length(xname) < length(xPos)) xname <- c(xname, rep(NA_character_, times = length(xPos) -length(xname)))
  if (rot == 0) {
    rot0_line <- map.(str_count(xname, '\n'), ~ whichSize.(ref = ., vec = 0:2, c(0.35, 1.2, 1.3))) %>%
                 {. *whichSize.(ref = length(yL), vec = c(8, 15, 35, 60, 100), mirror = c(0.7, 0.8, 1, 1.5, 2))}
    rot0_cex <- map.(str_count(xname, '\n'), ~ whichSize.(ref = ., vec = 0:2, mirror = c(0.90, 0.85, 0.6))) %>%
                {. *whichSize. (ref = length(yL), vec = c(8, 15, 35, 60), mirror = c(1, 0.8, 0.6, 0.37))}
    mtext(xname, at = xPos, side = 1, las = 1, cex = cex_xname %||% rot0_cex, family = jL.(xname), line = rot0_line)
  }

  if (rot != 0) {
    yPos <- par('usr')[3] -0.035 *delta.(par('usr')[3:4]) *whichSize.(ref = length(yL), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.9))
    nameLen <- stringi::stri_numbytes(xname) %>% {skipMess.(max.(.))}  # Count including multi bytes char and space
    rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.8, 0.7, 0.6)) %>%
               {. *whichSize.(ref = length(yL), vec = c(8, 15, 35, 60, 100), c(0.9, 0.8, 0.7, 0.6, 0.4))}
    text(xPos, yPos, xname, srt = rot,  xpd = T, adj = c(1, 1), cex = cex_xname %||% rot_cex, family = jL.(xname))
  }

  ## boxplot
  points_outliers()
  box_whiskers()
  if (val == TRUE) textFun()
}


## Boxplot oriented for quantile limit and full/half box == (2023-10-16) ========================
box2. <- function(d, type = c('half', 'full')[1], sel = NULL, name = NULL, col = NULL, pareto = F, cut = F, xlab = NULL, ylab = NULL, ylim = NA,
                  wid = 0.65, jit = T, val = T, n = F, rot = 0, cex_xname = 0.8, digit = NULL, time_div = NULL, PDF = T, ...) {
  ## d <- sample_n(diamonds[-8:-10], 200)
  ## transform data
  tmp <- box2nest.(d, time_div)

  ## y-axis label
  ylab <- if (nrow(tmp) == 1 && ncol(tmp) == 4) ylab %||% names(tmp)[4] else ''

  ## make the size of figure margin for plot number
  if (!is.na(tmp$row_title[1]) || ncol(tmp) > 4) par(mfrow = c(nrow(tmp), ncol(tmp) - 3), oma = c(0, 3, 2.5, 0))

  ## boxplot
  for (i in 1:nrow(tmp)) for (j in 4:ncol(tmp)) {
    ## sel works on [y1,y2, ...] or [ID, y1,y2, ...]; "name" is only available with single-row box plot
    tmpij <- tmp[[i, j]][[1]]
    len <- seq_along(tmpij[[1]])
    if (nrow(tmp) == 1 && !is.null(name)) tmpij[[1]] <- name[len]
    if (nrow(tmp) == 1 && !is.null(sel)) tmpij <- c(sel, len) %>% .[. <= max(len)] %>% unique() %>% tmpij[., ]

    yL <- tmpij[[2]] %>%
          map(~ {if (all(is.na(.))) NA_real_ else .[!is.na(.)]}) %>%
          set_names(tmpij[[1]])

    ## color
    col2 <- color2.(col, len = length(yL)) %>% set_names(names(yL))

    ## sort in size to show the graph like Pareto chart
    if (pareto == TRUE) {
      yL <- yL %>% map_dbl(~ median(., na.rm = T)) %>% order(., decreasing = T) %>% yL[.]
      col2 <- match(names(col2), names(yL)) %>% col2[.]  # re-ordered colors as well as levels
    }

    ## adjust range
    if (cut == TRUE) {
      cut_vec <- function(x) {
        out2 <- quantile(x, probs = c(0.25, 0.75), na.rm = T) +c(-1, 1) *IQR(x, na.rm = T) *3.0  # last term is the cut-off criteria
        x[which(x < out2[1] | x > out2[2])] <- NA  # delete too large or small outliers
        return(x)
      }
      yL <- map(yL, cut_vec)
    }

    ## plot
    box1plot.(yL, type=type, col=col2, xlab=xlab, ylab=ylab, ylim=ylim, wid=wid, jit=jit, val=val, n=n, rot=rot, cex_xname=cex_xname, digit=digit)
    if (nrow(tmp) > 1 || ncol(tmp) > 4) {
      if (i == 1) mtext(names(tmp)[j], side=3, las=1, cex=1.3, family=jL.(names(tmp)[j]), font=2, line=par('mar')[3] +0.5)
      if (j == 4) mtext(tmp$row_title[i], side=2, las=1, cex=0.6, family=jL.(tmp$row_title[i]), font=2, line=par('mar')[2]-1.5, padj=-1.5)
    }
  }

  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()
# box2.(iris, pareto=T, cex_xname=0.8)  box2.(USArrests, rot=20, cut=T)  box2.(economics[1:50,])  box2.(diamonds[1:300, 1:5], pareto=T)
# box2.(sample_n(diamonds[-8:-10], 200)); save.(wh=c(30,30))
}


## Boxplot oriented for quantile limit and full/half box == (2023-07-27) ========================
boxplot22. <- function(tnL, type, jit, val, wid, ylim, rot, cex, cut, digit, mark, col, name, xlab, ylab, ...) {
  if (cut == TRUE) {
    for (i in seq(nrow(tnL))) {
      vec <- tnL[[2]][[i]]
      outs <- quantile(vec, probs = c(0.25, 0.75), na.rm = T) +c(-1, 1) *IQR(vec, na.rm = T) *3.0  # Last term is the cut-off criteria
      vec[which(vec < outs[1] | vec > outs[2])] <- NA  # Delete too large or small outliers
      tnL[[2]][[i]] <- tnL[[2]][[i]] %>% {.[which(!is.na(vec))]}
      if (!is.null(mark)) tnL[[3]][[i]] <- tnL[[3]][[i]] %>% {.[which(!is.na(vec))]}
    }
  }
  ## Scale of half or full boxplot
  if (type == 'full' || type == 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(0, wid *0.6, wid, wid))
  if (type != 'full' && type != 'f') def.(c('AT', 'jitW', 'leftW', 'rightW'), list(wid /2, wid /2, wid, 0))

  ## plot & color data
  yL <- tnL[[2]] %>% map(~ .[!is.na(.)]) %>% set_names(tnL[[1]])
  if (!is.null(mark)) {
    markL <- tnL[[3]] %>% set_names(tnL[[1]])
    lvs <- markL %>% unlist() %>% unique()
    color_lvs <- rev(color2.())[seq_along(lvs)]  # color2.(len = length(lvs))
    bg_markL <- markL %>% map(function(x) map_chr(x, ~ {lvs %in% .} %>% color_lvs[.]))
  } else {
    bg_markL <- map2(as.list(col), yL, ~ dplyr::if_else(.x == '#FFFFFF00', 'grey13', .x) %>% rep(times = length(.y)))
  }
  xPos <- 2 *seq(length(yL)) -1  # NA is not omitted yet
  CX <- length.(yL) %>% max() %>% whichSize.(ref = ., vec = c(500, 100, 13, 4), c(0.2, 0.35, 0.7, 0.8))

  ## fivenum() is agreed with quantile() if vec is odd, but if even, fivenum() is a bit wider than quantile()
  ## Moreover, some cases make wrong whiskers that have no points more or less than 95th or 5th by quantile()
  c1 <- map_dbl(yL, function(x) {fivenum(x)[2] -1.5 *IQR(x)} %>% c(., min(x)) %>% max())
  c2 <- map_dbl(yL, ~ fivenum(.)[2])
  c3 <- map_dbl(yL, ~ fivenum(.)[3])
  c4 <- map_dbl(yL, ~ fivenum(.)[4])
  c5 <- map_dbl(yL, function(x) {fivenum(x)[4] +1.5 *IQR(x)} %>% c(., max(x)) %>% min())

  ## for loop is needed because outlier isn't always just one ...
  points_outliers <- function(...) {
    for (i in seq_along(yL)) {
      d_strip <- tibble(x = xPos[i], y = yL[[i]]) %>%
                 mutate(pch = dplyr::case_when(y >= c1[i] & y <= c5[i] ~ 21, TRUE ~ 4),
                        col = dplyr::case_when(pch == 21 ~ col_tr.(bg_markL[[i]], 0.55), TRUE ~ col_tr.('grey13', 0.8)),
                        bg = bg_markL[[i]]
                 ) %>%
                 dplyr::filter(!is.na(y))

      if (jit == TRUE) {
        for (ipch in c(21, 4)) {
          d_strip %>%
          dplyr::filter(pch == ipch) %>%
          {stripchart(.$y, at = .$x[1] +AT, vertical = T, method = 'jitter', jitter = jitW, add = T, lwd = 0.35, cex = CX,
                     col = .$col, bg = .$bg, pch = ipch)}
        }
      } else {
        Yout <- d_strip %>% dplyr::filter(y < c1[i] | y > c5[i])
        if (nrow(Yout) != 0) points(Yout$x, Yout$y, lwd = 0.3, cex = CX)
      }
    }
    if (i == length(yL) && !is.null(mark)) print(color_lvs %>% set_names(unique(markL[[1]])))
  }
  ## Scale of half or full boxplot
  box_whiskers <- function(...) {
    rect(xPos -leftW, c2, xPos +rightW, c4, border = 0, col = col_tr.(col, 0.55))
    segments(xPos -leftW, c3, xPos +rightW, c3, col = col_tr.('grey13', 0.95), lwd = 3, lend = 'butt')
    rect(xPos -leftW, c2, xPos +rightW, c4, border = col_tr.('grey13', 0.95))
    segments(xPos -leftW /2, c1, xPos +rightW/2, c1, col = col_tr.('grey13', 0.95))
    segments(xPos -leftW/2, c5, xPos +rightW/2, c5, col = col_tr.('grey13', 0.95))
    segments(xPos, c1, xPos, c2, lty = 'dashed', col = col_tr.('grey13', 0.95))
    segments(xPos, c4, xPos, c5, lty = 'dashed', col = col_tr.('grey13', 0.95))
  }
  ## Show values
  textFun <- function(...) {
    Digit <- if (!is.integer(digit) && !is.null(digit)) {
               digit
             } else {
        	   unlist(yL) %>%
               delta.(.) %>%
        	   {if (. < 1 && all(median.(yL) < 1)) 3 else whichSize.(ref = ., vec = c(50, 5, 1), mirror = c(0, 1, 2))}
        	 }
    tCex <- whichSize.(ref = length(yL), vec = c(4, 13, 30), mirror = c(0.7, 0.6, 0.5))
    dD  <-  map(yL, quantile, probs = c(0, 0.5, 1), na.rm = T) %>% data.frame(.)
    for (i in seq_along(dD)) {
      if (!is.na(dD[1, i])) {
        if (round(dD[2, i] -dD[1, i], Digit +1) < 0.7 *10 ^(-Digit)) {
          dD[1, i] <- NA
        }
      }
      if (!is.na(dD[3, i])) {
        if (round(dD[3, i] -dD[2, i], Digit +1) < 0.7 *10 ^(-Digit)) {
          dD[3, i] <- NA
        }
      }
    }
    # haloText.(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), cex = tCex *1.5)  # Too slow ...
    text(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), col = 'white', cex = tCex *1.5 *1.05)  # Alternative
    text(xPos +AT, dD[2, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[2, ]), col = 'grey13', cex = tCex *1.5 *0.95)
    text(xPos +AT, dD[1, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[1, ]), col = 'grey70', cex = tCex, adj = c(0.5, 1.8))
    text(xPos +AT, dD[3, ], labels = sprintf(str_c('%.', Digit, 'f'), dD[3, ]), col = 'grey70', cex = tCex, adj = c(0.5, -1.0))
  }  # END of textFun()

  ## base plot
  par(xaxs = 'i', yaxs = 'i', mgp = c(0, 0.4, 0), ann = F)
  if (length(yL) > 30) par(mar = c(4, 3.3, 0.1, 1.0))
  xlim2 <- c(-1, 2 *length(yL) +1) +wid *c(1, -1)
  ylim2 <- pr.(yL, ylim, 0.07)  # NOTE: text of Max or Min is not shown by 0.05
  xlab <- xlab %||% ''
  ylab <- ylab %||% ''
  plot.new()
  plot.window(xlim = xlim2, ylim = ylim2)
  abline(v = par('usr')[1], h = par('usr')[3], col = 'grey13', lwd = 2 *par('lwd'))  # needed twice normal lwd
  axis(1, at = xPos, labels = F, lwd.ticks = par('lwd'), tcl = -par('tcl'), cex.axis = 1, lend = 'butt')
  walk(1:2, ~ axis(2, at =axisFun.(ylim2, n=6)[[.]], labels =(.==1), lwd.ticks =par('lwd'), tcl =-par('tcl')/.,
                   cex.axis=ifelse(yPos.(ylim2)>0.9,1,0.9), lend ='butt')
  )
  mtext(xlab, side = 1, las = 1, cex = whichSize.(ref = nchar(xlab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(xlab),
        line = par('mar')[1] -1.00)
  mtext(ylab, side = 2, las = 3, cex = whichSize.(ref = nchar(ylab), vec = c(15, 35, 50), c(1, 0.8, 0.5)), family = jL.(ylab),
        line = par('mar')[2] -yPos.(ylim2))
  ## x-label
  labAdj <- function(name) {
    Line <- map.(str_count(name, '\n'), ~ whichSize.(ref = ., vec = 0:2, c(0.3, 1.3, 1.3))) %>%
            {. *whichSize.(ref = length(yL), vec = c(8, 15, 35, 60, 100), mirror = c(1, 1, 1, 1.5, 2))}
    Cex <- map.(str_count(name, '\n'), ~ whichSize.(ref = ., vec = 0:2, mirror = c(0.90, 0.85, 0.6))) %>%
           {. *whichSize. (ref = length(yL), vec = c(8, 15, 35, 60), mirror = c(1, 0.8, 0.6, 0.37))}
    return(list(line = Line, cex = cex %||% Cex))
  }
  name <- {name %||% names(yL) %||% str_c('#', seq_along(yL))} %>% gsub('\\n', '\n', ., fixed = T) %>% correctChr.()
  if (length(name) < length(xPos)) name <- c(name, rep(NA_character_, times = length(xPos) -length(name)))
  if (rot == 0) {
    mtext(name, at = xPos, side = 1, las = 1, cex = labAdj(name)$cex, family = jL.(name), line = labAdj(name)$line)
  } else {
    yPos <- par('usr')[3] -0.035 *delta.(par('usr')[3:4]) *whichSize.(ref = length(yL), vec = c(8, 25, 35, 60), c(0.9, 0.8, 0.7, 0.9))
    nameLen <- stringi::stri_numbytes(name) %>% max.()  # Count including multi bytes char and space
    rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.8, 0.7, 0.6)) %>%
               {. *whichSize.(ref = length(yL), vec = c(8, 15, 35, 60, 100), c(0.9, 0.8, 0.7, 0.6, 0.4))}
    text(xPos, yPos, name, srt = rot,  xpd = T, adj = c(1, 1), cex = cex %||% rot_cex, family = jL.(name))
  }
  ## boxplot
  points_outliers()
  box_whiskers()
  if (val == TRUE) textFun()
}
## ========================

## ========================
box22. <- function(d, type = 'half', jit = T, val = T, ord = F, wid = 0.65, ylim = NA, col = NULL, name = NULL, xlab = NULL, ylab = NULL,
                  rot = 0, cex = 0.8, cut = F, sel = NULL, pareto = F, name_marking = NULL, col_marking = NULL,
                  digit = NULL, div = NULL, mark = NULL, PDF = T, ...) {
  ## d <- sample_n(diamonds[-8:-10], 200)
  ## Select data
  if (is.atomic(d)) d <- as_tibble(d) %>% set_names('x')
  d <- list2tibble.(d) %>%
       time2.(d, div) %>%
       mutate_if(is.character, as.factor)
  if (!is.null(mark) && mark %in% names(d)) {
    d_mark <- d[mark]
    d <- d %>% select(-all_of(mark))
  }
  tab_col <- d %>% select_if(is.factor) %>% names()
  all_fac_len <- d[tab_col] %>% map_df(n_distinct) %>% unlist() %>% prod()  # all factor number (row length in the graph)
  num_col <- d %>% select_if(is.numeric) %>% names()  # columns length in the graph
  if(length(num_col) == 0) stop('The data does NOT include any numeric data...\n\n', call. = F)
  d <- d %>% select(!!tab_col, !!num_col)
  if (!is.null(mark)) d <- d %>% bind_cols(mark)

  ## Make a nested tibble for the combination matrix
  if (length(tab_col) == 0) {  # [y1, y2, ...]
    dL <- if (is.null(mark) == TRUE) {
            tibble(var = names(d)) %>% mutate(value = as.list(d), NUMs = NA) %>% list(Yall = .)
          } else {
            tibble(var = names(d)[names(d) != mark]) %>%
            mutate(value = list(d), NUMs = NA, !!mark := list(d_mark)) %>%
            list(Yall = .)
          }
    tab_col <- 'var'
    num_col <- 'value'
  } else {  # [ID1, ID2, ... , y1, y2, ...]
    dL <- rep_along(tab_col, list()) %>% set_names(tab_col)
    for (i in seq_along(tab_col)) {  # separate the data into tab(i) and num(all)
      dL[[i]] <- d %>%
                 group_nest(across(tab_col[i])) %>%
                 mutate(NUMs = map(data, ~ select(., contains(c(num_col, mark))))) %>%
                 select(!data) %>% {
                   if (pareto == F && is.logical(ord) == T) {
                     if (ord == T) {  # [A,C,B] --> [A,B,C]
                       .[naturalsort::naturalorder(.[[tab_col[i]]]), ]
                     } else if (ord == F) {  # [A,C,B] --> [A,C,B]
                       {.[match(unique(d[[tab_col[i]]]), .[[tab_col[i]]]), ]}
                     } else {
                       .
                     }
                   } else if (is.numeric(ord) == T) {  # in case of ord like c(3,1,2)
                     .[ord, ]
                   } else {
                     .
                   }
                 }
      for (j in seq_along(num_col)) {  # increase num(1), num(2), ... as just one dbl data based on num(all)
        dL[[i]] <- dL[[i]] %>% mutate(!!num_col[j] := map(NUMs, ~ select(., num_col[j]) %>% pull()))
        if (j == length(num_col) && !is.null(mark)) {
          dL[[i]] <- dL[[i]] %>% mutate(!!mark := map(NUMs, ~ select(., all_of(mark)) %>% pull()))
        }
      }  # END of for(j)
    }  # END of for(i)
  }

  ## picking for selection is allowed only when factor column is one
  if (length(tab_col) == 1 && !is.null(sel)) {
    dL[[1]] <- dL[[1]][sel, ] %>%
               dplyr::filter(tab_col %>% is.null(.) | is.na(.) %>% `!`) %>%
               {if (nrow(.) == 0) stop('All the selected numbers of the factors are wrong...\n\n', call. = F) else .}
  }

  ## matrix of boxplot1: x = tab_col, y = num_col
  if (length(tab_col) *length(num_col) != 1) par(mfrow = c(length(tab_col), length(num_col)), oma = c(0, 1.5, 2.5, 0))

  for (i in seq_along(dL)) {
    tmp <- dL[[i]] %>% select(-NUMs)
    ## color marking
    col2 <- color2.(col, len = nrow(dL[[i]])) %>% set_names(tmp[[1]])
    if (!is.null(name_marking) && !is.null(col_marking)) {
      for (ii in seq_along(col_marking)) col2[which(tmp[[1]] %in% name_marking[[ii]])] <- col_marking[[ii]]
    }
    ## targeting
    for (j in seq_along(num_col)) {
      tnL <- tmp %>% select(!!tab_col[i], !!num_col[j], !!mark) # tibble of nested list

      ## sort in size to show the graph like Pareto chart
      if (pareto == TRUE) {
        ord <- tnL[[2]] %>% map_dbl(~ median(., na.rm = T)) %>% order(., decreasing = T)
        tnL <- tnL[ord, ]
        col2 <- match(names(col2), tnL[[1]]) %>% col2[.]  # re-order
      }

      boxplot2.(tnL=tnL, type=type, jit=jit, val=val, wid=wid, ylim=ylim, rot=rot, cex=cex, cut=cut, digit=digit, mark=mark,
                col=col2, name = name %||% tnL[[1]], xlab=xlab,
                ylab = if (length(dL) == 1 && ncol(dL[[1]]) == 3) ylab %||% num_col[j] else '')
      if (length(c(tab_col, num_col)) > 2) {
        if (i == 1) mtext(num_col[j], side = 3, las = 1, cex = 1.3, family = jL.(num_col[j]), font = 2, line = par('mar')[3] +0.5)
        if (j == 1) mtext(names(dL)[i], side = 2, las = 3, cex = 1.3, family = jL.(names(dL)[i]), font = 2, line = par('mar')[2] -0.15)
      }
    }
  }

  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()
# box2.(iris, pareto = T)  box2.(USArrests, rot = 20, cut = T)  box2.(economics[1:50, ])  box2.(iris[1:300, 1:3], mark = 'Sepal.Width')
# box2.(diamonds[1:300, 1:3])
}


## Bar plot == (2023-08-03) ========================
barp. <- function(d, wid = 0.5, spacer = 0.5, cum = F, xyChange = F, digit = NULL, val_onbar = F, elementChange = F,
                  xlab = '', ylab = '', ylim = c(0, NA), col = NULL, legePos = NULL, name = NULL, cex = NULL,
                  rot = 0, cex.digit = 0.7, ord = F, sel = NULL, lege_ord = F, lab_pos = NULL, ...) {
  ## bothersome settings... (val_onbar means to show values on bar or on error bar)
  query_lib.(stringi)
  d <- list2tibble.(d, ord)
  tab_col <- names(d)[!sapply(d, is.numeric)]

  d2 <- if (map_lgl(d, is.numeric) %>% all()) {  # [y1, y2, ...]
          if (is.atomic(d)) {
            d %>% set_names(if (!is.null(names(d))) names(d) else str_c('n', seq_along(d))) %>% t() %>% as_tibble()
          } else {
            d %>% as_tibble()
          }
        } else if (map_lgl(d, ~ is.character(.) | is.factor(.)) %>% sum() == 1) {  # [ID, y1, y2]
          if (!is.null(sel)) {
            d %>% mutate_if(is.character, as.factor) %>% group_by(!!tab_col := factor(get(tab_col), levels = unique(d[[tab_col]])[sel]))
          } else {
            d %>% group_by_if(~ is.numeric(.) %>% `!`)  # very important for barp.(iris); see mat_avg for iris & d2
          }
        } else {
          stop('The data has more than TWO character columns; delete them to 0 or 1...\n\n', call. = F)
        }

  mat_avg <- summarise_all(d2, mean.) %>% select_if(is.numeric) %>% as.matrix()
  erH <- summarise_all(d2, function(x) {
           mean.(x) +sd.(x) /sqrt(length.(x)) *ifelse(length.(x) < 5, 2.776, qt(0.05 /2, length.(x) -1, lower.tail = F))
         }) %>% select_if(is.numeric)
  erL <- summarise_all(d2, function(x) {
           mean.(x) -sd.(x) /sqrt(length.(x)) *ifelse(length.(x) < 5, 2.776, qt(0.05 /2, length.(x) -1, lower.tail = F))
         }) %>% select_if(is.numeric)
  if (nrow(mat_avg) > 1) {
    rownames(mat_avg) <- summarise_all(d2, mean.) %>% .[[1]]
    ## Re-order
    if (is.numeric(lege_ord)) {
      if (length(lege_ord) != nrow(mat_avg)) stop('lege_ord does NOT match the length of the data...\n\n', call. = F)
    } else {
      lege_ord <- if (lege_ord == T) naturalsort::naturalorder(rownames(mat_avg)) else 1:nrow(mat_avg)
    }
    if (ncol(mat_avg) != 1) {
      mat_avg <- mat_avg[lege_ord, ]
      erH <- erH[lege_ord, ]
      erL <- erL[lege_ord, ]
    } else {  # Note: check iris[, 4:5] --> d2 --> mat_avg
      mat_avg <- t(mat_avg) %>%as.data.frame %>%  .[1, lege_ord] %>% as.matrix
      erH <- t(erH) %>% as.data.frame %>% .[1, lege_ord]
      erL <- t(erL) %>% as.data.frame %>% .[, lege_ord]
    }
  }

  if (xyChange == TRUE) {
    mat_avg <- mat_avg %>% {
                 if (!cum) {
                   matrix(rev(.), nrow(.), ncol(.), dimnames = list(rownames(.) %>% rev(), colnames(.) %>% rev()))
                 } else {
                   {.[, ncol(.):1]}
               }}
    erH <- erH %>% unlist() %>% rev()
    erL <- erL %>% unlist() %>% rev()
  } else if (elementChange == TRUE) {
    mat_avg <- t(mat_avg)
    erH <- t(erH) %>% unlist()
    erL <- t(erL) %>% unlist()
  } else {
    erH <- erH %>% unlist()
    erL <- erL %>% unlist()
  }

  ## Base plot
  col2 <- color2.(col, len = if (nrow(mat_avg) == 1) ncol(mat_avg) else nrow(mat_avg)) %>% col_tr.(tr = 0.6)
  pos <- barplot(mat_avg, beside = !cum, horiz = xyChange, plot = F,
                 space = if (!cum) NULL else spacer,
                 width = if (!cum) c(rep(wid, nrow(mat_avg)), spacer) else 1
         ) %>% as.vector()
  pos_range <- {range(pos) +0.5 *c(-1, 1)} %>% delta.() %>% {. *0.05}  # Spread out by 5%
  pos_lim <- {range(pos) +0.5 *c(-1, 1)} %>% {if (!xyChange) c(.[1] -pos_range, .[2]) else c(.[1], .[2] +pos_range)}
  bar_lim <- {if (!cum && !all(is.na(erH))) erH else apply(mat_avg, 2, sum.)} %>%  # error bar or cumulative bar
             {c(0, max.(.))} %>%
             pr.(., ylim, 0.13)
  par(mar = if (!xyChange) c(2.4, 4, 0.5, 1) else c(0.5, 5.5, 1.5, 0.5), mgp = if (!xyChange) c(0, 0.4, 0) else c(0, 0.2, 0), ann = F)
  plot.new()
  plot.window(xlim = if(!xyChange) pos_lim else bar_lim, ylim = if(!xyChange) bar_lim else pos_lim)
  barplot(mat_avg, beside = !cum, horiz = xyChange, axes = F, axisnames = F, add = T, col = col2,
          space = if (!cum) NULL else spacer,
          width = if (!cum) c(rep(wid, nrow(mat_avg)), spacer) else 1
  )

  ## Error bar
  if (!cum) {
    erW <- wid /4
    ord <- if (!xyChange) 1:4 else c(2, 1, 4, 3)
    walk(list(list(pos, erH, pos, erL)[ord], list(pos -erW, erH, pos +erW, erH)[ord], list(pos -erW, erL, pos +erW, erL)[ord]),
         ~ segments(.[[1]], .[[2]], .[[3]], .[[4]], col = col_tr.('grey13', 0.95)))
  }

  ## Show values
  if (is.null(digit)) {
    digit <- delta.(unlist(mat_avg)) %>% {if (all(unlist(mat_avg) < 1)) 3 else whichSize.(ref = ., vec = c(50, 5, 1), c(0, 1, 2))}
  }
  if (!cum) {  # multi bar
    if (xyChange == FALSE) {
      textX <- pos
      textY <- (if (val_onbar == T) mat_avg else erH) +diff(par('usr')[3:4]) *0.04
   } else {
      textX <- (if (val_onbar == T) mat_avg else erH) +diff(par('usr')[1:2]) *0.04
      textY <- pos
   }
  } else {  # cumulative bar
    tmp <- as_tibble(mat_avg)  # very bothersome...
    for (i in seq(nrow(tmp))) tmp[i, ] <- as_tibble(mat_avg)[i, ] /2 +if (i == 1) 0 else sum.(as_tibble(mat_avg)[1:(i-1), ])
    textX <- if (!xyChange) rep(pos, each = nrow(mat_avg)) else unlist(tmp)
    textY <- if (!xyChange) unlist(tmp) else rep(pos, each = nrow(mat_avg))
  }
  textL <- sprintf(str_c('%.', digit, 'f'), as.vector(mat_avg))
  text(textX, textY, labels = textL, col = 'grey13', cex = cex.digit)

  ## Axis
  {if (!xyChange) list(NULL, par('usr')[1]) else list(par('usr')[4], NULL)} %>%
  {abline(h = .[[1]], v = .[[2]], col = 'grey13', lwd = 2 *par('lwd'))}
  walk(1:2, ~ axis(ifelse(!xyChange,2,3), at=axisFun.(bar_lim, n=6)[[.]], labels=(.==1), lwd.ticks=par('lwd'), tcl=-par('tcl')/.,
                   cex.axis=ifelse(yPos.(bar_lim)>0.9,1,0.9), lend='butt')
  )
  if (xyChange == FALSE) {
    mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
  }
  mtext(ylab, side=ifelse(!xyChange,2,3), las=ifelse(!xyChange,3,1), cex=whichSize.(ref=nchar(ylab), vec=c(15, 35, 50), c(1, 0.8, 0.5)),
        family = jL.(ylab), line = ifelse(!xyChange, par('mar')[2] -yPos.(bar_lim), par('mar')[4] +0.30))

  ## Label names
  xPos <- if (!cum) matrix(pos, nrow = nrow(mat_avg)) %>% apply(2, mean.) else pos
  yPos <- lab_pos %||% whichSize.(ref = length(pos), vec = c(8, 15, 35, 60), c(0.9, 0.8, 0.7, 0.9)) %>%
          {. *par('usr')[ifelse(!xyChange, 3, 1)] -ifelse(!xyChange, 0.035, 0.015) *delta.(par('usr')[if(!xyChange) 3:4 else 1:2])}

  if (is.null(name)) name <- colnames(mat_avg)# %>% {if (!xyChange) . else rev(.)}
  nameLen <- stringi::stri_numbytes(name) %>% max.(.)  # Count including multi bytes char and space
  rot_cex <- whichSize.(ref = nameLen, vec = c(5, 10, 15), c(0.8, 0.7, 0.6)) %>%
             {. *whichSize.(ref = length((xPos)), vec = c(3, 8, 15, 35, 60, 100), c(1, 0.9, 0.8, 0.7, 0.6, 0.4))}
  adj <- if (!xyChange) {if (rot == 0) NULL else c(1, 1)} else 1
  if (xyChange) def.(c('xPos', 'yPos'), list(yPos, xPos))
  text(xPos, yPos, name, srt = rot,  xpd = T, adj = adj, cex = ifelse(is.null(cex), rot_cex, cex),
       family = jL.(name))

  ## Legend
  if (nrow(mat_avg) > 1) {
    leges <- legePos %||% {if (!xyChange) c(0.75, 0.99) else c(0.7, 0.3)}
    legen2.(rownames(mat_avg), legePos = leges, col = col2, cex = NULL)  # cex = 0.65
  }
  if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
# barp.(iris)  barp.(iris,cum=T)  barp.(iris,xyChange=T,rot=25)  barp.(iris,cum=T,xyChange=T)  barp.(iris, elementChange=T,cex=.9)
# barp.(iris[-5],spacer=-0.1)  barp.(iris, lege_ord=c(3,1,2)) barp.(iris[4:5], sel=3:1)
}


## Super mimizu: horizontal barplot == (2023-09-28) ========================
smz. <- function(d, sel = NULL, pareto = F, this = NULL,  # this means the hightlight No.
                   xlim = NA, ylim = NA, xlab = NULL, ylab = NULL, name = NULL, col = NULL, cex = NULL, PDF = T, ...) {
  query_lib.(berryFunctions)

  ## tuning the data set
  if (!is.data.frame(d)) stop('The data should be a data frame ...\n\n', call. = F)
  tab_lgl <- map_lgl(d, ~ is.character(.) | is.factor(.))
  num_lgl <- sapply(d, is.numeric)
  tab_col <- choice.(names(d)[tab_lgl], note = 'ID column', chr = T, one = T)
  num_col <- choice.(names(d)[num_lgl], note = 'Numeric column', chr = T, one = T)
  if (is.null(tab_col) || is.null(num_col)) stop('The columns of the data should have [chr] [num] ...\n\n', call. = F)

  ## add error bar into the data
  d <- d[c(tab_col, num_col)] %>%
       .[complete.cases(.), ] %>%
       group_by(!!tab_col := factor(get(tab_col), levels = unique(d[[tab_col]]))) %>%  # keep original order as you see
       summarise_all(
         list(
           n = ~ n(),  # none of dot
           avg = ~ mean.(.),
           lower = ~ dplyr::if_else(n == 1, NA_real_, mean.(.) - sd.(.)),
           upper = ~ dplyr::if_else(n == 1, NA_real_, mean.(.) + sd.(.)),
           m = ~ min.(.),
           M = ~ max.(.),
           p5 = ~ percentile.(., probs = 0.05),
           p50 = ~ percentile.(., probs = 0.5),
           p95 = ~ percentile.(., probs = 0.95)
         )
       ) %>%
       rename(!!num_col := avg)

  ## reorder
  if (!is.null(sel)) d <- d[sel, ]
  if (pareto == TRUE) d <- d %>% {if (anyNA(d$M)) arrange(., desc(!!rlang::sym(num_col))) else arrange(., desc(M))}
  d <- d %>% mutate(y = nrow(.) : 1)

  ## color assignment
  col_base <- '#4b4936'  # bar color: dark gray color2.(col, len = 3)
  col_sub <- 'lightgoldenrod2'  # point color: wasabi '#4b4936' light gray '#e3e1d6'
  col_base2 <- '#f3981c'  # orange
  col_sub2 <- '#ee7d50'

  col_bar <- rep(col_base, nrow(d))
  col_point <- rep(col_sub, nrow(d))
  if (!is.null(this)) {
    col_bar[this] <- col_base2  # highlighted coloring
    col_point[this] <- col_sub2
  }

  ## setting for plot
  xlim2 <- pr.(range(d$m, d$M), xlim, expand_ratio = 0.13)
  ylim2 <- c(0.3, nrow(d) + 0.7)  # pr.(c(0, nrow(d) + 1), ylim, expand_ratio = 0.12)

  ## plot frame
  plot_base <- function() {
    if (Sys.info()['sysname'] == 'Darwin') grDevices::quartz.options(width = 5.0, height = 3.3)  # for Mac
    if (Sys.info()['sysname'] == 'Linux') options(repr.plot.width = 5.0, repr.plot.height = 3.3)  # for JupyterLab; confirm by options()$bitmapType
    if (Sys.info()['sysname'] == 'windows') grDevices::windows.options(width = 5.0, height = 3.3)  # for Windows
    par(xaxs = 'i', yaxs = 'i', ann = F, mar = c(2.5, 7, 0.5, 1))
    plot.new()
    plot.window(xlim = xlim2, ylim = ylim2)

    abline(h = 1 : nrow(d), col = 'grey85', lty = 3, lwd = 1)
    for (i in 1:2) axis(1, at = axisFun.(xlim2, n = 5)[[i]], labels = (i == 1), cex.axis = 1, padj = -0.1, tcl = par('tcl') / i, lend = 'butt')
    axis(2, at = 1 : nrow(d), labels = F, tcl = - par('tcl'), lend = 'butt')
    box()
    xlabcex <- whichSize.(ref = nchar(xlab %||% num_col), vec = c(15, 35, 50), c(1, 0.8, 0.5))
    mtext(xlab %||% num_col, side = 1, las = 1, cex = xlabcex, family = jL.(xlab %||% num_col), line = par('mar')[1] -1.00)

    name2 <- name %||% d[[tab_col]]
    name_len <- stringi::stri_numbytes(name2) %>% max.()  # Count including multi bytes char and space
    xPos <- par('usr')[1] -0.025 *delta.(par('usr')[1:2])
    cexlabel <- cex %||% whichSize.(ref = name_len, vec = c(4, 8, 15, 35), c(1.3, 1, 0.75, 0.5))
    text(x = xPos, y = d$y, labels = name2, adj = c(1, 0.5), xpd = T, cex = cexlabel, family = jL.(d[[tab_col]]), col = col_bar)
  }


  ## plot with graphic type
  plot_paint <- function() {
    for (i in 1 : nrow(d)) {
      if (d$n[i] == 1) {
      ## bar plot
      # xleft <- d[[i, num_col]] %>% ifelse(. > 0, par('usr')[1], .)  # mean > 0
      # xright <- d[[i, num_col]] %>% ifelse(. < 0, par('usr')[3], .) # mean < 0
      # rect(xleft, d$y[i] + 0.25, xright, d$y[i] - 0.25, col = col_tr.(col_bar[i], tr = 0.35), border = F)
        points(d[[i, num_col]], d[[i, 'y']], pch = 19, col = col_tr.(col_point[i], tr = 0.9, lwd = 1.3, cex = 1))
      } else {
      ## error bar
      # arrows(d[[i, 'lower']], d[[i, 'y']], d[[i, 'upper']], d[[i, 'y']], angle = 90, code = 3, length = 0.03, lwd = 0.8, col = col_bar[i])
      ## https://albert-rapp.de/posts/ggplot2-tips/11_rounded_rectangles/11_rounded_rectangles.html
        berryFunctions::roundedRect(d$m[i], d$y[i] - 0.25, d$M[i], d$y[i] + 0.25, col = col_tr.(col_bar[i], tr = 0.35), border = F, rounding = 0.55)
      # berryFunctions::roundedRect(d$p5[i], d$y[i] - 0.25, d$p95[i], d$y[i] + 0.25, col = col_tr.(col_bar[i], tr = 0.35), border = F, rounding = 0.5)
        lines(x = c(d$p5[i], d$p5[i]), y = c(d$y[i] - 0.2, d$y[i] + 0.2), col = col_tr.(col_bar[i], tr = 0.45), lwd = 0.9)
        lines(x = c(d$p50[i], d$p50[i]), y = c(d$y[i] - 0.2, d$y[i] + 0.2), col = col_tr.(col_bar[i], tr = 0.45), lwd = 0.9)
        lines(x = c(d$p95[i], d$p95[i]), y = c(d$y[i] - 0.2, d$y[i] + 0.2), col = col_tr.(col_bar[i], tr = 0.45), lwd = 0.9)
        points(d[[i, num_col]], d$y[i], pch = 19, col = col_tr.(col_point[i], tr = 0.9), lwd = 1.3, cex = 1.5)  # mean
      }
    }
  }

  plot_base()
  plot_paint()
  if (names(dev.cur()) == 'cairo_pdf' && PDF == T) skipMess.(dev.off())
  gp.()
# smz.(diamonds[1:2], this = 2, pareto = T)
}


## Scatter plot marix (Non-parametric evaluation) == (2023-11-13) ================================================
sp. <- function(d, col = NULL, sel = NULL, bg = NULL, xlab = '', ylab = '', cut = F, conv = F, stats = F, write = F, cex = NULL, ...) {
  query_lib.(psych)
  d <- list2tibble.(d) %>%
       clean2.(na0 = T) %>%
       select_if(~ n_distinct(.) > 1) %>% {  # sd.(.) > 0; sd = 0 causes error; delete somthing like machine number
         if (cut == TRUE) {
           mutate_if(., ~ is.numeric(.), function(vec) {
             outs <- quantile(vec, probs = c(0.25, 0.75), na.rm = T) +c(-1, 1) *IQR(vec, na.rm = T) *3.0  # Last term is the cut-off criteria
             vec[which(vec < outs[1] | vec > outs[2])] <- NA  # Delete too large or small outliers
             return(vec)
           })
         } else .
       } %>% {
         if (conv == TRUE) scale.(.) else .  # 0 ~ 1 normalization
       }

  if (nrow(d) == 0) stop('No available data...\n\n', call. = F)

# okngCol_TF <- d %>%
#               select_if(~ is.character(.)) %>% {
#                 if (ncol(.) > 0) map_lgl(., ~ str_detect(., pattern = 'OK|NG') %>% any.()) else F
#               }
# resultCol <- which(okngCol_TF) %>% names() %>% str_subset(., pattern = '結果|Result|result') %>% .[1]  # If failed, return NA
# if (!is.na(resultCol)) {
#   Results <- map.(d[[resultCol]], function(x) if (x %in% 'OK') 1 else if (x %in% 'NG') 0 else NA)
#   d <- tibble(Results) %>% bind_cols(., d)
# }

  ## color variation 
  d <- d %>%
       mutate_if(is.character, as.factor) %>%  # %>% mutate_if(is.factor, ~ as.numeric(.) -1)
       select(where(is.factor), where(is.numeric)) %>% {  # reordered interested columns first
         if (!is.null(sel) && is.numeric(sel)) {
           select(., sel, everything())
         } else if (!is.null(sel) && is.character(sel)) {
           select(., all_of(sel), everything())
         } else {
           .
         }
       }
  factor_num <- d %>% map_lgl(is.factor) %>% sum()

  if (is.character(col)) stop('The argument \"col\" must be numeric...\n\n', call. = F)
  if (is.null(col) == TRUE) {
    cols <- c(0, 0, 0)
    bg_col <- col_tr.(cols[2], 0.5)
  } else {
    cols <- list(c('grey88', 'grey55', 'grey35'), c('darkorange1', 'darkorange4', 'darkorange'),
                 c('springgreen1', 'springgreen4', 'seagreen3'), c('dodgerblue1', 'dodgerblue4', 'skyblue3'),
                 c('tomato1', 'tomato4', 'red1')
            ) %>% 
            .[[n_cyc.(col, 5, len_max = 1)]]
    if (factor_num <= 1) {
      bg_col <- col_tr.(cols[2], 0.3)

    } else {
      factor_vec <- d %>% select(where(is.factor)) %>% pull()
      bg_col <- bg %||% col_tr.(color2.()[1:n_distinct(factor_vec) +1], 0.5) %>% .[factor_vec] 
    }
  }

  cex <- cex %||% whichSize.(ref = nrow(d), vec = c(5, 10, 15, 30), mirror = c(1.3, 0.8, 0.6, 0.3))

  par(mar = c(1, 1, 1, 1), family = jL.(names(d)))
  skipMess.(
    psych::pairs.panels(
      d, smooth = T, scale = T, density = T, ellipses = T, lm = T, stars = T, method = 'pearson',
      pch = 21, gap = 0.3, cex.cor = 1.3, cex.labels = cex, cex.axis = ifelse(par('family') == 'Avenir Next', 1, 0.95),
      col = cols[1], bg = bg, hist.col = col_tr.(cols[3], 0.8)
    )
  )
  mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), outer = T, line = par('mar')[1] -1.00)
  mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), outer = T, line = par('mar')[2] -0.88)
  if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
  if (stats == TRUE) corMat.(d, write)  # Research of several types of correlations
  cat('\n    Data number = ', nrow(d), '\n\n')
  gp.()
# sp.(iris, col=3); save.(wh=c(4.3, 3.3)*1.8)
}


## Research of several types of correlations == (2023-07-20) ========================
corMat. <- function(d, write = F, ...) {
  query_lib.(minerva)
  ## https://nigimitama.hatenablog.jp/entry/2018/03/02/165421
  ## Not use a data like mtcars but use [y1, y2, ...]
  d <- list2tibble.(d) %>% select_if(~ is.numeric(.))
  ## Delete the row containing one NA, Inf, or -Inf at least
  d <- d %>% map_df(~ {!is.na(.) & . < Inf & . > -Inf}) %>% {rowSums(.) == ncol(d)} %>% d[., ]

  comb_mat <- combn(ncol(d), 2)
  for(i in seq(ncol(comb_mat))) {
    tenta <- comb_mat[, i] %>% d[.]
    d_pile <- tibble(comb = names(tenta) %>% str_flatten(., ' vs '),
                     Pearson = cor.test(tenta[[1]], tenta[[2]], method = 'pearson', exact = F)$estimate,  # Linear (parameteric)
                     Spearman = cor.test(tenta[[1]], tenta[[2]], method = 'spearman', exact = F)$estimate,  # Monotonic (non-parameteric)
                     MIC = skipMess.(minerva::mine(tenta[[1]], tenta[[2]])$MIC),  # Non-linear (Maximum Information Coefficient)
                     TIC = skipMess.(minerva::mine(tenta[[1]], tenta[[2]], normalization = T)$TIC)  # (Total Information Coefficient)
              )
    d_cor <- if (i == 1) d_pile else bind_rows(d_cor, d_pile)
  }
  print(d_cor)
  if (write == T) write2.(d_cor)
# corMat.(iris)
}


## Polygon short-cut function == (2022-07-11) ========================
polygon. <- function(d, col = 'grey95', ...) {
  d <- list2tibble.(d) %>% {if (ncol(.) == 1) rowid_to_column(., 'index') else .}
  if (ncol(d) < 3) {  # [index,y], [x,y]
    d_poly <- tibble(x = c(d[[1]], rev(d[[1]])), y = c(rep(0, nrow(d)), rev(d[[2]])))
  } else {  # [x,lowerY,upperY]
    d_poly <- tibble(x = c(d[[1]], rev(d[[1]])), y = c(d[[2]], rev(d[[3]])))
  }
  polygon(d_poly, border = NA, col = col_tr.(col, tr = 0.8))
# plt.(psd[2:3],add=0); polygon.(psd[2:3][30:50, ]); plt.(psd[2:3],add=2)
}


## Logarithm / Exponential approximation  == (2024-10-02) ========================
fit_log. <- function(d, xlab = NULL, ylab = NULL) {
  if (is.data.frame(d) != T || ncol(d) != 2) stop('It\'s not two-column data ...\n\n', call. = F)
  d <- d %>% arrange(across(names(d)[1]))
  plt.(d, type = 'pp', xlab = xlab, ylab = ylab)

  ## non-linear regression
  names(d) <- c('x', 'y')
  if ({nrow(d) - map_int(d, n_distinct)} %>% {diff(.) <= 0}) {  # duplicated num x >= y
    d_unique <- d %>% group_by(x) %>% summarise(y = median.(y)) %>% group_by(y) %>% summarise(x = median.(x)) %>% select(x, y)
  } else {  # duplicated num x >= y
    d_unique <- d %>% group_by(y) %>% summarise(x = median.(x)) %>% group_by(x) %>% summarise(y = median.(y))
  }

# str_c('https://github.com/Nyu3/psd_R/blob/master/PSD_archive.R', '?raw=TRUE') %>%
# readLines(., encoding = 'UTF-8') %>%
# {eval(parse(text = .), envir = .nya0env)}
# attach(.nya0env)
  source(file.path('~/Library/Mobile Documents/com~apple~CloudDocs/R/tuningPSD', 'PSD_archive.R'), chdir = F)
  pLL1 <- pLL_al
  pLL2 <- pLL_nu_lam
  pLL3 <- pLL_nu_lam_al

  ## Inverse
  fi1 <- function(x,al) al / x
  tmp_inv1 <- lazy_call.(d_unique[[1]], log(d_unique[[2]]), pLL1, fi1)
  ## Logarithm
  fl2 <- function(x,nu,lam) log(nu * x ^lam)
  fl3 <- function(x,nu,lam,al) log(nu * x ^lam +al)
  tmp_log2 <- lazy_call.(d_unique[[1]], log(d_unique[[2]]), pLL2, fl2)
  tmp_log3 <- lazy_call.(d_unique[[1]], log(d_unique[[2]]), pLL3, fl3)
  ## Exponential
  fe2 <- function(x,nu,lam) nu * x ^lam
  fe3 <- function(x,nu,lam,al) nu * x ^lam +al
  tmp_exp2 <- lazy_call.(d_unique[[1]], d_unique[[2]], pLL2, fe2)
  tmp_exp3 <- lazy_call.(d_unique[[1]], d_unique[[2]], pLL3, fe3)
  ## Better model
  mdls <- list(tmp_inv1, tmp_log2, tmp_log3, tmp_exp2, tmp_exp3)
  better_num <- map_dbl(mdls, ~ .$deviance) %>% which.min()

  ## plot2
  if (c(2, 3) %in% better_num %>% any()) {
    xy <- mdls[[better_num]]$xy %>% mutate(y = exp(y))
  } else {
    xy <- mdls[[better_num]]$xy
  }
  lines(xy, lty = 2, col = color2.(len = 1))

  ## result message
  cat(c('Inverse','Logarithm','Logarithm','Exponential','Exponential')[better_num], 'approximation\n\n')
  function_text <- str_c('y = ', c('al / x', 'nu * x ^lam', 'nu * x ^lam + al', 'nu * x ^lam', 'nu * x ^lam +al')[better_num])
  mdls[[better_num]]$model %>%
  capture.output() %>%
  str_c(., collapse = '\n') %>%
  str_replace('y ~ eval\\(parse\\(text = fun_quasi\\)\\)', function_text) %>%
  cat()

# curve(8205.5 *x ^(-0.915), -1, 200, add = T, col='red')
# curve(10267 *x ^(-0.99), -1, 200, add = T, col='seagreen3')
# tibble(x=c(1.3,2.15,2.69,3.28,4.94,7.64,10.2,15,22,28.8,39.6,52.3,73.1,101,137,198),y=c(7,5,4,3,2,1.5,1,.75,.5,.4,.3,.2,.15,.1,.075,.05)*1000) %>% fit_log.()
}


## Chi-squared test  == (2024-09-13) ========================
x2. <- function(d, col = 2, ...) {
  query_lib.(corrplot)

  if (is.data.frame(d) == FALSE) stop('Make the data with ONE column...\n\n', call. = F)
  d <- hablar::retype(d) %>%
       mutate_if(is.character, as.factor)
  x_name <- names(d)[sapply(d, is.factor)]
  y_name <- names(d)[sapply(d, is.numeric)]


  chisq <- function(tmp0, txt = NULL, ...) {
      tmp <- tmp0 %>% chisq.test(., correct = T)  # Yates' contiunity correction
      print(tmp$observed)
      print(tmp)

      if (tmp$statistic == 0) return(cat('There is absolutely no relationship because p-value =', tmp$p.value, '\n\n'))
      par(family = jL.(c(rownames(tmp$observed), colnames(tmp$observed))))
      ## http://www.sthda.com/english/wiki/chi-square-test-of-independence-in-r
      contrib <- round(100 * tmp$residuals ^2 / tmp$statistic, 1)  # contribution in percentate (%) = r2 / x2
      corrplot::corrplot(contrib,
                         is.cor = F,
                         cl.pos = 'n',  # 'b'
                         addCoef.col = 'grey65',
                         tl.col = 'grey13',
                         tl.cex = 0.8,
                         tl.srt = 45,
                         col = c('Oranges', 'Purples', 'Reds', 'Blues', 'Greens', 'Greys', 'OrRd', 'YlOrRd', 'YlOrBr', 'YlGn')[col] %>%
                               corrplot::COL1(.)
      )
      mtext(side = 1, expression(paste('Contribution (%) for ', chi ^2, ' score')), cex = 1.3, line = par('mar')[1] - 1.5)
      if (tmp$p.value > 0.05) cat(txt, 'There is no relationship.\n\n') else cat(txt, 'Significantly associated.\n\n')
  }


  ## Cross table
  if (length(x_name) == 1) {
    if (nrow(d) == n_distinct(d[x_name]) && all(sapply(d[y_name], is.integer)) == TRUE) {
      tmp0 <- d %>%
              column_to_rownames(x_name)
      chisq(tmp0)
    }
  ## [chr1, chr2, chr3, ...]
  } else if (length(x_name) > 1 && length(y_name) == 0) {
    x_name2 <- if (length(x_name) == 2) x_name else choice.(x_name, note = 'Choose less than TWO X factors')[1:2]
    tmp0 <- table(d[[x_name2[1]]], d[[x_name2[2]]])
    chisq(tmp0, txt = str_c('Results of ', x_name2[1], ' vs ', x_name2[2], ': '))
  } else {
    stop('Needed table [level, y1, y2, ...] or data [level1, level2, level3, ...]...\n\n', call. = F)    
  }
# tibble(ID=LETTERS[1:3],n=c(40,30,7),y=c(30,20,5),a=c(6,10,5)) %>% x2.()  x2.(diamonds[2:4], rot = 0, col = 2)
}


## Random forest == (2021-08-12) ========================
rf. <- function(d, col_target = 1, col = 1, type = 1, cex = 0.8, rot = 0, ...) {
  query_lib.(randomForest, randomForestExplainer, viridis)
  d <- list2tibble.(d) %>% mutate_if(is.character, as.factor) %>% na.omit
  ## Auto correction for the col_target
  tf <- map_lgl(d, ~ is.character(.) | is.factor(.)) %>% which
  if (length(tf) != 0 && !col_target %in% tf) col_target <- tf
  chr_mdl <- as.formula(str_c(names(d)[col_target], ' ~ .'))
  mdl <- randomForest::randomForest(formula = chr_mdl, data = d, ntree = 1000, localImp = ifelse(type == 1, F, T))  #  mtry = 5
  mdl_conf <- mdl$confusion[1:4]  # TP FP FN TN
  mdl_accuracy <- sum(mdl_conf[c(1, 4)]) /sum(mdl_conf)  # TP+TN /(TP+FP+FN+TN)
  mdl_precision <- mdl_conf[1] /sum(mdl_conf[1:2])  # TP /(TP+FP)
  mdl_recall <- mdl_conf[1] /sum(mdl_conf[c(1, 3)])  # TP /(TP+FN)
  cat('\n    Accuracy = ', round(mdl_accuracy, 4),
      '\n    Precision = ', round(mdl_precision, 4),
      '\n    Recall = ', round(mdl_recall, 4),
      '...\n\n')
  if (type == 1) {
    tenta <- mdl$importance %>% t
    tenta1 <- sort(tenta, decreasing = T)
    tenta_n <- colnames(tenta)[order(tenta, decreasing = T)]
    colors <- n_factor.(tenta) %>% RColorBrewer::brewer.pal(., 'Spectral')
  # {list(viridis::viridis(.), viridis::magma(.), viridis::inferno(.), viridis::plasma(.), viridis::cividis(.))[[n_cyc.(col, 5)]]}
    barp.(tenta1, xyChange = T, name = rev(tenta_n), spacer = -0.6, wid = 0.8, col = colors, cex = cex, rot = rot)
  } else if (type != 1) {
    randomForestExplainer::plot_min_depth_interactions(mdl)
  }
  if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
# rf.(iris, type = 1)
}


## SOM = Self-Organizing Map == (2022-01-26) ========================
som. <- function(d, dims = 20, main = '', pal = 1, target = 1, k = NULL, ...) {  # target means the col which is the actor of color pattern
  query_lib.(kohonen, viridisLite)
  d0 <- d %>% .[complete.cases(.), ] %>% unique(.)  # might include character /factor column
  d1 <- d0 %>% select_if(is.numeric) %>% map_df(scale) %>% select_if(~ is.finite(.) %>% all) %>% as.matrix  # for som calculation; map_df(d1, anyNA) %>% t
  ## dimension
  if (length(dims) == 1) {
    xdim <- ydim <- dims
  } else {
    xdim <- dims[1]; ydim <- dims[2]
  }
  for (i in 0:(min(dims)-1)) {
    tmp <- try(kohonen::som(d1, grid = kohonen::somgrid(xdim = xdim -i, ydim = ydim -i, topo = 'hexa'), rlen = 500), silent = T)  # str(tmp)
    if (!'try-error' %in% class(tmp)) break
  }  # plot(tmp, type = 'changes', main = 'Change in similarity as traing data')
  ## labeling
  d2 <- select_if(d0, ~ is.character(.) | is.factor(.))
  if (ncol(d2) == 0) {
    labs <- 1:nrow(d1)  # rowid type
  } else {
    labs <- choice.(names(d2), one = T) %>% d2[[.]]  # when the original data has chr column as label
  }
  ## coloring
  if (pal != 0) {
    col_text <- c("colorRampPalette(c('grey0', 'grey100'))",
                  str_c('viridisLite::', c('turbo', 'mako', 'magma')),
                  str_c(c('cm', 'heat', 'terrain', 'topo'), '.colors'))[pal]
    tmp2 <- kohonen::getCodes(tmp)
    col_n <- trunc(nrow(tmp2) /1)
    color <- str_c(col_text, '(col_n)[rank(tmp2[, target])]') %>% {eval(parse(text = .))}
    par(col = 'grey50')
  }
  plot(tmp, type = 'mapping', labels = labs, shape = 'straight', main = ' ', bgcol = if (pal == 0) NULL else color, cex = 0.75) # ?kohonen::plot.kohonen
  mtext(main, family = jL.(main), side = 3, line = -2, cex = 1.3)  # box()  par('usr')[4] +diff(par('usr')[3:4]) *0.1
  ## k: desired group number (k > 1)
  if (!is.null(k) == TRUE) {
    som.hc <- stats::cutree(stats::hclust(kohonen::object.distances(tmp, 'codes')), k = k %||% {ifelse(ncol(d1) > 9, 3, ncol(d1) -1)})
    kohonen::add.cluster.boundaries(tmp, som.hc)
  }
  gp.()
# mutate(iris, Species = as.numeric(Species) %>% as.factor) %>% som.(., pal = 3)
#  stats.(iris[-5], transpose = T) %>% .[-c(32, 33)] %>% som.(.)
#  iris %>% group_by(Species) %>% summarise_all(mean) %>% som.(.)
}


## matplot2 == (2020-02-08) ========================
mat2. <- function(dt, xlim = NA, ylim = NA, xlab = '', ylab = '', ...) {  # matplot cannot draw a missing line across NA part
  ## dt := [x, y1, y2, ...]
  cols <- c('grey13', 'springgreen3', 'tomato2', 'dodgerblue3', 'maroon3', rep('grey13', 10))
  def.(c('x', 'dty', 'xlim', 'ylim'), list(dt[[1]], dt[, -1], range.(dt[[1]]), unlist(dt[, -1]) %>% range.(.)))
  xlim2 <- pr.(vec = x, xlim, 0.02)
  ylim2 <- pr.(vec = dty, ylim, 0.12)
  plot.new()
  plot.window(xlim = xlim2, ylim = ylim2)
  for (i in seq_along(dty)) data.frame(x, dty[[i]]) %>% na.omit(.) %>% lines(., col = cols[i])
  for (i in seq_along(dty)) data.frame(x, dty[[i]]) %>% na.omit(.) %>% points(., pch = 21, col = 'grey13', bg = col_tr.(cols[i], 0.75))
  for (i in 1:2) {
    axis(side = 1, at = axisFun.(xlim2, n = 6)[[i]], labels = (i == 1), tcl = par('tcl') /i, lend = 'butt', padj = -0.2)
    axis(side = 2, at = axisFun.(ylim2, n = 6)[[i]], labels = (i == 1), tcl = par('tcl') /i, lend = 'butt')
  }
  box()
  mtext(xlab, side = 1, las = 1, cex = 1, family = jL.(xlab), line = par('mar')[1] -1.00)
  mtext(ylab, side = 2, las = 3, cex = 1, family = jL.(ylab), line = par('mar')[2] -yPos.(ylims))
  if (names(dev.cur()) == 'cairo_pdf') skipMess.(dev.off())
}  # mat2.(iris[-5])


## Inf & NaN -> zero == (2023-07-21) ========================
clean0. <- function(y, ...) y %>% {dplyr::case_when(. %in% c(NA, NaN, -Inf, Inf) ~ 0, TRUE ~ .)}  # clean0.(c(1:3, NA, NaN, Inf, -Inf))
clean1. <- function(d, ...) {
  d <- rowid_to_column(d, 'iD')
  clean_row <- d %>%
               select_if(is.numeric) %>%
               dplyr::filter(rowSums(is.na(.)) == 0, rowSums(.) > -Inf, rowSums(.) < Inf) %>%
               .[['iD']]
  out <- d[clean_row, ] %>% dplyr::select(!'iD')
  return(out)
# tibble(nya=c(1:3, NA, NaN, Inf, -Inf)) %>% clean1.
}

## Inf & NaN -> NA == (2022-06-10) ========================
clean2. <- function(d, na0 = T, ...) {
  if ('data.frame' %in% class(d)) {
    out <- d %>%
      mutate_if(is.numeric, ~ ifelse(. %in% c(NaN, -Inf, Inf), NA, .)) %>%
      filter(rowSums(is.na(.)) != ncol(.)) %>%
      select_if(colSums(is.na(.)) != nrow(.)) %>%
      {if (na0 == TRUE) .[complete.cases(.), ] else .}
  } else {
    out <- d
  }
  return(out)
# tibble(x=c(NA,1:2,NA,4:5), y=c(NA, NA, NaN, Inf,1,2),z=rep(NA,6)) %>% clean2.(na0=F)
}

## Omit undesirable values == (2023-07-28) ========================
omit2. <- function(x) {
  tf_vec <- !is.na(x) & !is.nan(x) & !is.infinite(x)
  if (any(tf_vec) == T) x[tf_vec] else rep(NA_real_, length(x))
# c(NA, NaN, Inf, -Inf, 123) %>% omit2.()
}

## Omit outliers == (2024-03-06) ========================
trim. <- function(d, cuts = c(0.25, 0.75)) {
  out <- map_df(d, function(x) {
                     if (!is.numeric(x)) return(x)
                     dplyr::case_when(x < percentile.(x, cuts[1]) ~ NA_real_,
                               x > percentile.(x, cuts[2]) ~ NA_real_,
                               TRUE ~ x
                     )
                   }
         ) %>%
         .[complete.cases(.), ]
  return(out)
# trim.(iris[-1:-2]) %>% ellip.()
}

## Multiple definition == (2019-01-10) ========================
def. <- function(defnames, values) for (i in seq_along(defnames)) assign(defnames[i], values[[i]], envir = parent.frame())
# def.(c('cat', 'run'), list(35, 1:23))

## map() returns vectors == (2019-01-09) ========================
map. <- function(.x, .f, ... ) purrr::map(.x, .f, ... ) %>% unlist(., use.names = F)

## Signature function for math treatment == (2020-01-05) ========================
sgn. <- function(x) dplyr::case_when(x > 0 ~ 1, x == 0 ~ 0, x < 0 ~ -1)

## Rescaling (min-max normalization) for math treatment == (2023-07-21) ========================
rescaling. <- function(x) {  # convert into [0, 1]
  if (is.atomic(x) && is.numeric(x)) {
    (x -min.(x)) /(max.(x) -min.(x))
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) {(. -min.(.)) /(max.(.) -min.(.))} else .)
  } else {
    NA_real_
  }
# rescaling.(iris) %>% box2.
}

scale. <- function(x, scale_range = c(0, 1)) {  # Normalization: [0,1], Standarization: (x-u) /sd
  scale0. <- function(x, scale_range) {
    scale(x, center = min.(x), scale = delta.(x)) %>%
    as.vector() %>%
    {. *delta.(scale_range) +scale_range[1]}
  }
  if (is.atomic(x) && is.numeric(x)) {
    scale0.(x, scale_range)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) scale0.(., scale_range) else .)
  } else {
    NA_real_
  }
# scale.(iris) %>% box2.  scale.(iris, c(-1, 1)) %>% box2.
}

## Short cut to kill bothersome etc. == (2023-12-10) ========================
pmax. <- function(x) omit2.(x) %>% pmax()

pmin. <- function(x) omit2.(x) %>% pmin()

any. <- function(x) as.logical(x) %>% any(., na.rm = T)

ymd. <- function(x) if (is.POSIXct (x)) floor_date(x, 'day') %>% as.character() %>% gsub(' JST', '', .) else x

range. <- function(x) {
  if (is.atomic(x)) {
    if (is.null(x)) NA_real_ else {
      omit2.(x) %>%  {if (is.numeric(.) || is_time.(.)) range(.) else if (is.character(.) || is.factor(.)) range(seq(.)) else NA_real_}
    }
  } else if (is.list(x)) {
    list2tibble.(x) %>%
    map_df(~ .x %>% omit2.() %>% {
             if (is.numeric(.) || is_time.(.)) range(.) else if (is.character(.) || is.factor(.)) range(seq(.))
           }
    ) %>% {
      if (ncol(.) == 1) .[[1]] else .  # don't use unlist() for .[[1]]
    }
  } else {
    NA_real_
  }
# range.(economics)  range.(economics[1])  range.(iris[5])
}

mean. <- function(x, trim = 0) {  # mean0.(rep(NA, 3))  mean0.(rep(NA_real_, 3))
  mean0. <- function(x, trim = 0) {
    if (is.atomic(x) && is.numeric(x) || is_time.(x) || is.logical(x)) {
      omit2.(x) %>% mean(., trim = trim) %>% ymd.()
    } else {
      NA_real_
    } %>% ifelse(is.nan(.), NA_real_, .)
  }
  if (is.atomic (x)) {
    mean0.(x, trim)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.) | is.logical(.)) mean0.(., trim) else NA_real_)
  } else {
    NA_real_
  }
}

## propotional mean
prop_mean. <- function(x) {
  if (is.atomic(x)) {
    omit2.(x) %>% {sum(. ^2) /sum(.)}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {sum(. ^2) /sum(.)} else NA_real_)
  } else {
    NA_real_
  }
}

## geometric mean
geo_mean. <- function(x) {
  geo_mean0. <- function(x) {
    if (is.atomic(x) && is.numeric(x)) {
      if (any(x[!is.na(x)] <= 0)) {
        NA_real_
      } else {
        omit2.(x) %>% {sum(log(.)) /length(.)} %>% exp()
      }
    } else {
      NA_real_
    }
  }
  if (is.atomic(x)) {
    geo_mean0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) geo_mean0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## harmonic mean
har_mean. <- function(x) {
  har_mean0. <- function(x) {
    if (is.atomic(x) && is.numeric(x)) {
      if (any(x[!is.na(x)] == 0)) {
        NA_real_
      } else {
        omit2.(x) %>% {length(.) /sum(1 /.)}
      }
    } else {
      NA_real_
    }
  }
  if (is.atomic(x)) {
    har_mean0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) har_mean0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## Hodges-Lehmann estimator == (2024-03-08) ==
hl_mean. <- function(x) {
  query_lib.(rt.test)
  hl_mean0. <- function(x) {
    if (is.atomic(x) && is.numeric(x)) {
      if (any(x[!is.na(x)] == 0)) {
        NA_real_
      } else {
        omit2.(x) %>% rt.test::HL.estimate(., IncludeEqual = T)
      }
    } else {
      NA_real_
     }
  }
  if (is.atomic(x)) {
    hl_mean0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) hl_mean0.(.) else NA_real_)
  } else {
    NA_real_
  }  
}

median. <- function(x) {
  if (is.atomic(x) && is.numeric(x) || all(is_time.(x))) {
    omit2.(x) %>% median() %>% ymd.()
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if(is.numeric(.) | is_time.(.)) omit2.(.) %>% median() %>% ymd.() else NA_real_)
  } else {
    NA_real_
  }
}

percentile. <- function(x, probs = 0.50) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% quantile(., probs = probs, name = F)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% quantile(., probs = probs, name = F) else NA_real_)
  } else {
    NA_real_
  }
}

p5. <- function(x) percentile.(x, probs = 0.05)
p50. <- function(x) percentile.(x, probs = 0.50)
p95. <- function(x) percentile.(x, probs = 0.95)

max. <- function (x, Nth = 1) {
  max0. <- function(x, Nth = 1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) omit2.(x) %>% sort(., decreasing = T) %>% .[Nth] %>% ymd.() else NA_real_
  if (is.atomic(x)) {
    max0.(x, Nth)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) max0.(., Nth) else NA_real_)
  } else {
    NA_real_
  }
}

min. <- function(x, Nth = 1) {
  min0. <- function(x, Nth = 1) if (is.atomic(x) && is.numeric(x) || is_time.(x)) omit2.(x) %>% sort(., decreasing = F) %>% .[Nth] %>% ymd.() else NA_real_
  if (is.atomic(x)) {
    min0.(x, Nth)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) min0.(., Nth) else NA_real_)
  } else {
    NA_real_
  }
}

max2. <- function(x, na = F) {
  max2_base. <- function(x, na) {
    if (! is.atomic(x) || ! is.numeric(x)) return(NA_real_)
    def.(c('whisker', 'Max'),  list(quantile(x, probs = 0.75, na.rm = T) + IQR(x, na.rm = T) * 1.5, max(x, na.rm = T)))
    out <- if (na == T && setequal(whisker, Max)) NA_real_ else min(c(whisker, Max), na.rm = T)
    return(out)
  }
  if (is.atomic(x)) {
    max2_base.(x, na)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) max_base0.(., na) else NA_real_)
  } else {
    NA_real_
  }
}

min2. <- function(x, na = F) {
  min2_base. <- function(x, na) {
    if (!is.atomic(x) || !is.numeric(x)) return(NA_real_)
    def.(c('whisker', 'Min'), list(quantile(x, probs = 0.25, na.rm = T) - IQR(x, na.rm = T) * 1.5, min(x, na.rm = T)))
    out <- if (na == T && setequal(whisker, Min)) NA_real_ else max(c(whisker, Min), na.rm = T)
    return(out)
  }
  if (is.atomic(x)) {
    min2_base.(x, na)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) min2_base.(., na) else NA_real_)
  } else {
    NA_real_
  }
}

sd. <- function(x) {
  if (is.atomic(x) && !(is.character(x) && !is.factor(x))) {
    omit2.(x) %>% sd()
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.) | is.logical(.)) omit2.(.) %>% sd() else NA_real_)
  } else {
    NA_real_
  }
}

## robust deviation: https://stats.stackexchange.com/questions/123895/mad-formula-for-outlier-detection
sd2. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {median(abs(. -median(.))) /0.6745}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) omit2.(.) %>% {median(abs(. -median(.))) /0.6745} else NA_real_)
  } else {
    NA_real_
  }
}

## regular sd
sd_reg. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sd.(.) /sqrt(mean.(.) *(length(.) -1))}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) omit2.(.) %>% {sd.(.) /sqrt(mean.(.) *(length(.) -1))} else NA_real_)
  } else {
    NA_real_
  }
}

var. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% var(., y = NULL)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) omit2.(.) %>% var(., y = NULL) else NA_real_)
  } else {
    NA_real_
  }
}

skew. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {(. -mean(.)) ^3 /sd(.) ^3} %>% mean()
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {(. -mean(.)) ^3 /sd(.) ^3} %>% mean() else NA_real_)
  } else {
    NA_real_
  }
}

skew_reg. <- function(x) {
  skew_reg0. <- function(x) if (is.atomic(x) && is.numeric(x) && length(x[!is.na(x)]) > 2) {
    omit2.(x) %>% {skew.(.) *(length(.) -1) ^0.5 /(length(.) -2)}
   } else {
    NA_real_
  }
  if (is.atomic(x)) {
    skew_reg0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) skew_reg0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

kurt. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {(. -mean(.)) ^4 /sd(.) ^4} %>% mean(.)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {(. -mean(.)) ^4 /sd(.) ^4} %>% mean(.) else NA_real_)
  } else {
    NA_real_
  }
}

kurt_reg. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {kurt.(.) *(length(.) -1) /(length(.) ^2 -3 *length(.) +3)}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {kurt.(.) *(length(.) -1) /(length(.) ^2 -3 *length(.) +3)} else NA_real_)
  } else {
    NA_real_
  }
}

delta. <- function(x, unit = 'day') {
  delta0. <- function(x, unit = 'day') {
    if (is.atomic(x) && is.numeric(x)) {
      range.(x) %>% diff() %>% as.numeric()
    } else if (is.atomic(x) && is_time.(x)) {
      range.(x) %>% diff() %>% time_length(., unit = unit) %>% as.numeric()
    } else {
      NA_real_
    }
  }
  if (is.atomic(x)) {
    delta0.(x, unit)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) delta0.(., unit) else NA_real_)
  } else {
    NA_real_
  }
}

iqr. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% IQR()
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% IQR() else NA_real_)
  } else {
    NA_real_
  }
}

## min-max ratio
mmr. <- function(x) {
  mmr0. <- function(x) if (is.atomic(x) && is.numeric(x) && !all(is.na(x))) omit2.(x) %>% {min(.) /max(.)} else NA_real_
  if (is.atomic(x)) {
    mmr0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) mmr0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## coefficient of variance
cv. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sd(.) /mean(.)}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.)) omit2.(.) %>% {sd(.) /mean(.)} else NA_real_)
  } else {
    NA_real_
  }
}

## root mean square
rms. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sqrt(sum(. ^2)/length(.))}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {sqrt(sum(. ^2)/length(.))} else NA_real_)
  } else {
    NA_real_
  }
}

## root mean squared error (not for model evaluation)
rmse_stats. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sqrt(sum((. -mean(.)) ^2)/length(.))}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {sqrt(sum((. -mean(.)) ^2)/length(.))} else NA_real_)
  } else {
    NA_real_
  }
}

## mean squared logarithmic error
msle. <- function(x) {
  msle0. <- function(x) if (is.atomic(x) && is.numeric(x)) {
    if (min(x, na.rm = T) > -1) {
      omit2.(x) %>% {sum(log(1 +.) -log(1 +mean(.)))/length(.)}
    } else {
      NA_real_
    }
  } 
  if (is.atomic(x)) {
    msle0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) msle0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## mean absolute error
mae. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sum(abs(. -mean(.)))/length(.)}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {sum(abs(. -mean(.)))/length(.)} else NA_real_)
  } else {
    NA_real_
  }
}

## declining distribution index
ddi. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {sum(abs(. -mean(.))) *length(.) /(2 *(length(.) -1) *sum(.))}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {sum(abs(. -mean(.))) *length(.) /(2 *(length(.) -1) *sum(.))} else NA_real_)
  } else {
    NA_real_
  }
}

## index of balance
balance. <- function(x) {
  balance0. <- function(x) if (is.atomic(x) && is.numeric(x)) {
    tmp <- omit2.(x) %>% {. -median(.)}
    tmp_posi <- tmp[tmp >= 0] %>% sum()
    tmp_nega <- tmp[tmp < 0] %>% {-sum(.)}
    return((tmp_posi -tmp_nega) /(tmp_posi +tmp_nega))
  } else {
    NA_real_
  }
  if (is.atomic(x)) {
    balance0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) balance0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## Gini coefficient
gini. <- function(x) {
  gini0. <- function(x) {
    if (is.atomic(x) && is.numeric(x)) {
      x <- omit2.(x) %>% .[order(., decreasing = F)]
      num <- 1:length(x)
      y <- cumsum(x)/sum(x)
      AB <- length(x)/2  # triangle
      B <- sum(y)
      gini <- (AB-B) /AB
      return(gini)
    } else {
       NA_real_
    }
  }
  if (is.atomic(x)) {
    gini0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) gini0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## Hurst exponent: https://ito-hi.blog.ss-blog.jp/2016-10-15
hurst. <- function(x) {  # the trend keeping ability random ~ 0.5, plus regular ~ 0.5-1, minus ~ 0-0.5
  if (is.atomic(x)) {
    omit2.(x) %>% {pracma::hurstexp(., display = F)$Hs}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {pracma::hurstexp(., display = F)$Hs} else NA_real_)
  } else {
    NA_real_
  }
}

## crest factor:= peak /RMS
cf. <- function(x) {
  if (is.atomic(x) && is.numeric(x)) {
    omit2.(x) %>% {max(.) /rms.(.)}
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) omit2.(.) %>% {max(.) /rms.(.)} else NA_real_)
  } else {
    NA_real_
  }
}

## spectral flatness measure := geometric mean /arithmetric mean
sfm. <- function(x) {
  sfm0. <- function(x) {
    if (is.atomic(x) && is.numeric(x)) {
      if (mean(x, na.rm = T) != 0) {
        omit2.(x) %>% {geo_mean.(.) /mean.(.)}
      } else {
        NA_real_
      }
    } else {
      NA_real_
    }
  }
  if (is.atomic(x)) {
    sfm0.(x)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) sfm0.(.) else NA_real_)
  } else {
    NA_real_
  }
}

## Mean crossing rate
mcr. <- function(x, zero = F) {
  ## https://www.researchgate.net/publication/323935725_Analyzing_User_Emotions_via_Physiology_Signals
  ## https://slimelimestech.hatenablog.com/entry/2019/10/03/231228
  ## https://www.sciencedirect.com/topics/engineering/zero-crossing-rate
  mcr0. <- function(x, zero) if (is.atomic(x) && is.numeric(x)) {
    target <- if (zero == TRUE) 0 else mean(x, na.rm = T)  # or zero crossing rate
  # dplyr::case_when(x -target >= 0 ~ 1, TRUE ~ -1) %>% {. *lag(.)} %>% .[!is.na(.)] %>% {dplyr::case_when(. < 0 ~ 1, TRUE ~ 0)} %>% mean
    omit2.(x) %>% {dplyr::case_when(. >= target ~ 1, TRUE ~ -1)} %>% {mean((. -lag(.)) /2, na.rm = T)}
  } else {
    NA_real_
  }
  if (is.atomic(x)) {
    mcr0.(x, zero)
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.)) mcr0.(., zero) else NA_real_)
  } else {
    NA_real_
  }
}

which.max. <- function(x, Nth = 1) {  # which.max0.(c(9, NA, 8:1), 1:2)  which.max0.(c(1,1,1))
  which.max0. <- function(x, Nth = 1:1) {
    if (is.atomic(x) && is.numeric(x) || is_time.(x)) {
      sort(x, decreasing = T, index.return = T) %>% {set_names(.$ix, .$x)[Nth]}
    } else NA_real_
  }
  if (is.atomic (x)) {
    which.max0.(x, Nth)
  } else if (is.list(x)) {
    ten_num <- list2tibble.(x) %>% select_if(~is.numeric(.)) %>% which.min()
    ten_time <- list2tibble.(x) %>% select_if(~is_time.(.)) %>% which.min()
    if (length(ten_num) == 0 && length(ten_time) != 0) return(ten_time)
    if (length(ten_num) != 0 && length(ten_time) == 0) return(ten_num)
    if (length(ten_num) != 0 && length(ten_time) != 0) return(bind_cols(ten_num, ten_time))
    if (length(ten_num) == 0 && length(ten_time) == 0) return(1 %>% set_names(names(x)[1]))  # In case of all NA columns
  } else {
    NA_real_
  }
}

which.min. <- function(x, Nth = 1) {  # which.min0. (c (3,-10,5,-88), 1:2)
  which.min0. <- function(x, Nth = 1:1) {
    if (is.atomic(x) && is.numeric(x) || is_time.(x)) {
      sort(x, decreasing = T, index.return = T) %>% {set_names(.$ix, .$x)[Nth]}
    } else NA_real_
  }
  if (is.atomic(x)) {
    which.min0.(x, Nth)
  } else if (is.list(x)) {
    ten_num <- list2tibble.(x) %>% select_if(~is.numeric(.)) %>% which.max()
    ten_time <- list2tibble.(x) %>% select_if(~is_time.(.)) %>% which.max()
    if (length(ten_num) == 0 && length(ten_time) != 0) return(ten_time)
    if (length(ten_num) != 0 && length(ten_time) == 0) return(ten_num)
    if (length(ten_num) != 0 && length(ten_time) != 0) return(bind_cols(ten_num, ten_time))
    if (length(ten_num) == 0 && length(ten_time) == 0) return(1 %>% set_names(names(x)[1]))  # In case of all NA columns
  } else {
    NA_real_
  }
}

sum. <- function(x) {  # Note; sum function eats T/F
  if (is.atomic(x) && is.numeric(x) || is.logical(x)) {
    omit2.(x) %>% sum()
  } else if (is.list(x)) {
    list2tibble.(x) %>% map_df(~ if (is.numeric(.) | is_time.(.) | is.logical(.)) omit2.(.) %>% sum() else NA_real_)
  } else {
    NA_real_
  }
}

length. <- function(x) {
  if (is.atomic(x)) {
    omit2.(x) %>% length(.)
  } else {
    if (is.list(x)) sapply(x, function(y) y[!is.na(y)] %>% length(.)) else NA_real_
  }
}
n_factor. <- function(x) {
  if (is.atomic(x)) length.(x) else if (is.data.frame(x)) ncol(x) else if ('list' %in% class(x)) length(x) else NA_real_
}


## dividing data into list with the size on 0~1 ratio == (2022-05-31) ========================
## overlapping sliding window-based segmentation; imagine a trump sliding
## called by https://www.researchgate.net/publication/323935725_Analyzing_User_Emotions_via_Physiology_Signals
## proposed by https://www.fbs.osaka-u.ac.jp/labs/ishijima/FFT-09.html
## [1:150] by block = 7 --> [1:21], [22:42], ... [127:147], you can also change sliding width percent
## Note: .f must be vector function, i.e., use not nrow() but length()
data_slider. <- function(d, overlap = 0, block = 3, len = NULL, .f = NULL, .col = 1, chained = T, ...) {
  if (overlap < 0 || overlap >= 1) stop('    The overlap ratio among sliders must be [0, 1)...\n\n', call. = F) 
  d <- list2tibble.(d) %>% rowid_to_column('id')  # to remind the original row number
  window <- len %||% {nrow(d) %/% block}  # division sytle; block or length
  sliding_pitch <- trunc(window *(1 -overlap))  # if the overlap ratio = 0, it'll be a simple division of data
  if (window == 0) stop('The block is larger than the data length...\n\n', call. = F)

  srt <- seq(1, nrow(d), by = sliding_pitch)
  end <- srt +window -1
  if (chained == TRUE) {
    end <- end %>% ifelse(. > nrow(d), nrow(d), .) %>% unique  # correct extra end number
  } else {
    end <- end[end <= nrow(d)]  # cut extra data in the end to divide the data into the same number
  }
  dL <- list()
  for (i in seq_along(1:min(length(srt), length(end)))) dL[[i]] <- i %>% {srt[.]:end[.]} %>% d[., ]
  if (is.null(.f) == TRUE) {
    return(dL)  # return a divided raw data as list
  } else if (class(.f) == 'function') {
    stop('Please make .f define in your namespace and input function name as character like .f = \'f\' not .f = f ...\n\n', call. = F)
  } else {
    ## when using a function, you must point the column number that is calculated
    if (length(.col) > 1 || .col < 1 || .col > ncol(d) -1) stop('    Select one column...\n\n', call. = F)
    if (chained == TRUE) {  # the divided list data should be binded as chained like time series
      fun_names <- names(d)[1 +.col] %>% str_c(.f, '_', .)
      tmp <- map_dfr(dL, function(x) {
               dL2 <- list(map_df(x[1], mean))  # ID included
               for (j in seq_along(fun_names)) {
                 dL2 <- c(dL2, map_df(x[1 +.col], get(.f[j])) %>% set_names(fun_names[j]))
               }
               bind_cols(dL2) %>%
               return()
             })
    } else {  # the divided list as cyclic data --> parallel --> rowwise mean
      tmp <- skipMess.(map_dfc(dL, function(x) map_df(x[-1] %>% .[.col], get(.f)))) %>%  # ID skipped
             mutate(y = rowMeans(., na.rm = T)) %>% select(y)  # integrated into mean
    }
    return(tmp)  # return the calculated data frame
  }
# Note: you cannot use any arguments... 'mean' is OK, but 'mean(x, na.rm = T)' is NG
# data_slider.(iris, overlap = 0.5, block = 15)
# data_slider.(iris, len = 10, .f = c('mean', 'sd'), .col = 2)
# plt.(iris[1]);  data_slider.(iris, overlap = 0.5, len = 10, .f = 'mean', .col = 1) %>% plt.(add = 2, col = 'blue')
# ff <- function(x) Mod(fft(x)) /(length(x)/2);  data_slider.(iris, overlap = 0.5, block = 15, .f = 'ff', .col = 1, chained = F)
}  


## Numeric conversion of lower and/or upper bounds generated by cut() == (2022-01-26) ========================
## [0,0.5], (0.5,1]
## https://stackoverflow.com/questions/32356108/output-a-numeric-value-from-cut-in-r
cut_borders. <- function(x) {
  pattern <- '(\\(|\\[)(-*[0-9]+\\.*[0-9]*),(-*[0-9]+\\.*[0-9]*)(\\)|\\])'
  start <- as.numeric(gsub(pattern, '\\2', x))
  end <- as.numeric(gsub(pattern, '\\3', x))
  tibble(start, end)
# cut_interval(iris$Sepal.Length, length = 0.5) %>% cut_borders.(.)
}
cut_centers. <- function(x) {
  chrs <- gsub(',', '+', x) %>% gsub('\\[', '\\(', .) %>% gsub('\\]', '\\)', .) %>% str_c(., '/2')
  out <- sapply(chrs, function(y) eval(parse(text = y)))
  return(out)
# iris[1] %>% mutate(interval = cut_width(Sepal.Length, width = 1)) %>% group_by(interval) %>% summarise(nya = sum(Sepal.Length)) %>% mutate(center = cut_centers.(interval))
}



## Peak detection == (2022-02-25) ========================
## https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
## https://github.com/stas-g/findPeaks
## https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima
find_peaks. <- function (d, m = 10, peaklen = 10, peakmax = Inf, type = c('p', 'v')[1], boost = F, lr = F, ...) {  # allowed [y] or [x,y]
  ## m means range of mini-calculation, boost uses lowess, lr contains local max in the skirt
  d <- list2tibble.(d) %>% dplyr::filter(rowSums(is.na(.)) != ncol(.))
  if (all(sapply(d, is.numeric)) && ncol(d) <= 2) {
    if (ncol(d) == 1) d <- d %>% rowid_to_column('index')
    if (boost == TRUE) {
      x <- ksmooth(d[[1]], d[[2]], bandwidth = 2)$y  # lowess(d, f = 0.01)$y
    } else {
      x <- d[[2]]
    }
  } else {
    stop('No proper form...\n\n', call. = F)
  }
  if (type == 'v') x <- -x  # when seeking valley
  shape <- diff(sign(diff(x, na.pad = F)))  # reduced the number; original -2
  pks <- sapply(which(shape < 0), function(i) {  # shape < 0
    z <- {i -m +1} %>% {ifelse(. > 0, ., 1)}
    w <- {i +m +1} %>% {ifelse(. < length(x), ., length(x))}
    if(all(x[c(z:i, (i+2):w)] <= x[i+1])) return(i+1)  # CAUTION: not '<' but '<=' for any data to work well
  }) %>% unlist(.)
  if (lr == TRUE) {  # add local values in the skirt even though they are not peaks
    pks <- c(pks, which.max(x[1:10]), which.max(x[(length(x)-9) : length(x)]) +length(x)-10) %>% sort(.)
  }

  ## select strong peaks
  if (is.null(pks)) {
    return(NA_real_)
  } else {
    pkL <- group_index.(pks, len = peaklen)
    pks_row <- map.(pkL, function(i) which.max(x[i]) %>% i[.])
    out <- d[pks_row, ] %>% {.[which(.[[2]] <= peakmax), ]}
    return(out)
  }
# plt.(Nile); points(find_peaks.(Nile, m = 10, peaklen = 20, peakmax = 715, type = 'v', boost = F, lr = T))
# find_peaks.(c(3,2,1,2,3), type='v')  find_peaks.(4:1, type='v')  find_peaks.(c(4,4,3,3,2,2,1,1), type='v')  find_peaks.(c(1,2,rep(10,10)), type='v')
}


## Area of polygon (widely applicable to any polygon or closed curve with x order like convex-hull) == (2019-12-17) ==
area_poly. <- function(x, y, ...) {
  ## Newton-Cotes formulae (to only area surrounding x-axis and a curve): sum(0.5 * diff(x) *(y[-1] +y[-length(y)]))
  if (length(x) != length(y)) stop('x and y do not have the same length.\n\n', call. = F)
  def.(c('x2', 'y2'), list(c(x[-1], x[1]), c(y[-1], y[1])))  # [x1, ... , xn] --> [x2, ... , xn, x1]
  calc <- abs(sum(x *y2 -x2 *y)) /2  # psd data; polygon type = 1.004, Newton type = 1
  return(calc)
}


## Area closed by curve & x axis (oriented for pdf curve) == (2020-06-05) ========================
area. <- function(x = NULL, y = NULL, ...) {
  if (is.list(x) && is.null(y)) def.(c('x', 'y'), list(x[[1]], x[[2]]))
  sum(0.5 * diff(x) *(y[-1] +y[-length(y)])) %>% return(.)
}  # area.(psd[2:3])


## Partial Area (Only applicable for PDF; whose cuve is surrounding x axis) == (2020-01-23) ========================
area_part. <- function(d, LRx, ...) {  # dt is PDF, LRx is partial range of x
  rowRange <- whichNear.(d$x, c(LRx[1], LRx[2])) %>% {.[1] : .[2]}
  def.(c('x', 'y'), list(d$x[rowRange], d$y[rowRange]))
  calc <- sum(0.5 *diff(x) *(y[-1] +y[-length(y)]))
  return(calc)
}


## Cumulative denstiy function == (2022-05-12) ========================
## Note: this is the function for a PDF, not for a vector like quantile(iris[[1]], 0.5)
## Microtrac D50 of psd[2:3] was 9.99006 (almost near peak)
## ~ cdf.(psd[2:3], p = 0.36) ~ psd[[2]][whichNear.(vec = cumsum(psd[[3]]) /sum(psd[[3]]), ref = 0.37)]
cdf. <- function(x, y = NULL, p = NULL, norm = T, ...) {
  if (is.data.frame(x) && ncol(x) == 2) def.(c('x', 'y'), list(x[[1]], x[[2]]))
  y_cum <- cumsum(0.5 *diff(x) *(y[-1] +y[-length(y)])) %>% c(0, .)
  if (norm == TRUE) y_cum <- (y_cum -min(y_cum)) /delta.(y_cum)
  out <- tibble(x = x, y = y_cum)
  if (!is.null(p) && all(between(p, 0, 1))) {
    out <- whichNear.(vec = out$y, ref = p) %>% out$x[.]  # return percentile (not quantile!!)
  }
  return(out)
}  # plot(psd[2:3]); abline(v=cdf.(psd[2:3], p=0.5)); par(new=T); plot(cdf.(psd[2:3]), type='l', ann=F, axes=F); axis(4)


## AIC calculation == (2019-05-11) ========================
aic. <- function(model, ...) if (is.null(model) || is.na(model)) NA else model %>% {-2 *logLik(., REML = F)[1] +2 *(length(coef(.)) +1L)}
aic2. <- function(model, ...) {
  if (is.null(model) || is.na(model)) return(NA)
  qy <- fitted(model) %>% .[. > 0]  # Sometimes a model returns minus fitted values partially and then results in NaN or Inf.
  return( -1 /length(qy) *sum(log(qy)) +(length(coef(model)) +1L) /length(qy))
}


## Other Information Criteria (OIC ?) calculation == (2022-05-12) ========================
## summary(mdl)$sigma
rmse. <- function(model) if (is.null(model) || anyNA(model)) NA else model %>% {sqrt(deviance(.) /(length(fitted(.)) -length(coef(.))))}
## Deviance
dev. <- function(model) if (is.null(model) || anyNA(model)) NA else model %>% {log(2 *pi *deviance(.) /length(fitted(.))) +1}
## Estimated relative error
ere. <- function(model) if (is.null(model) || anyNA(model)) NA else model %>% {deviance(.) /sum((fitted(.)) ^2)}
## BIC
bic. <- function(model) if (is.null(model) || anyNA(model)) NA else model %>% BIC()
## deviance
deviance. <- function(model) if (is.null(model) || is.na(model)) NA else model %>% deviance()


## Return with no error-stop risk? == (2023-07-18) ========================
tryReturn. <- function(yourtry) suppressWarnings(try(yourtry, silent = T) %>% {if ('try-error' %in% class(.)) NA else .})
# tryReturn.(nlsLM(y ~ x, start = ... ))  tryReturn.(date('123456-1-2'))


## Subdivide a vector into each group list == (2022-04-11) ========================
portion. <- function(x, div = 3, ...) {  # ID = 1:7 extracted each for 2 --> [ [1,2],[3,4],[5,6],[7] ]
  if (!is.atomic(x)) stop('Use only a vector...\n\n', call. = F)
  id <- seq_along(x)
  div_int <- {length(id) %/% div} + dplyr::if_else(length(id) %% div == 0, 0, 1)
  if (length(id) <= div) return(list(x))

  tmpL <- list()
  for (i in seq(div_int)) {
    tmpL[[i]] <- {seq(div) +div *(i-1)} %>% {.[. <= last(id)]} %>% x[.]
  }
  return(tmpL)
}  # portion.(LETTERS, 5)  portion.(list.files(pattern = 'csv'), 10)


## Divide index into groups for find_peaks.() == (2022-02-20) ========================
group_index. <- function(x, len = 10, ...) {
  gL <- vector('list', length(x))
  for (i in seq_along(x)) {
    if (i == 1) {
      ctr <- 1
      gL[[ctr]] <- x[i]
    } else {
      if (x[i] -x[i-1] > len) ctr <- ctr +1
      gL[[ctr]] <- c(gL[[ctr]], x[i])
    }
  }
  out <- map_lgl(gL, ~ !is.null(.)) %>% gL[.]
  return(out)
}  # group_index.(c(2,41,42,45,50,63,64,106,143,145,148,249,286,288,291,332,364,395,431,434,436,540,576,579,581,631,683,720,722,725,772,827,864), 20)


## Find the interval points on both sides of local plus/minus change in vector == (2023-09-07) ========================
interval2. <- function(vec, valley = T, ... ) {  # True means a "single" local valley
  vec <- lowess(seq_along(vec), vec, f = 0.3)$y  # stepwise data make a confusion
  vecTF <- map_dbl(diff(vec), ~ if (abs(.) < 1e-09) 0 else .) %>% {if (valley) . >= 0 else . < 0}  # Search for a valley or peak
  if (sum(vecTF) %in% c(0, length(vecTF))) {
    if (valley == T && sum(vecTF) == 0) return(length(vec))  # Find a valley at the end
    if (valley == T && sum(vecTF) == length(vecTF)) return(1)  # Find a valley at the start
    if (valley == F && sum(vecTF) == 0) return(length(vec))  # Find a peak at the start
    if (valley == F && sum(vecTF) == length(vecTF)) return(1)  # Find a peak at the end
  } else {  # 2 Numbers on the change point
    if (valley == TRUE) {
      vecTF %>% diff(.) %>% {which(. == 1)[1]} %>% {c(., . +2)} %>% {if (anyNA(.)) which.min(vec)[1] else .} %>% return(.)
    } else {
      vecTF %>% diff(.) %>% {which(. == 1)[1]} %>% {c(., . +2)} %>% {if (anyNA(.)) which.max(vec)[1] else .} %>% return(.)
    }
  }
# interval2.(vec = c(3,2,1,2,3), T)  interval2.(4:1)  interval2.(c(4,4,3,3,2,2,1,1))  interval2.(c(1,2,rep(10,10)))
# c(.883,.655,.377,.234,.125,.050,  .009,.001,.010,  .054,.089,.132,.183,.243,.311,.388,.474,.568,.670,.670,.781) %>% {.[interval2.(.)]}
}


## Find sequencial vector list, dividing sequential numbers into small groups == (2019-01-14) ========================
seqCtr. <- function(hit, Seq = 1, ...) {  # 'hit' is the target number out of the rule, and 'magicSeq' is the sequence number.
  if (all(is.na(hit))) return(NA)
  hit <- hit[!is.na(hit)]  # hit <- c(1,2,9,10,11,14,17,18,19,22,40,41,42)
  dummy <- nth(hit, 1) :nth(hit, -1)  #  NOTE: both of the initial and last 'dummy' always are not NA.
  dummy[!dummy %in% hit] <- NA
  def.(c('grp', 'grpList', 'j'), list(vector(), list(), 1))
  for (i in seq_along(dummy)) {
    if (!is.na(dummy[i])) grp <- c(grp, dummy[i])
    if (is.na(dummy[i]) && !is.logical(grp) || i == length(dummy)) {
      grpList[[j]] <- grp
      grp <- vector()
      j <- j +1
    }
  }
  grpList <- {lapply(grpList, length) >= Seq} %>% grpList[.]  # Cropping larger than 'Seq'
  if (length(grpList) == 0) grpList <- NA  # In case that grpList = list () because of too large magicSeq or none of sequance.
  return(grpList)
}  # which(hit > 15) %>% seqCtr.(., Seq = 5)  :=  trueList.(hit > 15) %>% {.[map.(., ~ length(.) > 5)]}


## Pick up only true vector and return their list == (2023-03-02) ========================
trueList. <- function(tf_vec, .seq = 1,　tf = F,  ...) {
  def.(c('ctr', 'ctr_list', 'grp_list'), list(vector(), 1, list()))
  for (i in seq_along(tf_vec)) {
    if (tf_vec[i] == TRUE) {
      ctr <- c(ctr, i)
      if (i == length(tf_vec)) {
        grp_list[[ctr_list]] <- ctr
      }
    } else if (i > 1 && tf_vec[i-1] == TRUE && tf_vec[i] == FALSE) {
      grp_list[[ctr_list]] <- ctr
      ctr_list <- ctr_list +1
      ctr <- vector()
    }
  }
  grp_list <- map_lgl(grp_list, ~ length(.) >= .seq) %>%
             grp_list[.]
  if (tf == TRUE) grp_list <- seq_along(tf_vec) %in% unlist(grp_list)

  return(grp_list)
# trueList.(c(T,F,F, T,T,T,T), .seq = 3)
# iris %>% filter(trueList.(Sepal.Length > 5, .seq = 35, tf = T))
}


## Fast Anomaly Detection == (2020-01-11) ========================
cFilter. <- function(vec, shaper = 5, ...) {  # More odd shaper, more vivid change
  scaler1 <- length(vec) ^shaper *exp((mean.(vec) /sd.(vec)) ^(-1))  # Keep uniform for vector length in a way like coefficient of variation
  scaler2 <- 10 ^(-2 *shaper) *2 ^((shaper -3) /2)  # Keep uniform for many shapers
  vec %>% scale(.) %>% {cumsum(.) /length(.)} %>% {-. ^shaper} %>% {. *scaler1 *scaler2}
}
## 1st prototype == (2020-01-16) ========================
fad0. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3
  scaler1 <- (1 *length (vec)) ^shaper *exp((mean.(vec) /sd.(vec)) ^(-1) )  # "
  scaler2 <- 10 ^(-2 *shaper) *2 ^((shaper -3) /2)  # "
  balancer <- c(0, 0, diff(vec, differences = 2) ^shaper) %>% scale(.)  # Improvement
  vec %>% scale(.) %>% {cumsum(.) /length(.)} %>% {-. ^shaper} %>% {. *scaler1 *scaler2 *balancer}  # Watch the last term
}
## 2nd keen to outbreak == (2020-01-17) ========================
fad_keen. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3 (only odd)
  zero_leveler <- function(x) dplyr::case_when(abs(x) < 1e-20 ~ 0, x == Inf ~ 0, TRUE ~ x)  # Fear of float Inf effect approx. 0 on diff() or . /.
  seq_diff <- function(x) {
    c(0, 0, diff(x, differences = 2)) %>%
    {sgn.(.) *. ^(shaper -1)} %>%
    zero_leveler(.) %>%
    {if ({. == 0} %>% all(.)) rep(0, length(x)) else .}
  }
  sigmoid_weight <- {10 /length(vec)} %>% {(1 +exp(-. *(seq_along(vec) -length(vec) /2))) ^(-1)}
  mu_increment <- cumsum(vec) /seq_along(vec)
  var_increment <- {1 /(seq_along(vec) -1) *(cumsum(vec ^2) -1 /seq_along(vec) *(cumsum(vec)) ^2)}
  var_increment[1] <- var_increment[2]
  punisher <- log(sigmoid_weight *((mu_increment -vec) ^2 +var_increment))
  stabler <- 2 ^((shaper -3) /2)  # To control variation due to many shapers
  out <- seq_diff(vec) %>% {cumsum(scale(.)) /100} %>% {-. ^shaper} %>% {. *punisher *stabler} %>% zero_leveler(.) %>% {sgn.(.) *log(1 +abs(.))}
  return(out)
}
## 3rd robust to change, for Melon analysis == (2020-01-19) ========================
fad. <- function(vec, shaper = 3, ...) {  # Strongly recommended shaper = 3 (only odd)
  zero_leveler <- function(x) dplyr::case_when(abs(x) < 1e-20 ~ 0, x == Inf ~ 0, TRUE ~ x)  # Fear of float Inf effect approx. 0 on diff() or . /.
  seq_diff <- function(x) {
    c(0, 0, diff(x, differences = 2)) %>%
    {sgn.(.) *. ^(3 -1)} %>%
    zero_leveler(.) %>%
    {if ({. == 0} %>% all(.)) rep(0, length(x)) else .}
  }
  sigmoid_weight <- {10 /length(vec)} %>% {(1 +exp(-. *(seq_along(vec) -length(vec) /2))) ^(-1)}
  mu_increment <- cumsum(vec) /seq_along(vec)
  var_increment <- {1 /(seq_along(vec) -1) *(cumsum(vec ^2) -1 /seq_along(vec) *(cumsum(vec)) ^2)}
  var_increment[1] <- var_increment[2]
  punisher <- log(sigmoid_weight *((mu_increment -vec) ^2 +var_increment)) %>% {dplyr::case_when(. == Inf ~ 1, . == -Inf ~ 1, TRUE ~ .)}
  stabler <- 10 ^(length(vec) %>% log10(.) %>% {floor(.) +2}) *2 ^(6 *(shaper -3))
  out <- {seq_diff(vec) *seq_diff(punisher)} %>% {cumsum(scale(.)) /length(vec)} %>% {-. ^shaper *stabler}# %>% {sgn.(.) *log(1 +abs(.))}
  return(out)
}
# vec <- psd[[1]]; plot(vec, type = 'l', lwd = 0.8); lines(cpDetecter.(vec), type = 'l', col = 'yellow')
# par(new = T); plot(cFilter.(vec), type = 'l', lwd = 0.8, col = 'darkseagreen', axes = F); axis(4, col.axis = 'darkseagreen')
# par(new = T); plot(fad0.(vec), type = 'l', lwd = 0.8, col = 'blue', axes = F)


## Change points detection == (2021-08-12) ========================
cpDetecter. <- function(vec, Lper = 0.05, entryRate = 0.25, gapRate = 0.60, ...) {
## https://qiita.com/hoxo_m/items/1afa288178422fad9076
## https://speakerdeck.com/hoxom/bi-nu-nizhen-rarenaitamefalsebian-hua-jian-zhi-ru-men
## https://www.slideshare.net/siroyui/20160924-r-57
## PELT (Pruned Exact Linear Time) is the fastest & most precise to seek several change points.
## CROPS (Change points for a Range Of PenaltieS) makes decision of optimized change points. Without it, there produce too many points.
## The original cpt program can detect even tiny change but industrial trend requires a big change so that any can recognize it as cpt.
## NOTE: changepoint package doesn't follow NA action.
## NOTE: 'minseglen' := minmum of cut segment length: Kill neighbors in the same large deviation
  query_lib.(changepoint)
  if (is.data.frame(vec) && ncol(vec) == 1) vec <- unlist(vec)
  vec0 <- vec  # vec0 (original) --> vec (no NA) --> vec1 (mean bars in the whole span)
  if (anyNA(vec)) vec <- vec0[!is.na(vec0)]
  if (length(vec) < 10) stop('    Element length of the object is too short, ', length(vec), '\n\n', call. = F)
  penValues <- c(5, 1000)  # Very significant
  Minseg <- ifelse(length(vec) < 100, 5, ceiling(length(vec) *Lper))  #Any time it's larger than 5. Try Lper (length %).
  invisible(capture.output(  # cpt.meanvar() produces a model but also crap output: Needed invisible (cap ~)
    craps <- changepoint::cpt.meanvar(vec, method = 'PELT', penalty = 'CROPS', pen.value = penValues, minseglen = Minseg)
  ))
  ## 1) List up x position of cpts with some penalty
  ten0 <- craps %>% changepoint::cpts.full(.) %>% as.data.frame(.)  # NOTE:  6 is my limit numer of. cpts
  selectCptRow <- pmap(ten0, ~ is.na(c(...)) %>% {ncol(ten0) -sum(.)}) %>% {which(. <= 6)} %>% {
                    if (length (.) != 0) {
                      min.(.) : nrow(ten0)
                    } else {
                      1 : nrow(ten0)
                  }}
  ten1 <- ten0[selectCptRow, ] %>% as.data.frame(.)  # NOTE: if fully deleted it will change into integer.
  players <- unlist(ten1) %>% table(.)
  players[seq(players)] <- 0  # Once reset zero
  if (length (players) > 0) {
    for (i in seq(nrow(ten1))) {for (j in seq(ncol(ten1))) {
      if (!is.na(ten0[i, j])) {
        ijPos <- which(names(players) %>% parse_number(.) == ten1[i, j])
        players[ijPos] <- players[ijPos] +changepoint::pen.value.full(craps)[selectCptRow][i]
      }
    }}  # This method always leave 1 cpt at least.
  }
  players <- players /max.(players)  # plot(vec, type = 'l'); abline(v = names(players) %>% parse_number(.))
  players <- players[players > entryRate]  # TRY AGAIN  # plot(vec, type = 'l'); abline(v = names(players) %>% parse_number(.))
  ## plot(craps, cpt.width = 1.5, cpt.col = 'blue', ncpts = whichSize.(ref = length(vec), vec = c(30, 80, 150, 300), c(1, 3, 3, 6)))
  ## ten0
  ## NOTE: cpts' candidates in ten0 are placed at NOT the start but the END of avg lines.
  ## pen.value.full(craps)
  ## plot(craps, diagnostic = T); lines(seq(pen.value.full(craps)) -1, rev(pen.value.full(craps)), col = 'blue', lwd = 3, lty = 3)

  ## 2) Check how vivid the changes are.
  gap <- gapRate *sd.(vec)  # Very touchable on you observer for its stepwise appearance
  xcpt <- names(players) %>% as.numeric() %>% c(1, ., length(vec))
  finalSelect1 <- function(...) {  # Updating with re-calculation of avg
    if (length(xcpt) > 2) {
      j <- 1
      while (j <= 3) {  # Difference situation will change once any points vanish.
        muSeg <- rep(NA_real_, length(xcpt))
        for (i in seq_along(muSeg) [-1]) muSeg[i-1] <- mean.(vec[xcpt[i-1] : xcpt[i]])
        bigPoint <- which(abs(diff(muSeg)) >= gap) %>% {xcpt[. +1]}
        xcpt <- c(1, bigPoint, length(vec))
        j <- j +1
      }
      if (length(xcpt) > 2) {  # Kill a change point with tiny band
        del_tiny_Xwidth <- which(diff(xcpt) <= 2) %>% c(., . +1) %>% unique(.)
        if (length(del_tiny_Xwidth) != 0) xcpt <- xcpt[-del_tiny_Xwidth]
      }
    }
    return(xcpt)
  }
  finalSelect2 <- function(...) {  # No considering update with re-calculation of avg
    if (length(xcpt) > 2) {
      muSeg <- rep(NA_real_, length(xcpt) -1)
      for (i in seq_along(muSeg)) muSeg[i] <- mean.(vec[xcpt[i] : xcpt[i+1]])
      bigPoint <- diff(muSeg) %>% {which(abs(.) >= gap)} %>% {xcpt[. +1]}
      xcpt <- c(1, bigPoint, length(vec))
    }
    return(xcpt)
  }
  xcpt <- finalSelect1()  # plot(vec, type = 'l'); abline(v = finalSelect1())
# xcpt <- finalSelect2()  # plot(vec, type = 'l'); abline(v = finalSelect2())

  ## 3) Assign all the element numbers on each of mean Yi or NA
  ## Prepare avgs' vector including NA
  xseg <- {xcpt[- c(1, length(xcpt))] +1} %>% c(xcpt, .) %>% sort() %>% matrix(., ncol = 2, byrow = T)  # Shift 1 for segmentation
  vec1 <- rep(NA_real_, nrow(xseg))
  for (i in seq_along(vec1)) vec1[xseg[i, 1] : xseg[i, 2]] <- mean.(vec[xseg[i, 1] : xseg[i, 2]])
  vec0[!is.na(vec0)] <- vec1  # At this moment, vec0 has been rewritten to avg info with NAs.
  ## Insert 2 medium points at each of xcpts. This is just a gimmic so that the cpt graph looks stepwise at cpt points
  xcpt2 <- xcpt[-c(1, length(xcpt))] # If skip NA, lines() draws a single line. But the procedure below draws multiple lines in NA space
  num <- vec0  # xcpt2 is based on vec without NA, and needs adjusted to numbering with NA inserting
  num[!is.na(num)] <- which(!is.na(num)) %>% seq()
  dtCpt <- tibble(num = num, x = seq(vec0), y = vec0)    #  x is changing but num keeps original element numbers
  for (i in seq_along(xcpt2)) {
    iRow <- which(dtCpt$num == xcpt2[i]) # Considered a data frame after i-th inserting
    xInsert2 <- iRow %>% dtCpt[., 'x'] %>% pull(.) %>% {rep(. +0.5, times = 2)}  # It's very confusing...
    yInsert2 <- dtCpt[iRow : (iRow +1), 'y'] %>% pull(.)
    dtCpt <- add_row(dtCpt, num = NA, x = xInsert2, y = yInsert2, .after = iRow)
  }
  df <- data.frame(dplyr::select(dtCpt, - num))  # NOTE: lines() cannot work with tibble style
  return(df)  # In case of no change point, it returns just a all avg line
   # plot(vec, type = 'l'); lines(df, lwd = 3.5, col = 'dodgerblue3')
}  # plot(psd[[1]], type = 'l'); lines(cpDetecter.(psd[[1]]), col = 'blue3', lwd = 1.5)


## My built-in data sets == (2020-10-02) ========================
psd <- tibble::tibble (
  D50_trend = c(10.3505,10.3097,9.4281,9.2974,10.3038,10.0402,10.2097,10.0547,10.1309,10.1652,10.1814,10.0664,10.0646,9.8605,9.9342,9.7808,9.9886,9.7485,9.5912,9.9526,9.5072,9.6256,9.483,9.7851,9.7415,9.8569,9.8133,9.7731,9.831,9.8736,10.1973,10.1464,10.0612,10.0225,9.8804,9.8155,9.9157,9.9467,9.4711,9.3546,9.4517,9.75,10.0349,9.4525,9.4903,9.4697,9.4011,10.311,10.0956,10.2623,9.9476,9.9276,9.4301,9.4204,9.2182,10.2067,10.2306,10.2957,9.8977,10.0762,10.0518,10.2966,10.2803,10.2331,10.1465,10.0151,10.3837,10.0558,10.3344,9.0899,9.1398,9.1047,9.1635,8.618,9.0998,9.3331,9.2716,9.1881,9.2299,9.2615,9.0782,9.2421,9.2541,9.2267,9.1766,9.0327,8.8731,8.8742,8.8404,9.0275,8.9354,9.1421,8.8086,8.9932,9.0468,9.4939,9.5158,9.3488,9.3252,9.4686,9.2631,8.9989,9.0577,9.0464,8.8967,9.0058,9.2276,9.0795,9.0424,8.965,8.9938,9.0738,9.0631,8.9404,8.8106,8.7996,9.2329,8.8762,9.2519,9.2447,9.1554,9.2142,9.1968,9.1114,9.1923,9.1031,9.103,9.2033,9.2678,9.1331,9.2128,9.334,9.1667,9.2228,9.209,9.2497,9.2587,9.2215,9.0974,9.0589,8.9901,9.227,9.2319,9.1942,9.1959,9.2053,9.2142,9.1342,8.449,8.4339,8.5195,8.4926,8.2906,8.4999,8.4717,8.4853,8.4439,8.4278,8.4237,8.5516,8.7148,8.7185,8.7048,8.7232,8.4943,8.439,8.3935,8.6768,8.4648,8.5077,8.7209,8.8719,8.6227,8.3717,8.5017,8.7542,8.4899,8.4815,8.723,8.6122,8.5992,8.4312,8.641,8.6623,8.6808,9.1556,8.5823,8.5338,8.8629,8.8065,8.955,8.9868,8.6425,8.9018,8.9346,9.0891,9.1269,8.974,8.7781,8.7591),
  x_IMPMNN30_0612 = c(4.52,4.66597989949749,4.81195979899497,4.95793969849246,5.10391959798995,5.24989949748744,5.39587939698492,5.54185929648241,5.6878391959799,5.83381909547739,5.97979899497487,6.12577889447236,6.27175879396985,6.41773869346734,6.56371859296482,6.70969849246231,6.8556783919598,7.00165829145729,7.14763819095477,7.29361809045226,7.43959798994975,7.58557788944724,7.73155778894472,7.87753768844221,8.0235175879397,8.16949748743719,8.31547738693467,8.46145728643216,8.60743718592965,8.75341708542713,8.89939698492462,9.04537688442211,9.1913567839196,9.33733668341709,9.48331658291457,9.62929648241206,9.77527638190955,9.92125628140704,10.0672361809045,10.213216080402,10.3591959798995,10.505175879397,10.6511557788945,10.797135678392,10.9431155778894,11.0890954773869,11.2350753768844,11.3810552763819,11.5270351758794,11.6730150753769,11.8189949748744,11.9649748743719,12.1109547738693,12.2569346733668,12.4029145728643,12.5488944723618,12.6948743718593,12.8408542713568,12.9868341708543,13.1328140703518,13.2787939698492,13.4247738693467,13.5707537688442,13.7167336683417,13.8627135678392,14.0086934673367,14.1546733668342,14.3006532663317,14.4466331658291,14.5926130653266,14.7385929648241,14.8845728643216,15.0305527638191,15.1765326633166,15.3225125628141,15.4684924623116,15.614472361809,15.7604522613065,15.906432160804,16.0524120603015,16.198391959799,16.3443718592965,16.490351758794,16.6363316582915,16.7823115577889,16.9282914572864,17.0742713567839,17.2202512562814,17.3662311557789,17.5122110552764,17.6581909547739,17.8041708542714,17.9501507537688,18.0961306532663,18.2421105527638,18.3880904522613,18.5340703517588,18.6800502512563,18.8260301507538,18.9720100502513,19.1179899497487,19.2639698492462,19.4099497487437,19.5559296482412,19.7019095477387,19.8478894472362,19.9938693467337,20.1398492462312,20.2858291457286,20.4318090452261,20.5777889447236,20.7237688442211,20.8697487437186,21.0157286432161,21.1617085427136,21.3076884422111,21.4536683417085,21.599648241206,21.7456281407035,21.891608040201,22.0375879396985,22.183567839196,22.3295477386935,22.475527638191,22.6215075376884,22.7674874371859,22.9134673366834,23.0594472361809,23.2054271356784,23.3514070351759,23.4973869346734,23.6433668341709,23.7893467336683,23.9353266331658,24.0813065326633,24.2272864321608,24.3732663316583,24.5192462311558,24.6652261306533,24.8112060301508,24.9571859296482,25.1031658291457,25.2491457286432,25.3951256281407,25.5411055276382,25.6870854271357,25.8330653266332,25.9790452261307,26.1250251256281,26.2710050251256,26.4169849246231,26.5629648241206,26.7089447236181,26.8549246231156,27.0009045226131,27.1468844221106,27.292864321608,27.4388442211055,27.584824120603,27.7308040201005,27.876783919598,28.0227638190955,28.168743718593,28.3147236180905,28.4607035175879,28.6066834170854,28.7526633165829,28.8986432160804,29.0446231155779,29.1906030150754,29.3365829145729,29.4825628140704,29.6285427135678,29.7745226130653,29.9205025125628,30.0664824120603,30.2124623115578,30.3584422110553,30.5044221105528,30.6504020100503,30.7963819095477,30.9423618090452,31.0883417085427,31.2343216080402,31.3803015075377,31.5262814070352,31.6722613065327,31.8182412060302,31.9642211055276,32.1102010050251,32.2561809045226,32.4021608040201,32.5481407035176,32.6941206030151,32.8401005025126,32.98608040201,33.1320603015075,33.278040201005,33.4240201005025,33.57),
  y_IMPMNN30_0612 = c(-7.9530711793018e-05,0.00101274158900976,0.00213908660736725,0.00333357706083414,0.00463028566696521,0.00606328981383565,0.0076718256345041,0.00951014919878543,0.0116352351121738,0.0141040579801634,0.0169735924082484,0.0202989680702933,0.0241025701561287,0.0283786596416507,0.0331205833319908,0.0383216880322807,0.0439753205476521,0.0500746972987648,0.056584416920589,0.0634047417560321,0.0704271531669352,0.0775431325151394,0.0846441611624855,0.0916217204708148,0.0983672918019682,0.104781250622823,0.110809729355957,0.116413533697992,0.121553476084998,0.126190368953045,0.130285024738203,0.133798255876541,0.136690874804129,0.138924703928163,0.14049614375949,0.141443976523298,0.141809575426262,0.141634313675059,0.140959564476364,0.139826701036854,0.138277096563204,0.13635212426209,0.134093157340188,0.131541106399515,0.128731282785419,0.125695277394384,0.122464614076339,0.119070816681213,0.115545409058934,0.111919915059433,0.108225858532637,0.104494763328476,0.100758153296879,0.0970475522877748,0.0933940955359903,0.0898162479173802,0.0863173006287028,0.0828996504801904,0.0795656942820756,0.0763178288445908,0.0731584509779685,0.0700899574924412,0.0671147451982412,0.0642352109056011,0.0614537514247533,0.0587727635659303,0.0561946441393645,0.0537217317389861,0.0513540462524939,0.0490885629512234,0.0469220503236397,0.0448512768582075,0.0428730110433916,0.0409840213676569,0.0391810763194681,0.0374609443872902,0.035820394059588,0.0342561938248262,0.0327651121714698,0.0313439175879835,0.0299893785628322,0.0286982635844807,0.0274673532239345,0.0262939152803951,0.0251758608057169,0.024111144896071,0.0230977226476284,0.0221335491565599,0.0212165795190366,0.0203447688312295,0.0195160721893095,0.0187284446894476,0.0179798414278147,0.017268217500582,0.0165915280039202,0.0159477280340005,0.0153347726869937,0.0147506170590708,0.0141932162464029,0.0136605256504285,0.0131508009024678,0.0126631599419899,0.0121968745653802,0.0117512165690241,0.0113254577493068,0.0109188699026136,0.0105307248253299,0.0101602943138409,0.009806850164532,0.0094696641737885,0.00914800813799573,0.00884115385353899,0.00854837311680358,0.00826893772417484,0.00800211947203808,0.0077471901567786,0.00750342157478173,0.00727008552243277,0.00704645379611708,0.00683179846144114,0.00662551610763445,0.00642732526782091,0.00623699581200669,0.00605429761019795,0.00587900053240083,0.00571087444862149,0.00554968922886612,0.00539521474314083,0.00524722086145182,0.00510547745380526,0.00496975439020727,0.00483982154066401,0.00471544877518168,0.00459640596376639,0.00448246297642436,0.00437338968316172,0.00426895595398461,0.00416893165889921,0.00407308666791167,0.00398119085102817,0.00389301407825486,0.00380832621959787,0.00372689719860161,0.00364851179001374,0.00357298981458826,0.00350015614608493,0.00342983565826351,0.00336185322488378,0.00329603371970547,0.00323220201648841,0.00317018298899233,0.00310980151097699,0.00305088245620217,0.00299325069842762,0.00293673111141313,0.00288114856891845,0.00282632794470337,0.00277209411252761,0.00271827194615098,0.00266468631933323,0.00261116210583415,0.00255752417941347,0.00250359741383098,0.00244920668284643,0.00239417686021961,0.00233833281971025,0.00228149943507817,0.00222350158008309,0.00216416412848481,0.00210332057195625,0.00204092796064925,0.0019770357763079,0.00191169572834714,0.00184495952618196,0.00177687887922735,0.00170750549689826,0.0016368910886097,0.00156508736377662,0.001492146031814,0.00141811880213687,0.00134305738416017,0.00126701348729885,0.00119003882096792,0.00111218509458238,0.00103350401755716,0.000954047299307289,0.000873866649247704,0.000793013776793396,0.000711540391359329,0.000629498202360514,0.00054693891921194,0.000463914251328528,0.000380475908125277,0.000296675599017218,0.000212565033419267,0.000128195920746418,4.36199704136631e-05,-4.11111081640382e-05,-0.000125945605571681,-0.000210831812394303),
  x_IMPRBN30_0612 = c(3.84,3.95763819095477,4.07527638190955,4.19291457286432,4.3105527638191,4.42819095477387,4.54582914572864,4.66346733668342,4.78110552763819,4.89874371859296,5.01638190954774,5.13402010050251,5.25165829145729,5.36929648241206,5.48693467336683,5.60457286432161,5.72221105527638,5.83984924623116,5.95748743718593,6.0751256281407,6.19276381909548,6.31040201005025,6.42804020100503,6.5456783919598,6.66331658291457,6.78095477386935,6.89859296482412,7.0162311557789,7.13386934673367,7.25150753768844,7.36914572864322,7.48678391959799,7.60442211055276,7.72206030150754,7.83969849246231,7.95733668341709,8.07497487437186,8.19261306532663,8.31025125628141,8.42788944723618,8.54552763819095,8.66316582914573,8.7808040201005,8.89844221105528,9.01608040201005,9.13371859296482,9.2513567839196,9.36899497487437,9.48663316582915,9.60427135678392,9.72190954773869,9.83954773869347,9.95718592964824,10.074824120603,10.1924623115578,10.3101005025126,10.4277386934673,10.5453768844221,10.6630150753769,10.7806532663317,10.8982914572864,11.0159296482412,11.133567839196,11.2512060301508,11.3688442211055,11.4864824120603,11.6041206030151,11.7217587939698,11.8393969849246,11.9570351758794,12.0746733668342,12.1923115577889,12.3099497487437,12.4275879396985,12.5452261306533,12.662864321608,12.7805025125628,12.8981407035176,13.0157788944724,13.1334170854271,13.2510552763819,13.3686934673367,13.4863316582915,13.6039698492462,13.721608040201,13.8392462311558,13.9568844221106,14.0745226130653,14.1921608040201,14.3097989949749,14.4274371859296,14.5450753768844,14.6627135678392,14.780351758794,14.8979899497487,15.0156281407035,15.1332663316583,15.2509045226131,15.3685427135678,15.4861809045226,15.6038190954774,15.7214572864322,15.8390954773869,15.9567336683417,16.0743718592965,16.1920100502513,16.309648241206,16.4272864321608,16.5449246231156,16.6625628140704,16.7802010050251,16.8978391959799,17.0154773869347,17.1331155778894,17.2507537688442,17.368391959799,17.4860301507538,17.6036683417085,17.7213065326633,17.8389447236181,17.9565829145729,18.0742211055276,18.1918592964824,18.3094974874372,18.427135678392,18.5447738693467,18.6624120603015,18.7800502512563,18.8976884422111,19.0153266331658,19.1329648241206,19.2506030150754,19.3682412060301,19.4858793969849,19.6035175879397,19.7211557788945,19.8387939698492,19.956432160804,20.0740703517588,20.1917085427136,20.3093467336683,20.4269849246231,20.5446231155779,20.6622613065327,20.7798994974874,20.8975376884422,21.015175879397,21.1328140703518,21.2504522613065,21.3680904522613,21.4857286432161,21.6033668341709,21.7210050251256,21.8386432160804,21.9562814070352,22.0739195979899,22.1915577889447,22.3091959798995,22.4268341708543,22.544472361809,22.6621105527638,22.7797487437186,22.8973869346734,23.0150251256281,23.1326633165829,23.2503015075377,23.3679396984925,23.4855778894472,23.603216080402,23.7208542713568,23.8384924623116,23.9561306532663,24.0737688442211,24.1914070351759,24.3090452261307,24.4266834170854,24.5443216080402,24.661959798995,24.7795979899497,24.8972361809045,25.0148743718593,25.1325125628141,25.2501507537688,25.3677889447236,25.4854271356784,25.6030653266332,25.7207035175879,25.8383417085427,25.9559798994975,26.0736180904523,26.191256281407,26.3088944723618,26.4265326633166,26.5441708542714,26.6618090452261,26.7794472361809,26.8970854271357,27.0147236180905,27.1323618090452,27.25),
  y_IMPRBN30_0612 = c(0.000116369884183836,0.000562546873840057,0.00104105778304609,0.00158423653135167,0.00222441703830658,0.0029939299905362,0.00392362508095315,0.00504052898363828,0.00637105978617603,0.00794163557615081,0.00977867444114713,0.0119092146571373,0.0143777136891423,0.0172480051745098,0.020584935727051,0.0244533519605769,0.0289181004888987,0.0340440279258276,0.0398729965418128,0.0463340399023076,0.0533216265134535,0.0607302159237083,0.0684542676815296,0.0763882413353749,0.084426596433702,0.0924646194407076,0.100418353256255,0.108225610919073,0.115825235268786,0.123156069145017,0.130156955387388,0.136766736835523,0.142924256329045,0.148568356707577,0.153644732388217,0.158133910475971,0.162027460052259,0.165316954637097,0.1679939677505,0.170050072912482,0.17147684364306,0.172265853462247,0.172408675890059,0.171896884958288,0.170734928722463,0.16897357898561,0.166674022945905,0.163897447801523,0.16070504075064,0.15715798899143,0.153317479722069,0.149244700140732,0.145000837445596,0.140647078834834,0.136244611506623,0.131852232406191,0.127497895704255,0.123188102643232,0.118928920118378,0.114726415024946,0.110586654258189,0.106515704713362,0.102519633285718,0.0986045068705102,0.0947763923629935,0.0910413566584212,0.0874054666520471,0.0838747892391249,0.0804551448373589,0.0771483205544838,0.0739528120037685,0.070867018607886,0.0678893397895098,0.0650181749713132,0.0622519235759693,0.0595889850261517,0.0570277587445335,0.0545666441537882,0.0522040406765889,0.0499383477356091,0.0477679647535219,0.0456912911530008,0.0437067263567191,0.0418126332347276,0.0400066268937417,0.0382856229360858,0.0366465100767197,0.0350861770306034,0.0336015125126966,0.0321894052379592,0.030846743921351,0.0295704172778317,0.0283573140223614,0.0272043228698997,0.0261083325354065,0.0250662317338416,0.0240749091801649,0.0231312535893362,0.0222321536763152,0.0213744981560619,0.0205551823106917,0.0197717277529182,0.0190227911186038,0.018307148764763,0.0176235770484103,0.0169708523265602,0.0163477509562272,0.0157530492944257,0.0151855236981704,0.0146439505244757,0.0141271061303561,0.0136337668728262,0.0131627091089003,0.0127127091955931,0.0122825434899191,0.0118709883488926,0.0114768201295283,0.0110988151888407,0.0107357498838441,0.0103864005715533,0.0100495554474117,0.00972442654271301,0.00941075629701428,0.00910832063950523,0.00881689549937552,0.0085362568058149,0.00826618048801305,0.00800644247515973,0.0077568186964446,0.00751708508105741,0.00728701755818786,0.00706639205702563,0.00685498450676049,0.00665257083658209,0.00645892697568022,0.00627382885324452,0.00609705239846474,0.00592837354053058,0.00576756820863178,0.00561441233195803,0.00546868183969902,0.00533015266104451,0.00519860072518417,0.00507379969101563,0.00495546594165864,0.00484325562437493,0.00473682202364686,0.00463581842395682,0.00453989810978717,0.00444871436562029,0.00436192047593852,0.00427916972522429,0.00420011539795991,0.00412441077862779,0.00405170915171026,0.00398166380168973,0.00391392801304856,0.0038481550702691,0.00378399825783373,0.00372111086022485,0.00365914616192481,0.00359775744741596,0.0035365980011807,0.00347532110770137,0.00341358005146039,0.00335102811694008,0.00328731858862282,0.00322210475099099,0.00315503988852697,0.00308577768404197,0.0030140868905578,0.00294000997147197,0.00286362920697759,0.0027850268772679,0.00270428526253596,0.00262148664297494,0.00253671329877812,0.00245004751013854,0.00236157155724939,0.00227136772030387,0.00217951827949509,0.00208610551501626,0.00199121170706054,0.00189491913582104,0.00179731008149096,0.00169846682426345,0.00159847164433169,0.00149740682188881,0.00139535463712802,0.00129239737024243,0.00118861730142523,0.00108409671086957,0.000978917878768635,0.000873163085315551,0.000766914610703496,0.000660254735125634,0.000553265738775129,0.000446029901845128,0.000338629504528811,0.00023114682701933),
  x_MBM_0510 = c(2.23,2.31688442211055,2.40376884422111,2.49065326633166,2.57753768844221,2.66442211055276,2.75130653266332,2.83819095477387,2.92507537688442,3.01195979899498,3.09884422110553,3.18572864321608,3.27261306532663,3.35949748743719,3.44638190954774,3.53326633165829,3.62015075376884,3.7070351758794,3.79391959798995,3.8808040201005,3.96768844221106,4.05457286432161,4.14145728643216,4.22834170854271,4.31522613065327,4.40211055276382,4.48899497487437,4.57587939698492,4.66276381909548,4.74964824120603,4.83653266331658,4.92341708542714,5.01030150753769,5.09718592964824,5.18407035175879,5.27095477386935,5.3578391959799,5.44472361809045,5.53160804020101,5.61849246231156,5.70537688442211,5.79226130653266,5.87914572864322,5.96603015075377,6.05291457286432,6.13979899497487,6.22668341708543,6.31356783919598,6.40045226130653,6.48733668341709,6.57422110552764,6.66110552763819,6.74798994974874,6.8348743718593,6.92175879396985,7.0086432160804,7.09552763819095,7.18241206030151,7.26929648241206,7.35618090452261,7.44306532663317,7.52994974874372,7.61683417085427,7.70371859296482,7.79060301507538,7.87748743718593,7.96437185929648,8.05125628140703,8.13814070351759,8.22502512562814,8.31190954773869,8.39879396984925,8.4856783919598,8.57256281407035,8.6594472361809,8.74633165829146,8.83321608040201,8.92010050251256,9.00698492462312,9.09386934673367,9.18075376884422,9.26763819095477,9.35452261306533,9.44140703517588,9.52829145728643,9.61517587939698,9.70206030150754,9.78894472361809,9.87582914572864,9.9627135678392,10.0495979899497,10.1364824120603,10.2233668341709,10.3102512562814,10.397135678392,10.4840201005025,10.5709045226131,10.6577889447236,10.7446733668342,10.8315577889447,10.9184422110553,11.0053266331658,11.0922110552764,11.1790954773869,11.2659798994975,11.352864321608,11.4397487437186,11.5266331658291,11.6135175879397,11.7004020100503,11.7872864321608,11.8741708542714,11.9610552763819,12.0479396984925,12.134824120603,12.2217085427136,12.3085929648241,12.3954773869347,12.4823618090452,12.5692462311558,12.6561306532663,12.7430150753769,12.8298994974874,12.916783919598,13.0036683417085,13.0905527638191,13.1774371859296,13.2643216080402,13.3512060301508,13.4380904522613,13.5249748743719,13.6118592964824,13.698743718593,13.7856281407035,13.8725125628141,13.9593969849246,14.0462814070352,14.1331658291457,14.2200502512563,14.3069346733668,14.3938190954774,14.4807035175879,14.5675879396985,14.654472361809,14.7413567839196,14.8282412060302,14.9151256281407,15.0020100502513,15.0888944723618,15.1757788944724,15.2626633165829,15.3495477386935,15.436432160804,15.5233165829146,15.6102010050251,15.6970854271357,15.7839698492462,15.8708542713568,15.9577386934673,16.0446231155779,16.1315075376884,16.218391959799,16.3052763819095,16.3921608040201,16.4790452261307,16.5659296482412,16.6528140703518,16.7396984924623,16.8265829145729,16.9134673366834,17.000351758794,17.0872361809045,17.1741206030151,17.2610050251256,17.3478894472362,17.4347738693467,17.5216582914573,17.6085427135678,17.6954271356784,17.7823115577889,17.8691959798995,17.95608040201,18.0429648241206,18.1298492462312,18.2167336683417,18.3036180904523,18.3905025125628,18.4773869346734,18.5642713567839,18.6511557788945,18.738040201005,18.8249246231156,18.9118090452261,18.9986934673367,19.0855778894472,19.1724623115578,19.2593467336683,19.3462311557789,19.4331155778894,19.52),
  y_MBM_0510 = c(-0.000327318093696737,0.000835733645400564,0.00200474890905977,0.00318569122184272,0.00438452410831129,0.00561315764626261,0.006958975240445,0.00856124522924285,0.0105602561437762,0.0130962965151652,0.0163049838217485,0.0202551298888539,0.0249656539667749,0.0304542774861463,0.0367387218776032,0.0438367085717805,0.0517600746672222,0.0604758836251104,0.0699306172435053,0.0800706486992754,0.0908423511692893,0.102192097830416,0.114066261859523,0.126387022612734,0.138964437832094,0.151576108722749,0.163999633181261,0.176012609104191,0.1873926343881,0.197917306929549,0.207364224625099,0.215545642738184,0.22245522223327,0.228145777880875,0.232670157047752,0.236081207100652,0.238431775406325,0.239774709331525,0.240162856243002,0.239649063507508,0.238286652606477,0.236138096712236,0.233274141307921,0.229765829552448,0.225684204604734,0.221100309623695,0.216085187768248,0.210709882197308,0.205045436069792,0.199162892544616,0.193133294780697,0.187024777943342,0.18087579979768,0.174707465148626,0.16854066045531,0.16239627217686,0.156295186772404,0.150258290701072,0.144306470421992,0.138460612394293,0.132741603077103,0.127170328929551,0.121767676410766,0.116554531979875,0.111547991973824,0.106747221272044,0.102146087022894,0.0977384555900449,0.093518193337168,0.089479166627934,0.0856152418260143,0.0819202852950797,0.0783881633988011,0.0750127425008499,0.071787888964897,0.0687074691546134,0.0657653494336701,0.0629553961657383,0.0602714757540107,0.0577078780720282,0.055260362373377,0.0529250064008833,0.0506978878973732,0.048575084605673,0.0465526742686089,0.0446267346290071,0.0427933434296938,0.0410485784134953,0.0393885173232376,0.0378092379017471,0.0363068178918499,0.0348773350363723,0.0335168670781404,0.0322214917599805,0.0309872868247188,0.0298103300151816,0.0286868026848876,0.0276140844230653,0.0265903249221866,0.0256136865777987,0.0246823317854487,0.0237944229406836,0.0229481224390508,0.022141592676097,0.0213729960473696,0.0206404949484156,0.0199422517747822,0.0192764289220164,0.0186411887856653,0.018034693761276,0.0174551062443957,0.0169005886305715,0.0163693033153504,0.0158594126942796,0.0153690791629061,0.0148964651167772,0.0144397589735714,0.0139978747367525,0.0135705305154474,0.0131574861968837,0.0127585016682894,0.012373336816892,0.0120017515299193,0.0116435056945991,0.0112983591981589,0.0109660719278266,0.0106464037708299,0.0103391146143965,0.0100439643457541,0.00976071285213043,0.0094891200207532,0.00922894573885017,0.00897994989364903,0.00874189237237749,0.00851453306226326,0.00829763185053412,0.00809094862441774,0.00789424327114187,0.00770727567793419,0.00752980573202244,0.00736157552357313,0.00720218092328931,0.00705114604199447,0.00690799449504263,0.00677224989778783,0.00664343586558408,0.00652107601378537,0.00640469395774572,0.00629381331281919,0.00618795769435979,0.00608665071772151,0.00598941599825839,0.00589577715132442,0.00580525779227369,0.00571738153646013,0.00563167199923784,0.0055476527959608,0.00546484754198305,0.00538277985265854,0.00530097334334138,0.00521895162938553,0.00513623832614505,0.00505235704897392,0.00496683141322619,0.00487918503425586,0.00478894152741695,0.00469562450806351,0.00459875773066275,0.0044980017837137,0.00439341027059674,0.00428510691925268,0.00417321545762251,0.00405785961364711,0.00393916311526736,0.00381724969042415,0.00369224306705842,0.00356426697311106,0.00343344513652295,0.00329990128523503,0.00316375914718818,0.00302514245032328,0.00288417492258127,0.00274098029190301,0.00259568228622945,0.00244840463350147,0.00229927106165996,0.00214840529864582,0.00199593107239997,0.00184197211086331,0.00168665214197674,0.00153009489368115,0.00137242409391742,0.00121376347062648,0.00105423675174925,0.000893967665226628,0.000733079938999473,0.000571697301008709,0.000409943479195256,0.00024794220149999,8.58171958638476e-05)
)
nya0 <- tibble::tibble(t1=as.integer(c(3,5,-3,-5)), t2=c(1.2,3.4,5.6,7.8), t3=c('Cats','can','run','fun'),
                       t4=c('2020/9/1', '2020/9/2', '2020/9/3', '2020/9/4'), t5=NA_real_, t6=c('24:00', '123:00', NA, '1:30:00'),
                       t7=c('2020/9/1 00:00:00', '2020/9/2 01:23:45', '2020/9/3 12:34:56', '2020/9/4 23:59:59'))  # hablar::retype(nya0)
assign('iris', tibble::as_tibble(datasets::iris), envir = .GlobalEnv)

## END ##
