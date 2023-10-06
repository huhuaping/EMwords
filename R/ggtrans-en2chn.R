
# ==== pkgs ====
require(knitr)
require(tidyverse)
require(stringr)
require(mgsub)
require(purrr)
require(here)
#renv::install("ropensci/googleLanguageR")
require(googleLanguageR)

# ==== read file ====
file_path <- here("text/index/abbrev-Cameron-Stata-a.qmd")
#file_path <- chpt_path
tbl_tex <- readLines(file_path) %>%
  as_tibble() %>%
  rename("text" = "value") %>%
  add_column(row = 1:nrow(.), 
             .before = "text")

# ==== step 1 handle math environment ====

tbl_dollar <- tbl_tex %>%
  mutate(
    double_dollar = str_detect(text, "^\\$\\$$"),
    index_raw = row_number(row),
    index_detect = ifelse(
      double_dollar==TRUE, 
      index_raw,NA)
  ) 

# identify position
lines <- tbl_dollar %>% 
  filter(!is.na(index_detect)) %>%
  pull(index_detect)



if (length(lines) >0) {
  # case exist math environment
  # assumes existing perfect paired double dollar symbols
  row_tar <- seq_len(length(lines))%%2  # row indicator
  lines_star <- lines[row_tar==1]   # start row (odd)
  lines_end <- lines[row_tar==0]   # end row (enven)
  
  lines_tar <- NULL
  for (i in 1:length(lines_star)){
    lines_tem <- lines_star[i]:lines_end[i]
    lines_tar <- c(lines_tar, lines_tem)
  }
  
  tbl_dollar  <- tbl_dollar %>%
    # tag lines within double dollar pairs
    mutate(
      dollars = ifelse(
        index_raw %in% lines_tar, 
        TRUE, FALSE)
    )
} else {
  # case no math environment
  tbl_dollar  <- tbl_dollar %>%
    # tag lines within double dollar pairs
    mutate( dollars = FALSE )
}



# tag it 
tbl_work <- tbl_dollar  %>%
  mutate(
    tabs = str_detect(text, "^\\|.*?\\|$")) %>%
  # tag lines of markdown image
  mutate(
    img = str_detect(text, "^\\!\\[\\]\\(images")
  ) %>%
  # tag empty lines
  mutate(
    empty = ifelse(text=="", TRUE, FALSE)
  ) %>%
  # tag lines need to translate
  mutate(
    gotrans = ifelse(
      dollars+tabs+img+empty==0,
      TRUE, FALSE)
  ) %>%
  # tag heading
  mutate(
    heading = str_extract(text, "^#{1,3}"),
    text_nohash = str_replace_all(text, "(^#{1,3})", "")
    )

# ===== Helper Translate paragraph =====

#' Translate paragraph contains math symbol whin sigle dollar symbol pairs.
#'
#' @param text character. Target character paragraph.
#' @param auth character. File path of googleLanguageR API authorized json file
#'
#' @return
#' @export trans_mathParagraph
#'
#' @examples
#' math_complex <- tbl_work$text[12]
#' auth_json <- "C:/Users/huhua/json/googleLanguageR.json"
#' 
#' transed_math <- trans_mathParagraph(
#'   text = math_complex, 
#'   auth = auth_json) 

trans_mathParagraph <- function(text, auth) {
  # prepare
  library(googleLanguageR)
  library(mgsub)
  gl_auth(auth)
  
  # handle dollar units "\\$ 15.6"
  ## first substitute the unit dollar, 
  ## then we have to change it back. 
  text <- mgsub::mgsub(
    string =  text, 
    pattern = "\\\\\\$",
    replacement = "us_dollar"
  )
  
  # now we split whole paragraph
  split_raw <- strsplit(text,
                        split = "(?=[\\$])",
                        perl = TRUE) %>% 
    unlist()
  lines <- grep("\\$", split_raw)
  
  split_handle <- split_raw
  
  # identify position
  row_tar <- seq_len(length(lines))%%2  # row indicator
  lines_star <- lines[row_tar==1]   # start row (odd)
  lines_end <- lines[row_tar==0]   # end row (enven)
  
  # loop substitute 
  # i <- 1
  lines_math <- NULL
  lines_handle <- NULL
  for (i in 1:length(lines_star)){
    # text of raw math
    lines_math[i] <- split_handle[lines_star[i]+1]
    # replace 
    lines_eq <- paste0("matheq", i)
    split_handle[lines_star[i]+1] <- lines_eq
    # text of handle math
    lines_handle[i] <- lines_eq
  }
  
  # paste as paragraphs
  paragraph_handle <- paste0(split_handle, collapse = "")
  # now translate it
  trans_result <- googleLanguageR::gl_translate(
    t_string = paragraph_handle, 
    target = "zh-CN")$translatedText
  Sys.sleep(0.2)
  
  # then re-split the paragraph
  split_trans <- strsplit(trans_result,
                          split = "(?=[\\$])",
                          perl = TRUE) %>% 
    unlist()
  
  # bug fix: inline math with chn characters, such as "$matheq4 之间$"
  ##  just simplify it by remove cjk characters
  split_trans <- str_replace_all(
    split_trans,
    "(?<=^matheq\\d{1,3})(.*)",
    ""
  )
  
  # match math after translation
  ## note: math eq order may not be the same sequence as it before.
  
  ## here is the math pairs
  tbl_math <- tibble(
    math_raw = lines_math,
    math_handle = lines_handle)
  
  ## now match and replace
  tbl_result <- tibble(chn = split_trans) %>%
    left_join(., tbl_math, 
              by = c("chn"="math_handle")) %>%
    mutate(
      chn_final = ifelse(
        str_detect(chn, "^matheq\\d{1,2}"),
        math_raw, chn))
  
  # get the paragraph
  paragraph_final <- paste0(tbl_result$chn_final, collapse = "")
  # now substitute the dollar unit
  paragraph_final <- mgsub::mgsub(
    string =  paragraph_final, 
    pattern = "us_dollar",
    replacement = "\\\\\\$"
  )
  return(paragraph_final)
}

# ==== step 2 translate paragraphs ====

n_pars <- sum(tbl_work$gotrans)
auth_json <- "C:/Users/huhua/json/googleLanguageR.json"
#auth_json <- "C:/Users/huhua/json/GoogleTransEM37897.json"

## !important! run only once!
## Google translate will charge your fees!!
tbl_trans <- tbl_work %>%
  #head(100) %>%
  mutate(
    chn = map2(
      .x = text_nohash, .y = gotrans,
      .f = function(x,y){
        if(y==TRUE) {
          out <- trans_mathParagraph( #custom function
            text = x,
            auth = auth_json)
        } else{
          out <- ""
        }
        return(out)
      }
    )
  )

# now tidy the result
tbl_tidy <- tbl_trans %>%
  mutate( # add hashed chn
    text_tidy = ifelse(
      str_detect(heading,"^#{1,3}"),
      str_c(heading, chn, sep = " "),
      chn)
  ) %>%
  mutate( # add other chn
    text_tidy = ifelse(
      is.na(text_tidy),
      chn, text_tidy
    )
  ) %>%
  mutate(     # fill other lines
    text_tidy = ifelse(
      text_tidy=="",
      text, text_tidy
    )
  )

# ==== write out qmd file ====
file_base <- basename(path = file_path)
file_out <- here(
  paste0(
    "trans/",
    str_replace(file_base, "\\.qmd$","-chn\\.qmd")
  )
)
# write out all modified text
writeLines(unlist(tbl_tidy$text_tidy), file_out)

# ==== export out final table ====

file_base <- basename(path = file_path)
file_out <- here(
  paste0(
    "trans/",
    str_replace(file_base, "\\.qmd$","-chn\\.rds")
  )
)

# write table
write_rds(tbl_tidy, file_out)
