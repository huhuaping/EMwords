
#====prepare pkgs====
library(tidyverse)
library(readr)
library(magrittr)
library(purrr)
#renv::install("strex")
require(strex)
library(glue)



files_all <- dir("text/hansen",full.names = T) # all files
tot <- length(files_all)

for (i in 1:tot){
  #====read and clean data set====
  file_path <- files_all[i]
  name_use <- str_extract(file_path, "(?<=chpt)(.+)(?=-chn)")
  
  dt_clean <- read_rds(file_path) %>%
    filter(gotrans) %>%
    select(text_nohash, chn, heading) %>%
    mutate(chn = unlist(chn)) %>%
    mutate(text_nohash = str_trim(text_nohash)) %>%
    filter(!is.na(text_nohash) | text_nohash!="") %>%
    # detect total numbers of sentence
    mutate(
      tot_sentence = str_count(text_nohash, "\\. ") + 1,
      tot_sentence_chn = str_count(chn, "。")
    ) %>%
    mutate(tot_sentence_chn = ifelse(
      str_detect(heading, "#")&tot_sentence_chn==0,
      tot_sentence_chn + 1,
      tot_sentence_chn
    )
    ) %>%
    # extract the first sentence
    mutate(
      eng_first = str_extract(text_nohash, "(^.*?)(?=\\. [A-Z])"),
      chn_first = str_extract(chn, "(^.*?)(?=。)")
    ) %>%
    # handle lines with only sentence.
    mutate(
      eng_first = ifelse(
        is.na(eng_first) &!is.na(text_nohash) & tot_sentence==1,
        text_nohash,
        eng_first
      ),
      chn_first = ifelse(
        is.na(chn_first) &!is.na(chn) & tot_sentence==1,
        chn,
        chn_first
      )
    ) %>%
    # extract the last sentence
    mutate(
      #eng_last = str_extract(text_nohash, "(?<=\\. )([A-Z].*?)\\.$"),
      #chn_last = str_extract(chn, "(?<=。)(.*?)(?=。$)")
      eng_last = ifelse(
        tot_sentence > 1,
        #strex::str_after_last(text_nohash, "[a-z]{2,50}\\. "),
        strex::str_after_nth(text_nohash, "\\. ", n = tot_sentence -1),
        NA
        #str_extract(text_nohash, "[\\w\\s]+.$")
        #sub('.*\\. ', '', text_nohash)
      ),
      tot_sentence_chn = replace_na(tot_sentence_chn, 0),
      chn_last =ifelse(
        tot_sentence_chn >1 ,
        strex::str_after_nth(chn, "。", n = tot_sentence_chn -1),
        NA
        #str_extract(chn, "[\\w\\s]+。$")
      ) 
    ) %>%
    # tidy the last sentence
    mutate(
      eng_last = str_replace(eng_last, ".$", ""),
      chn_last = str_replace(chn_last, "。$", "")
    ) %>%
    # remove some lines
    filter(!str_detect(text_nohash, "^\\*")) %>%
    filter(!str_detect(text_nohash, "^\\$")) %>%
    filter(!str_detect(text_nohash, "^\\("))
  
  
  
  # ====construct tsv format====
  
  ## get the index of the Exercises
  index_ex <- which(dt_clean$text_nohash=="Exercises") -1
  if (length(index_ex) ==0) {
    index_ex <- nrow(dt_clean)
  }
  
  ## remove the meaningless lines
  ptn <- c("with", "or", "Thus", "and", "where",
           "Also", "Hence")
  
  dt_tsv <- dt_clean %>%
    # remove lines of "Exercises"
    .[1:index_ex, ] %>%
    # handle no match pairs
    mutate(
      eng_first = ifelse(eng_first %in% ptn, NA, eng_first),
      eng_last = ifelse(eng_last %in% ptn, NA, eng_last)
    ) %>%
    mutate(
      st_first = str_c(eng_first, chn_first, sep=" hhp "),
      st_last = str_c(eng_last, chn_last, sep=" hhp")
    ) %>%
    filter(!is.na(st_first)|!is.na(st_last)) %>%
    add_column(index = 1:nrow(.) ) %>%
    select(index, heading, st_first, st_last) %>%
    mutate(tsv = map2(.x = st_first, .y = st_last, 
                      .f = function(x, y)tibble(st =c(x, y))
    )
    ) %>%
    select(index, heading, tsv) %>%
    unnest(cols = tsv) %>%
    filter(!is.na(st)) %>%
    # tidy and filter again
    mutate(n_sentence = str_count(st, "。")+1) %>%
    filter(n_sentence ==1) %>%
    filter(st!=" hhp ") %>%
    separate(st, into = c("eng", "chn"), sep = "hhp") %>%
    select(heading, eng, chn) 
  
  #==== simplify sentences====
  
  dt_simple <- dt_tsv %>%
    # trim both side
    mutate_all(., str_trim, side = "both") %>%
    # remove the tail dot
    mutate(eng = str_replace(eng, "\\.$|\\*$", "")) %>%
    mutate(chn = str_replace(chn, "\\.$|\\*$", "")) %>%
    # delete rows containing dollar sign or url link
    filter(!str_detect(eng, "\\$|\\\\href")) %>%
    # delete rows start with slash sign
    filter(!str_detect(eng, "^\\\\")) %>%
    # delete rows with items style
    filter(!str_detect(eng, "^\\d{1,2}\\.")) %>%
    # count words in the sentence
    mutate(tot_words = str_count(eng, "\\w+")) %>%
    # delete rows with less words
    filter(str_detect(heading, "^#") | tot_words > 4)
  
  
  
  #====write out====
  
  (file_out <- glue("data/hansen/sentences-dt{name_use}.rds"))
  write_rds(dt_simple, file_out)
  
  cat(glue("the {i}th of total {tot} files /n"))
  
}







