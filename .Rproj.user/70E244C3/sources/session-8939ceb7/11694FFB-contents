#====prepare pkgs====
library(tidyverse)
library(readr)
library(magrittr)
library(purrr)
#renv::install("strex")
require(strex)
#renv::install("janitor")
require(janitor)
library(glue)



files_all <- dir("data/hansen",full.names = T) # all files
tot <- length(files_all)

dt_read <- tibble(path = files_all) %>%
  mutate(chpt = str_extract(path, "(?<=sentences-).+(?=\\.rds)")) %>%
  mutate(dt = map(path, read_rds)) %>%
  unnest(cols = "dt") %>%
  select(-path)

tbl_check <- janitor::get_dupes(select(dt_read, eng))

dt_clean <- dt_read %>%
  # delete rows with duplicated sentence
  distinct(., eng, .keep_all = TRUE)

file_out <- "tsv/hansen.tsv"
write_tsv(select(dt_clean, eng, chn), file = file_out, col_names = FALSE)

file_out <- "tsv/hansen_hand.tsv"
write_tsv(select(dt_clean, eng, chn), file = file_out, col_names = FALSE)
