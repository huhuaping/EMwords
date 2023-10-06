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


files_tar <- "text/index/abbrev-Cameron-Stata-a-chn.rds"

dt_read <- read_rds(files_tar) %>%
  mutate(text_nohash = str_trim(text_nohash, "both"))

tbl_check <- janitor::get_dupes(select(dt_read, text_nohash))

dt_clean <- dt_read %>%
  # delete rows with duplicated sentence
  distinct(., text_nohash, .keep_all = TRUE) %>%
  filter(!empty) %>%
  select(text_nohash, text_tidy) %>%
  rename_all(., ~c("eng", "chn")) %>%
  # must change column attributes induced by translation
  mutate(chn = map(chn, ~unname(unlist(.x)))) %>%
  mutate(chn = as.character(chn))

(file_out1 <- str_replace(files_tar, "\\.rds", "\\.tsv"))
readr::write_tsv(dt_clean, file = file_out1, col_names = FALSE)

(file_out2 <- str_replace(files_tar, "\\.rds", "-hand\\.tsv"))
write_tsv(dt_clean, file = file_out2, col_names = FALSE)
