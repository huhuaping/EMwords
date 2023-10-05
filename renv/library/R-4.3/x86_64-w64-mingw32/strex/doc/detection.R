## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(strex)

## ----examples-----------------------------------------------------------------
str_detect_all("quick brown fox", c("x", "y", "z"))
str_detect_all(c(".", "-"), ".")
str_detect_all(c(".", "-"), coll("."))
str_detect_all(c(".", "-"), coll("."), negate = TRUE)
str_detect_all(c(".", "-"), c(".", ":"))
str_detect_all(c(".", "-"), coll(c(".", ":")))
str_detect_all("xyzabc", c("a", "c", "z"))
str_detect_all(c("xyzabc", "abcxyz"), c(".b", "^x"))
str_detect_any("quick brown fox", c("x", "y", "z"))
str_detect_any(c(".", "-"), ".")
str_detect_any(c(".", "-"), coll("."))
str_detect_any(c(".", "-"), coll("."), negate = TRUE)
str_detect_any(c(".", "-"), c(".", ":"))
str_detect_any(c(".", "-"), coll(c(".", ":")))
str_detect_any(c("xyzabc", "abcxyz"), c(".b", "^x"))

## ----performance--------------------------------------------------------------
bench::mark(
  str_detect_all(rep("*", 1000), rep(str_escape("*"), 555)),
  str_detect_all(rep("*", 1000), coll(rep("*", 555))),
  min_iterations = 100
)

