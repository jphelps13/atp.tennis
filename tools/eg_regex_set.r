ff <- sample_dt$score[1:20]


gregexpr("a", "caba")
k <- "6-7(4) 1-6 6-0 6-4 6-1"
pos_win <- gregexpr("(.-)+" , k)[[1]]

library(stringr)
k <- "6-7(4) 1-6 6-0 6-4 6-1"


str_extract_all(k, "((?<=-)[0-9]+)")

v <- c(k, k)



z <- dput(match_dt[1:10, score])

regmatches(k, regexpr("(.-)+", k))


z <- c("6-4 8-10 6-4 6-4", "9-7 6-4 6-4", "11-9 9-11 6-4 6-3", "6-1 4-6 10-8 6-3", 
       "6-1 6-4 6-4", "6-2 8-6 6-2", "6-3 6-2 0-6 6-3", "6-3 6-0", "5-7 6-4 6-0", 
       "4-6 6-2 6-2")

library(stringr)
library(stringi)

library(tictoc)

v <- str_extract_all(z, "((?<=-)[0-9]+)")

v <- str_extract_all(z, "([0-9]+(?=-))")
v <- vapply(v, FUN = function(x){
  sum(as.numeric(x))
}, FUN.VALUE = numeric(1))

#z <- match_dt$score
tic()
v <- str_extract_all(z, "([0-9]+(?=-))")
v <- vapply(v, FUN = function(x){paste(x, collapse="+")}, FUN.VALUE = character(1))
v <- stri_replace_all_fixed(v, "-", "")
v[v == ""] <- "0"
v <- vapply(v, FUN = function(x){
  f <- eval(parse(text=x))
}, FUN.VALUE = numeric(1))
toc()

tic()
v <- str_extract_all(z, "([0-9])-+")
v <- vapply(v, FUN = function(x){paste(x, collapse="+")}, FUN.VALUE = character(1))
v <- stri_replace_all_fixed(v, "-", "")
v[v == ""] <- "0"
v[v == "NA"] <- "0"
set(match_dt, j = "v1", value = v)
match_dt[, index := (1:.N)]
setkey(match_dt, index)
match_dt[, v2 := eval(parse(text = v1)), by = index]
toc()

ff <- which(is.na(match_dt$v2))
ff[1]

v <- gsub("-", "", v)
v <- unlist(v)
