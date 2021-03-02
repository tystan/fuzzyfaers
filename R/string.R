
# ---- string_funcs ----



ldist <- function(str1, str2) {
  return(
    1 - ( adist(str1, str2) / pmax(nchar(str1), nchar(str2)) )
  )
}


### notes about stringdist::stringdistmatrix(a, b, ...)
# method = c("osa", "lv", "dl", "hamming", "lcs", "qgram","cosine", "jaccard", "jw", "soundex")
# a is target
# b is source
# insertions, deletions or substitutions of a single character
# note transposition mean swap of two adjacent characters


# rm_punc1 <- function(x) gsub("[\\s\\d!@#$%^&*()=?<>'-/\\[\\];]", "", x, perl = TRUE) # table(rm_punc(srce_str) == rm_punc1(srce_str))
rm_punc <- function(x) gsub("[\\s]+", " ", gsub("[\\d[:punct:]]", "", x, perl = TRUE), perl = TRUE)
norm_len <- function(x, y) min(nchar(x), nchar(y))
col_min <- function(x) apply(x, 2, min)
whch_col_min <- function(x) apply(x, 2, which.min)



dl_dist <- function(targ, srce, weights = c(d = 1 / 20, i = 1 / 4, s = 1, t = 1), kp_punc = FALSE, normalise = FALSE, first_letter_penalty = 1) {
  targ <- tolower(targ)
  srce <- tolower(srce)
  if (!kp_punc) srce <- rm_punc(srce)
  ii <- length(targ)
  jj <- length(srce)
  normalisation_mat <- matrix(0, nrow = ii, ncol = jj, dimnames = list(targ, srce))
  for (i in 1:ii) for (j in 1:jj) normalisation_mat[i, j] <- norm_len(targ[i], srce[j])
  dists <- stringdistmatrix(targ, srce, method = "dl", weight = weights)
  if (first_letter_penalty > 0) {
    first_srce <- substr(srce, 1, 1)
    first_targ <- substr(targ, 1, 1)
    add_penalties <- outer(first_targ, first_srce, "!=") * first_letter_penalty
    dists <- dists + add_penalties
  }
  if (normalise) dists <- dists / normalisation_mat
  tibble(
    srce = srce,
    min_dist = col_min(dists),
    targ_min = srce[whch_col_min(dists)],
    norm_denom = pmin(nchar(srce), nchar(targ_min))
  )
}




fuzzy_regex <- function(targ, srce, kp_punc = FALSE) {
  targ <- tolower(targ)
  srce <- tolower(srce)
  if (!kp_punc) srce <- rm_punc(srce)
  
  # fuzzy_exp <- gsub("a", "[ae]", targ)
  # fuzzy_exp <- gsub("(e|i)", "[aei]", fuzzy_exp)
  # fuzzy_exp <- gsub("(o|u)", "[ou]", fuzzy_exp)
  fuzzy_exp <- paste0("(", paste0(targ, sep = "", collapse = "|"), ")")
  # print(fuzzy_exp)
  
  as.integer(
    # grepl(contains_exp, srce, ignore.case = TRUE) | 
    grepl(fuzzy_exp, srce, ignore.case = TRUE)
  )
  
}
