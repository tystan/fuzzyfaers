

# ---- general_funcs ----


# input is non quoted object, returns a string value of object's name
get_name <- function(object) {
  
  o1 <- enquo(object) # just needs "!!" to extract non-standard eval value
  o1_nm <- quo_name(o1) # string version
  
  return(o1_nm)
  
}


is_match_linear <- function(x1, x2, int, slope) {
  (x2 - slope * x1 - int) >= 0
}


is_na_or_null <- function(x) {
  # %in% always returns a TRUE or FALSE value
  return(TRUE %in% is.null(x) | TRUE %in% is.na(x))
}
# is_na_or_null(NULL)
# is_na_or_null(NA)
# is_na_or_null(c(NA, NA, Inf))
# is_na_or_null(c(NULL, 1))
# is_na_or_null(c("NULL jlkdfsgkjhlfdgshjkl"))


