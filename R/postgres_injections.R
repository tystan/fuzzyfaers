
# ---- postgres_func_injections ----


add_fuzz_functions_to_con <- function(con) {
  
  # postgres side function to remove punctuation, numbers and spaces
  return_cde <-
    execute_pg(
      con = con,
      "DROP FUNCTION IF EXISTS rm_punc(text);",
      "CREATE FUNCTION rm_punc(str text) RETURNS text AS $$",
      "SELECT regexp_replace(str, '[^a-zA-Z]+', '','g');",
      "$$", # '[^a-zA-Z]' finds NON-alpha character, 'g' is for global replace
      "LANGUAGE SQL;"
    )
  
  if (return_cde != 0) {
    stop("pgsql rm_punc() function unsuccessfully submitted to connection")
  }
  
  # postgres side function to get the smallest character length of two strings
  return_cde <-
    execute_pg(
      con = con,
      "DROP FUNCTION IF EXISTS min_str_len(text, text);",
      "CREATE FUNCTION min_str_len(str1 text, str2 text) RETURNS int AS $$",
      "SELECT GREATEST(LEAST(char_length(str1), char_length(str2)), 1);",
      "$$",
      "LANGUAGE SQL;"
    )
  
  if (return_cde != 0) {
    stop("pgsql min_str_len() function unsuccessfully submitted to connection")
  }
  
  # postgres side fully sick function to return string distance from string1 to string2
  # distance is calculated once superfluous numbers, punctuation, spaces are removed
  return_cde <-
    execute_pg(
      con = con,
      "DROP FUNCTION IF EXISTS str_dist(text, text);",
      "CREATE FUNCTION str_dist(str1 text, str2 text) RETURNS float AS $$",
      "SELECT ",
      "  CAST (",
      "    LEVENSHTEIN(",
      "      lower(substring(rm_punc(str1) from 1 for 100)), ",
      "      lower(substring(rm_punc(str2) from 1 for 100))  ",
      "    ) ",
      "  AS float) ",
      "/ ",
      "  CAST (min_str_len(rm_punc(str1), rm_punc(str2)) AS float);",
      "$$",
      "LANGUAGE SQL;"
    )  
  
  if (return_cde != 0) {
    stop("pgsql str_dist() function unsuccessfully submitted to connection")
  }
  
}


