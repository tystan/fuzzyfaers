
# ---- db_setup_funcs ----



yaml_setup <- function(dbname, file = "cons/config.yml") {
  con_dets <-
    list(
      db = list(
        host = "localhost",
        port = 5432L,
        dbname = dbname,
        user = "postgres",
        password = "admin"
      )
    )
  cat("Writing the connection details to ", file, ":\n", sep = "")
  print(con_dets)
  write_yaml(con_dets, file = file)
  return(invisible(FALSE))
}

create_pg_con <- function(dbname, yaml_file = "cons/config.yml") {
  
  yaml_setup(dbname = dbname, file = yaml_file)
  
  con <- 
    do.call(
      dbConnect, 
      c(
        drv = dbDriver("PostgreSQL"), 
        yaml.load_file(yaml_file)$db
      )
    )
  return(con)
  
}

# q is a query string
pretty_print_qstr <- function(q){
  
  # make pretty for printing
  q <- gsub(";",  ";\n|",  q)
  q <- gsub(" (from|FROM) ",  "\n|    \\1 ",  q)
  q <- gsub(" (where|WHERE) ", "\n|    \\1 ", q)
  q <- gsub(" (and|AND) ",   "\n|    \\1 ",   q)
  q <- gsub(" (on|ON) ",    "\n|    \\1 ",    q)
  q <- gsub(" (or|OR) ",    "\n|    \\1 ",    q)
  q <- gsub(" (limit|LIMIT) ", "\n|    \\1 ",    q)
  q <- gsub(" (order by|ORDER BY) ", "\n|    \\1 ",    q)
  q <- gsub(" (group by|GROUP BY) ", "\n|    \\1 ",    q)
  q <- gsub(" (create|CREATE) ", "\n|    \\1 ",    q)
  q <- gsub(" (inner|INNER) ", "\n|    \\1 ",    q)
  q <- gsub(" (\\()([ ]+select)", "\\1\n|    \\2",    q)
  q <- gsub(" (\\()([ ]+SELECT)", "\\1\n|    \\2",    q)
  q <- gsub(" ([a-zA-Z ]+)(select)", " \\1\n|    \\2",    q)
  q <- gsub(" ([a-zA-Z ]+)(SELECT)", " \\1\n|    \\2",    q)
  q <- gsub(" (\\$\\$ )(SELECT)", " \\1\n|    \\2",    q)
  q <- paste0("|   ", q)
  
  return(q)
  
}
# pretty_print_qstr("REATE FUNCTION min_str_len(str1 text, str2 text) RETURNS int AS $$ SELECT GREATEST(...")

query_pg <- function(con, ...){
  
  q_ <- gsub("\n", "", paste(..., collapse = " "))
  
  # make pretty for printing
  pretty_q <- pretty_print_qstr(q_)
  
  cat(
    "Running query: \n", 
    pretty_q, 
    "\nusing provided connection '", 
    get_name(con), 
    "'\n", 
    sep = ""
  )
  
  # remove imposed "|"s
  #q_ <- gsub("\\| ", " ", q_)
  
  ptm <- proc.time()[3]
  
  tmp <- as_tibble(dbGetQuery(con, q_))
  
  
  cat(
    "Query: has returned ", 
    nrow(tmp), 
    " row(s) and ", 
    ncol(tmp), 
    " var(s) in ",
    proc.time()[3] - ptm, " seconds\n\n\n",
    sep = ""
  )
  
  return(tmp)
}

execute_pg <- function(con, ..., quiet = FALSE){
  
  q_ <- gsub("\n", "", paste(..., collapse = " "))
  
  # make pretty for printing
  pretty_q <- pretty_print_qstr(q_)
  
  if (!quiet) {
    cat(
      "Executing query: \n", 
      pretty_q, 
      "\nusing provided connection '", 
      get_name(con), 
      "'\n", 
      sep = ""
    )
  }
  
  # remove imposed "|"s
  # q_ <- gsub("\\| ", " ", q_)
  
  ptm <- proc.time()[3]
  
  tmp <- dbExecute(con, q_)
  
  cat(
    "Executed Query in ",
    proc.time()[3] - ptm, " seconds\n\n\n",
    sep = ""
  )
  
  return(tmp)
}

add_extention <- function(con, ext_nm){
  
  cur_ext <- query_pg(con, "SELECT * FROM pg_extension;") %>% pull(extname)
  if (ext_nm %in% cur_ext) {
    cat(ext_nm, "is already loaded in the database", get_name(con), "\n")
  } else {
    execute_pg(con = con, "CREATE EXTENSION ", ext_nm, ";")
  }
  
  return(invisible(TRUE))
}


set_searchpath <- function(con, schema_name){
  
  # con_nm <- get_name(con) # deparse(substitute(con))
  curr_sp <- query_pg(con, "SHOW search_path;") %>% pull(search_path)
  
  if (all(grepl(schema_name, curr_sp))) {
    cat("'", schema_name, "' is currently part of the schema serch path. Nothing to execute.\n", sep = "")
  } else {
    cat("'", schema_name, "' is not currently part of the schema serch path. Attempting to add now.\n", sep = "")
    add_sp_str <- paste("SET search_path TO", schema_name, ";")
    execute_pg(con, add_sp_str)
    curr_sp <- query_pg(con, "SHOW search_path;") %>% pull(search_path)
  }
  
  return(curr_sp)
  
}


add_rownum_col_to_tab <- function(con, tab_nm, new_col, pmy_key = "drugname") {
  
  col_exist_q <- list()
  col_exist_q$c1 <- "SELECT column_name" 
  col_exist_q$c2 <- "FROM information_schema.columns" 
  col_exist_q$c3 <- paste0("WHERE table_name='", tab_nm, "' and column_name='", new_col, "';")
  col_exist_df <- query_pg(con = con, paste(col_exist_q, collapse = "       "))
  
  
  if (nrow(col_exist_df) > 0) {
    cat("Column: '", new_col, "' in table '", tab_nm, "' already exists, attempting to overwrite.\n", sep = "")
  } else {
    cat(
      "Column: '", new_col, "' in table '", tab_nm, "' does not exist;",
      "adding and populating column with unique ID values\n", sep = ""
    )
    execute_pg(con = con, paste0("ALTER TABLE ", tab_nm, " ADD COLUMN ", new_col," bigint;"))
  }
  
  add_q <- list()
  add_q$a1 <- paste0("UPDATE ", tab_nm, " set ", new_col," = tt.rn")
  add_q$a2 <- "FROM ( "
  add_q$a3 <- paste0("SELECT row_number() over () AS rn, ", pmy_key, " FROM ", tab_nm)
  add_q$a4 <- " ) as tt"
  add_q$a5 <- paste0("WHERE ", tab_nm, ".", pmy_key, " = tt.", pmy_key, ";")
  
  cat("Creating column containing the row number in the column: '", new_col, "' in table '", tab_nm, "'\n", sep = "")
  
  execute_pg(con = con, paste(add_q, collapse = "       "))
  
  return(add_q)
  
}


add_match_col_to_tab <- function(con, r_match_tab, pgsql_tab_nm) {
  
  # testing
  # ALTER TABLE temp_possibles ADD COLUMN r_match boolean;
  # UPDATE temp_possibles set r_match = (id in (1,2,3,4));
  
  col_exist_q <- list()
  col_exist_q$c1 <- "SELECT column_name" 
  col_exist_q$c2 <- "FROM information_schema.columns" 
  col_exist_q$c3 <- paste0("WHERE table_name='", pgsql_tab_nm, "' and column_name='r_match';")
  col_exist_df <- query_pg(con = con, paste(col_exist_q, collapse = "       "))
  
  
  if (nrow(col_exist_df) > 0) {
    cat("Column: 'r_match' in table '", pgsql_tab_nm, "' already exists, attempting to overwrite.\n", sep = "")
  } else {
    execute_pg(con = con, paste0("ALTER TABLE ", pgsql_tab_nm, " ADD COLUMN r_match boolean;"))
  }
  
  r_match_ids <- r_match_tab$id[r_match_tab$match_fuzz]
  
  rmatch_q <- list()
  rmatch_q$r1 <- paste0("UPDATE ", pgsql_tab_nm, " set r_match = ")
  rmatch_q$r2 <- paste0("(id in (", paste(r_match_ids, collapse = ","), "))")
  
  cat("Creating column 'r_match' in table '", pgsql_tab_nm, "' based on r_match_tab input\n", sep = "")
  
  execute_pg(con = con, paste(rmatch_q, collapse = "       "))
  
  return(rmatch_q)
  
}


