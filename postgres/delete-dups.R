
### This file is used to create the SQL code to remove duplicate FAERS records in the
### postgres database

### (this R code only kept for posterity, if any changes are required)


pids_lst <- list()
# just primaryid is enough, even though not unique, only want the newest 
# primaryid records for a FAERS quarterly extract
pids_lst$ther <- c("primaryid") #, "dsg_drug_seq")
pids_lst$indi <- c("primaryid") #, "indi_drug_seq")
pids_lst$drug <- c("primaryid") #, "drug_seq")
pids_lst$demo <- c("primaryid")
pids_lst$reac <- c("primaryid")
pids_lst$outc <- c("primaryid")
pids_lst$rpsr <- c("primaryid")
pids_lst

n_tabs <- length(pids_lst)




dups_print <- function(tab_nm, pids) {
  
  paste(
    # paste0("drop table if exists tmp_dups_", tab_nm, ";"),
    # paste0("create temp table tmp_dups_", tab_nm, " as"),
    paste0("select 'Duplicate records in ", tab_nm, " to be deleted:' as dup_descrip, count(*) as dup_count"),
    paste0("  from ", tab_nm, " as t0"),
    "where exists (",
    "  select 1",
    paste0("  from ", tab_nm, " as t1"),
    paste0(
      "  where " ,
      paste(paste0("t0.", pids, " = t1.", pids), collapse = " and ")
    ),
    "  and t0.qtr < t1.qtr",
    ")",
    # paste0("order by ", paste(pids, collapse = ", "), ", qtr"),
    # ";",
    sep = "\n"
  )
  
}

### test
# cat(dups_print(names(pids_lst)[1], pids_lst[[1]]))

for (i in 1:n_tabs) {
  
  cat(
    "\n", 
    dups_print(names(pids_lst)[i], pids_lst[[i]]), 
    "\n\n", 
    sep = ""
  )
  
  if (i < n_tabs) {
    cat("union\n")
  } else {
    cat(";\n\n")
  }
  
}


dups_delete <- function(tab_nm, pids) {
  
  paste(
    paste0("DELETE FROM ", tab_nm),
    "where exists (",
    "  select 1",
    paste0("  from ", tab_nm, " as t1"),
    paste0(
      "  where " ,
      paste(paste0(tab_nm, ".", pids, " = t1.", pids), collapse = " and ")
    ),
    paste0("  and ", tab_nm, ".qtr < t1.qtr"),
    ")",
    "-- RETURNING * /* optional to print deleted rows */",
    ";",
    sep = "\n"
  )
  
}

for (i in 1:n_tabs) {
  
  cat(
    "\n", 
    dups_delete(names(pids_lst)[i], pids_lst[[i]]), 
    "\n\n", 
    sep = ""
  )
  
}






