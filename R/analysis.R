

# ---- analysis_funcs ----


concat_analysis_dat <- function(...) {
  
  list_of_fuzzy_faers_drug_obj <- list(...)
  
  if (!is.list(list_of_fuzzy_faers_drug_obj)) {
    stop("Input must be an arbitary number of inputs that can be converted to a list().")
  }
  
  # remove NULL elements as function accepts NULLs
  nulls <- unlist(lapply(list_of_fuzzy_faers_drug_obj, is.null))
  list_of_fuzzy_faers_drug_obj <- list_of_fuzzy_faers_drug_obj[!nulls]
  
  n_dat <- length(list_of_fuzzy_faers_drug_obj)
  for (i in 1:n_dat) {
    if(!("fuzzy_faers_drug_obj" %in% class(list_of_fuzzy_faers_drug_obj[[i]]))) {
      stop("Input must be a list of 'fuzzy_faers_drug_obj' objects.")
    }
  }
  
  ns <- unlist(lapply(list_of_fuzzy_faers_drug_obj, nrow))
  
  out_analysis_dat <-
    foreach(i = 1:n_dat, .combine = bind_rows) %do% {
      list_of_fuzzy_faers_drug_obj[[i]]
    }
  
  if (!(nrow(out_analysis_dat) == sum(ns))) {
    stop("Error in concatonating the datasets: the resulting row number has changed.")
  }
  
  return(out_analysis_dat)
  
}


create_row_per_caseid <- function(dat, outc_of_interest, role_filter = c("PS", "SS", "C", "I"), outc_exclusions = NULL) {
  
  outc_of_interest <- tolower(outc_of_interest)  
  outc_concat <- paste0("[", paste(outc_of_interest, collapse = "|"), "]")
  
  if (!is_na_or_null(outc_exclusions)) {
    outc_exclusions <- tolower(outc_exclusions)
  }
  if (!is_na_or_null(role_filter)) {
    dat <-
      dat %>%
      dplyr::filter(role_cod %in% role_filter)
  }
  dat$pt <- tolower(dat$pt)
  
  # remove rows without fuzzy match on drug
  dat <-
    dat %>%
    dplyr::filter(!is.na(drug_catch))
  
  if (!is_na_or_null(outc_exclusions)) {
    outc_exclusions <- tolower(outc_exclusions)
    outc_exclusions <- outc_exclusions[!(outc_exclusions %in% outc_of_interest)] # don't exclude outcome of interest!
    cat("Now excluding primaryids where the following preferred terms are included the record reaction:\n-----------\n")
    if (length(outc_exclusions) > 3) {
      cat(paste(c(outc_exclusions[1:2], "...", outc_exclusions[length(outc_exclusions)]), collapse = "\n"), "\n-----------\n")
    } else {
      cat(paste(outc_exclusions, collapse = "\\n"), "\n-----------\n")
    }
    
    pids_prior <- dat %>% group_by(drug_search, primaryid) %>% summarise(n = n()) %>% nrow(.)
    exclude_pids <-
      dat %>%
      dplyr::filter(pt %in% outc_exclusions) %>%
      distinct(primaryid, pt)    
    keep_pids <-
      dat %>%
      dplyr::filter(pt %in% outc_of_interest) %>%
      distinct(primaryid)
    exclude_pids <-      
      anti_join(
        exclude_pids,
        keep_pids,
        "primaryid"
      )
    dat <-
      anti_join(
        dat,
        exclude_pids,
        "primaryid"
      )
    pids_post <- dat %>% group_by(drug_search, primaryid) %>% summarise(n = n()) %>% nrow(.)
    cat(
      "From removing primaryids with preferred terms to be excluded, the number of primaryids is now",
      pids_post, "after removing", pids_prior - pids_post, "\n"
    )
    
  }
  
  dat$outc <- dat$pt %in% outc_of_interest
  
  grepl_trgt <- paste0("(", paste(outc_of_interest, collapse = "|"), ")")
  outc_grepl <- grepl(grepl_trgt, dat[["pt"]])
  
  if (any(outc_grepl != dat[["outc"]])) {
    cat(
      "The following PTs contain the outc_of_interest strings but are not exact matches.\n",
      "Should they be included in the outc_of_interest vector?\n"
    )
    print(table(dat$pt[!dat[["outc"]] & outc_grepl]))
  }
  
  dat_with_outc <-
    dat %>%
    dplyr::filter(outc) %>%
    arrange(drug_search, primaryid, drug_seq) %>%
    group_by(drug_search, primaryid) %>%
    dplyr::filter(1 == row_number())  %>%
    mutate(pt = outc_concat)
  
  dat_wout_outc <-
    dat %>%
    dplyr::filter(!outc) %>%
    arrange(drug_search, primaryid, drug_seq) %>%
    group_by(drug_search, primaryid) %>%
    dplyr::filter(1 == row_number()) %>%
    mutate(pt = paste0("not ", outc_concat))
  
  dat <- bind_rows(dat_with_outc, dat_wout_outc)
  
  # remove "_not_outc_of_interest" rows where there is a primaryid, drug combo with "outc_of_interest"
  dat <- 
    dat %>%
    # order by pt = outcome of interest first
    arrange(drug_search, primaryid, pt) %>%
    group_by(drug_search, primaryid) %>%
    dplyr::filter(1 == row_number()) %>%
    ungroup()
  
  cat("### Summary of data (cross-tab of drug vs outcome) before caseids with both drugs are removed.\n")
  dat %>% 
    group_by(drug_search, pt) %>% 
    summarise(n = n()) %>%
    kable(.) %>%
    print(.)
  
  ###### OLD
  # dat <-
  #   dat %>%
  #   group_by(drug_search, caseid) %>%
  #   summarise(outc = sum(outc)) %>%
  #   ungroup() %>%
  #   mutate(outc = ifelse(outc > 0, outc_of_interest, paste0("_not ", outc_of_interest)))
  # 
  # cat("### Summary of data (cross-tab of drug vs outcome) before caseids with both drugs are removed.\n")
  # dat %>% 
  #   group_by(drug_search, outc) %>% 
  #   summarise(n = n()) %>%
  #   kable(.) %>%
  #   print(.)
  
  # dups <-
  #   dat %>% group_by(caseid, outc) %>% summarise(cnt = n()) %>% dplyr::filter(cnt > 1)
  # 
  # n_dup <- nrow(dups)
  # if (n_dup > 0) {
  #   dup_tab <- table(dups$outc)
  #   cat(
  #     "NOTE:\n",
  #     "* there are ", dup_tab[1], " caseids where both drugs are used and the outcome '", names(dup_tab)[1], "' occurs, and\n",
  #     "* there are ", dup_tab[2], " caseids where both drugs are used and the outcome '", names(dup_tab)[2], "' occurs.\n",
  #     "ALL ", sum(dup_tab), " of these records are being removed from the analysis data.\n",
  #     sep = ""
  #   )
  #   
  #   n_dat <- nrow(dat)
  #   dat <-
  #     dat %>%
  #     anti_join(., dups, "caseid")
  #   
  #   if ((n_dat - 2 * sum(dup_tab)) != nrow(dat)) {
  #     cat("There were", n_dat, "rows,", sum(dup_tab), "duplicates, leaving a remaining", nrow(dat), "rows\n")
  #     stop("Error in removing duplicate rows")
  #   }
  #   
  #   cat("### Summary of data (cross-tab of drug vs outcome) AFTER caseids with both drugs are removed.\n")
  #   dat %>% 
  #     group_by(drug_search, outc) %>% 
  #     summarise(n = n()) %>%
  #     kable(.) %>%
  #     print(.)
  #   
  # } else {
  #   
  #   cat("### Good news, no caseids with both drugs were found, the above data summary reflects data being returned.\n")
  # 
  # }
  
  return(dat)
  
}

create_row_per_caseid_kp_excl <- function(dat, outc_of_interest, role_filter = c("PS", "SS", "C", "I"), outc_exclusions = NULL) {
  
  outc_of_interest <- tolower(outc_of_interest)
  outc_concat <- paste0("[", paste(outc_of_interest, collapse = "|"), "]")
  
  if (!is_na_or_null(outc_exclusions)) {
    outc_exclusions <- tolower(outc_exclusions)
  }
  if (!is_na_or_null(role_filter)) {
    dat <-
      dat %>%
      dplyr::filter(role_cod %in% role_filter)
  }
  dat$pt <- tolower(dat$pt)
  
  # remove rows without fuzzy match on drug
  dat <-
    dat %>%
    dplyr::filter(!is.na(drug_catch))
  
  
  # dat$outc <- grepl(outc_of_interest, dat$pt)
  
  dat$outc <- dat$pt %in% outc_of_interest

  grepl_trgt <- paste0("(", paste(outc_of_interest, collapse = "|"), ")")
  outc_grepl <- grepl(grepl_trgt, dat[["pt"]])

  if (any(outc_grepl != dat[["outc"]])) {
    cat(
      "The following PTs contain the outc_of_interest strings but are not exact matches.\n",
      "Should they be included in the outc_of_interest vector?\n"
    )
    print(table(dat$pt[!dat[["outc"]] & outc_grepl]))
  }

  
  
  
  dat_with_outc <-
    dat %>%
    dplyr::filter(outc) %>%
    arrange(drug_search, primaryid, drug_seq) %>%
    group_by(drug_search, primaryid) %>%
    dplyr::filter(1 == row_number()) %>%
    ungroup() %>%
    mutate(pt = paste("(1)", outc_concat))
  
  if (!is_na_or_null(outc_exclusions)) {
    
    dat$excl <- pull(dat, pt) %in% outc_exclusions
    
    dat_excl <-
      dat %>%
      dplyr::filter(excl) %>%
      arrange(drug_search, primaryid, drug_seq) %>%
      group_by(drug_search, primaryid) %>%
      dplyr::filter(1 == row_number()) %>%
      ungroup() %>%
      mutate(pt = "(2) exclusion conds")
    
    dat_excl <- dat_excl %>% dplyr::select(-excl)
    
    # don't want those in both (1) and (2)
    dat_excl <-
      anti_join(
        dat_excl,
        dat_with_outc,
        c("drug_search", "primaryid")
      )
    
    
    # add exclusion outcomes to rolling dataset
    dat_with_outc <- bind_rows(dat_with_outc, dat_excl)
  }
  
  dat_wout_outc <-
    dat %>%
    dplyr::filter(!outc) %>%
    arrange(drug_search, primaryid, drug_seq) %>%
    group_by(drug_search, primaryid) %>%
    dplyr::filter(1 == row_number()) %>%
    ungroup() %>%
    mutate(pt = paste0("(3) not ", outc_concat))
  
  # don't want those in either (1) and (2)
  dat_wout_outc <-
    anti_join(
      dat_wout_outc,
      dat_with_outc,
      c("drug_search", "primaryid")
    )
  
  dat <- bind_rows(dat_with_outc, dat_wout_outc)
  
  dat <- dat %>% dplyr::select(-outc)
  
  cat("### Summary of data (cross-tab of drug vs outcome) before caseids with both drugs are removed.\n")
  dat %>% 
    group_by(drug_search, pt) %>% 
    summarise(n = n()) %>%
    kable(.) %>%
    print(.)
  
  return(dat)
  
}























