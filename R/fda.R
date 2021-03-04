

# ---- fda_funcs ----


create_pos_drug_lst <- function(cdm_con, cdm_schema, fda_con, fda_schema, drug, form = "") {
  
  set_searchpath(fda_con, fda_schema)
  set_searchpath(cdm_con, cdm_schema)
  
  
  # need to also remove punctuation in future
  temp_tab_nm <- paste0("temp_", gsub("(-|[ _,./'])", "", drug),"_possibles")
  cat("The temp table '", temp_tab_nm, "' will be created using the fda_con within the ", fda_schema, "\n", sep = "")
  
  
  drug_syns <- find_drug_synonyms(con = cdm_con, drug, form) 
  
  search_for <- drug
  if (form != "") search_for <- c(paste(drug, form), search_for)
  search_for <- c(search_for, drug_syns)
  search_for <- tolower(search_for)
  
  n_search_for <- length(search_for)
  search_re <- paste(search_for, collapse = "|")
  
  cat("Searching for", search_re, "using the fda_con within the fda_schema.\n")
  
  ###
  ### temptab_q_init: create temp table of wide-net catches
  ###
  temptab_q_init <- list()
  temptab_q_init$drop_state <- paste0("DROP table IF EXISTS ", temp_tab_nm, "_initial_list;  ")
  temptab_q_init$ttab_state <- paste0("CREATE temp table ", temp_tab_nm, "_initial_list as ")
  
  temptab_q_init$sele_state <- "SELECT drugname, count(*) as occurrences"
  temptab_q_init$from_state <- paste0("from ", fda_schema, ".drug")
  
  # note: difference(string1, string2) is actually soundex similaririty taking values {0,1,2,3,4}
  # where 4 = all 4 of "AXXX" soundex code are equal
  # 0 = no common soundex elements
  where_temp <- list()
  where_temp$w1 <- paste("where", paste(paste0("difference(drugname, '", search_for, "') >= 2"), collapse = " or "))
  where_temp$w2 <- unique(c(str_sub(search_for, 1, 3), str_sub(search_for, -3, -1)))
  where_temp$w2 <- paste(paste0("drugname like '%", where_temp$w2, "%'"), collapse = " or ")
  temptab_q_init$where_state <- paste(where_temp, collapse = " or ")
  
  temptab_q_init$group_state <- "GROUP BY drugname"
  
  # cat("Running the following query using the fda_con within the ", fda_schema, ":\n", sep = "")
  # print(temptab_q_init)
  cat("THIS MAY TAKE ~10 minutes\n", sep = "")
  execute_pg(con = fda_con, paste(temptab_q_init, collapse = "       "))
  
  ###
  ### temptab_q: of wide-net catches, order by similarity
  ###
  temptab_q <- list()
  temptab_q$drop_state <- paste0("DROP table IF EXISTS ", temp_tab_nm, ";  ")
  temptab_q$ttab_state <- paste0("CREATE temp table ", temp_tab_nm, " as ")
  
  select_temp <- list()
  select_temp$s1 <- paste0("SELECT drugname, '", search_re, "' as real_str")
  select_temp$s2 <- paste0("str_dist(drugname, '", search_for, "')")
  select_temp$s3 <- paste0("least(", paste(select_temp$s2, collapse = ", "), ") as lev_dist0")
  select_temp$s2 <- paste(paste0(select_temp$s2, " as lev_dist", 1:n_search_for), collapse = ", ")
  select_temp$s4 <- "occurrences"
  temptab_q$sele_state <- paste(select_temp, collapse = ", ")
  
  temptab_q$from_state <- paste0("from ", temp_tab_nm, "_initial_list")
  temptab_q$order_state <- "ORDER BY lev_dist0, occurrences desc"
  
  # print(temptab_q)
  cat("THIS MAY TAKE a little time\n", sep = "")
  execute_pg(con = fda_con, paste(temptab_q, collapse = "       "))
  
  # check table has > 0 rows!
  check_tab <- query_pg(fda_con, paste0("select * from ", temp_tab_nm, " limit 100;"))
  if (!(nrow(check_tab) > 0)) {
    stop("The created temporary table '", temp_tab_nm, "' does not have any rows, likely an error in its construction.")
  }
  
  add_rownum_col_to_tab(con = fda_con, temp_tab_nm, new_col = "id", pmy_key = "drugname")
  
  return(list(
    temp_tab = temp_tab_nm,
    search_strings = search_for
  ))
  
}


do_r_fuzzy_matching <- function(con, db_tab, search_strings, manual_add = NULL, manual_rem = NULL, fuzzy_match_thresh = 0.5) {
  
  targ_str <- search_strings 
  targ_exp_strs <- paste0(targ_str, collapse = "|")
  fda_drugs_df <- query_pg(con = con, paste0("select id, drugname, occurrences from ", db_tab))
  fda_drugs_df$id <- as.integer(fda_drugs_df$id)
  srce_str <- fda_drugs_df$drugname
  
  # dl_dist(targ_str, srce_str, normalise = FALSE) 
  # cbind(srce_str, fuzzy_regex(targ_str, srce_str))
  
  
  # this is the calculaiton of our linear discrimination line using fuzzy_match_thresh
  re_thresh <- 2
  match_slope <- 1 / (re_thresh - fuzzy_match_thresh)
  match_int <- 0 - fuzzy_match_thresh * match_slope
  
  # do regular expression search and fuzzy matching
  fda_drugs_df <-
    fda_drugs_df %>%
    mutate(
      re = fuzzy_regex(targ_str, srce_str),
      fm = dl_dist(targ_str, srce_str, normalise = FALSE)$min_dist,
      match_fuzz = is_match_linear(fm, re, match_int, match_slope)
    )
  
  if (!is.null(manual_add)) {
    cat("These are the fuzzy matching rows coresponding to drugnames for manual adding to override auto fuzzy matching:\n")
    fda_drugs_df %>% dplyr::filter(drugname %in% manual_add) %>% kable(., format = "markdown") %>% print(., n = nrow(.))
    cat("\n\nManually adding ('", paste(manual_add, collapse = "','"), "') to matches now.\n", sep = "")
    fda_drugs_df <-
      fda_drugs_df %>%
      mutate(match_fuzz = ifelse(drugname %in% manual_add, TRUE, match_fuzz))
  } 
  
  if (!is.null(manual_rem)) {
    re_rm_pat <- paste0("(", paste(tolower(manual_rem), collapse = "|"), ")")
    cat("These are the fuzzy matching rows coresponding to drugnames for manual removal to override auto fuzzy matching:\n")
    cat("(using the regular expression search: '", re_rm_pat, "')\n", sep = "")
    # limit to where match_fuzz == TRUE to avoid a long list of already not matched drugs
    {rm_drgs <- fda_drugs_df %>% dplyr::filter(match_fuzz, str_detect(tolower(drugname), re_rm_pat))} %>% kable(., format = "markdown") %>% print(., n = nrow(.))
    rm_drgs <- rm_drgs %>% distinct(drugname) %>% pull(drugname)
    cat("\n\nManually removing ('", paste(rm_drgs, collapse = "','"), "') from matches now.\n", sep = "")
    fda_drugs_df <-
      fda_drugs_df %>%
      mutate(match_fuzz = ifelse(str_detect(tolower(drugname), re_rm_pat), FALSE, match_fuzz))
  } 
  
  
  cat("This is a table of fuzzy match statuses:\n")
  print(with(fda_drugs_df, table(match_fuzz)))
  cat(
    "This is the number of occurences of '", targ_exp_strs, "' in faers using fuzzy matching: ",
    sum(fda_drugs_df$occurrences[fda_drugs_df$match_fuzz]), "\n\n",
    sep = ""
  )
  
  # fda_drugs_df_subset <- 
  #   bind_rows(
  #     fda_drugs_df %>% 
  #       dplyr::filter(match_fuzz),
  #     fda_drugs_df %>% 
  #       dplyr::filter(!match_fuzz) %>% 
  #       arrange(id) %>% 
  #       dplyr::filter(row_number() <= 100),
  #   )
  
  fda_drugs_df_subset <- 
    bind_rows(
      fda_drugs_df %>% 
        dplyr::filter(match_fuzz),
      fda_drugs_df %>% 
        dplyr::filter(!match_fuzz, re < 1) %>% 
        arrange(fm) %>% 
        dplyr::filter(row_number() <= 100),
      fda_drugs_df %>% 
        dplyr::filter(!match_fuzz, re > 0) %>% 
        arrange(fm) %>% 
        dplyr::filter(row_number() <= 20)
    )
  
  
  # plotting
  npoints <- 100
  max_x <- max(fda_drugs_df_subset$fm)
  bckgnd_class <-
    as.data.frame(expand.grid(x1 = seq(0, max_x, length = npoints), x2 = seq(0, 1, length = npoints))) %>%
    mutate(match_fuzz = is_match_linear(x1, x2, match_int, match_slope)) 
  
  
  
  ggp <-
    ggplot() +
    geom_point(data = bckgnd_class, aes(x1, x2, col = factor(match_fuzz)), alpha = 0.2) +
    geom_abline(slope = match_slope, intercept = match_int) +
    geom_label_repel(data = fda_drugs_df_subset, aes(fm, re, col = factor(match_fuzz), label = drugname)) +
    geom_point(data = fda_drugs_df_subset, aes(fm, re, col = factor(match_fuzz))) +
    theme_bw() +
    labs(
      x = paste0("Lower case, punctuation removed fuzzy matching distance from '", targ_exp_strs, "'"),
      y = paste0("Regular expression match on '(", targ_exp_strs, ")'"), # with non-penalised vowel exchange"),
      col = "Matched" # "\n(0=FALSE,\n1=TRUE)"
    )
  
  print(ggp)
  
  return(fda_drugs_df)
  
}


get_fuzz_drug_dat <- function(con, drug_nm, tab_str_match) {
  
  cat("Capturing all occurences of", drug_nm, "and synonyms in the drug table\n")
  cat("Occurences are stored in the temporary table 'temp_1'\n")
  q1_list <- list()
  q1_list$drop_state <- "DROP table IF EXISTS temp_1;  "
  q1_list$crea_state <- "CREATE TEMP TABLE temp_1 AS"
  q1_list$sele_state <- "SELECT d2.primaryid, d2.caseid, d2.drug_seq, de.caseversion, d2.drugname"
  q1_list$from_state <- "from faers_dat.drug AS d2"
  q1_list$join_state <- "inner join faers_dat.demo as de ON d2.primaryid = de.primaryid"
  q1_list$wher_state <- paste(
    "where d2.drugname in ( select drugname from ", tab_str_match, " where r_match = TRUE )"
  )
  q1_list$order_state <- "order by d2.primaryid, d2.drug_seq;"
  
  q1_res <- execute_pg(con = con, paste(q1_list, collapse = "       "))
  cat(q1_res, "occurences of", drug_nm, "and synonyms have been found\n")
  
  cat("Now removing occurences of", drug_nm, "(and synonyms) where there is a newer caseversion\n")
  cat("The remaining rows are stored in the temporary table 'temp_2'\n")
  q2_list <- list()
  q2_list$drop_state <- "DROP table IF EXISTS temp_2;  "
  q2_list$crea_state <- "CREATE TEMP TABLE temp_2 AS"
  q2_list$sele_state <- "SELECT t1.*"
  q2_list$from_state <- "  from temp_1 AS t1"
  q2_list$where_state <- paste(
    "where not exists ",
    "  (select 1",
    "  from faers_dat.demo as d",
    "  where d.caseversion > t1.caseversion",
    "  and d.caseid = t1.caseid)"
  )
  q2_list$orde_state <- "order by t1.primaryid, t1.drug_seq;"
  
  q2_res <- execute_pg(con = con, paste(q2_list, collapse = "       "))
  distinct_cases <- pull(query_pg(con = con, "select count(distinct caseid) from temp_2;"))
  cat(q2_res, "occurences of", drug_nm, "and synonyms remaining after removing old case versions,")
  cat("equating to", distinct_cases, "unique case reports\n")
  
  # query_pg(con = con, "select * from temp_2;")
  # equiv to above: query_pg(con = con, "select count(distinct primaryid) from temp_2;")
  
  q3_list <- list()
  q3_list$drop_state <- "DROP table IF EXISTS temp_3;  "
  q3_list$crea_state <- "CREATE TEMP TABLE temp_3 AS"
  q3_list$sele_state <- paste0(
    "select '", drug_nm, "' as drug_search, ",
    "r.primaryid, r.caseid, ",
    "de.caseversion, r.pt, ",
    "dr.drug_seq, dr.role_cod, ",
    "dr.drugname, dr.prod_ai, ",
    "r.qtr, r.drug_rec_act, ",
    "de.event_dt, de.fda_dt, de.rept_cod, ",
    "de.age, de.age_cod, de.sex, de.occr_country, ",
    "dr.route, dr.dose_amt, ",
    "dr.dose_unit, dr.dose_form"
  )
  q3_list$from_state <- "  from temp_2 as t"
  q3_list$joi1_state <- "  inner join faers_dat.reac as r on t.primaryid = r.primaryid"
  q3_list$joi2_state <- "  inner join faers_dat.drug as dr on r.primaryid = dr.primaryid"
  q3_list$joi3_state <- "  inner join faers_dat.demo as de on dr.primaryid = de.primaryid"
  q3_list$wher_state <- "  WHERE r.primaryid in (SELECT t.primaryid from temp_2 AS t)"
  q3_list$orde_state <- "  order by r.primaryid, de.caseversion, dr.drug_seq;"
  
  q3_res <- execute_pg(con = con, paste(q3_list, collapse = "       "))
  
  
  q4_list <- list()
  q4_list$drop_state <- "DROP table IF EXISTS temp_4;  "
  q4_list$crea_state <- "CREATE TEMP TABLE temp_4 AS"
  q4_list$sele_state <- "select t1.drugname as drug_catch, t3.* "
  q4_list$from_state <- "  from temp_3 as t3"
  # if (limit_to_drug_records_only) {
  q4_list$joi1_state <- "  inner join temp_1 as t1"
  # } else {
  #   q4_list$joi1_state <- "  left join temp_1 as t1"
  # }
  q4_list$on_state <- paste(
    "on t1.primaryid = t3.primaryid", 
    "and t1.caseid = t3.caseid", 
    "and t1.caseversion = t3.caseversion", 
    "and t1.drug_seq = t3.drug_seq" 
  )
  q4_list$orde_state <- "  order by t3.primaryid, t3.caseversion, t3.drug_seq;"
  
  q4_res <- execute_pg(con = con, paste(q4_list, collapse = "       "))
  
  data_ret <- query_pg(con = con, "select * from temp_4;")
  format(object.size(data_ret), "Mb")
  
  q5_list <- list()
  q5_list$sele_state <- "select d.primaryid, d.drug_seq, d.drugname, d.prod_ai, d.qtr"
  q5_list$from_state <- "from drug as d"
  q5_list$whe1_state <- "where exists (" # d.drug_seq < 21 and
  q5_list$exis_state <- paste(
    "select 1 from temp_4 as t",
    "where t.primaryid = d.primaryid"
    # "and t.drug_seq <> d.drug_seq"
  )
  q5_list$whe2_state <- ")"
  q5_list$orde_state <- "order by d.primaryid, d.drug_seq, d.qtr DESC"
  drug_oth <- query_pg(con = con, paste(q5_list, collapse = "  "))
  
  drug_oth <- 
    drug_oth %>% 
    mutate(drug_seq = sprintf("%04.0f", drug_seq))
  
  # these are most recent quarters when there are duplicate entires
  drug_oth <- 
    drug_oth %>% 
    group_by(primaryid, drug_seq) %>%
    dplyr::filter(row_number() == 1) %>% 
    ungroup() %>%
    dplyr::select(-qtr)
  
  drug_oth_w1 <- 
    pivot_wider(
      data = drug_oth %>% dplyr::select(primaryid, drug_seq, drugname),
      names_from = drug_seq,
      names_prefix = "d",
      values_from = drugname
      # values_fn = list(drugname = length)
    )
  
  # error testing 
  # filter_at(drug_oth_w1, vars(starts_with("d")), any_vars(. > 1))
  # drug_oth %>% dplyr::filter(primaryid == 94306431)
  # query_pg(con = fda_con, "select * from drug where primaryid = 94306431")
  
  drug_oth_cns1 <- colnames(drug_oth_w1)
  drug_oth_w1 <-
    unite(
      data = drug_oth_w1,
      col = "oth_drg",
      tidyselect::vars_select(drug_oth_cns1, matches("d[0-9]{4}", perl = TRUE)),
      sep = "!",
      na.rm = TRUE,
      remove = TRUE
    )
  
  drug_oth_w2 <- 
    pivot_wider(
      data = drug_oth %>% dplyr::select(primaryid, drug_seq, prod_ai),
      names_from = drug_seq,
      names_prefix = "d",
      values_from = prod_ai
    )
  drug_oth_cns2 <- colnames(drug_oth_w2)
  drug_oth_w2 <-
    unite(
      data = drug_oth_w2,
      col = "oth_pai",
      tidyselect::vars_select(drug_oth_cns2, matches("d[0-9]{4}", perl = TRUE)),
      sep = "!",
      na.rm = TRUE,
      remove = TRUE
    )
  
  drug_oth_w <- left_join(drug_oth_w1, drug_oth_w2, "primaryid")
  # format(object.size(drug_oth_w), "Mb")
  # clean up for garbage collection
  rm(drug_oth, drug_oth_w1, drug_oth_w2)
  
  
  q6_list <- list()
  q6_list$sele_state <- "select i.primaryid, i.indi_drug_seq, i.indi_pt, i.qtr"
  q6_list$from_state <- "from indi as i"
  q6_list$whe1_state <- "where i.indi_pt is not null and exists ("
  q6_list$exis_state <- paste(
    "select 1 from temp_4 as t",
    "where t.primaryid = i.primaryid",
    "and t.drug_seq = i.indi_drug_seq"
  )
  q6_list$whe2_state <- ")"
  q6_list$orde_state <- "order by i.primaryid, i.indi_drug_seq, i.qtr DESC"
  data_indi <- query_pg(con = con, paste(q6_list, collapse = "  "))
  
  
  # data_indi %>% group_by(primaryid, indi_drug_seq) %>% summarise(n = n()) %>% dplyr::filter(n > 1)
  # query_pg(con = fda_con, "select * from temp_4 where primaryid =  37184223") %>% print(., n = nrow(.))
  # query_pg(con = fda_con, "select * from indi where primaryid =  37184223")
  # data_indi %>% group_by(primaryid, indi_drug_seq) %>% summarise(n = n()) %>% dplyr::filter(primaryid ==  37184223)
  
  
  data_indi <-
    data_indi %>% 
    group_by(primaryid, indi_drug_seq) %>% 
    mutate(indi_n = 1:n(), indi_n = sprintf("%04.0f", indi_n)) %>% 
    ungroup() %>%
    rename(drug_seq = indi_drug_seq)
  
  # test
  # data_indi %>% dplyr::filter(primaryid == 131671791)
  
  data_indi_w <- 
    pivot_wider(
      data = data_indi,
      id_cols = c(primaryid, drug_seq),
      names_from = indi_n,
      names_prefix = "pt",
      values_from = indi_pt,
      values_fill = NULL
      # this only required if pivot wasn't unique within primaryid, drug_seq comb
      # values_fn = list(indi_pt = function(x) paste(x, collapse = "!"))
    ) 
  
  indi_cns <- colnames(data_indi_w)
  data_indi_w <-
    unite(
      data = data_indi_w,
      col = "indis",
      tidyselect::vars_select(indi_cns, matches("pt[0-9]{4}", perl = TRUE)),
      sep = "!",
      na.rm = TRUE,
      remove = TRUE
    )
  
  n_ret <- nrow(data_ret)
  data_ret <- left_join(data_ret, data_indi_w, c("primaryid", "drug_seq"))
  
  if (nrow(data_ret) != n_ret) {
    warning(
      "*********** When left joining indications to the temp_4 data, the number of rows was altered.",
      "Likely duplicate rows in 'indi' ***********"
    )
  }
  
  data_ret <- left_join(data_ret, drug_oth_w, "primaryid")
  
  if (nrow(data_ret) != n_ret) {
    warning(
      "*********** When left joining other_drugs to the temp_4 data, the number of rows was altered.",
      "Likely duplicate rows in 'other_drugs' ***********"
    )
  }
  
  # optional, but removes potentially large temp tables
  # execute_pg(con = con, "DROP table IF EXISTS temp_1, temp_2, temp_3;") # keep temp_4
  
  return(data_ret)
  
}


# return tibble but has attrs: temp_tab_nm, search_strings, postgres_fuzz_search, r_fuzz_search
auto_get_dat <- function(cdm_con, cdm_schema, fda_con, fda_schema, drug, form = "", manual_add = NULL, manual_rem = NULL) {
  
  create_pos_drug_lst_obj <-
    create_pos_drug_lst( # returns $temp_tab name and $search_strings
      cdm_con = cdm_con, 
      cdm_schema = cdm_schema, 
      fda_con = fda_con, 
      fda_schema = fda_schema, 
      drug = drug, 
      form = form
    )
  
  temp_tab_nm <- create_pos_drug_lst_obj$temp_tab
  search_strings <- create_pos_drug_lst_obj$search_strings
  
  # list of fuzzy match candidates from PGSQL
  postgres_fuzz_search <-
    query_pg(fda_con, paste0("select * from ", temp_tab_nm, ";"))
  # query_pg(fda_con, paste0("select * from ", temp_tab_nm, " limit 10000;")) # check
  
  # list of fuzzy matches using R algorithm
  r_fuzz_search <- 
    do_r_fuzzy_matching( # returns a tibble
      con = fda_con, 
      db_tab = temp_tab_nm, 
      search_strings = search_strings, 
      manual_add = manual_add,
      manual_rem = manual_rem
    )
  
  # no output in R, just updating tables in postgres based on R's fuzzty matches
  add_match_col_to_tab(
    con = fda_con, 
    r_match_tab = r_fuzz_search,
    pgsql_tab_nm = temp_tab_nm
  )
  
  # test R and postgres have the same capture of drug numbers
  n_match_pgsql <- pull(query_pg(con = fda_con, paste0("select sum(occurrences) from ", temp_tab_nm, " where r_match = true")))
  n_match_r <- sum(r_fuzz_search$occurrences[r_fuzz_search$match_fuzz])
  if (n_match_pgsql != n_match_r) {
    stop("PGSQL table IDs likely to not have correct r_match values associated with them.")
  }
  
  # get line data that can be used for ROR and PRR analysis
  raw_analysis_dat <-
    get_fuzz_drug_dat(con = fda_con, drug_nm = drug, tab_str_match = temp_tab_nm) # %>%
  # mutate_at(vars(caseversion, drug_seq), as.integer) %>%
  # mutate_at(vars(primaryid, caseid), as.character)
  
  
  class(raw_analysis_dat) <- c("fuzzy_faers_drug_obj", class(raw_analysis_dat))
  
  attr(raw_analysis_dat, "temp_tab_nm") <- temp_tab_nm
  attr(raw_analysis_dat, "search_strings") <- search_strings
  attr(raw_analysis_dat, "postgres_fuzz_search") <- postgres_fuzz_search
  attr(raw_analysis_dat, "r_fuzz_search") <- r_fuzz_search
  
  return(raw_analysis_dat)
  
}



get_drug_outc_2x2 <- function(con, drug, outc, drug_role = c("PS", "SS")) {
  
  
  drug_concat <- paste0("[", paste(drug, collapse = "|"), "]")
  outc_concat <- paste0("[", paste(outc, collapse = "|"), "]")
  
  q5_list <- list()
  q5_list$crea_state <- "CREATE TEMP TABLE temp_5  AS"
  q5_list$sele_state <- paste0("select distinct primaryid, '", drug, "' as drug_search")
  q5_list$from_state <- "from temp_4"
  q5_list$wher_state <- paste0("where role_cod in ('", paste(drug_role, collapse = "','"), "')")
  execute_pg(con, "DROP table IF EXISTS temp_5;  ")
  execute_pg(con, paste(q5_list, collapse = "       "))
  
  
  q6_list <- list()
  q6_list$crea_state <- "CREATE TEMP TABLE temp_6  AS"
  q6_list$sele_state <- "select distinct d.primaryid from demo as d"
  # -- make sure only latest caseversion is included, remove old ones
  # -- love an "exists/not exists" clause! */
  q6_list$note_state <- paste(
    "where not exists (select 1",
    "    from demo as d0",
    "    where d0.caseversion > d.caseversion",
    "    and d0.caseid = d.caseid",
    "  ) ;"
  )
  execute_pg(con, "DROP table IF EXISTS temp_6;  ")
  execute_pg(con,  paste(q6_list, collapse = "       "))
  
  
  q7_list <- list()
  q7_list
  q7_list$crea_state <- "CREATE TEMP TABLE temp_7  AS"
  q7_list$sele_state <- "select distinct t6.primaryid, t5.drug_search "
  q7_list$from_state <- "from temp_6 as t6"
  q7_list$join_state <- "left join temp_5 as t5 on t6.primaryid = t5.primaryid"
  execute_pg(con, "DROP table IF EXISTS temp_7;  ")
  execute_pg(con, paste(q7_list, collapse = "       "))
  execute_pg(con, paste0("UPDATE temp_7 SET drug_search = '", drug_concat, "' WHERE drug_search is not null;"))
  execute_pg(con, paste0("UPDATE temp_7 SET drug_search = 'not ", drug_concat, "' WHERE drug_search is null;"))
  # query_pg(con, "select drug_search, count(*) from temp_7 group by drug_search;")
  
  
  q8_list <- list()
  q8_list$crea_state <- "CREATE TEMP TABLE temp_8  AS "
  q8_list$sele_state <- "select distinct primaryid, lower(pt) as pt"
  q8_list$from_state <- "from reac"
  q8_list$wher_state <- 
    paste0(
      "where lower(pt) in ('", 
      paste(outc, collapse = "','"), 
      "')"
    )
  execute_pg(con, "DROP table IF EXISTS temp_8;  ")
  execute_pg(con, paste(q8_list, collapse = "       "))
  
  
  q9_list <- list()
  q9_list
  q9_list$crea_state <- "CREATE TEMP TABLE temp_9  AS"
  q9_list$sele_state <- "select distinct t7.*, t8.pt "
  q9_list$from_state <- "from temp_7 as t7"
  q9_list$join_state <- "left join temp_8 as t8 on t7.primaryid = t8.primaryid"
  execute_pg(con, "DROP table IF EXISTS temp_9;  ")
  execute_pg(con,  paste(q9_list, collapse = "       "))
  execute_pg(con,  paste0("UPDATE temp_9 SET pt = '", outc_concat, "' WHERE pt is not null;"))
  execute_pg(con,  paste0("UPDATE temp_9 SET pt = 'not ", outc_concat, "' WHERE pt is null;"))
  
  res_tab <- query_pg(con,  "select drug_search, pt, count(distinct primaryid) as count from temp_9 group by drug_search, pt;")
  res_tab <- xtabs(count ~ drug_search + pt, data = res_tab)
  return(res_tab)
  
}




get_drug_outc_2x3 <- function(con, drug, outc1, outc2, drug_role = c("PS", "SS"), outc1_lab = "outc1", outc2_lab = "outc2") {
  
  q5_list <- list()
  q5_list$crea_state <- "CREATE TEMP TABLE temp_5  AS"
  q5_list$sele_state <- paste0("select distinct primaryid, '", drug, "' as drug_search")
  q5_list$from_state <- "from temp_4"
  q5_list$wher_state <- paste0("where role_cod in ('", paste(drug_role, collapse = "','"), "')")
  execute_pg(con, "DROP table IF EXISTS temp_5;  ")
  execute_pg(con, paste(q5_list, collapse = "       "))
  
  
  q6_list <- list()
  q6_list$crea_state <- "CREATE TEMP TABLE temp_6  AS"
  q6_list$sele_state <- "select distinct d.primaryid from demo as d"
  # -- make sure only latest caseversion is included, remove old ones
  # -- love an "exists/not exists" clause! */
  q6_list$note_state <- paste(
    "where not exists (select 1",
    "    from demo as d0",
    "    where d0.caseversion > d.caseversion",
    "    and d0.caseid = d.caseid",
    "  ) ;"
  )
  execute_pg(con, "DROP table IF EXISTS temp_6;  ")
  execute_pg(con,  paste(q6_list, collapse = "       "))
  
  
  q7_list <- list()
  q7_list
  q7_list$crea_state <- "CREATE TEMP TABLE temp_7  AS"
  q7_list$sele_state <- "select distinct t6.primaryid, t5.drug_search "
  q7_list$from_state <- "from temp_6 as t6"
  q7_list$join_state <- "left join temp_5 as t5 on t6.primaryid = t5.primaryid"
  execute_pg(con, "DROP table IF EXISTS temp_7;  ")
  execute_pg(con, paste(q7_list, collapse = "       "))
  execute_pg(con, paste0("UPDATE temp_7 SET drug_search = '_not ", drug, "' WHERE drug_search is null;"))
  # query_pg(con, "select drug_search, count(*) from temp_7 group by drug_search;")
  
  
  q8a_list <- list()
  q8a_list$crea_state <- "CREATE TEMP TABLE temp_8a  AS "
  q8a_list$sele_state <- paste0("select distinct primaryid, '", outc1_lab, "' as pt")
  q8a_list$from_state <- "from reac"
  q8a_list$wher_state <- paste0("where lower(pt) in ('", paste(outc1, collapse = "','"), "')")
  execute_pg(con, "DROP table IF EXISTS temp_8a;  ")
  execute_pg(con, paste(q8a_list, collapse = "       "))
  
  q8b_list <- list()
  q8b_list$crea_state <- "CREATE TEMP TABLE temp_8b  AS "
  q8b_list$sele_state <- paste0("select distinct primaryid, '", outc2_lab, "' as pt")
  q8b_list$from_state <- "from reac"
  q8b_list$wher_state <- paste0("where lower(pt) in ('", paste(outc2, collapse = "','"), "')")
  execute_pg(con, "DROP table IF EXISTS temp_8b;  ")
  execute_pg(con, paste(q8b_list, collapse = "       "))
  
  
  q9_list <- list()
  q9_list
  q9_list$crea_state <- "CREATE TEMP TABLE temp_9  AS"
  q9_list$sele_state <- "select distinct t7.*, t8a.pt as pt, t8b.pt as pt2"
  q9_list$from_state <- "from temp_7 as t7"
  q9_list$join_stat1 <- "left join temp_8a as t8a on t7.primaryid = t8a.primaryid"
  q9_list$join_stat2 <- "left join temp_8b as t8b on t7.primaryid = t8b.primaryid"
  execute_pg(con, "DROP table IF EXISTS temp_9;  ")
  execute_pg(con,  paste(q9_list, collapse = "       "))
  execute_pg(con,  paste0("UPDATE temp_9 SET pt = pt2 WHERE pt is null and pt2 is not null;"))
  execute_pg(con,  paste0("UPDATE temp_9 SET pt = '_not either' WHERE pt is null;"))
  
  res_tab <- query_pg(con,  "select drug_search, pt, count(distinct primaryid) as count from temp_9 group by drug_search, pt;")
  res_tab <- xtabs(count ~ drug_search + pt, data = res_tab)
  return(res_tab)
  
}







plot_fuzz_match <- function(capture_dat, fuzzy_match_thresh = 0.5, reg_exp_thresh = 2.0, save_plot = NULL) {
  
  if (is.null(attr(capture_dat, "r_fuzz_search"))) {
    return("dataset provided does not have 'r_fuzz_search' attribute so cannot produce plot")
  } 
  
  if (is.null(attr(capture_dat, "search_strings"))) {
    return("dataset provided does not have 'search_strings' attribute so cannot produce plot")
  } 
  
  targ_exp_strs <- paste(attr(capture_dat, "search_strings"), collapse = "|")
  
  fuz_dat <- attr(capture_dat, "r_fuzz_search")
  
  # this is the calculaiton of our linear discrimination line using fuzzy_match_thresh
  match_slope <- 1 / (reg_exp_thresh - fuzzy_match_thresh)
  match_int <- 0 - fuzzy_match_thresh * match_slope
  
  fuz_dat_subset <- 
    bind_rows(
      fuz_dat %>% 
        dplyr::filter(match_fuzz),
      fuz_dat %>% 
        dplyr::filter(!match_fuzz, re < 1) %>% 
        # arrange(id) %>% 
        arrange(fm) %>% 
        dplyr::filter(row_number() <= 100),
      fuz_dat %>% 
        dplyr::filter(!match_fuzz, re > 0) %>% 
        # arrange(id) %>% 
        arrange(fm) %>% 
        dplyr::filter(row_number() <= 20)
    )
  
  # plotting
  npoints <- 100
  max_x <- max(fuz_dat_subset$fm)
  bckgnd_class <-
    as.data.frame(
      expand.grid(
        x1 = seq(0, max_x, length = npoints), 
        x2 = seq(0, 1, length = npoints)
      )
    ) %>%
    mutate(match_fuzz = is_match_linear(x1, x2, match_int, match_slope))   
  
  ggp <-
    ggplot() +
    geom_point(data = bckgnd_class, aes(x1, x2, col = factor(match_fuzz)), alpha = 0.2) +
    geom_abline(slope = match_slope, intercept = match_int) +
    geom_label_repel(data = fuz_dat_subset, aes(fm, re, col = factor(match_fuzz), label = drugname)) +
    geom_point(data = fuz_dat_subset, aes(fm, re, col = factor(match_fuzz))) +
    theme_bw() +
    labs(
      x = paste0("Lower case, punctuation removed fuzzy matching distance from '", targ_exp_strs, "'"),
      y = paste0("Regular expression match on '(", targ_exp_strs, ")'"), # with non-penalised vowel exchange"),
      col = "Matched" # "\n(0=FALSE,\n1=TRUE)"
    )
  
  
  if (!is.null(save_plot)) {
    ggsave(
      filename = save_plot,
      plot = ggp,
      path = NULL,
      scale = 1,
      width = 15,
      height = 9,
      units = "in"
    )
  } 
  
  return(ggp)
  
}




