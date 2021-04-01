

# ---- cdm_funcs ----

find_drug_cdm <- function(con, search_str) {
  
  query_pg(
    con = con,
    "select *", 
    "from concept",
    paste0("where lower(concept_name) like '%", search_str, "%'"),
    "and vocabulary_id = 'RxNorm'",
    "and invalid_reason is NULL",
    "and concept_class_id = 'Clinical Drug Form'",
    "order by concept_id"
  )
  
}

find_drug_concept <- function(con, search_str, form = "") {
  
  search_str <- tolower(search_str)
  sql_res <- find_drug_cdm(con = con, search_str)
  n_res <- nrow(sql_res)
  
  if (n_res < 1) {
    stop("No 'Clinical Drug Form' could be found associated with '", search_str, "'")
  } else if (n_res > 1) {
    cat("#### More than one possible result returned - automatically selecting best match ####\n")
    
    poss_concepts <- pull(sql_res, concept_name)
    str_sims <- as.vector(ldist(paste(search_str, form), poss_concepts))
    row_choose <- which.max(str_sims)
    cat(
      "Of the possible matches: '", 
      paste(poss_concepts, collapse = "'|'"), 
      "'\n'", poss_concepts[row_choose],"' was chosen by string similarity\n", 
      sep = ""
    )
    sql_res <- sql_res[row_choose, ]
    
  }
  
  return(pull(sql_res, concept_id))
  
}



find_drug_synonyms <- function(con, search_str, form = "") {
  
  cid <- find_drug_concept(con = con, search_str, form)
  sql_res <-
    query_pg(
      con = con,
      "select cr.concept_id_1, cr.concept_id_2, ",
      "  c1.concept_name as cn1, cr.relationship_id, c2.concept_name as cn2, ",
      "  c1.concept_class_id as ccid1, c2.concept_class_id as ccid2",
      "from concept_relationship as cr",
      "join concept as c1",
      "on cr.concept_id_1 = c1.concept_id",
      "join concept as c2",
      "on cr.concept_id_2 = c2.concept_id",
      "where (cr.concept_id_1 = ", cid, " or cr.concept_id_2 = ", cid, ") ",
      "and cr.invalid_reason is NULL",
      "and c1.invalid_reason is NULL",
      "and c2.invalid_reason is NULL",
      "and cr.relationship_id like '%radename%'",
      "order by cr.relationship_id"
    )
  
  syn_list <- unique(c(pull(sql_res, cn1), pull(sql_res, cn2)))
  syn_list <- syn_list[grepl("\\[.*\\]", syn_list)]
  syn_list <- gsub(".*\\[(.*)\\].*", "\\1", syn_list)
  return(syn_list)
  
}




find_branded_cdm <- function(con, search_str, max_char = 8, first_word = TRUE) {
  
  
  search_str <- tolower(search_str)
  if (nchar(search_str) > max_char) search_str <- substr(search_str, 1, 8)
  if (first_word) search_str <- gsub("^([A-Za-z0-9\\-]+) .*", "\\1", search_str)
  
  query_pg(
    con = con,
    "select *", 
    "from concept",
    paste0("where lower(concept_name) like '%", search_str, "%'"),
    "and vocabulary_id = 'RxNorm'",
    "and invalid_reason is NULL",
    "and concept_class_id = 'Branded Drug Form'",
    "order by concept_id"
  )
  
}
# USAGE:
# find_branded_cdm(cdm_con, "humira")


find_branded_concept <- function(con, search_str, form = "", max_char = 8, first_word = TRUE) {
  
  search_str <- tolower(search_str)
  sql_res <- find_branded_cdm(con = con, search_str, max_char = max_char, first_word = first_word)
  n_res <- nrow(sql_res)
  
  if (n_res < 1) {
    cat("No 'Branded Drug Form' could be found associated with '", search_str, "'\n", sep = "")
    return(-1)
  } else if (n_res > 1) {
    cat("#### More than one possible result returned - automatically selecting best match ####\n")
    
    poss_concepts <- pull(sql_res, concept_name)
    str_sims <- as.vector(ldist(paste(search_str, form), poss_concepts))
    row_choose <- which.max(str_sims)
    cat(
      "Of the possible matches: '", 
      paste(poss_concepts, collapse = "'|'"), 
      "'\n'", poss_concepts[row_choose],"' was chosen by string similarity\n", 
      sep = ""
    )
    sql_res <- sql_res[row_choose, ]
    
  }
  
  return(pull(sql_res, concept_id))
  
}
# USAGE:
# find_branded_concept(cdm_con, "humira")


branded_drug_to_clinical <- function(con, search_str, form = "", max_char = 8, first_word = TRUE) {
  
  cid <- find_branded_concept(con = con, search_str, form, max_char = max_char, first_word = first_word)
  
  if (cid < 0) {
    return(search_str)
  }
  
  sql_res <-
    query_pg(
      con = con,
      "select cr.concept_id_1, cr.concept_id_2, ",
      "  c1.concept_name as cn1, cr.relationship_id, c2.concept_name as cn2, ",
      "  c1.concept_class_id as ccid1, c2.concept_class_id as ccid2",
      "from concept_relationship as cr",
      "join concept as c1",
      "on cr.concept_id_1 = c1.concept_id",
      "join concept as c2",
      "on cr.concept_id_2 = c2.concept_id",
      "where (cr.concept_id_1 = ", cid, " or cr.concept_id_2 = ", cid, ") ",
      "and cr.invalid_reason is NULL",
      "and c1.invalid_reason is NULL",
      "and c2.invalid_reason is NULL",
      "and cr.relationship_id like '%radename%'",
      "order by cr.relationship_id"
    )
  
  syn_list <- unique(c(
    pull(sql_res %>% dplyr::filter(ccid1 == "Clinical Drug Form"), cn1), 
    pull(sql_res %>% dplyr::filter(ccid2 == "Clinical Drug Form"), cn2)
  ))
  
  if (length(syn_list) < 1) {
    return(search_str)
  }
  
  return(paste(syn_list, collapse = "|"))
  
}
# USAGE:
# branded_drug_to_clinical(cdm_con, "humira")
# branded_drug_to_clinical(cdm_con, "metronidazole")
# branded_drug_to_clinical(cdm_con, "prednisone")
# branded_drug_to_clinical(cdm_con, "methotrexate")
# branded_drug_to_clinical(cdm_con, "mabthera")
# branded_drug_to_clinical(cdm_con, "tamoxifen")
# branded_drug_to_clinical(cdm_con, "sofosbuvir")


