


/* ++++++++++++++++++++++++++++++++++++++++++++ */
/* NB:                                          */
/* the below assumes the user is `postgres' and */
/* has the permissions to create schema,        */
/* tables etc                                   */
/* ++++++++++++++++++++++++++++++++++++++++++++ */


-- create the schema you want to use
CREATE SCHEMA faers_dat
    AUTHORIZATION postgres;
	
SHOW search_path;
SET search_path TO faers_dat;
SHOW search_path;




/* ++++++++++++++++++++++++++++++++++++++++++++ */
/* create table structure ready for data import */
/* ++++++++++++++++++++++++++++++++++++++++++++ */


-- note I may have made some columns bigger than they strictly need to be


DROP TABLE IF EXISTS faers_dat.indi;
CREATE TABLE faers_dat.indi (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  indi_drug_seq    BIGINT     NOT NULL, -- VARCHAR(3) 
  indi_pt     VARCHAR(100)     NOT NULL, -- VARCHAR(93)
  qtr     VARCHAR(6)     NOT NULL
);



DROP TABLE IF EXISTS faers_dat.outc;
CREATE TABLE faers_dat.outc (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  outc_cod     VARCHAR(4)     NOT NULL, -- VARCHAR(2)
  qtr     VARCHAR(6)     NOT NULL
);



DROP TABLE IF EXISTS faers_dat.reac;
CREATE TABLE faers_dat.reac (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  pt     VARCHAR(100)     NOT NULL, -- VARCHAR(93)
  qtr     VARCHAR(6)     NOT NULL,
  drug_rec_act     VARCHAR(90)     NULL -- VARCHAR(66)
);



DROP TABLE IF EXISTS faers_dat.rpsr;
CREATE TABLE faers_dat.rpsr (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  rpsr_cod     VARCHAR(6)     NOT NULL, -- VARCHAR(3)
  qtr     VARCHAR(6)     NOT NULL
);


DROP TABLE IF EXISTS faers_dat.ther;
CREATE TABLE faers_dat.ther (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  dsg_drug_seq     BIGINT     NOT NULL, -- VARCHAR(3)
  start_dt     VARCHAR(8)     NULL,
  end_dt     VARCHAR(8)     NULL,
  dur     VARCHAR(7)     NULL,
  dur_cod     VARCHAR(10)     NULL, -- VARCHAR(5)
  qtr     VARCHAR(6)     NOT NULL
);



DROP TABLE IF EXISTS faers_dat.demo;
CREATE TABLE faers_dat.demo (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  caseversion     BIGINT     NOT NULL, -- VARCHAR(3)
  i_f_code     VARCHAR(1)     NOT NULL,
  event_dt     VARCHAR(8)     NULL,
  mfr_dt     VARCHAR(8)     NULL,
  init_fda_dt     VARCHAR(8)     NOT NULL,
  fda_dt     VARCHAR(8)     NOT NULL,
  rept_cod     VARCHAR(5)      NULL,
  mfr_num     VARCHAR(90)     NULL,
  mfr_sndr     VARCHAR(65)     NULL,
  age     NUMERIC(12, 2)     NULL, -- VARCHAR(7)
  age_cod     VARCHAR(3)     NULL,
  gndr_cod     VARCHAR(3)     NULL,
  e_sub     VARCHAR(1)     NOT NULL,
  wt     VARCHAR(14)    NULL, -- VARCHAR(7) -> NUMERIC(14, 5) 
  wt_cod     VARCHAR(5)     NULL,
  rept_dt     VARCHAR(8)     NULL,
  to_mfr     VARCHAR(1)     NULL,
  occp_cod     VARCHAR(5)     NULL,
  reporter_country     VARCHAR(21)     NULL,
  occr_country     VARCHAR(2)     NULL,
  qtr     VARCHAR(6)     NOT NULL,
  auth_num     VARCHAR(100)     NULL,
  lit_ref     VARCHAR(500)     NULL,
  age_grp     VARCHAR(1)     NULL,
  sex     VARCHAR(3)     NULL
);


DROP TABLE IF EXISTS faers_dat.drug;
CREATE TABLE faers_dat.drug (
  primaryid     BIGINT     NOT NULL,
  caseid     BIGINT     NOT NULL,
  drug_seq     BIGINT     NOT NULL, -- VARCHAR(3)
  role_cod     VARCHAR(2)     NULL,
  drugname     VARCHAR(500)     NULL,
  val_vbm     BIGINT     NOT NULL, -- VARCHAR(1)
  route     VARCHAR(37)     NULL,
  dose_vbm     VARCHAR(288)     NULL,
  cum_dose_chr     VARCHAR(15)     NULL,
  cum_dose_unit     VARCHAR(8)     NULL,
  dechal     VARCHAR(1)     NULL,
  rechal     VARCHAR(2)     NULL,
  lot_num     VARCHAR(363)     NULL,
  exp_dt     VARCHAR(139)     NULL,
  nda_num     VARCHAR(27)     NULL,
  dose_amt     VARCHAR(15)     NULL,
  dose_unit     VARCHAR(10)     NULL,
  dose_form     VARCHAR(50)     NULL,
  dose_freq     VARCHAR(11)     NULL,
  qtr     VARCHAR(6)     NOT NULL,
  prod_ai     VARCHAR(300)     NULL
);





/* +++++++++++++++++++++++++++++++++++++++++ */
/* Import CSVs */
/* +++++++++++++++++++++++++++++++++++++++++ */


/* Windows file locations but change for Linux or macOS */


COPY faers_dat.outc FROM 'C:/Users/Public/faers/csv/outc.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.outc limit 100;

COPY faers_dat.rpsr FROM 'C:/Users/Public/faers/csv/rpsr.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.rpsr limit 100;

COPY faers_dat.reac FROM 'C:/Users/Public/faers/csv/reac.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.reac limit 100;

COPY faers_dat.indi FROM 'C:/Users/Public/faers/csv/indi.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.indi limit 100;

COPY faers_dat.ther FROM 'C:/Users/Public/faers/csv/ther.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.ther limit 100;

COPY faers_dat.demo FROM 'C:/Users/Public/faers/csv/demo.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.demo limit 100;

/* +++++++++++++++++++++++++++++++++++++++++ */
/* 
NOTE: you may get an error with the below COPY statement on Windows
please see the below comments for work-around 
*/
/* +++++++++++++++++++++++++++++++++++++++++ */

COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.drug limit 100;



/* +++++++++++++++++++++++++++++++++++++++++ */
/* 

+++ work-around for large csv imports +++

NOTE: the above load into DRUG table may not work on windows because of 
a bug with file sizes > 1GB 

the below is a work around using WSL shell/bash on Linux

NB: the split csvs do NOT have header rows 

*/
/* +++++++++++++++++++++++++++++++++++++++++ */

-- first of all you need to split the drug.csv file into smaller chunks
-- you can use the `split` utility within WSL shell/bash on Linux 
-- below is the code to do so

/*

cd /mnt/c/Users/Public/faers/csv
# let's create a sub directory to keep smaller chunks
mkdir drug_split

# this creates drug textfile chunks of ~900mb each named drug01, drug02, ... etc
split -C 900m --numeric-suffixes drug.csv drug_split/drug

# now append ".csv" to each split file in drug_split/
cd drug_split
for j in *; do mv -- "$j" "${j}.csv"; done

# you are ready to go!

*/


/* NB: the split csvs do NOT have header rows except for the first one */
COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug_split/drug00.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.drug limit 100;
select count(*) from faers_dat.drug;-- should be ~10 mil

COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug_split/drug01.csv' WITH DELIMITER ',' CSV QUOTE '"' ;
select count(*) from faers_dat.drug; -- should be ~10 mil + ~10 mil = ~20 mil


COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug_split/drug02.csv' WITH DELIMITER ',' CSV QUOTE '"' ;
COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug_split/drug03.csv' WITH DELIMITER ',' CSV QUOTE '"' ;
COPY faers_dat.drug FROM 'C:/Users/Public/faers/csv/drug_split/drug04.csv' WITH DELIMITER ',' CSV QUOTE '"' ;
select count(*) from faers_dat.drug; -- ~40 mil


/* +++++++++++++++++++++++++++++++++++++++++ */
/* Create indexes etc to make queries faster */
/* +++++++++++++++++++++++++++++++++++++++++ */



CREATE INDEX idx_indi_primaryid  ON indi (primaryid ASC);
CLUSTER indi USING idx_indi_primaryid ;
CREATE INDEX idx_indi_caseid  ON indi (caseid ASC);


CREATE INDEX idx_outc_primaryid  ON outc (primaryid ASC);
CLUSTER outc USING idx_outc_primaryid ;
CREATE INDEX idx_outc_caseid  ON outc (caseid ASC);


CREATE INDEX idx_reac_primaryid ON reac (primaryid ASC);
CLUSTER reac USING idx_reac_primaryid ;
CREATE INDEX idx_reac_caseid  ON reac (caseid ASC);

CREATE INDEX idx_reac_pt  ON reac (pt ASC);

CREATE INDEX idx_rpsr_primaryid  ON rpsr (primaryid ASC);
CLUSTER rpsr USING idx_rpsr_primaryid ;
CREATE INDEX idx_rpsr_caseid  ON rpsr (caseid ASC);


CREATE INDEX idx_ther_primaryid  ON ther (primaryid ASC);
CLUSTER ther USING idx_ther_primaryid ;
CREATE INDEX idx_ther_caseid  ON ther (caseid ASC);


CREATE INDEX idx_demo_primaryid  ON demo (primaryid ASC);
CLUSTER demo USING idx_demo_primaryid ;
CREATE INDEX idx_demo_caseid  ON demo (caseid ASC);


CREATE INDEX idx_drug_primaryid  ON drug (primaryid ASC);
CLUSTER drug USING idx_drug_primaryid ;
CREATE INDEX idx_drug_caseid  ON drug (caseid ASC);

CREATE INDEX idx_drug_drugname  ON drug (drugname ASC);
CREATE INDEX idx_drug_prod_ai  ON drug (prod_ai ASC);



ALTER TABLE demo ADD CONSTRAINT xpk_demo PRIMARY KEY (primaryid,caseid,caseversion,qtr);
ALTER TABLE drug ADD CONSTRAINT xpk_drug PRIMARY KEY (primaryid,caseid,drug_seq,qtr);





/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */
/* delete duplicate records in tables                               */
/* (see postgres/delete-dups.R to see how sql was generated)        */
/* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ */

SET search_path TO faers_dat;

/* to see the counts of rows to be deleted (before deleting) */

/* 
previous output (up to 2020 Q3 extract) looks like this:

"Duplicate records in reac to be deleted:"	1477
"Duplicate records in outc to be deleted:"	456
"Duplicate records in drug to be deleted:"	6181
"Duplicate records in rpsr to be deleted:"	120
"Duplicate records in demo to be deleted:"	487
"Duplicate records in indi to be deleted:"	4432
"Duplicate records in ther to be deleted:"	3769
*/


select 'Duplicate records in ther to be deleted:' as dup_descrip, count(*) as dup_count
  from ther as t0
where exists (
  select 1
  from ther as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in indi to be deleted:' as dup_descrip, count(*) as dup_count
  from indi as t0
where exists (
  select 1
  from indi as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in drug to be deleted:' as dup_descrip, count(*) as dup_count
  from drug as t0
where exists (
  select 1
  from drug as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in demo to be deleted:' as dup_descrip, count(*) as dup_count
  from demo as t0
where exists (
  select 1
  from demo as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in reac to be deleted:' as dup_descrip, count(*) as dup_count
  from reac as t0
where exists (
  select 1
  from reac as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in outc to be deleted:' as dup_descrip, count(*) as dup_count
  from outc as t0
where exists (
  select 1
  from outc as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

union

select 'Duplicate records in rpsr to be deleted:' as dup_descrip, count(*) as dup_count
  from rpsr as t0
where exists (
  select 1
  from rpsr as t1
  where t0.primaryid = t1.primaryid
  and t0.qtr < t1.qtr
)

;


/* !!! actual deletions !!! */

/* 
each query finds records that are superseded by the same primaryid (and unique identifiers) in a new quarter's data 
*/


DELETE FROM ther
where exists (
  select 1
  from ther as t1
  where ther.primaryid = t1.primaryid
  and ther.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM indi
where exists (
  select 1
  from indi as t1
  where indi.primaryid = t1.primaryid
  and indi.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM drug
where exists (
  select 1
  from drug as t1
  where drug.primaryid = t1.primaryid
  and drug.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM demo
where exists (
  select 1
  from demo as t1
  where demo.primaryid = t1.primaryid
  and demo.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM reac
where exists (
  select 1
  from reac as t1
  where reac.primaryid = t1.primaryid
  and reac.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM outc
where exists (
  select 1
  from outc as t1
  where outc.primaryid = t1.primaryid
  and outc.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;


DELETE FROM rpsr
where exists (
  select 1
  from rpsr as t1
  where rpsr.primaryid = t1.primaryid
  and rpsr.qtr < t1.qtr
)
-- RETURNING * /* optional to print deleted rows */
;





