


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

  
DROP TABLE IF EXISTS demo;
DROP TABLE IF EXISTS drug;
DROP TABLE IF EXISTS indi;
DROP TABLE IF EXISTS outc, reac, rpsr, ther;



/* ++++++++++++++++++++++++++++++++++++++++++++ */
/* create table structure ready for data import */
/* ++++++++++++++++++++++++++++++++++++++++++++ */


-- note I may have made some columns bigger than they strictly need to be


CREATE TABLE faers_dat.indi (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
indi_drug_seq    BIGINT     NOT NULL, -- VARCHAR(3) 
indi_pt     VARCHAR(100)     NOT NULL, -- VARCHAR(93)
qtr     VARCHAR(6)     NOT NULL
);

CREATE TABLE faers_dat.outc (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
outc_cod     VARCHAR(4)     NOT NULL, -- VARCHAR(2)
qtr     VARCHAR(6)     NOT NULL
);

CREATE TABLE faers_dat.reac (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
pt     VARCHAR(100)     NOT NULL, -- VARCHAR(93)
qtr     VARCHAR(6)     NOT NULL,
drug_rec_act     VARCHAR(90)     NULL -- VARCHAR(66)
);

CREATE TABLE faers_dat.rpsr (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
rpsr_cod     VARCHAR(6)     NOT NULL, -- VARCHAR(3)
qtr     VARCHAR(6)     NOT NULL
);


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

CREATE TABLE faers_dat.demo (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
caseversion     BIGINT     NOT NULL, -- VARCHAR(3)
i_f_code     VARCHAR(1)     NOT NULL,
event_dt     VARCHAR(8)     NULL,
mfr_dt     VARCHAR(8)     NULL,
init_fda_dt     VARCHAR(8)     NOT NULL,
fda_dt     VARCHAR(8)     NOT NULL,
rept_cod     VARCHAR(3)     NOT NULL,
mfr_num     VARCHAR(73)     NULL,
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

CREATE TABLE faers_dat.drug (
primaryid     BIGINT     NOT NULL,
caseid     BIGINT     NOT NULL,
drug_seq     BIGINT     NOT NULL, -- VARCHAR(3)
role_cod     VARCHAR(2)     NULL,
drugname     VARCHAR(500)     NULL,
val_vbm     BIGINT     NOT NULL, -- VARCHAR(1)
route     VARCHAR(37)     NULL,
dose_vbm     VARCHAR(288)     NULL,
cum_dose_chr     VARCHAR(10)     NULL,
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


/* Linux (or Mac OS) */

COPY faers_dat.outc FROM '/directory/location/outc.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.outc limit 100; -- just a visual check to see everything looks OK

COPY faers_dat.rpsr FROM '/directory/location/rpsr.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.rpsr limit 100;

COPY faers_dat.reac FROM '/directory/location/reac.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.reac limit 100;

COPY faers_dat.indi FROM '/directory/location/indi.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.indi limit 100;

COPY faers_dat.ther FROM '/directory/location/ther.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.ther limit 100;

COPY faers_dat.demo FROM '/directory/location/demo.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.demo limit 100;

COPY faers_dat.drug FROM '/directory/location/drug.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.drug limit 100;



/* OR Windows  */


COPY faers_dat.outc FROM 'C:/directory/location/outc.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.outc limit 100;

COPY faers_dat.rpsr FROM 'C:/directory/location/rpsr.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.rpsr limit 100;

COPY faers_dat.reac FROM 'C:/directory/location/reac.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.reac limit 100;

COPY faers_dat.indi FROM 'C:/directory/location/indi.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.indi limit 100;

COPY faers_dat.ther FROM 'C:/directory/location/ther.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.ther limit 100;

COPY faers_dat.demo FROM 'C:/directory/location/demo.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.demo limit 100;

COPY faers_dat.drug FROM 'C:/directory/location/drug.csv' WITH DELIMITER ',' CSV HEADER QUOTE '"' ;
select * from faers_dat.drug limit 100;




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


















