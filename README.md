


# The `fuzzyfaers` package

Functions to extract drug records from the [FAERS (FDA Adverse Event Reporting System)](https://www.fda.gov/drugs/surveillance/questions-and-answers-fdas-adverse-event-reporting-system-faers) data in a `postgreSQL` database for analysis in `R`. 

As drug names (whether they be generic/clinical or branded) are provided to the FDA and do not undergo standardisation, this package provides an automated way to extract records of a drug, its synonyms and their inevitable misspellings (plus superfluous text). Fuzzy string matching is required to do this, thus, `fuzzyfaers`.

## Background and motivation

Signal detection of adverse drug events often requires analysis of tabulated count data as seen in the below table.

|         | `Event(s) X`| `Event(s) Y` |
|:--------|---------:|----------:|
|**`Drug(s) A`**|       *a*|        *b*|
|**`Drug(s) B`**|       *c*|        *d*|


To get to this data, or a flat dataset with a single record for each adverse event is a time consuming process. Not only that but finding synonyms to drug names is a painfully manual process, compounded by alternative spellings and superfluous text included in the drug text fields.

This package aims to

1. provide data is as ready to go format as possible with instructions to house it in a manageable database environment accessable by `R`, and
2. automate the drug synonym (and mispelling) searching in the database to extract single record adverse event `data.frame`s in `R`.

## Prerequisites

All required software are free and available on Linux, Mac OS and Windows. Testing has been undertaken on Ubuntu LTS 18.04 and Windows 10 systems. You will likely need ~10Gb of disk space for the database/data installation and enough memory to deal with `data.frames` in `R` potentially with millions of records (I have found 32Gb of memory sufficient for me).

* Installation of [`PostgreSQL server` and `pgAdmin`](https://www.postgresql.org/download/) which are normally both included in the installation.
* The currated tables (`demo`, `drug`, `indi`, `outc`, `reac`, `rpsr`, `ther`) from FAERS as csvs available [with the link found here](https://github.com/tystan/fuzzyfaers/blob/master/postgres/csvs-url.txt). These csvs are ready-to-go data compiled from the raw and seperated quarterly extracts available [here](https://www.fda.gov/drugs/questions-and-answers-fdas-adverse-event-reporting-system-faers/fda-adverse-event-reporting-system-faers-latest-quarterly-data-files) from the quarters 2013Q1 to 2020Q3 inclusive. I will endevour to update these files to include new quarterly data. For reproducibility, I also include a [HTML rendered rmarkdown document detailing the pre-processing of the FAERS ascii text quarterly extracts to single files for each FAERS tables](https://github.com/tystan/fuzzyfaers/blob/master/postgres/faers-extracts-to-csv.html).
* [OHDSI (Observational Health Data Sciences and Informatics)](https://www.ohdsi.org/) common data model (CDM) vocabulary files. These files allow us to automatically check for drug synonyms through their excellent vocabulary mappings. The vocabulary files can be downloaded at [Athena](https://athena.ohdsi.org/) (sign up required, free).
* An installation of [`R`](https://cran.r-project.org/).

## Example usage

```R
library(devtools) # see https://www.r-project.org/nosvn/pandoc/devtools.html
devtools::install_github('tystan/fuzzyfaers')
library(fuzzyfaers)
### see help file to run example


```


## `fuzzyfaers` process to extact drug specific FAERS data


![](https://github.com/tystan/fuzzyfaers/blob/master/fig/fuzzyfaers-scheme.png)



