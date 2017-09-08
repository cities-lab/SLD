library(tidyverse)
library(foreign)
library(readr)

zip_url <- "https://edg.epa.gov/data/PUBLIC/OP/SLD/SLD_dbf.zip"
data_dir <- tempdir()
zip_file <- file.path(data_dir, "SLD_dbf.zip")

download.file(zip_url, destfile=zip_file, method="libcurl")
unzip(zip_file, exdir = data_dir)

SLD_file <- file.path(data_dir, "SmartLocationDb.dbf")

stopifnot(SLD_file %>% file.exists)

sld_raw <- read.dbf(SLD_file, as.is=TRUE) %>% as_tibble()

#data dictionary: https://www.epa.gov/sites/production/files/2014-03/documents/sld_userguide.pdf

#code NA from -99999 (NA coding used by SLD)
SLD_df <- sld_raw %>% na_if(-99999)

SLD_df <- SLD_df %>% rename(BGWORKERS=WORKERS)

SLD_df <- SLD_df %>% group_by(SFIPS, CFIPS, TRFIPS) %>%
  mutate(TRAC_UNPR=sum(AC_UNPR),
         TRAC_LAND=sum(AC_LAND),
         TRShape_Area=sum(Shape_Area),
         TRAREA=ifelse(TRAC_UNPR<=5, TRAC_LAND, TRAC_UNPR),
         TRAREA=ifelse(TRAREA==0, TRShape_Area, TRAREA),
         TRPOP=sum(TOTPOP10),
         TRHU=sum(COUNTHU10),
         TREMP=sum(EMPTOT),
         TRACT=sum(EMPTOT+COUNTHU10),
         TRE5_ENT10=sum(E5_ENT10),
         TRE5_IND10=sum(E5_IND10),
         TRE5_OFF10=sum(E5_OFF10),
         TRE5_RET10=sum(E5_RET10),
         TRE5_SVC10=sum(E5_SVC10),
         TRE8_ED10 =sum(E8_ED10),
         TRE8_ENT10=sum(E8_ENT10),
         TRE8_HLTH10=sum(E8_HLTH10),
         TRE8_IND10=sum(E8_IND10),
         TRE8_OFF10=sum(E8_OFF10),
         TRE8_PUB10=sum(E8_PUB10),
         TRE8_RET10=sum(E8_RET10),
         TRE8_SVC10=sum(E8_SVC10)
  ) %>%
  ungroup()

### Urban Area level information ###
## load block-UA/UACE mapping
## http://www2.census.gov/geo/tiger/TIGER2010/TABBLOCK/2010/
bg_file <- "inst/extdata/tl_2013_us_bg.csv"
## concatenate from state block files if not existing
if (!file.exists(bg_file)) {
  for (sid in 1:78){
    file_name <- paste0("inst/extdata/tl_2013_", str_pad(sid, 2, pad="0"), "_tabblock.dbf")
    if (!file.exists(file_name)) next
    blk <- read.dbf(file_name, as.is = TRUE)
    blk <- blk %>% mutate(bg=substr(GEOID, 1, 12))
    bg <- blk %>%
      group_by(GEOID10=bg) %>%
      summarize(UR10=first(UR10),
                UACE10=first(UACE10)) %>%
      ungroup() %>%
      transmute(GEOID10,
                Urban=ifelse(UR10=="U", 1L, 0L),
                UACE=UACE10)
    write_csv(bg, bg_file, append=TRUE)
  }
}

bg2uace <- read_csv(bg_file, col_names=c("GEOID10", "Urban", "UACE"))

# eliminate duplicate GEOID10 in bg (28 rows)
# TODO: how did they get there?
bg2uace <- bg2uace %>% group_by(GEOID10) %>% filter(row_number() == 1)

SLD_df <- SLD_df %>% left_join(bg2uace, by="GEOID10")

### compute UA level variables
ua_df <- SLD_df %>%
  group_by(UACE) %>%
  summarize(UZAAC_UNPR=sum(AC_UNPR),
            UZAAC_LAND=sum(AC_LAND),
            UZAShape_Area=sum(Shape_Area),
            UZAAREA=ifelse(UZAAC_UNPR<=5, UZAAC_LAND, UZAAC_UNPR),
            UZAAREA=ifelse(UZAAREA==0, UZAShape_Area, UZAAREA),
            UZAPOP=sum(TOTPOP10),
            UZAPOPDEN=UZAPOP/UZAAREA,
            UZAHUDEN=sum(COUNTHU10)/UZAAREA,
            UZAEMPDEN=sum(EMPTOT)/UZAAREA,
            UZAACTDEN=sum(EMPTOT+COUNTHU10)/UZAAREA) %>%
  ungroup()

#load UA names
#UA <- read_excel("data/Census/ua_list_ua.xls")
#UA <- read_csv("data/Census/ua_list_ua.csv")
## https://www.census.gov/geo/reference/ua/ualists_layout.html
ua_name <- read_fwf("inst/extdata/ua_list_all.txt",
                      col_positions = fwf_positions(start=c(1, 11, 76, 90, 104, 123, 137, 156, 170, 184),
                                                    end=  c(5, 70, 84, 98, 117, 131, 150, 164, 178, 185),
                                                    col_names=c("UACE", "NAME", "POP", "HU", "AREALAND", "AREALANDSQMI",
                                                                "AREAWATER", "AREAWATERSQMI", "POPDEN", "LSADC")),
                      skip=1)

ua_name <- ua_name %>%
  rename(UA_NAME=NAME) %>%
  dplyr::select(UACE, UA_NAME, LSADC)

ua_df <- ua_df %>% left_join(ua_name, by="UACE")

SLD_df <- SLD_df %>% left_join(ua_df, by="UACE")

devtools::use_data(SLD_df, overwrite=TRUE)
