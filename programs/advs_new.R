library(admiral)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(stringr)
library(tibble)
library(metacore)
library(xportr)

vs <- haven::read_xpt('~/admiraladam/sdtm/vs.xpt')
adsl <- haven::read_xpt('~/admiraladam/adam/adsl.xpt')

vs <- convert_blanks_to_na(vs)
adsl <- convert_blanks_to_na(adsl)

adsl_vars <- exprs(TRTSDT, TRTEDT, TRT01A, TRT01P)


# advs <- vs %>% left_join(adsl %>% select(STUDYID, USUBJID,TRTSDT, TRTEDT, TRT01A, TRT01P ), join_by(STUDYID, USUBJID))

advs <- derive_vars_merged(
  vs,
  dataset_add = adsl,
  new_vars = exprs(TRTSDT, TRTEDT, TRT01A, TRT01P),
  by_vars = exprs(STUDYID, USUBJID)
)

advs <- derive_vars_dt(advs, new_vars_prefix = "A", dtc = VSDTC)


# any(str_length(advs$VSDTC)<10)

advs <- derive_vars_dy(advs, reference_date = TRTSDT, source_vars = exprs(ADT))

# names(advs)

# any(is.na(advs$ADY))

param_lookup <- tribble(
  ~VSTESTCD,                 ~VSTEST, ~PARAMCD,                       ~PARAM, ~PARAMN,
  "SYSBP", "Systolic Blood Pressure",  "SYSBP", "Syst Blood Pressure (mmHg)", 1,
  "DIABP", "Diastolic Blood Pressure",  "DIABP", "Dia Blood Pressure (mmHg)", 2,
  "PULSE", "Pulase Rate",               "PULSE", "Pulse Rate (bpm)",          3,
  "WEIGHT",                 "Weight", "WEIGHT",                "Weight (kg)", 4,
  "HEIGHT",                 "Height", "HEIGHT",                "Height (cm)", 5,
  "TEMP",              "Temperature",   "TEMP",            "Temperature (C)", 6,
  "MAP",    "Mean Arterial Pressure",    "MAP",   "Mean Art Pressure (mmHg)", 7,
  "BMI",           "Body Mass Index",    "BMI",    "Body Mass Index(kg/m^2)", 8,
  "BSA",         "Body Surface Area",    "BSA",     "Body Surface Area(m^2)",  9
)

# param_lookup2 <- tibble(VSTESTCD=c("SYSBP",  "WEIGHT", "HEIGHT", "TEMP" ,  "MAP"  ,  "BMI" ,   "BSA" ),
#                         VSTEST=c("Systolic Blood Pressure", "Weight" ,                 "Height" ,                 "Temperature",
#                                   "Mean Arterial Pressure" , "Body Mass Index"    ,     "Body Surface Area"  )
#                         )


advs <- derive_vars_merged_lookup(
  advs,
  dataset_add = param_lookup,
  new_vars = exprs(PARAMCD),
  by_vars = exprs(VSTESTCD)
)


advs <- advs %>% mutate(AVAL=VSSTRESN)

# debugonce(derive_param_map)
advs <- derive_param_map(
  advs,
  by_vars = exprs(STUDYID, USUBJID, !!!adsl_vars, VISIT, VISITNUM, ADT, ADY, VSTPT, VSTPTNUM),
  set_values_to = exprs(PARAMCD = "MAP"),
  get_unit_expr = VSSTRESU,
  filter = VSSTAT != "NOT DONE" | is.na(VSSTAT)
)

# names(advs)

# unique(advs$PARAMCD)

# Derive PARAM and PARAMN
advs <- derive_vars_merged(
  advs,
  dataset_add = select(param_lookup, -VSTESTCD, -VSTEST),
  by_vars = exprs(PARAMCD)
)

# names(advs)

# advs %>% select(VISIT, VISITNUM) %>% unique()

advs <- advs %>%
  mutate(
    AVISIT = case_when(
      str_detect(VISIT, "SCREEN") ~ NA,
      str_detect(VISIT, "UNSCHED") ~ NA,
      str_detect(VISIT, "RETRIEVAL") ~ NA,
      str_detect(VISIT, "AMBUL") ~ NA,
      !is.na(VISIT) ~ str_to_title(VISIT)
    ),
    AVISITN = as.numeric(case_when(
      VISIT == "BASELINE" ~ "0",
      str_detect(VISIT, "WEEK") ~ str_trim(str_replace(VISIT, "WEEK", ""))
    )),
    ATPT = VSTPT,
    ATPTN = VSTPTNUM
  )

# count(advs, VISITNUM, VISIT, AVISITN, AVISIT) similar to proc freq

# count(advs, VSTPTNUM, VSTPT, ATPTN, ATPT)


advs <- derive_basetype_records(
  dataset = advs,
  basetypes = exprs(
    "LAST: AFTER LYING DOWN FOR 5 MINUTES" = ATPTN == 815,
    "LAST: AFTER STANDING FOR 1 MINUTE" = ATPTN == 816,
    "LAST: AFTER STANDING FOR 3 MINUTES" = ATPTN == 817,
    "LAST" = is.na(ATPTN)
  )
)

# count(advs, ATPT, ATPTN, BASETYPE)

advs <- restrict_derivation(
  advs,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = exprs(STUDYID, USUBJID, BASETYPE, PARAMCD),
    order = exprs(ADT, ATPTN, VISITNUM),
    new_var = ABLFL,
    mode = "last"
  ),
  filter = (!is.na(AVAL) & ADT <= TRTSDT & !is.na(BASETYPE))
)

advs <- derive_var_base(
  advs,
  by_vars = exprs(STUDYID, USUBJID, PARAMCD, BASETYPE),
  source_var = AVAL,
  new_var = BASE
)

advs <- derive_var_chg(advs)

advs <- derive_var_pchg(advs)

# names(advs)
#
# advs %>% select(PARAMCD, PARAM, PARAMN, VSTEST) %>% unique()
#
# advs$CHG

advs <- restrict_derivation(
  advs,
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = exprs(STUDYID, USUBJID, BASETYPE, PARAMCD, AVISIT),
    order = exprs(ADT, ATPTN, AVAL),
    new_var = ANL01FL,
    mode = "last"
  ),
  filter = !is.na(AVISITN)
) %>% arrange(STUDYID, USUBJID, PARAMCD, ADT)



advs <- advs %>%
  derive_vars_merged(
    dataset_add = select(adsl, !!!negate_vars(adsl_vars)),
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  select(-c('DOMAIN', 'VSTESTCD', 'VSTEST', 'VSPOS', 'VSORRES', 'VSORRESU', 'VSSTRESC', 'VSSTRESN', 'VSSTRESU', 'VSSTAT', 'VSLOC', 'VSBLFL', 'VISITDY', 'VSDTC', 'VSDY', 'VSTPT', 'VSTPTNUM', 'VSELTM', 'VSTPTREF', 'TRT01A', 'TRT01P', 'SUBJID', 'SITEGR1', 'ARM', 'TRT01PN', 'TRT01AN', 'TRTDURD', 'AVGDD', 'CUMDOSE', 'AGEU', 'ETHNIC', 'ITTFL', 'EFFFL', 'COMP8FL', 'COMP16FL', 'COMP24FL', 'DISCONFL', 'DSRAEFL', 'DTHFL', 'BMIBL', 'BMIBLGR1', 'HEIGHTBL', 'WEIGHTBL', 'EDUCLVL', 'DISONSDT', 'DURDIS', 'DURDSGR1', 'VISIT1DT', 'RFSTDTC', 'RFENDTC', 'VISNUMEN', 'RFENDT', 'DCDECOD', 'EOSSTT', 'DCSREAS', 'MMSETOT'))


metacore <- metacore::spec_to_metacore('metadata/specs.xlsx', where_sep_sheet = F, quiet = T)

advs_spec <- metacore %>% select_dataset('ADVS')


dataset_spec <- readxl::read_xlsx('metadata/specs.xlsx', sheet = "Datasets") %>%
  dplyr::rename_with(tolower)

# codelist <- as.data.frame(advs_spec$codelist) %>% select(codes) %>% unlist()

# bind_cols(codelist[1,],advs_spec$codelist$codes[[1]]) %>% select(-codes)

advs %>%
  xportr_metadata(advs_spec, "ADVS") %>%
  xportr_type(verbose = "warn") %>%
  xportr_length(verbose = "warn") %>%
  xportr_label(verbose = "warn") %>%
  xportr_order(verbose = "warn") %>%
  xportr_format() %>%
  xportr_df_label(dataset_spec, "ADVS") %>%
  xportr_write("~/admiraladam/adam/advs_new.xpt")





