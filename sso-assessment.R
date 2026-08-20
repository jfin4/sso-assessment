# SSO Assessment
# John.Inman@waterboards.ca.gov
# 2026-06-01
# ==============================================================================

# LOAD ========================================================================={{{

.start <- lubridate::now()
cat("writing")

# load packages ----------------------------------------------------------------{{{

suppressPackageStartupMessages({
    library(data.table)
    library(digest)
    library(fs)
    library(glue)
    library(readxl)
    library(rlang)
    library(tools)
    library(tidyverse)
    library(writexl)
})# }}}

# # read files -------------------------------------------------------------------{{{

# trash <- setdiff(ls(all = TRUE), ".start")
# rm(list = trash)

# my_read_table <- function(path, sheet = NULL) {
#     ext <- file_ext(path)

#     if (ext == "xlsx") {
#         read_excel(path, 
#                    sheet = sheet, 
#                    col_types = "text", 
#                    na = "", 
#                    progress = FALSE)
#     } else {
#         fread(path, 
#               colClasses = "character", 
#               na.strings = "", 
#               showProgress = FALSE)
#     }
# }

# .schema <- my_read_table("./sso-assessment-schema.csv") |>
#     as_tibble()

# dir <- "./raw-data"
# .raw_data <- dir_ls(dir, type = "file") |>
#     map(my_read_table) |>
#     bind_rows()

# dir <- "./lookup-tables"
# .lookups <- dir_ls(dir, type = "file") |>
#     map(\(x) {
#         sheets <- excel_sheets(x)
#         map(sheets, \(y) my_read_table(x, y)) |>
#         set_names(sheets)
#     }) |> 
#     list_c()

# .ssos <- my_read_table("./sso-assessment-lookup.csv")

# .data_loaded <- NULL

# inputs <- grepv("^\\.(?!start)", ls(all = TRUE), perl = TRUE)
# save(list = inputs, file = "./.RData")# }}}

# load data --------------------------------------------------------------------{{{

rm(list = ls())

if (!exists(".data_loaded")) {
    load("./.RData")
}# }}}

# global variables -------------------------------------------------------------

author <- "jinman"
region <- "3"
ir_year <- "2028"
suffix <- ""# }}}}}}

# FORMAT ======================================================================={{{
cat(".") 

# data object is non-lossy
# annotated data : data -> loe :: comp report : loe -> decision

# create data object -----------------------------------------------------------{{{

data <- select(.raw_data, all_of(na.omit(.schema$data)))

# TODO: whence come the dupes?
data <- distinct(data)# }}}

# join lookup tables -----------------------------------------------------------{{{

ssos <- .ssos |>
    rename(ObjectiveUnit = UnitName) |> 
    select(all_of(na.omit(.schema$ssos)))

qapps <- .lookups$Data_Used |>
    filter(Region %in% region) |>
    rename(AssessParentProject = Assess) |>
    select(all_of(na.omit(.schema$qapps)))

stations <- .lookups$Sites |>
    filter(Region %in% region,
           STATUS %in% "Completed") |>
    select(all_of(na.omit(.schema$stations)))

sample_types <- .lookups$Acceptable_SampleTypeNames |>
    filter(Region %in% region,
           Valid %in% "Y") |>
    rename(AcceptableSampleType = Acceptable) |>
    select(all_of(na.omit(.schema$sample_types)))

pollutants_xwalk <- .lookups$ReLEP_to_CalWQA_Lookup |>
    filter(Valid %in% "Y") |>
    rename(AnalyteName = ReLEP_AnalyteName) |> 
    select(all_of(na.omit(.schema$pollutants_xwalk)))

data <- data |> 
    left_join(ssos, by = c("AnalyteName", "StationCode")) |>
    left_join(qapps, by = "ParentProjectName") |>
    left_join(stations, by = "StationCode") |>
    left_join(sample_types, by = "SampleTypeName") |>
    left_join(pollutants_xwalk, by = "AnalyteName")# }}}

# add remaining columns --------------------------------------------------------{{{

remaining <- 
    setdiff(.schema$all, names(data)) |>
    (\(x) set_names(rep(NA_character_, length(x)), x))() |>
    as_tibble_row()

data <- cross_join(data, remaining)# }}}

# convert column types ---------------------------------------------------------{{{

# numeric
data <- mutate(data, across(.schema$all[.schema$type %in% "numeric"], as.numeric))

# SampleDate
data <- data |>
    # helper col to id date format
    mutate(date_format = case_when(
        str_detect(SampleDate, "^\\d+$")                       ~ "excel",
        str_detect(SampleDate, "^\\d{1,2}/\\d{1,2}/\\d{2,4}$") ~ "mdy",
        str_detect(SampleDate, "^\\d{4}-\\d{1,2}-\\d{1,2}$")   ~ "ymd",
        .default = NA_character_
    )) |>
    # transform to date type
    mutate(SampleDate = case_when(
        date_format %in% "excel" ~ SampleDate |>
                                 as.numeric() |>
                                 as.Date(origin = "1899-12-30"),
        date_format %in% "mdy" ~ mdy(SampleDate, quiet = TRUE),
        date_format %in% "ymd" ~ ymd(SampleDate, quiet = TRUE),
        .default = NA_Date_
    )) |>
    # remove helper row
    select(all_of(.schema$all))

# DATE_CREATED, DATE_UPDATED
data <- mutate(data, across(c(DATE_CREATED, DATE_UPDATED), as.Date)) # }}}}}}

# SCREEN ======================================================================={{{
cat(".") 

# define screening functions ---------------------------------------------------{{{

add_flag <- function(data, f) {
    mutate(data,
           new_flag = !!f_rhs(f),
           all_flags = str_c(Issue, new_flag, sep = "; "),
           Issue = if_else(!!f_lhs(f), coalesce(all_flags, new_flag), Issue)) |>
    select(-new_flag, -all_flags)
}

flag_data <- function(data, ...) {
    reduce(list(...), add_flag, .init = data)
}

`%nin%` <- Negate(`%in%`)# }}}

# TODO: flag unexpected fractions?

# duplicate WQID ---------------------------------------------------------------{{{

data <- data |> 
    mutate(is_dup = duplicated(WQID) | duplicated(WQID, fromLast = TRUE)) |>
    flag_data(is_dup ~ "duplicate WQID") |>
    select(all_of(.schema$all))# }}}

# resqualcode checks -----------------------------------------------------------{{{

# codes reference
# https://ceden.waterboards.ca.gov/CEDEN_Checker/DisplayLookUp.aspx?List=ResQualLookUp

# =, >, >= errors
quant_codes <- c("=", ">", ">=")

data <- data |>
    mutate(code_is_quant = ResQualCode %in% quant_codes,
           result_is_lte_zero = Result <= 0,
           result_is_lt_rl = Result < RL,
           result_is_not_quant = result_is_lte_zero | result_is_lt_rl) |>
    flag_data(code_is_quant & result_is_not_quant ~ "Result < RL") |>
    select(all_of(.schema$all))

# < errors
data <- data |>
    # TODO: maybe just test for less than RL; test for <= 0 universally?
    mutate(code_is_lt = ResQualCode == "<",
           result_is_lt_zero = Result < 0,
           # TODO: changing to > to match relep; possibly incorrect
           # result_is_gte_rl = Result >= RL,
           result_is_gt_rl = Result > RL,
           result_is_not_lt = result_is_lt_zero | result_is_gt_rl) |>
    flag_data(code_is_lt & result_is_not_lt ~ "Result > RL or Result < 0") |>
    select(all_of(.schema$all))# }}}

# rl checks --------------------------------------------------------------------{{{

# flag missing RL for nd'ish rows
ndish_codes <- c("ND", "DNQ", "<")
data <- data |>
    mutate(code_is_ndish = ResQualCode %in% ndish_codes,
           rl_is_missing = RL %in% c(NA, -88)) |>
    flag_data(code_is_ndish & rl_is_missing ~ "NDish missing RL") |>
    select(all_of(.schema$all))

# flag RL less than MDL
data <- data |>
           # rules out (multiples of after unit conversion) -88
    mutate(rl_is_not_negative = RL >= 0, 
           rl_is_not_missing = !is.na(RL),
           rl_is_lt_mdl = RL < MDL) |>
    flag_data(rl_is_not_negative & rl_is_not_missing & rl_is_lt_mdl ~ 
                "RL less than MDL") |>
    select(all_of(.schema$all))# }}}

# missing result ---------------------------------------------------------------{{{

ndish_codes <- c("ND", "DNQ", "<")
data <- data |> 
    mutate(code_is_ndish = ResQualCode %in% ndish_codes,
           result_is_missing = is.na(Result)) |>
    flag_data(result_is_missing & !code_is_ndish ~ "missing Result") |>
    select(all_of(.schema$all))# }}}

# remaining checks -------------------------------------------------------------{{{

expected_codes <- c("ND", "DNQ", "=", ">", ">=", "<")
expected_matricies <- c("samplewater")
expected_units <- c(
    "%",    "Deg C", "deg C", "FNU",  "g/L", "mg/L", "mS/cm",    "ng/L",
    "none", "NTRU",  "NTU",   "pg/L", "ppt", "ug/L", "umhos/cm", "uS/cm"
)
data <- flag_data(data, 
    AcceptableSampleType != "Yes" ~ "unacceptable SampleTypeName",
    AssessParentProject != "Yes" ~ "ParentProjectName marked do not assess",
    DataQuality != "Passed QC" ~ "failed DataQaulity check",
    MatrixName %nin% expected_matricies ~ "unexpected MatrixName",
    ResQualCode %nin% expected_codes ~ "unexpected ResQualCode",
    UnitName %nin% expected_units ~ "unexpected UnitName",
    is.na(BeneficialUse) ~ "missing BeneficialUse",
    is.na(Objective)  ~ "missing Objective",
    is.na(PollutantName_in_CalWQA) ~ "missing PollutantName_in_CalWQA",
    is.na(QA_INFO_REFERENCES) ~ "missing QA_INFO_REFERENCES",
    is.na(ResQualCode) ~ "missing ResQualCode",
    is.na(SampleDate) ~ "unknown date format",
    is.na(WQID) ~ "missing WQID"
)# }}}

# id clean samples -------------------------------------------------------------{{{

data <- mutate(data, is_clean = is.na(Issue))# }}}}}}

# QUANTIFY ====================================================================={{{
cat(".") 

# convert units to ug/L --------------------------------------------------------{{{

data <- data |>
    mutate(across(c(Result, MDL, RL), \(x) case_when(
            UnitName %in% "mg/L" ~ x * 1e3,
            UnitName %in% "g/L" ~ x * 1e6,
            .default = x
        ))) |>
    mutate(Objective = case_when(
            ObjectiveUnit %in% "mg/L" ~ Objective * 1e3,
            ObjectiveUnit %in% "g/L" ~ Objective * 1e6,
            .default = Objective
        )) |>
    mutate(across(c(UnitName, ObjectiveUnit), \(x) {
        if_else(x %in% c("mg/L", "g/L"),
                "ug/L",
                x)
        }))# }}}

# quantify nondetect-ish results -----------------------------------------------{{{

# NOTE: needs additional scrutiny if assessing summing pollutants. converting
# nd's to 0.5MDL could lead to inflated results due to summing
data <- data |>
    mutate(usable_mdl = if_else(MDL < 0, 0, MDL),
           ndish_result = coalesce(usable_mdl * 0.5, 0),
           is_ndish = ResQualCode %in% c("ND", "DNQ", "<"),
           Result = if_else(is_ndish, ndish_result, Result)) |>
    select(all_of(.schema$all))# }}}

# id quantifiable samples ------------------------------------------------------{{{

data <- data |> 
    mutate(is_quant_code = ResQualCode %in% c("=", ">", ">="),
           rl_lte_obj = RL <= Objective,
           is_quant = is_quant_code | rl_lte_obj) |>
    select(all_of(.schema$all))# }}}}}}

# AVERAGE ======================================================================{{{
cat(".") 

# standardize non/ionic fractions ----------------------------------------------{{{

nonionic <- c(
    "Alkalinity as CaCO3",
    "ElectricalConductivity",
    "Hardness as CaCO3",
    "Oxygen, Dissolved",
    "pH",
    "Scum/Foam-Unatural",
    "SpecificConductivity",
    "Temperature"
)

ionic <- c(
    "Ammonia as N",
    "Ammonia as N, Unionized",
    "Ammonia as NH3",
    "Ammonia as NH3, Unionized",
    "Chloride",
    "Sodium",
    "Sulfate",
    "Total Dissolved Solids" # nonionic, but here for consistency
)

data <- data |>
    mutate(FractionName = case_when(
            AnalyteName %in% nonionic ~ "None",
            AnalyteName %in% ionic ~ "Dissolved",
            .default = FractionName
    ))# }}}

# make loe id ------------------------------------------------------------------{{{

# grouping factors that uniqely identify loes
# NOTE: should include ir year
loe_cols <- c(
    "ParentProjectName",
    "StationCode",
    "MatrixName",
    "AnalyteName",
    "FractionName",
    "BeneficialUse",
    "Eval_Ref_Number", # some assessments apply ancillary threshold
    "Alias" # currently used for multi-ended thresholds
)

# loe id 
# hash is computed deterministically from loe cols
data <- data |> 
    mutate(loe_id = pmap_chr(pick(all_of(loe_cols)), function(...) {
        digest(list(...), algo = "xxhash32")
    }))# }}}

# make sample id ---------------------------------------------------------------{{{

# # 365-day, event-triggered, forward- moving sample id
# data <- data |>
#     # event-triggered, forward moving bins of size "AveragingPeriod"
#     arrange(SampleDate) |> # important!
#     mutate(days_since = c(0, diff(SampleDate)), # days since last sample date
#            counter_w_reset = 
#                 accumulate(days_since, \(x, y) {
#                     counter <- x + y
#                     # NOTE: important! x, y are diffs; add 1 to get n days 
#                     days_total <- counter + 1
#                     # AveragingPeriod has length(loe_id); [1] makes scalar
#                     ave_period <- AveragingPeriod[1]
#                     if (days_total > ave_period) 0 else days_total
#                 }),
#            is_reset = counter_w_reset == 0,
#            is_new_day = days_since > 0, # don't reset at replicates
#            is_new_sample = is_reset & is_new_day,
#            sample_id = cumsum(is_new_sample) + 1, # start ids at 1
#            .by = loe_id) |>
#     select(all_of(.schema$all))

# calendar year sample id
data <- mutate(data, sample_id = as.character(year(SampleDate)))# }}}

# average results --------------------------------------------------------------{{{

data <- data |>
    # average replicates to avoid overweighting replicate results
    mutate(subsample_result = mean(Result[is_clean & is_quant]), 
           subsample_is_usable = any(is_clean & is_quant),
           subsample_row_1 = !duplicated(SampleDate),
           .by = c(loe_id, sample_id, SampleDate)) |>
    # average subsamples within averaging period
    mutate(Result = mean(subsample_result[subsample_is_usable & subsample_row_1]),
           .by = c(loe_id, sample_id)) |>
    select(all_of(.schema$all))# }}}}}}

# QUERY ========================================================================{{{
cat(".") 

# samples and exceedances ------------------------------------------------------{{{

data <- data  |>
    mutate(sample_is_usable = any(is_clean & is_quant),
           sample_row_1 = !duplicated(sample_id),
           sample_is_exceedance = any(Result > Objective),
           .by = c(loe_id, sample_id)) |>
    mutate(SAMPLE_COUNT = sum(sample_is_usable & sample_row_1),
           EXCEEDANCE_COUNT = sum(sample_is_usable & sample_row_1 & sample_is_exceedance),
           .by = loe_id) |>
    select(all_of(.schema$all))

# NOTE: zero of zero loes sample and exceedances are already 0 by virtue of
# how r handles numeric vectors of length zero. e.g., sample_id[is_usable]
# evaluates to numeric(0) if is_usable is all false. n_distinct(numeric(0)) in
# turn evaluates to 0. }}}

# data used --------------------------------------------------------------------{{{

no_nonquant_lang <- 
    "Water Board staff assessed {ParentProjectName} data for {Waterbody}
    to determine beneficial use support and results are as follows:
    {EXCEEDANCE_COUNT} of {SAMPLE_COUNT} samples exceeded the water quality
    threshold for {AnalyteName}."

some_nonquant_lang <- 
    "Water Board staff assessed {ParentProjectName} data for {Waterbody}
    to determine beneficial use support and the results are as follows:
    {EXCEEDANCE_COUNT} of the {SAMPLE_COUNT} samples exceeded the water
    quality threshold for {AnalyteName}.  Although a total of {n_total}
    samples were collected, {n_nonquant} of these samples were not included in
    the assessment because the laboratory data reporting limit(s) was above
    the water quality threshold and therefore the results could not be
    quantified with the level of certainty required by the Listing Policy
    Section
    6.1.5.5."

all_nonquant_lang <- 
    "Water Board staff assessed {ParentProjectName} data for {Waterbody}
    to determine beneficial use support, and the results are as follows:
    {EXCEEDANCE_COUNT} of the {SAMPLE_COUNT} samples exceeded the evaluation
    guideline for {AnalyteName}. Although a total of {n_total}, samples
    were collected, {n_nonquant} of these samples were not included in the
    assessment because the laboratory data reporting limit(s) was above the
    water quality threshold and therefore the results could not be quantified
    with the level of certainty required by the Listing Policy Section
    6.1.5.5."

data <- data |>
    # make helper cols sample level
    mutate(sample_is_clean = any(is_clean),
           sample_has_quant = any(is_quant & is_clean),
           sample_has_nonquant = any(!is_quant & is_clean),
           sample_is_nonquant = sample_has_nonquant & !sample_has_quant,
           sample_row_1 = !duplicated(sample_id),
           .by = c(loe_id, sample_id)) |>
    # inject language accordingly, at loe level
    mutate(n_total = sum(sample_is_clean & sample_row_1),
           n_nonquant = sum(sample_is_nonquant & sample_row_1),
           DATA_USED = str_squish(case_when(
                n_nonquant == 0 ~ glue(no_nonquant_lang),
                0 < n_nonquant & n_nonquant < n_total ~ glue(some_nonquant_lang),
                n_nonquant == n_total ~ glue(all_nonquant_lang),
                .default = DATA_USED
           )),
           .by = loe_id) |>
    select(all_of(.schema$all))# }}}

# assessor comment -------------------------------------------------------------{{{

version_lang <- 
    system("git rev-parse --short HEAD", intern = TRUE) |>
    (\(x) if_else(length(x) > 0, 
                  glue("LOE written by SSO assessment script (commit {x})."), 
                  "LOE written by SSO assessment script"))()

issue_lang <- 
    "{n_not_clean} rows of data were screened out for
    additional review because: {loe_issues}."

# get uniqe list of issues for each loe
loe_issues <- data |> 
    separate_rows(Issue, sep = "; ") |>
    filter(is.na(Issue)) |>
    arrange(tolower(Issue)) |>
    distinct(loe_id, Issue) |>
    summarize(loe_issues = str_flatten(Issue, collapse = "; "), 
              .by = c(loe_id))

data <- data |> 
    left_join(loe_issues, by = "loe_id") |>
    mutate(n_not_clean = sum(!is_clean),
           ASSESSOR_COMMENT = str_squish(
                if_else(n_not_clean > 0,
                        glue(version_lang, issue_lang, .sep = " "),
                        glue(version_lang))),
           .by = loe_id)# }}}

# temporal rep -----------------------------------------------------------------{{{

temporal_lang <- 
    "The samples were collected between the dates of {min_date} and
    {max_date}."

data <- data |> 
    mutate(min_date = if (any(is_clean)) min(SampleDate[is_clean]) else NA,
           max_date = if (any(is_clean)) max(SampleDate[is_clean]) else NA,
           TEMPORAL_REP = str_squish(glue(temporal_lang)),
           .by = loe_id) |>
    select(all_of(.schema$all))# }}}

# spatial rep ------------------------------------------------------------------{{{

spatial_lang <- 
    "The samples were collected at 1 monitoring site: {StationCode}
    ({StationName})."

data <- mutate(data, SPATIAL_REP = str_squish(glue(spatial_lang)))# }}}

# xwalk pollutant names --------------------------------------------------------{{{

data <- mutate(data, AnalyteName = coalesce(PollutantName_in_CalWQA, AnalyteName))# }}}

# remaining fields -------------------------------------------------------------{{{

data <- mutate(data,
    ASSESSMENT_STATUS = "LOE In Progress",
    AUTHOR = author,
    DATA_TYPE = "PHYSICAL/CHEMICAL MONITORING",
    DATE_CREATED = today(),
    MATRIX = "Water",
    REGION = region,
    SUB_GROUP = "Pollutant-Water"
)# }}}}}}

# WRITE ========================================================================{{{
cat(".") 

# script should write annotated data even if there are no loes

# loes -------------------------------------------------------------------------{{{

data_to_loe_names <- .schema |>
    filter(loes %nin% c(all, NA))  |>
    distinct(all, loes) |>
    (\(x) set_names(x$all, x$loes))()

loe_cols <- .schema |> 
    arrange(order) |>
    filter(!is.na(loes)) |>
    pull(loes)

loes <- data |> 
    filter(is_clean) |>
    rename(!!!data_to_loe_names) |>
    select(all_of(loe_cols)) |>
    distinct()

loe_path <- glue(
    "./loes/",
    "LOEs_R{region}_{ir_year}_IR_{today()}{suffix}.xlsx"
)
write_xlsx(loes, loe_path)# }}}

# annotated data ---------------------------------------------------------------{{{

annotated_path <- glue(
    "./annotated-data/",
    "Annotated_Data_R{region}_{ir_year}_IR_{today()}{suffix}.xlsx"
)
write_xlsx(data, annotated_path)#}}}

.seconds <- .start |> interval(now()) |> as.numeric() |> round()
cat(nrow(loes), " loes in ", .seconds, " seconds\n", sep = "")# }}}

# style guide: https://style.tidyverse.org
# vim: set foldmethod=marker
