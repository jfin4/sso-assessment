cat("starting at line 1\n")
# compare rloes to sloe writer output
# john.inman@waterboards.ca.gov
# 2026-08-13

# load packages ----------------------------------------------------------------{{{

library(data.table)
library(digest)
library(fs)
library(readxl)
library(writexl)
library(tidyverse)

rm(list = ls())# }}}

# read files -------------------------------------------------------------------{{{

# old loes
path <- dir_ls("./loes/", regex = "old") |>
    tail(1)
.old_loes <- read_excel(path, col_types = "text", na = "")

# new loes
path <- dir_ls("./loes/", regex = "new") |>
    tail(1)
.new_loes <- read_excel(path, col_types = "text", na = "")

# old anno
path <- dir_ls("./annotated-data/", regex = "old") |> 
    tail(1)
.old_anno <- read_excel(path, col_types = "text", na = "")

# new anno
path <- dir_ls("./annotated-data/", regex = "new") |> 
    tail(1)
.new_anno <- read_excel(path, col_types = "text", na = "")# }}}

# define compare_loes() --------------------------------------------------------{{{

cols <-  c("DATA_USED_REFERENCES", "STATION_CODE", "POLLUTANT", "FRACTION")
# cols <-  c("MIGRATION_ID")
tags <- c("_old", "_new")
compare_loes <- function(old, new, id_cols = cols, suffix = tags) {
    value_cols <- setdiff(names(old), id_cols)

    joined <- full_join(old, new, by = id_cols, suffix = suffix)

    out <- value_cols %>%
        set_names() %>%
        map(function(col) {
                old_col <- paste0(col, suffix[1])
                new_col <- paste0(col, suffix[2])
                joined %>%
                    select(all_of(id_cols), any_of(old_col), any_of(new_col)) %>%
                    filter(!map2_lgl(.data[[old_col]], .data[[new_col]], identical))
        })

    out_names <- names(out) |>
        str_replace_all("\\W", "_")  |>
        str_trim() |>
        str_sub(1, 31) # excel sheet name limit

    set_names(out, out_names)
}# }}}

# compare loes -----------------------------------------------------------------{{{

diffs <- compare_loes(.old_loes, .new_loes) |>
    keep(\(x) nrow(x) > 0)# }}}

# end

# pull loe field
col <- "DATA_USED"
map(c("_old", "_new"), \(x) pull(diffs[[col]][1, ], ends_with(x)))

# make example lookup
col <- "SAMPLE_COUNT"
diffs[[col]]
eg_lu <- diffs[[i]] |>
    slice_head() |>
    mutate(loe_id = MIGRATION_ID) |>
    select(MIGRATION_ID, loe_id, ends_with(c("_old", "_new")))

# compare data
walk(list(.old_anno, .new_anno), \(data) {
        data |> 
            semi_join(eg_lu) |>
            print(n = Inf)
})
