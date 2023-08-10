#### CMHC IMPORT ###############################################################

source("R/01_startup.R")
library(furrr)
CSD <- qread("output/data/CSD.qs")


# Arguments for CMHC import function ---------------------------------------

files_name <- list.files("data/cmhc/")

regions <- unique(str_remove(files_name, "_.*"))
data <- unique(str_extract(files_name, "(?<=_).*(?=_)"))
years <- as.numeric(unique(str_extract(files_name, "\\d{4}(?=.csv)")))

type <- c("bachelor", "one_bedroom", "two_bedroom", "three_bedroom", "total")
cities_to_remove <- c("Abbotsford - Mission", "Abbotsford-Mission", 
                      "Chilliwack", "Kamloops", "Kelowna", "Nanaimo", 
                      "Prince George", "Vancouver", "Victoria", 
                      "British Columbia")


# Arguments for CMHC import function --------------------------------------

import_cmhc <- function(region, data, year) {
  
  read_csv(paste0("data/cmhc/", region, "_", data, "_", year, ".csv"), 
           skip = 2) |> 
    select(...1, Bachelor, `1 Bedroom`, `2 Bedroom`, `3 Bedroom +`, Total) |> 
    set_names(c("neighbourhood", "bachelor", "one_bedroom", "two_bedroom", 
                "three_bedroom", "total")) |> 
    mutate(across(all_of(type), ~ifelse(.x == "**", NA, .x))) |>
    mutate(across(all_of(type), ~str_remove_all(.x, "\\,"))) |> 
    mutate(across(all_of(type), ~as.numeric(.x))) |> 
    filter(!if_all(all_of(type), is.na)) |> 
    mutate(year = year, region = region, data = data) |> 
    filter(!neighbourhood %in% cities_to_remove) |> 
    mutate(neighbourhood = if_else(
      str_starts(neighbourhood, "Nanaimo"), neighbourhood, 
      str_remove_all(neighbourhood, " \\(.*\\)$")))
  
}

cmhc <- 
  future_map(set_names(data), function(dat) {
    future_map_dfr(regions, function(region) {
      future_map_dfr(years, function(year) {
        import_cmhc(region, dat, year)
      })
    })
  })


# Update CMHC names to fit with zones -------------------------------------

cmhc <- 
  map(cmhc, function(dat) {
    dat |> 
      mutate(neighbourhood = if_else(
        neighbourhood == "Downtown", 
        paste0("Downtown - ", region), neighbourhood)) |> 
      # Fixing some neighbourhood names to fit with the cmhc zones
      mutate(neighbourhood = case_when(
        neighbourhood == "Centre" ~ "Nanaimo (Centre)",
        neighbourhood == "South" ~ "Nanaimo (South)",
        neighbourhood == "North & Periphery" ~ "Nanaimo (North & Periphery)",
        neighbourhood == "Summerland DM" ~ "Summerland",
        TRUE ~ neighbourhood)) 
  })


# Save --------------------------------------------------------------------

qsave(cmhc, "output/data/cmhc.qs")
