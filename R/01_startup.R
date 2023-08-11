#### 01 STARTUP ################################################################

# There is usually no need to run this script directly; it is sourced from the
# other scripts which need it.


# Optionally install packages from GitHub ---------------------------------

# remotes::install_github("UPGo-McGill/upgo")
# remotes::install_github("UPGo-McGill/strr")
# remotes::install_github("UPGo-McGill/matchr")


# Load packages -----------------------------------------------------------

library(tidyverse)
library(lubridate)
library(upgo)
library(strr)
library(sf)
library(future)
library(progressr)
library(slider)
library(data.table)
library(gt)
library(extrafont)
library(patchwork)
library(qs)
library(cancensus)
library(tsibble)
plan(multisession)
handlers(global = TRUE)


# Set global variables ----------------------------------------------------

col_palette <-
  c("#B8D6BE", "#73AE80", "#B5C0DA", "#6C83B5", "#2A5A5B", "#B58A6C", "#5B362A",
    "#AE7673")
             

# Helper functions --------------------------------------------------------

mutate_tourism <- function(x) {
  x |> 
    mutate(tourism = case_when(
      tourism == "cariboo" ~ "Cariboo Chilcotin Coast",
      tourism == "kootenay" ~ "Kootenay Rockies",
      tourism == "northern" ~ "Northern BC",
      tourism == "thom_ok" ~ "Thompson Okanagan",
      tourism == "van_island" ~ "Vancouver Island",
      tourism == "vancouver" ~ "Vancouver Coast and Mountains"))
}

mutate_days <- function(x) {
  x |> 
  mutate(days = days_in_month(month(as.Date(month))))
}
  