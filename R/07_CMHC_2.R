#### ADDITIONAL CMHC PROCESSING ################################################

source("R/01_startup.R")

qload("data/model_chapter.qsm")
cmhc <- qread("output/data/cmhc.qs")
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
GH <- qread("output/data/GH.qs")


# Add year 6 to cmhc_str --------------------------------------------------

FREH_cmhc <- 
  monthly |> 
  filter(year(month) == 2022, !is.na(cmhc)) |> 
  summarize(FREH = sum(FREH_3) / 12, .by = cmhc)

GH_cmhc <- 
  GH |> 
  st_drop_geometry() |> 
  filter(year(month) == 2022, !is.na(cmhc)) |> 
  mutate_days() |> 
  summarize(GH = sum(housing_units * n / days) / 12, 
            .by = c(cmhc))

housing_loss_cmhc <- 
  FREH_cmhc |> 
  left_join(GH_cmhc, by = "cmhc") |> 
  mutate(across(c(FREH, GH), \(x) coalesce(x, 0))) |> 
  transmute(cmhc, housing_loss = FREH + GH)

cmhc_join <-
  cmhc$rent |> 
  filter(year == 2022) |> 
  select(neighbourhood, total_rent = total)

cmhc_str <-
  cmhc_str |> 
  filter(year == 5) |>
  left_join(housing_loss_cmhc, by = join_by(neighbourhood == cmhc)) |> 
  select(-total_rent) |> 
  left_join(cmhc_join, by = "neighbourhood") |> 
  relocate(total_rent, .after = year) |> 
  mutate(year = 6, iv = housing_loss / (dwellings * renter_pct) * 100 * 100) |> 
  select(-housing_loss) |> 
  bind_rows(cmhc_str) |> 
  arrange(neighbourhood, year)


# Save output -------------------------------------------------------------

qsave(cmhc_str, file = "output/data/cmhc_str.qs")
rm(cmhc, cmhc_join, cmhc_str, cmhc_zones, FREH_cmhc, GH, GH_cmhc,
   housing_loss_cmhc, model, monthly)
