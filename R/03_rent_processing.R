#### RENT PROCESSING ###########################################################

source("R/01_startup.R")

CSD <- qs::qread("output/data/CSD.qs", nthreads = future::availableCores())
CD <- qs::qread("output/data/CD.qs", nthreads = future::availableCores())
qload("data/model_chapter.qsm")


# Group CMHC zones by CD --------------------------------------------------

cmhc_to_CD <- 
  cmhc_zones |> 
  transmute(cmhc_zone, dwellings, cmhc_area = st_area(geometry)) |> 
  st_intersection(transmute(CD, CDUID, CD_name = name, 
                            CD_area = st_area(geometry))) |> 
  mutate(int_area = st_area(geometry)) |> 
  st_drop_geometry() |> 
  mutate(cmhc_pct = units::drop_units(int_area / cmhc_area),
         CD_pct = units::drop_units(int_area / CD_area)) |> 
  filter(cmhc_pct >= 0.1 | CD_pct >= 0.1) |> 
  select(cmhc_zone, CDUID) |> 
  arrange(cmhc_zone)

cmhc_zones <- 
  cmhc_zones |> 
  left_join(cmhc_to_CD, by = "cmhc_zone") |> 
  left_join(select(st_drop_geometry(CD), CDUID, tourism), by = "CDUID") |> 
  relocate(geometry, .after = last_col())

cmhc_conversion <- 
  cmhc |> 
  bind_rows() |> 
  filter(data %in% c("rent", "units"), year %in% c(2016, 2021)) |> 
  select(data, neighbourhood, total, year) |> 
  filter(neighbourhood != "James Bay" | total %in% c(1125, 1345, 2060, 1924)) |> 
  pivot_wider(id_cols = c(neighbourhood, year), 
              names_from = data, values_from = total) |> 
  arrange(neighbourhood, year) |> 
  rename(cmhc_zone = neighbourhood) |> 
  left_join(cmhc_to_CD, by = "cmhc_zone") |> 
  summarize(rent = sum(rent * units, na.rm = TRUE) / sum(units, na.rm = TRUE), 
            .by = c(CDUID, year)) |> 
  pivot_wider(id_cols = CDUID, names_from = year, values_from = rent, 
              names_prefix = "rent_") |> 
  left_join(st_drop_geometry(CD), by = "CDUID") |> 
  select(CDUID, name, avg_rent_2016, rent_2016, avg_rent_2021, rent_2021,
         dwellings_2016, dwellings_2021) |> 
  mutate(dif_2016 = rent_2016 / avg_rent_2016,
         dif_2021 = rent_2021 / avg_rent_2021) |> 
  summarize(ratio_2016 = mean(dif_2016, na.rm = TRUE),
            ratio_2021 = mean(dif_2021, na.rm = TRUE),
            ratio_2016_w = sum(dif_2016 * dwellings_2016, na.rm = TRUE) / 
              sum(dwellings_2016, na.rm = TRUE),
            ratio_2021_w = sum(dif_2021 * dwellings_2021, na.rm = TRUE) / 
              sum(dwellings_2021, na.rm = TRUE))

CSD_rent <-
  CSD |> 
  transmute(CSDUID, name, CDUID, tourism, tourism_name, dwellings_2016,
            dwellings_2021, renter_pct_2016, renter_pct_2021, avg_rent_2016, 
            avg_rent_2021,
            cmhc_rent_2016 = avg_rent_2016 * cmhc_conversion$ratio_2016_w,
            cmhc_rent_2021 = avg_rent_2021 * cmhc_conversion$ratio_2021_w)

qsave(CSD_rent, "output/data/CSD_rent.qs")
qsave(cmhc_zones, "output/data/cmhc_zones.qs")

rm(cmhc, cmhc_conversion, cmhc_str, cmhc_to_CD, cmhc_zones, model)
