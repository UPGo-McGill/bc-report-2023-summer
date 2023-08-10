#### Chapter 2 #################################################################

source("R/01_startup.R")
library(feasts)
library(fable)

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
GH <- qread("output/data/GH.qs")
CSD <- qread("output/data/CSD.qs")
CSD_rent <- qread("output/data/CSD_rent.qs")

qload("data/model_chapter.qsm")
model_iv_coef_dollar <- scales::dollar(model$coefficients[["iv"]], 0.01)
rm(cmhc, cmhc_zones, model)
cmhc <- qread("output/data/cmhc.qs")

cmhc_zones <- qread("output/data/cmhc_zones.qs")


# STR-induced housing loss ------------------------------------------------

FREH_total <- 
  monthly |> 
  filter(month >= yearmonth("2016-01-01")) |> 
  summarize(FREH = sum(FREH_3), .by = c(tourism, month))

GH_total <-
  GH |> 
  st_drop_geometry() |> 
  mutate_days() |> 
  summarize(GH = sum(housing_units * n / days), .by = c(tourism, month))

housing_loss <-
  FREH_total |> 
  left_join(GH_total, by = c("tourism", "month")) |> 
  rename(`Entire home/apt` = FREH, `Private room` = GH) |> 
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") |> 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

housing_loss_seasonal <-
  FREH_total |> 
  tsibble::as_tsibble(key = tourism, index = month) |> 
  model(STL(FREH, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(tourism, month, FREH = season_adjust) |> 
  left_join(GH_total, by = c("tourism", "month")) |> 
  rename(`Entire home/apt` = FREH, `Private room` = GH) |> 
  pivot_longer(c(`Entire home/apt`, `Private room`), 
               names_to = "Listing type",
               values_to = "Housing units") |> 
  mutate(`Listing type` = factor(`Listing type`, 
                                 levels = c("Private room", "Entire home/apt")))  

freh_2023 <-
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-06-01"), 
         `Listing type` == "Entire home/apt") |> 
  summarize(FREH = sum(`Housing units`)) |> 
  pull(FREH) |> 
  scales::comma(10)

gh_units_2023 <- 
  GH |> 
  st_drop_geometry() |> 
  mutate_days() |> 
  filter(month == yearmonth("2023-06-01")) |> 
  summarize(units = sum(housing_units * n / days)) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_2023 <- 
  housing_loss_seasonal |> 
  filter(month == yearmonth("2023-06-01")) |> 
  summarize(total = sum(`Housing units`, na.rm = TRUE)) |> 
  pull() |> 
  sum() |> 
  scales::comma(10)

housing_loss_change_pct_2023 <- 
  housing_loss_seasonal |> 
  mutate(year = year(month)) |> 
  filter(year %in% 2022:2023, month(month) == 6) |> 
  summarize(units = sum(`Housing units`, na.rm = TRUE), .by = year) |> 
  summarize(pct = (max(units) - min(units)) / min(units)) |> 
  pull() |> 
  scales::percent(0.1)

active_change_pct_2019_2023 <- 
  monthly |> 
  mutate(year = year(month)) |> 
  filter(year %in% c(2019, 2023), month(month) == 6) |> 
  summarize(active = sum(R + A), .by = year) |> 
  summarize(pct = (active[year == 2023] - active[year == 2019]) / 
              active[year == 2019]) |> 
  pull(pct) |> 
  abs() |> 
  scales::percent(0.1)

housing_loss_2019 <- 
  housing_loss_seasonal |> 
  filter(month == yearmonth("2019-06-01")) |> 
  summarize(total = sum(`Housing units`, na.rm = TRUE)) |> 
  pull() |> 
  sum() |> 
  scales::comma(10)

housing_loss_change_pct_2019_2023 <-
  ((parse_number(housing_loss_2023) - parse_number(housing_loss_2019)) / 
     parse_number(housing_loss_2019)) |> 
  scales::percent(0.1)


# Figure 6 ----------------------------------------------------------------

fig_6 <- 
  housing_loss_seasonal |> 
  mutate_tourism() |> 
  ggplot(aes(month, `Housing units`, fill = `Listing type`)) +
  geom_col(lwd = 0) +
  scale_fill_manual(values = col_palette[c(5, 4)]) +
  scale_x_yearmonth(name = NULL, limits = c(as.Date("2017-07-01"), NA)) +
  scale_y_continuous(name = NULL, label = scales::comma) +
  facet_wrap(~tourism, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "bottom", 
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_6.png", fig_6_2, width = 9, height = 5)


# Housing loss model ------------------------------------------------------

# Get daily housing loss
housing_loss_monthly_series <- 
  housing_loss |>  
  summarize(units = sum(`Housing units`, na.rm = TRUE), 
            .by = c(tourism, month)) |> 
  tsibble::as_tsibble(key = tourism, index = month)

# Create housing loss model
housing_loss_model <- 
  housing_loss_monthly_series |> 
  filter(month <= yearmonth("2019-11")) |> 
  model(units = decomposition_model(
    STL(units, robust = TRUE), RW(season_adjust ~ drift())))

# Create housing loss forecast
housing_loss_forecast <-
  housing_loss_model |> 
  forecast(h = "55 months") |> 
  as_tibble() |> 
  select(tourism, month, units_trend_month = .mean)

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_series |>  
  full_join(housing_loss_forecast, by = c("tourism", "month"))

# Add decay to growth rate
housing_loss_monthly_decay <-
  housing_loss_monthly_series |> 
  mutate(decay = 0.985 ^ (as.numeric(month) - 602)) |> 
  group_by(tourism) |> 
  mutate(
    lag = units_trend_month - 
      units_trend_month[month == yearmonth("Mar 2020")],
    units_trend_month = units_trend_month[month == yearmonth("Mar 2020")] + 
      (lag * decay)) |> 
  ungroup()

# Integrate forecast into monthly data
housing_loss_monthly_series <- 
  housing_loss_monthly_decay |> 
  select(tourism, month, units, units_trend = units_trend_month) |> 
  mutate(units_trend = if_else(month >= yearmonth("2020-03-01"), 
                               units_trend, NA_real_))

housing_loss_monthly_seasonal <- 
  housing_loss_monthly_series |> 
  filter(!is.na(units)) |> 
  tsibble::as_tsibble(key = tourism, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(tourism, month, units = season_adjust)
  
housing_loss_monthly_seasonal <- 
  housing_loss_monthly_series |> 
  mutate(units = coalesce(units_trend, units)) |> 
  tsibble::as_tsibble(key = tourism, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  select(tourism, month, units_trend = season_adjust) |> 
  mutate(units_trend = if_else(month < yearmonth("2020-03"), NA, 
                               units_trend)) |> 
  full_join(housing_loss_monthly_seasonal, by = c("tourism", "month"))

housing_loss_van_2023 <-
  housing_loss_monthly_seasonal |> 
  filter(tourism == "vancouver", month == yearmonth("2023 Jun")) |> 
  pull(units) |> 
  scales::comma(10)

housing_loss_van_trend_2023 <- 
  housing_loss_monthly_seasonal |> 
  filter(tourism == "vancouver", month == yearmonth("2023 Jun")) |> 
  pull(units_trend) |> 
  scales::comma(10)

housing_loss_van_dif_pct_2023 <- 
  housing_loss_monthly_seasonal |> 
  filter(tourism == "vancouver", month == yearmonth("2023 Jun")) |> 
  summarize(dif = (units_trend - units) / units) |> 
  pull(dif) |> 
  scales::percent(0.1)

housing_loss_trend_2023 <- 
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  filter(month == yearmonth("2023 Jun")) |> 
  summarize(units = sum(units), units_trend = sum(units_trend),
            dif = (units_trend - units) / units)
  

# Figure 7 ----------------------------------------------------------------

fig_7 <- 
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  filter(month <= yearmonth("2023-06-30")) |> 
  mutate_tourism() |> 
  pivot_longer(-c(tourism, month)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    tourism == "Cariboo Chilcotin Coast" & month == yearmonth("2020-07-05") & 
      name == "units" ~ "Actual housing loss", 
    tourism == "Cariboo Chilcotin Coast" & month == yearmonth("2020-03-07") & 
      name == "units_trend" ~ "Expected housing loss",
    .default = NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = month, ymin = units, ymax = units_trend, group = 1),
              data = mutate_tourism(filter(housing_loss_monthly_seasonal,
                                           month <= yearmonth("2023-06-30"))), 
              fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(month, value, color = name), lwd = 0.5) +
  geom_label(aes(month, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_yearmonth(name = NULL, limits = as.Date(c("2017-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tourism, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_7.png", fig_7, width = 9, height = 5)


# The impact of dedicated STRs on residential rents in BC -----------------

rent_change_table_raw <- 
  cmhc_str |> 
  mutate(less_rent = iv * parse_number(model_iv_coef_dollar)) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters, tourism), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  select(neighbourhood, tier, tourism, year, total_rent, iv, less_rent, 
         renters) |> 
  arrange(neighbourhood, year) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE),
         str_incr = str_change / rent_change, .by = neighbourhood)

rent_change_table_raw <- 
  rent_change_table_raw |> 
  mutate(tier = "All") |> 
  bind_rows(rent_change_table_raw)

rent_change_table <-
  rent_change_table_raw |> 
  mutate(year = case_when(year %in% 1:3 ~ "2017_2019",
                          year == 4 ~ "2020")) |> 
  filter(!is.na(year)) |> 
  group_by(year, tourism) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop")

rent_change_table_year <- 
  rent_change_table_raw |> 
  group_by(year, tourism) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop") |> 
  filter(year != 0) |> 
  mutate(year = 2016 + year, raw_rent = med_rent - med_str)


# Table 3 -----------------------------------------------------------------

rent_change_table |> 
  select(-starts_with("mean")) |> 
  pivot_wider(names_from = year, values_from = med_rent:str_incr) |> 
  relocate(ends_with("2019"), .after = tourism) |> 
  mutate_tourism() |> 
  mutate(across(c(med_rent_2017_2019, med_str_2017_2019, med_rent_2020,
                  med_str_2020), \(x) scales::dollar(x, 1)),
         across(contains("incr"), \(x) scales::percent(x, 0.1))) |> 
  mutate(med_str_2017_2019 = paste0(med_str_2017_2019, 
                                    " (", med_incr_2017_2019, ")"),
         med_str_2020 = paste0(med_str_2020, " (", med_incr_2020, ")")) |> 
  select(-med_incr_2017_2019, -med_incr_2020) |> 
  set_names(c("Tourism region", 
              "Median YOY monthly rent chg. (2017-19)",
              "Median YOY impact of STR chg. on monthly rent chg. (2017-19)",
              "Total YOY impact of STR chg. on monthly rent chg. (2017-19)",
              "Median YOY monthly rent chg. (2020)",
              "Median YOY impact of STR chg. on monthly rent chg. (2020)",
              "Total YOY impact of STR chg. on monthly rent chg. (2020)")) |> 
  gt::gt()


# Rising STR rent burdens in 2022 -----------------------------------------

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

cmhc_str_2022 <- 
  cmhc_str |> 
  filter(year == 5) |> 
  left_join(housing_loss_cmhc, by = join_by(neighbourhood == cmhc)) |> 
  mutate(year = 6,
         iv = housing_loss / (dwellings * renter_pct) * 100 * 100) |> 
  select(-housing_loss) |> 
  bind_rows(cmhc_str)

rent_change_table_raw_2022 <-
  cmhc_str_2022 |> 
  mutate(less_rent = iv * parse_number(model_iv_coef_dollar)) |> 
  left_join(select(st_drop_geometry(cmhc_zones), cmhc_zone, renters, tourism), 
            by = c("neighbourhood" = "cmhc_zone")) |> 
  select(neighbourhood, tier, tourism, year, total_rent, iv, less_rent, 
         renters) |> 
  arrange(neighbourhood, year) |> 
  filter(sum(year %in% 4:6) == 3, .by = neighbourhood) |> 
  mutate(rent_change = slide_dbl(total_rent, ~.x[2] - .x[1], .before = 1,
                                 .complete = TRUE),
         str_change = slide_dbl(less_rent, ~.x[2] - .x[1], .before = 1,
                                .complete = TRUE), .by = neighbourhood) |> 
  mutate(rent_change = if_else(year == 6, rent_change[year == 5], rent_change),
         .by = neighbourhood) |> 
  mutate(str_incr = str_change / rent_change, .by = neighbourhood) |> 
  filter(year %in% 5:6)

rent_change_table_raw_2022 <- 
  rent_change_table_raw_2022 |> 
  mutate(raw_rent = rent_change - str_change, .before = less_rent) |> 
  mutate(rent_change = if_else(year == 6, raw_rent[year == 5] + str_change, 
                               rent_change), .by = neighbourhood) |> 
  mutate(str_incr = str_change / rent_change)

rent_change_table_2022 <-
  rent_change_table_raw_2022 |> 
  mutate(year = case_when(year == 5 ~ "2021",
                          year == 6 ~ "2022")) |> 
  filter(!is.na(year)) |> 
  group_by(year, tourism) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop")

str_rent_change_median_2022 <- 
  rent_change_table_raw_2022 |> 
  mutate(year = case_when(year == 5 ~ "2021",
                          year == 6 ~ "2022")) |> 
  filter(!is.na(year)) |> 
  group_by(year) |> 
  summarize(
    med_rent = median(rent_change, na.rm = TRUE),
    med_str = median(str_change, na.rm = TRUE),
    med_incr = median(str_incr, na.rm = TRUE),
    mean_rent = mean(rent_change, na.rm = TRUE),
    mean_str = mean(str_change, na.rm = TRUE),
    mean_incr = mean(str_incr, na.rm = TRUE),
    str_incr = sum(str_change * renters, na.rm = TRUE) / 
      sum(rent_change * renters, na.rm = TRUE),
    .groups = "drop") |> 
  slice(2) |> 
  pull(mean_str) |> 
  scales::dollar(1)

str_rent_change_pct_2022 <- 
  rent_change_table_raw_2022 |> 
  filter(year == 6, !is.na(str_change), !is.na(rent_change)) |> 
  summarize(str_change = sum(str_change * renters) / 
              sum(rent_change * renters)) |> 
  pull() |> 
  scales::percent(0.1)


# Table 4 -----------------------------------------------------------------

rent_change_table_2022 |> 
  select(-starts_with("mean")) |> 
  pivot_wider(names_from = year, values_from = med_rent:str_incr) |> 
  relocate(ends_with("2021"), .after = tourism) |> 
  mutate_tourism() |> 
  mutate(across(c(med_rent_2021, med_str_2021, med_rent_2022, med_str_2022), 
                \(x) scales::dollar(x, 1)),
         across(contains("incr"), \(x) scales::percent(x, 0.1))) |> 
  mutate(med_str_2021 = paste0(med_str_2021, 
                                    " (", med_incr_2021, ")"),
         med_str_2022 = paste0(med_str_2022, " (", med_incr_2022, ")")) |> 
  select(-med_incr_2021, -med_incr_2022) |> 
  set_names(c("Tourism region", 
              "Median YOY monthly rent chg. (2021)",
              "Median YOY impact of STR chg. on monthly rent chg. (2021)",
              "Total YOY impact of STR chg. on monthly rent chg. (2021)",
              "Median YOY monthly rent chg. (2022 estimated)",
              "Median YOY impact of STR chg. on monthly rent chg. (2022)",
              "Total YOY impact of STR chg. on monthly rent chg. (2022)")) |> 
  gt::gt()


# Trend analysis: STR rent burden -----------------------------------------

housing_loss_2023 <- 
  housing_loss_monthly_series |> 
  filter(month == max(month)) |> 
  pull(units_trend) |> 
  sum() |> 
  scales::comma(100)

housing_loss_change_2022_2023 <-
  housing_loss_monthly_series |> 
  filter(month == yearmonth("2022-12-31") | month == yearmonth("2023-12-31")) |> 
  as_tibble() |> 
  summarize(dif = (sum(units_trend[year(month) == 2023], na.rm = TRUE) - 
                     sum(units, na.rm = TRUE)) / sum(units, na.rm = TRUE)) |> 
  pull(dif) |> 
  scales::percent(0.1)


# Figure 8 ----------------------------------------------------------------

fig_8 <-
  housing_loss_monthly_seasonal |> 
  as_tibble() |> 
  mutate_tourism() |> 
  pivot_longer(-c(tourism, month)) |> 
  filter(!is.na(value)) |> 
  mutate(label = case_when(
    tourism == "Cariboo Chilcotin Coast" & month == yearmonth("2020-07-05") & 
      name == "units" ~ "Actual housing loss", 
    tourism == "Cariboo Chilcotin Coast" & month == yearmonth("2020-03-07") & 
      name == "units_trend" ~ "Expected housing loss",
    .default = NA_character_)) |> 
  ggplot() +
  geom_ribbon(aes(x = month, ymin = units, ymax = units_trend, group = 1),
              data = mutate_tourism(filter(housing_loss_monthly_seasonal,
                                           month <= yearmonth("2023-06-30"))), 
              fill = col_palette[2], alpha = 0.3) +
  geom_line(aes(month, value, color = name), lwd = 0.5) +
  geom_label(aes(month, value, label = label, color = name), 
             fill = alpha("white", 0.75), size = 3) +
  scale_x_yearmonth(name = NULL, limits = as.Date(c("2017-01-01", NA))) +
  scale_y_continuous(name = NULL, limits = c(0, NA), 
                     label = scales::comma) +
  scale_color_manual(name = NULL, 
                     labels = c("Actual STR housing loss", 
                                "Expected STR housing loss"), 
                     values = col_palette[c(5, 6)]) +
  facet_wrap(~tourism, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_8.png", fig_8, width = 9, height = 5)


# Housing loss summaries --------------------------------------------------

CSD_seasonal <-
  housing_loss_monthly_series |> 
  filter(!is.na(units)) |> 
  tsibble::as_tsibble(key = tourism, index = month) |> 
  model(STL(units, robust = TRUE)) |> 
  components() |> 
  as_tibble() |> 
  transmute(tourism, month, adjust = season_adjust / units) |> 
  mutate(adjust = coalesce(if_else(is.infinite(adjust), 0, adjust), 0))

FREH_CSD <-
  monthly |> 
  mutate(year = year(month)) |> 
  filter(year >= 2016) |> 
  left_join(CSD_seasonal, by = c("tourism", "month")) |> 
  summarize(FREH_all = sum(FREH_3 * adjust), 
            FREH_june = sum(FREH_3[month(month) == 6] * 
                              adjust[month(month) == 6]),
            .by = c(tourism, tourism_name, CSDUID, year)) |> 
  mutate(FREH_all = if_else(year == 2023, FREH_all / 6, FREH_all / 12))

GH_CSD <- 
  GH |> 
  st_drop_geometry() |> 
  mutate(year = year(month)) |> 
  filter(year >= 2016) |> 
  left_join(CSD_seasonal, by = c("tourism", "month")) |> 
  mutate_days() |> 
  summarize(GH_all = sum(housing_units * adjust * n / days), 
            GH_june = sum(housing_units[month(month) == 6] * 
                            adjust[month(month) == 6] *
                            n[month(month) == 6] / days[month(month) == 6]), 
            .by = c(tourism, tourism_name, CSDUID, year)) |> 
  mutate(GH_all = if_else(year == 2023, GH_all / 6, GH_all / 12))

housing_loss_CSD <-
  FREH_CSD |> 
  full_join(GH_CSD, by = c("tourism", "tourism_name", "CSDUID", "year")) |> 
  mutate(across(c(FREH_all, FREH_june, GH_all, GH_june),
                \(x) coalesce(x, 0))) |> 
  transmute(tourism, tourism_name, CSDUID, year, 
            housing_loss_all = FREH_all + GH_all,
            housing_loss_june = FREH_june + GH_june) |> 
  arrange(tourism, tourism_name, CSDUID, year)

units_CSD <- 
  CSD |> 
  st_drop_geometry() |> 
  mutate(renter_pct_2021 = coalesce(renter_pct_2021, 0)) |> 
  summarize(dwellings = sum(dwellings_2021),
            rentals = sum(dwellings_2021 * renter_pct_2021),
            .by = c(tourism, tourism_name))

rental_factor <-
  cmhc_str |> 
  filter(!is.na(renter_pct), !is.na(dwellings)) |> 
  summarize(sum(dwellings * renter_pct) / sum(dwellings) / 100) |> 
  pull()


# Cariboo Chilcotin Coast -------------------------------------------------

fig_9 <- 
  housing_loss_CSD |> 
  filter(tourism == "cariboo") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "cariboo"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "cariboo"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "cariboo"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_9.png", fig_9, width = 9, height = 5)

tab_5 <-
  housing_loss_CSD |> 
  filter(tourism == "cariboo") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_5 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_5, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()


# Kootenay Rockies --------------------------------------------------------

fig_10 <- 
  housing_loss_CSD |> 
  filter(tourism == "kootenay") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "kootenay"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "kootenay"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "kootenay"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_10.png", fig_10, width = 9, height = 5)

tab_6 <-
  housing_loss_CSD |> 
  filter(tourism == "kootenay") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_6 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_6, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()


# Northern BC -------------------------------------------------------------

fig_11 <- 
  housing_loss_CSD |> 
  filter(tourism == "northern") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "northern"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "northern"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "northern"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_11.png", fig_11, width = 9, height = 5)

tab_7 <-
  housing_loss_CSD |> 
  filter(tourism == "northern") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_7 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_7, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()


# Thompson Okanagan -------------------------------------------------------

fig_12 <- 
  housing_loss_CSD |> 
  filter(tourism == "thom_ok") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "thom_ok"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "thom_ok"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "thom_ok"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_12.png", fig_12, width = 9, height = 5)

tab_8 <-
  housing_loss_CSD |> 
  filter(tourism == "thom_ok") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_8 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_8, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()


# Vancouver Island --------------------------------------------------------

fig_13 <- 
  housing_loss_CSD |> 
  filter(tourism == "van_island") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "van_island"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "van_island"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "van_island"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))


ggsave("output/figure_13.png", fig_13, width = 9, height = 5)

tab_9 <-
  housing_loss_CSD |> 
  filter(tourism == "van_island") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_9 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_9, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()


# Vancouver Coast and Mountains -------------------------------------------

fig_14 <- 
  housing_loss_CSD |> 
  filter(tourism == "vancouver") |> 
  filter(year == 2023) |> 
  left_join(select(CSD, CSDUID, name, dwellings_2021, renter_pct_2021, 
                   geometry), by = "CSDUID") |> 
  st_as_sf() |> 
  ggplot(aes(fill = housing_loss_june / 
               (dwellings_2021 * renter_pct_2021) * 100)) +
  geom_sf(data = CSD, colour = "transparent", fill = "grey80") +
  geom_sf(data = filter(CSD, tourism == "vancouver"), fill = "grey90",
          colour = "transparent") +
  geom_sf(colour = "grey50") +
  ggrepel::geom_label_repel(aes(label = name, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 2.5) +
  scale_fill_stepsn(name = "Commercial STRs / 100 rental units",
                    colours = col_palette[c(1, 2, 4, 7)], limits = c(0, 10),
                    oob = scales::squish) +
  coord_sf(xlim = st_bbox(filter(CSD, tourism == "vancouver"))[c(1, 3)],
           ylim = st_bbox(filter(CSD, tourism == "vancouver"))[c(2, 4)]) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_14.png", fig_14, width = 9, height = 5)

tab_10 <-
  housing_loss_CSD |> 
  filter(tourism == "vancouver") |> 
  summarize(loss_all = sum(housing_loss_all), 
            loss_june = sum(housing_loss_june), 
            .by = c(tourism, tourism_name, year)) |> 
  arrange(tourism_name, year) |> 
  filter(year %in% 2021:2023) |> 
  pivot_wider(id_cols = c(tourism, tourism_name), names_from = year,
              values_from = c(loss_all, loss_june)) |> 
  left_join(units_CSD, by = c("tourism", "tourism_name")) |> 
  mutate(loss_all_per_100 = loss_all_2023 / rentals * 100,
         loss_june_per_100 = loss_june_2023 / rentals * 100) |> 
  mutate(loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022, 
         .after = loss_june_2023) |> 
  mutate(str_rent_2022 = (loss_all_2022 - loss_all_2021) / 
           (dwellings * rental_factor) * 100 * 
           parse_number(model_iv_coef_dollar)) |> 
  mutate(total_rent = str_rent_2022 * rentals * 12) |> 
  select(tourism_name, loss_june_2022, loss_june_2023, loss_change, dwellings,
         rentals, loss_june_per_100, str_rent_2022, total_rent, loss_all_2021,
         loss_all_2022)

tab_10 |> 
  summarize(
    tourism_name = "All",
    loss_june_2022 = sum(loss_june_2022),
    loss_june_2023 = sum(loss_june_2023),
    loss_change = (loss_june_2023 - loss_june_2022) / loss_june_2022,
    dwellings = sum(dwellings),
    rentals = sum(rentals),
    loss_june_per_100 = loss_june_2023 / rentals * 100,
    str_rent_2022 = (sum(loss_all_2022) - sum(loss_all_2021)) / 
      (dwellings * rental_factor) * 100 * parse_number(model_iv_coef_dollar),
    total_rent = sum(total_rent)) |> 
  bind_rows(select(tab_10, -loss_all_2021, -loss_all_2022)) |> 
  mutate(str_rent_2022 = if_else(loss_june_2022 < 10, NA_real_, 
                                 str_rent_2022)) |> 
  mutate(total_rent = if_else(is.na(str_rent_2022), NA_real_, total_rent)) |> 
  transmute(
    tourism_name,
    loss_june_2022 = scales::comma(loss_june_2022, 1),
    loss_june_2023 = scales::comma(loss_june_2023, 1),
    loss_change = scales::percent(loss_change, 0.1),
    loss_june_per_100 = scales::comma(loss_june_per_100, 0.1),
    str_rent_2022 = scales::dollar(str_rent_2022, 1),
    total_rent = scales::dollar(total_rent, 0.1, scale = 1 / 1000000, 
                                suffix = " million")) |> 
  mutate(tourism_name = if_else(tourism_name == "Non-urban areas", 
                                "Other areas", tourism_name)) |> 
  arrange(tourism_name) |> 
  gt::gt()

