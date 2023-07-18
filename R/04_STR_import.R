#### IMPORT STR DATA ###########################################################

source("R/01_startup.R")


# Load old data -----------------------------------------------------------

qs::qload("data/data_processed.qsm", nthreads = future::availableCores())
CSD <- qs::qread("output/data/CSD.qs", nthreads = future::availableCores())
exchange_rates <- qread("data/exchange_rates.qs")


# Add new property file ---------------------------------------------------

property <- 
  read_csv("data/property.csv") |> 
  select(property_ID = `Property ID`, listing_title = `Listing Title`, 
         property_type = `Property Type`, listing_type = `Listing Type`,
         created = `Created Date`, scraped = `Last Scraped Date`, 
         latitude = `Latitude`, longitude = `Longitude`, bedrooms = `Bedrooms`,
         city = `City`, ab_property = `Airbnb Property ID`, 
         ab_host = `Airbnb Host ID`, ha_property = `HomeAway Property ID`, 
         ha_host = `HomeAway Property Manager ID`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-")) |>
  mutate(host_ID = coalesce(as.character(ab_host), ha_host),
         .after = property_ID) |>
  mutate(housing = property_type %in% unique(
    property$property_type[property$housing]), .after = scraped) |> 
  mutate(scraped = as.Date(scraped)) |> 
  full_join(property, by = "property_ID") |> 
  transmute(
    property_ID,
    host_ID = coalesce(host_ID.x, host_ID.y),
    listing_title = coalesce(listing_title.x, listing_title.y),
    property_type = coalesce(property_type.x, property_type.y),
    listing_type = coalesce(listing_type.x, listing_type.y),
    created = coalesce(created.y, created.x),
    scraped = coalesce(scraped.x, scraped.y),
    housing = coalesce(housing.x, housing.y),
    latitude = coalesce(latitude.x, latitude.y),
    longitude = coalesce(longitude.x, longitude.y),
    bedrooms = coalesce(bedrooms.x, bedrooms.y),
    city = coalesce(city.x, city.y),
    ab_property = coalesce(as.character(ab_property.x), ab_property.y),
    ab_host = coalesce(as.character(ab_host.x), ab_host.y),
    ha_property = coalesce(ha_property.x, ha_property.y),
    ha_host = coalesce(ha_host.x, ha_host.y)) |> 
  mutate(housing = if_else(property_type == "Vacation home", TRUE, housing))


# Process old data --------------------------------------------------------

daily_all <- 
  daily |> 
  select(-housing) |> 
  left_join(select(property, property_ID, housing), by = "property_ID") |> 
  relocate(housing, .after = listing_type)

daily <- 
  daily_all |> 
  filter(housing)

monthly_old <- 
  daily |> 
  mutate(month = yearmonth(date)) |> 
  summarize(rev = sum(price[status == "R"]), r = sum(status == "R"), 
            a = sum(status == "A"), b = sum(status == "B"),
            .by = c(property_ID, month))


# Add new monthly file ----------------------------------------------------

monthly_new_all <- 
  read_csv("data/monthly.csv") |> 
  select(property_ID = `Property ID`, month = `Reporting Month`,
         rev = `Revenue (USD)`, r = `Reservation Days`,
         a = `Available Days`, b = `Blocked Days`) |> 
  mutate(property_ID = str_replace(property_ID, "abnb_", "ab-"),
         property_ID = str_replace(property_ID, "vrbo_", "ha-"),
         month = tsibble::yearmonth(month)) |> 
  filter(month <= yearmonth("2023-06-30"))


# Convert currency --------------------------------------------------------

# exchange_rates <-
#   exchange_rates |>
#   bind_rows(
#     upgo::convert_currency(
#       start_date = as.Date("2023-01-01"),
#       end_date = as.Date("2023-06-30"))
#   )
# 
# qsave(exchange_rates, file = "output/data/exchange_rates.qs")

exchange_rates <- qread("output/data/exchange_rates.qs")

monthly_new_all <-
  monthly_new_all |>
  left_join(mutate(exchange_rates, month = yearmonth(year_month)), 
            by = "month") |> 
  mutate(rev = rev * exchange_rate) |> 
  select(-year_month, -exchange_rate)

rm(exchange_rates)


# Process monthly_new -----------------------------------------------------

monthly_new <- 
  monthly_new_all |> 
  filter(property_ID %in% property$property_ID[property$housing]) |> 
  # Hold on to old reservations for later adjusting revenue
  mutate(r_old = r)

# Start by removing all months that are < created, > scraped, or 0 days
monthly_new <- 
  monthly_new |> 
  left_join(select(property, property_ID, created, scraped), 
            by = "property_ID") |> 
  filter(month >= yearmonth(created), month <= yearmonth(scraped),
         r + a + b > 0)

# Proportionally remove entries so r + a + b == scraped_days
scraped_change <- 
  monthly_new |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(scraped)) |> 
  mutate(scraped_days = day(scraped)) |> 
  mutate(across(c(r:b), \(x) x * scraped_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > scraped_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < scraped_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:scraped_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > scraped_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > scraped_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < scraped_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < scraped_days, a + 1, a)) |> 
  select(-scraped_days)

monthly_new <- 
  monthly_new |> 
  anti_join(scraped_change, by = c("property_ID", "month")) |> 
  bind_rows(scraped_change) |> 
  arrange(property_ID, month)

# Repeat for created
created_change <-
  monthly_new |> 
  arrange(property_ID) |> 
  filter(month == yearmonth(created)) |> 
  mutate(created_days = days_in_month(
    month(as.Date(month))) - day(created) + 1) |> 
  mutate(across(c(r:b), \(x) x * created_days / (r + a + b))) |> 
  mutate(across(c(r:b), \(x) x - round(x), .names = "{.col}_gap")) |> 
  mutate(across(c(r:b), round)) |>
  mutate(across(c(r_gap:b_gap), \(x) {
    case_when(
      r + a + b > created_days & x == pmin(r_gap, a_gap, b_gap) ~ -1,
      r + a + b < created_days & x == pmax(r_gap, a_gap, b_gap) ~ 1,
      .default = 0)
  }, .names = "{.col}_adj")) |> 
  mutate(r = r + r_gap_adj, a = a + a_gap_adj, b = b + b_gap_adj) |> 
  select(property_ID:created_days) |> 
  # If further adjustment is necessary, prefer B then A
  mutate(b = if_else(r + a + b > created_days & b > 0, b - 1, b)) |> 
  mutate(a = if_else(r + a + b > created_days & a > 0, a - 1, a)) |> 
  mutate(b = if_else(r + a + b < created_days, b + 1, b)) |> 
  mutate(a = if_else(r + a + b < created_days, a + 1, a)) |> 
  select(-created_days)

monthly_new <- 
  monthly_new |> 
  anti_join(created_change, by = c("property_ID", "month")) |> 
  bind_rows(created_change) |> 
  arrange(property_ID, month)

# Adjust revenue with r / r_old
monthly_new <- 
  monthly_new |> 
  mutate(rev = coalesce(rev * r / r_old, 0)) |> 
  select(-r_old, -created, -scraped)

# Deflate revenue to match monthly_old
deflator <-
  monthly_old |> 
  summarize(rev = sum(rev), .by = month) |> 
  inner_join(summarize(monthly_new, rev = sum(rev), .by = month),
             by = "month") |> 
  transmute(month, rev = rev.x / rev.y) |> 
  filter(month >= yearmonth("2018-06-01"), month <= yearmonth("2021-12-31")) |> 
  filter(month <= yearmonth("2019-10-31") | month >= yearmonth("2020-07-01")) |>
  mutate(mon = month(month)) |>
  summarize(d_rev = mean(rev), .by = mon)

monthly_new <- 
  monthly_new |> 
  mutate(mon = month(month)) |>
  left_join(deflator, by = "mon") |> 
  mutate(rev = rev * d_rev) |> 
  select(-mon, -d_rev)

monthly <-
  monthly_new |> 
  left_join(select(property, property_ID, host_ID, listing_type, city),
            by = "property_ID") |> 
  transmute(property_ID, month, R = r, A = a, B = b, revenue = rev, host_ID, 
            listing_type, city)


# Update CSD in property/monthly ------------------------------------------

CSD_to_join <- 
  property |> 
  strr_as_sf(32610) |> 
  select(property_ID) |>
  mutate(CSDUID = CSD$CSDUID[st_nearest_feature(geometry, CSD)]) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID, tourism, is_city, 
                   tourism_name), by = "CSDUID") |> 
  st_drop_geometry() |> 
  select(property_ID, CSDUID, tourism, is_city, tourism_name)

property <- 
  property |>
  left_join(CSD_to_join, by = "property_ID")

monthly <- 
  monthly |> 
  left_join(CSD_to_join, by = "property_ID")


# Calculate multilistings -------------------------------------------------

monthly_host <- monthly
data.table::setDT(monthly_host)
monthly_host <- monthly_host[!is.na(host_ID), .(host_ID, month, listing_type)]
host <- monthly_host[, .(count = .N), by = .(host_ID, month, listing_type)]
host <- dplyr::as_tibble(host)
thresholds <- c(EH = 2L, PR = 3L, SR = NA, HR = NA)
thresholds_names <- names(thresholds)
thresholds <- if_else(thresholds == 0, NA_integer_, as.integer(thresholds))
col_names <- names(monthly)
data.table::setDT(monthly)
data.table::setDT(host)
EH <- thresholds[thresholds_names %in% c("EH", "Entire home/apt")]
PR <- thresholds[thresholds_names %in% c("PR", "Private room")]
SR <- thresholds[thresholds_names %in% c("SR", "Shared room")]
HR <- thresholds[thresholds_names %in% c("HR", "Hotel room")]
multi <- host[listing_type == "Entire home/apt" & count >= 
                EH][, `:=`(c(".ML", "count"), list(TRUE, NULL))]

multi <- data.table::rbindlist(list(multi, host[
  listing_type == "Private room" & count >= PR][
    , `:=`(c(".ML", "count"), list(TRUE, NULL))]))

join_cols <- setdiff(names(multi), ".ML")
multi <- multi[, .(.ML = sum(.ML)), by = c("host_ID", "month")][
  , `:=`(.ML, as.logical(.ML))]
join_cols <- setdiff(names(multi), c(".ML", "listing_type"))

monthly <- multi[monthly, on = join_cols][
  , `:=`(.ML, if_else(is.na(.ML), FALSE, .ML))]
monthly <- dplyr::as_tibble(monthly)

monthly <- 
  monthly |> 
  rename(multi = .ML) |> 
  relocate(multi, .after = listing_type) |> 
  relocate(property_ID, month) |> 
  relocate(host_ID, .after = revenue)


# Save output -------------------------------------------------------------

qsave(property, file = "output/data/property.qs", nthreads = availableCores())
qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())

rm(created_change, CSD_to_join, daily, daily_all, deflator, GH, host, 
   monthly_host, monthly_new, monthly_new_all, monthly_old, multi, 
   scraped_change, col_names, EH, HR, join_cols, PR, SR, thresholds, 
   thresholds_names)
