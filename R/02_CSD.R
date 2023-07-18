#### RURAL UPDATE CSD PROCESSING ###############################################

source("R/01_startup.R")


# Get 2021 census data ----------------------------------------------------

vec_2016 <- c(renter = "v_CA16_4838",
              renter_total = "v_CA16_4836",
              avg_rent = "v_CA16_4901",
              avg_dwelling_value = "v_CA16_4896",
              mover = "v_CA16_6698",
              mover_total = "v_CA16_6692")

vec_2021 <- c(renter = "v_CA21_4239",
              renter_total = "v_CA21_4237",
              avg_rent = "v_CA21_4318",
              avg_dwelling_value = "v_CA21_4312",
              mover = "v_CA21_5751",
              mover_total = "v_CA21_5745")

names(vec_2016) <- paste(names(vec_2016), "2016", sep = "_")
names(vec_2021) <- paste(names(vec_2021), "2021", sep = "_")


# CSD ---------------------------------------------------------------------

CSD_21 <- 
  get_census("CA21", regions = list(PR = "59"), level = "CSD", 
             vectors = vec_2021, geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(CSDUID = GeoUID, name, CDUID = CD_UID, dwellings_2021 = Dwellings, 
         dwellings_2016 = `Dwellings 2016`, all_of(names(vec_2021)), 
         geometry) |> 
  filter(!str_detect(name, "\\((IRI)|(TWL)|(TAL)|(IGD)|(S-É)\\)")) |> 
  mutate(area_2021 = st_area(geometry), .before = geometry)

CSD_16 <- 
  get_census("CA16", regions = list(PR = "59"), level = "CSD", 
             vectors = vec_2016, geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(CSDUID_16 = GeoUID, name_16 = name, dwellings_2016 = Dwellings, 
         all_of(names(vec_2016)), geometry) |> 
  filter(!str_detect(name_16, "\\((IRI)|(TWL)|(TAL)|(IGD)|(S-É)\\)")) |> 
  st_intersection(st_union(CSD_21)) |> 
  mutate(area_2016 = st_area(geometry), .before = geometry)

ints <- st_intersection(CSD_21, CSD_16)

updated <- 
  ints |> 
  mutate(area_int = st_area(geometry)) |> 
  filter(units::drop_units(area_int) > 100000) |> 
  st_drop_geometry() |> 
  mutate(across(c(dwellings_2021:mover_total_2021, 
                  dwellings_2016.1:mover_total_2016), \(x) coalesce(x, 0))) |> 
  group_by(CSDUID, name, CDUID, dwellings_2021, dwellings_2016, renter_2021, 
           renter_total_2021, avg_rent_2021, avg_dwelling_value_2021, 
           mover_2021, mover_total_2021, area_2021) |> 
  summarize(
    across(c(avg_rent_2016, avg_dwelling_value_2016),
           \(x) sum(x * dwellings_2016.1) / sum(dwellings_2016.1)),
    across(c(renter_2016, renter_total_2016, mover_2016, mover_total_2016,
             dwellings_2016.1), 
           \(x) units::drop_units(sum(x * area_int / area_2016))),
    .groups = "drop") |> 
  transmute(CSDUID, name, CDUID, dwellings_2016, dwellings_2021, renter_2016,
            renter_2021,
            renter_pct_2016 = renter_2016 / renter_total_2016,
            renter_pct_2021 = renter_2021 / renter_total_2021,
            avg_rent_2016, avg_rent_2021,
            avg_dwelling_value_2016, avg_dwelling_value_2021,
            mover_pct_2016 = mover_2016 / mover_total_2016,
            mover_pct_2021 = mover_2021 / mover_total_2021)

CSD <- 
  CSD_21 |> 
  select(CSDUID, geometry) |> 
  inner_join(updated, by = "CSDUID") |> 
  relocate(geometry, .after = last_col())


# CD ----------------------------------------------------------------------

CD_21 <-
  get_census("CA21", regions = list(PR = "59"), level = "CD", 
             vectors = vec_2021, geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(CDUID = GeoUID, name, dwellings_2021 = Dwellings, 
         dwellings_2016 = `Dwellings 2016`, all_of(names(vec_2021)), 
         geometry) |> 
  mutate(area_2021 = st_area(geometry), .before = geometry)

CD_16 <- 
  get_census("CA16", regions = list(PR = "59"), level = "CD", 
             vectors = vec_2016, geo_format = "sf") |> 
  st_transform(32610) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(CDUID_16 = GeoUID, name_16 = name, dwellings_2016 = Dwellings, 
         all_of(names(vec_2016)), geometry) |> 
  st_intersection(st_union(CD_21)) |> 
  mutate(area_2016 = st_area(geometry), .before = geometry)

ints <- st_intersection(CD_21, CD_16)

updated <- 
  ints |> 
  mutate(area_int = st_area(geometry)) |> 
  filter(units::drop_units(area_int) > 100000) |> 
  st_drop_geometry() |> 
  group_by(CDUID, name, dwellings_2021, dwellings_2016, renter_2021, 
           renter_total_2021, avg_rent_2021, avg_dwelling_value_2021, 
           mover_2021, mover_total_2021, area_2021) |> 
  summarize(
    across(c(avg_rent_2016, avg_dwelling_value_2016),
           \(x) sum(x * dwellings_2016.1) / sum(dwellings_2016.1)),
    across(c(renter_2016, renter_total_2016, mover_2016, mover_total_2016,
             dwellings_2016.1), 
           \(x) units::drop_units(sum(x * area_int / area_2016))),
    .groups = "drop") |> 
  transmute(CDUID, name, dwellings_2016, dwellings_2021, renter_2016,
            renter_2021,
            renter_pct_2016 = renter_2016 / renter_total_2016,
            renter_pct_2021 = renter_2021 / renter_total_2021,
            avg_rent_2016, avg_rent_2021,
            avg_dwelling_value_2016, avg_dwelling_value_2021,
            mover_pct_2016 = mover_2016 / mover_total_2016,
            mover_pct_2021 = mover_2021 / mover_total_2021)

CD <- 
  CD_21 |> 
  select(CDUID, geometry) |> 
  inner_join(updated, by = "CDUID") |> 
  relocate(geometry, .after = last_col())


# Add tourism zones -------------------------------------------------------

tourism_zones <- list(
  van_island = c("5943", "5924", "5926", "5923", "5921", "5919", "5917"),
  vancouver = c("5915", "5929", "5931", "5909", "5927"),
  cariboo = c("5945", "5941"),
  kootenay = c("5901", "5903", "5905", "5939"),
  northern = c("5957", "5959", "5949", "5955", "5951", "5953", "5947"),
  thom_ok = c("5933", "5907", "5937", "5935")) |> 
  enframe() |> 
  unnest(value) |> 
  rename(tourism = name, CDUID = value)

CD <- 
  CD |> 
  left_join(tourism_zones, by = "CDUID") |> 
  relocate(geometry, .after = last_col())

CSD <- 
  CSD |> 
  left_join(tourism_zones, by = "CDUID") |> 
  relocate(geometry, .after = last_col())


# Identify cities to be analyzed separately -------------------------------

CSD <- 
  CSD |> 
  mutate(is_city = name %in% c(
    # Northern
    "Fort St. John (CY)",
    "Dawson Creek (CY)",
    "Prince George (CY)",
    "Smithers (T)",
    "Prince Rupert (CY)",
    "Kitimat (DM)",
    "Terrace (CY)",
    # Cariboo
    "Williams Lake (CY)",
    "Quesnel (CY)",
    "One Hundred Mile House (DM)",
    # TK missing Bella Coola
    # Thompson-Okanagan
    "Kamloops (CY)",
    "Kelowna (CY)",
    "West Kelowna (CY)",
    "Peachland (DM)",
    "Summerland (DM)",
    "Penticton (CY)",
    "Oliver (T)",
    "Osoyoos (T)",
    "Merritt (CY)",
    "Valemount (VL)",
    "Salmon Arm (CY)",
    "Sun Peaks Mountain (VL)",
    "Vernon (CY)",
    # Vancouver Coast and Mountains
    "Sechelt (DM)",
    "Squamish (DM)",
    "Whistler (DM)",
    "Vancouver (CY)",
    "Richmond (CY)",
    "Surrey (CY)",
    "Burnaby (CY)",
    "North Vancouver (CY)",
    "Pemberton (VL)",
    "Abbotsford (CY)",
    "Chilliwack (CY)",
    "Langley (CY)",
    # Vancouver Island
    "Victoria (CY)",
    "Nanaimo (CY)",
    "Parksville (CY)",
    "Comox (T)",
    "Tofino (DM)",
    "Ucluelet (DM)",
    "Port Alberni (CY)",
    "Sooke (DM)",
    "Sidney (T)",
    "Campbell River (CY)",
    # Kootenay Rockies
    "Castlegar (CY)",
    "Creston (T)",
    "Cranbrook (CY)",
    "Fernie (CY)",
    "Invermere (DM)",
    "Nelson (CY)",
    "Kimberley (CY)",
    "Rossland (CY)",
    "Trail (CY)",
    "Golden (T)",
    "Revelstoke (CY)"
  )) |> 
  mutate(name = str_remove(name, " \\(.*\\)$")) |> 
  mutate(tourism_name = if_else(is_city, name, "Non-urban areas"))

qsave(CSD, file = "output/data/CSD.qs")
qsave(CD, file = "output/data/CD.qs")

rm(CD_16, CD_21, CSD_16, CSD_21, ints, tourism_zones, updated, vec_2016, 
   vec_2021)
