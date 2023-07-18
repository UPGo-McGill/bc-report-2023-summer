#### STR PROCESSING ############################################################

source("R/01_startup.R")
library(caret)

property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
CSD <- qs::qread("output/data/CSD.qs", nthreads = future::availableCores())
cmhc_zones <- qread("output/data/cmhc_zones.qs", 
                    nthreads = future::availableCores())


# FREH --------------------------------------------------------------------

monthly <- 
  monthly |> 
  arrange(property_ID, month) |> 
  mutate(R_12 = slide_dbl(R, sum, .before = 11), 
         A_12 = slide_dbl(A, sum, .before = 11), 
         AR_12 = R_12 + A_12, 
         .by = property_ID) |> 
  mutate(FREH = R_12 >= 90 & AR_12 >= 183 & listing_type == "Entire home/apt", 
         .after = revenue)


# Produce monthly activity table for all EH listings ----------------------

monthly_FREH <-
  monthly |> 
  left_join(select(property, property_ID, created), by = "property_ID") |> 
  # Trim listings to the start of the month
  mutate(created = if_else(day(created) == 1, created,
                           floor_date(created, "month") %m+% months(1))) |> 
  mutate(year = year(month),
         month = month(month),
         created_year = year(created),
         created_month = month(created),
         month_since_created = (year - created_year) * 12 +
           (month - created_month)) |> 
  select(property_ID, year, month, month_since_created, R, A, B, FREH) |> 
  left_join(select(property, property_ID, tourism), by = "property_ID")


# Model based on last 3 months --------------------------------------------

# Summarize by month
after_one_year <-
  monthly_FREH %>%
  filter(year < 2020) %>%
  mutate(month = month.name[.data$month], AR = A + R) %>%
  group_by(property_ID) %>%
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(R_3), !is.na(AR_3), month_since_created >= 12)


# Fit models and apply to listings > 2 months -----------------------------

model_3 <- glm(FREH ~ R_3 + AR_3 + month, data = after_one_year,
               family = binomial)

model_3_results <-
  monthly_FREH %>%
  mutate(month = month.name[.data$month],
         AR = A + R) %>%
  group_by(property_ID) %>%
  mutate(R_3 = slide_int(R, sum, .before = 2, .complete = TRUE),
         AR_3 = slide_int(AR, sum, .before = 2, .complete = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(R_3), !is.na(AR_3)) %>%
  modelr::add_predictions(model_3, type = "response") %>%
  mutate(FREH_3 = pred) %>%
  select(-pred) %>%
  rowwise() %>%
  mutate(month = which(month.name == month)) %>%
  ungroup()

monthly <-
  monthly |> 
  mutate(year = year(month), month_int = month(month)) |> 
  left_join(select(model_3_results, property_ID, year, month_int = month, 
                   FREH_3), by = c("property_ID", "year", "month_int")) |> 
  mutate(FREH_3 = coalesce(FREH_3, 0)) |> 
  relocate(FREH_3, .after = FREH) |> 
  select(-year, -month_int)


# Model testing -----------------------------------------------------------

# Split the data into training and test set
training_samples_3 <-
  after_one_year$FREH %>% createDataPartition(p = 0.80, list = FALSE)

train_data_3 <- after_one_year[training_samples_3, ]
test_data_3 <- after_one_year[-training_samples_3, ]

# Fit the model
model_3_test <- glm(FREH ~ R_3 + AR_3 + month, data = train_data_3,
                    family = binomial)

# Test models
probabilities_3 <- model_3_test %>% predict(test_data_3, type = "response")
predicted_classes_3 <- ifelse(probabilities_3 > 0.5, "TRUE", "FALSE")
mean(predicted_classes_3 == test_data_3$FREH)
# Outcome: 0.771


# GH ----------------------------------------------------------------------

GH <- strr_ghost(property)

GH <- 
  GH |> 
  select(-data) |> 
  mutate(month = yearmonth(date)) |> 
  summarize(n = n(), .by = c(ghost_ID, month, host_ID, listing_count, 
                             housing_units, property_IDs, geometry)) |> 
  relocate(n, .after = month)

GH <-
  GH |> 
  st_transform(32610) |> 
  mutate(CSDUID = CSD$CSDUID[st_nearest_feature(st_centroid(geometry), CSD)]) |> 
  left_join(select(st_drop_geometry(CSD), CSDUID, tourism, tourism_name), 
            by = "CSDUID") |> 
  relocate(tourism, tourism_name, .before = geometry)

GH_cmhc <- 
  GH |>
  mutate(geometry = st_centroid(geometry)) |> 
  st_intersection(select(cmhc_zones, cmhc_zone, geometry)) |> 
  st_drop_geometry() |> 
  select(ghost_ID, cmhc = cmhc_zone) |> 
  distinct()
  
GH <-
  GH |> 
  st_drop_geometry() |> 
  unnest(property_IDs) |> 
  rename(property_ID = property_IDs) |> 
  left_join(select(monthly, property_ID, month, R, A, B), 
            by = c("property_ID", "month")) |> 
  summarize(property_IDs = list(property_ID),
            active_pct = sum(R + A) / sum(R + A + B),
            .by = c(ghost_ID, month, n, host_ID, listing_count, housing_units, 
                    tourism, tourism_name, CSDUID)) |> 
  relocate(property_IDs, .after = housing_units)

GH <- 
  GH |> 
  left_join(GH_cmhc, by = "ghost_ID") |> 
  filter(month <= yearmonth("2023-06-01"))


# Add cmhc to monthly also ------------------------------------------------

property_cmhc <- 
  property |> 
  strr_as_sf(32610) |> 
  st_intersection(select(cmhc_zones, cmhc = cmhc_zone, geometry)) |> 
  select(property_ID, cmhc) |> 
  st_drop_geometry()

monthly <- 
  monthly |> 
  left_join(property_cmhc, by = "property_ID")


# Save output -------------------------------------------------------------

qsave(monthly, file = "output/data/monthly.qs", nthreads = availableCores())
qsave(GH, file = "output/data/GH.qs", nthreads = availableCores())
qsavem(after_one_year, model_3, model_3_results, 
       file = "output/data/FREH_model.qsm", nthreads = availableCores())

rm(after_one_year, model_3, model_3_results, model_3_test, monthly_FREH,
   test_data_3, train_data_3, training_samples_3, predicted_classes_3,
   probabilities_3, GH_cmhc, property_cmhc)
