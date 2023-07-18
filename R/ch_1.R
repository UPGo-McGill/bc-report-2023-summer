#### Chapter 1 #################################################################

source("R/01_startup.R")
property <- qread("output/data/property.qs", nthreads = availableCores())
monthly <- qread("output/data/monthly.qs", nthreads = availableCores())
CSD <- qread("output/data/CSD.qs")


# Active listings and revenue ---------------------------------------------

active_avg_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01")) |> 
  summarize(active = sum(R + A) / 30) |> 
  pull() |> 
  scales::comma(10)

hosts_avg_2023 <-
  monthly |> 
  filter(month == yearmonth("2023-06-01"), A + R > 0) |> 
  count(host_ID) |> 
  nrow() |> 
  mean() |> 
  scales::comma(10)

rev_total_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01")) |> 
  pull(revenue) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_host_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01"), !is.na(host_ID), R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(avg = mean(rev), med = median(rev))

rev_avg_2023 <- scales::dollar(rev_host_2023$avg, 100)
rev_med_2023 <- scales::dollar(rev_host_2023$med, 100)

active_growth_2022_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01") | month == yearmonth("2022-06-01")) |> 
  summarize(active_2023 = sum(R[year(month) == 2023] + 
                                A[year(month) == 2023]) / 30,
            active_2022 = sum(R[year(month) == 2022] + 
                                A[year(month) == 2022]) / 30,
            chg = (active_2023 - active_2022) / active_2022) |> 
  pull(chg) |> 
  scales::percent(0.1)

rev_growth_2022_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01") | month == yearmonth("2022-06-01")) |> 
  summarize(rev_2023 = sum(revenue[year(month) == 2023]),
            rev_2022 = sum(revenue[year(month) == 2022]),
            chg = (rev_2023 - rev_2022) / rev_2022) |> 
  pull(chg) |> 
  scales::percent(0.1)

active_avg_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01")) |> 
  summarize(active = sum(R + A) / 30) |> 
  pull() |> 
  scales::comma(10)

active_change_pct <- 
  ((parse_number(active_avg_2023) - parse_number(active_avg_2019)) / 
     parse_number(active_avg_2019)) |> 
  scales::percent(0.1)

hosts_avg_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01"), A + R > 0) |> 
  count(host_ID) |> 
  nrow() |> 
  mean() |> 
  scales::comma(10)

hosts_change_pct <- 
  ((parse_number(hosts_avg_2023) - parse_number(hosts_avg_2019)) / 
     parse_number(hosts_avg_2019)) |> 
  scales::percent(0.1)

rev_total_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01")) |> 
  pull(revenue) |> 
  sum() |> 
  scales::dollar(0.1, scale = 1/1000000, suffix = " million")

rev_change_pct <- 
  ((parse_number(rev_total_2023) - parse_number(rev_total_2019)) / 
     parse_number(rev_total_2019)) |> 
  scales::percent(0.1)

rev_host_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01"), !is.na(host_ID), R > 0) |> 
  summarize(rev = sum(revenue), .by = host_ID) |> 
  summarize(avg = mean(rev), med = median(rev))

rev_avg_2019 <- scales::dollar(rev_host_2019$avg, 100)
rev_med_2019 <- scales::dollar(rev_host_2019$med, 100)

rev_avg_change_pct <- ((rev_host_2023$avg - rev_host_2019$avg) / 
                         rev_host_2019$avg) |> 
  scales::percent(0.1)

rev_med_change_pct <- ((rev_host_2023$med - rev_host_2019$med) / 
                         rev_host_2019$med) |> 
  scales::percent(0.1)

rev_per_night_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01")) |> 
  summarize(rev_per_night = sum(revenue) / sum(R)) |> 
  pull() |> 
  scales::dollar(1)
  
rev_per_night_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01")) |> 
  summarize(rev_per_night = sum(revenue) / sum(R)) |> 
  pull() |> 
  scales::dollar(1)

rev_per_night_change_pct <- ((parse_number(rev_per_night_2023) - 
                                parse_number(rev_per_night_2019)) / 
                               parse_number(rev_per_night_2019)) |> 
  scales::percent(0.1)


# Figure 2 ----------------------------------------------------------------

active_monthly_tourism <-
  monthly |> 
  mutate_days() |> 
  summarize(active = sum(R + A) / mean(days), .by = c(month, tourism)) 

fig_2 <- 
  active_monthly_tourism |> 
  mutate_tourism() |> 
  filter(month >= yearmonth("2017-07-01")) |> 
  ggplot() +
  geom_line(aes(month, active, colour = tourism), linewidth = 1) +
  facet_wrap(~tourism, scales = "free_y") +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  scale_y_continuous(name = NULL, labels = scales::comma) +
  scale_x_yearmonth(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_2.png", fig_2, width = 9, height = 5)


# Table 1 -----------------------------------------------------------------

tourism_name_dwellings <- 
  CSD |> 
  st_drop_geometry() |> 
  summarize(dwellings = sum(dwellings_2021), .by = c(tourism, tourism_name))

tourism_dwellings <- 
  tourism_name_dwellings |> 
  summarize(dwellings = sum(dwellings), .by = tourism)

tab_1 <-
  monthly |> 
  filter(month(month) == 6, year(month) %in% c(2022, 2023)) |> 
  summarize(active_2023 = sum(A[year(month) == 2023] + 
                                R[year(month) == 2023]) / 30,
            active_2022 = sum(A[year(month) == 2022] + 
                                R[year(month) == 2022]) / 30,
            active_growth = (active_2023 - active_2022) / active_2022,
            rev_2023 = sum(revenue[year(month) == 2023]),
            rev_2022 = sum(revenue[year(month) == 2022]),
            rev_growth = (rev_2023 - rev_2022) / rev_2022,
            .by = c(tourism, tourism_name)) |> 
  arrange(tourism, tourism_name) |> 
  left_join(tourism_name_dwellings, by = c("tourism", "tourism_name")) |> 
  mutate(active_pct = active_2023 / dwellings, .after = active_growth) |> 
  select(-active_2022, -rev_2022, -dwellings)

tab_1 <- 
  monthly |> 
  left_join(select(st_drop_geometry(CSD), CSDUID, dwellings_2021), 
            by = "CSDUID") |> 
  filter(month(month) == 6, year(month) %in% c(2022, 2023)) |> 
  summarize(active_2023 = sum(A[year(month) == 2023] + 
                                R[year(month) == 2023]) / 30,
            active_2022 = sum(A[year(month) == 2022] + 
                                R[year(month) == 2022]) / 30,
            active_growth = (active_2023 - active_2022) / active_2022,
            rev_2023 = sum(revenue[year(month) == 2023]),
            rev_2022 = sum(revenue[year(month) == 2022]),
            rev_growth = (rev_2023 - rev_2022) / rev_2022,
            .by = tourism) |> 
  left_join(tourism_dwellings, by = "tourism") |> 
  mutate(active_pct = active_2023 / dwellings, .after = active_growth) |> 
  select(-active_2022, -rev_2022, -dwellings) |> 
  mutate(tourism_name = paste("All", tourism), .after = tourism) |> 
  bind_rows(tab_1)

tab_1 <- 
  monthly |> 
  filter(month(month) == 6, year(month) %in% c(2022, 2023)) |> 
  summarize(active_2023 = sum(A[year(month) == 2023] + 
                                R[year(month) == 2023]) / 30,
            active_2022 = sum(A[year(month) == 2022] + 
                                R[year(month) == 2022]) / 30,
            active_growth = (active_2023 - active_2022) / active_2022,
            rev_2023 = sum(revenue[year(month) == 2023]),
            rev_2022 = sum(revenue[year(month) == 2022]),
            rev_growth = (rev_2023 - rev_2022) / rev_2022) |> 
  mutate(tourism = "British Columbia", tourism_name = "British Columbia", 
         .before = active_2023) |> 
  mutate(active_pct = active_2023 / sum(CSD$dwellings_2021), 
         .after = active_growth) |> 
  select(-active_2022, -rev_2022) |> 
  bind_rows(tab_1)

tab_1 |> 
  arrange(tourism, tourism_name) |> 
  select(-tourism) |> 
  mutate(active_2023 = scales::comma(active_2023, 1),
         active_growth = scales::percent(active_growth, 0.1),
         active_pct = scales::percent(active_pct, 0.1),
         rev_2023 = scales::dollar(rev_2023, 0.1, scale = 1/1000000, 
                                   suffix = " million"),
         rev_growth = scales::percent(rev_growth, 0.1)) |> 
  gt::gt()


# Home sharers and commercial operators -----------------------------------

eh_pct_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01")) |> 
  summarize(active = sum(R + A), .by = listing_type) |> 
  mutate(pct = active / sum(active)) |> 
  slice(1) |> 
  pull(pct) |> 
  scales::percent(0.1)

eh_pct_2019 <- 
  monthly |> 
  filter(month == yearmonth("2019-06-01")) |> 
  summarize(active = sum(R + A), .by = listing_type) |> 
  mutate(pct = active / sum(active)) |> 
  slice(1) |> 
  pull(pct) |> 
  scales::percent(0.1)

revenue_colour <- rev(col_palette[c(5, 2, 1, 3, 4, 8, 6, 7)])

monthly_for_rev <- 
  monthly |>  
  filter(!is.na(host_ID), month == yearmonth("2023-06-01"), R > 0) |> 
  summarize(rev = sum(revenue), .by = c(tourism, host_ID))

host_deciles <-
  monthly_for_rev |> 
  group_by(tourism) |> 
  summarize(all = sum(rev),
            top_10 = sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_20 = sum(rev[rev > quantile(rev, c(0.80))] / all) - 
              sum(rev[rev > quantile(rev, c(0.90))] / all),
            top_30 = sum(rev[rev > quantile(rev, c(0.70))] / all) - 
              sum(rev[rev > quantile(rev, c(0.80))] / all),
            top_40 = sum(rev[rev > quantile(rev, c(0.60))] / all) - 
              sum(rev[rev > quantile(rev, c(0.70))] / all),
            top_50 = sum(rev[rev > quantile(rev, c(0.50))] / all) - 
              sum(rev[rev > quantile(rev, c(0.60))] / all),
            top_60 = sum(rev[rev > quantile(rev, c(0.40))] / all) - 
              sum(rev[rev > quantile(rev, c(0.50))] / all),
            top_70 = sum(rev[rev > quantile(rev, c(0.30))] / all) - 
              sum(rev[rev > quantile(rev, c(0.40))] / all),
            top_80 = sum(rev[rev > quantile(rev, c(0.20))] / all) - 
              sum(rev[rev > quantile(rev, c(0.30))] / all),
            top_90 = sum(rev[rev > quantile(rev, c(0.10))] / all) - 
              sum(rev[rev > quantile(rev, c(0.20))] / all),
            top_100 = sum(rev[rev > quantile(rev, c(0.00))] / all) - 
              sum(rev[rev > quantile(rev, c(0.10))] / all)) |> 
  select(-all) |> 
  pivot_longer(-tourism, names_to = "percentile", values_to = "value") |> 
  mutate(percentile = factor(percentile, levels = paste0("top_", 1:10 * 10))) |> 
  mutate(perfect_distribution = 0.1,
         decile = rep(1:10, 6),
         dummy_1 = perfect_distribution,
         dummy_2 = value) |> 
  rename("0" = perfect_distribution, "1" = value, "0.25" = dummy_1, 
         "0.75" = dummy_2) |> 
  pivot_longer(c("0","0.25", "0.75", "1"), names_to = "position") |> 
  mutate(position = as.numeric(position), 
         display_val = scales::percent(value, .1)) |> 
  group_by(tourism, position) |> 
  mutate(absolute_val = slider::slide_dbl(value, ~{.x[1] / 2 + sum(.x[-1])}, 
                                          .after = 9)) |> 
  ungroup() |> 
  mutate(
    display_val = paste0("earned ", display_val, "\nof revenue"),
    display_percentile = case_when(
      percentile == "top_10" ~ "Top 10% of hosts...",
      percentile == "top_20" ~ "Next 10% of hosts...",
      TRUE ~ NA_character_))

host_rev_data <- 
  monthly_for_rev |> 
  summarize(rev = sum(rev), .by = host_ID) |> 
  arrange(-rev) |> 
  summarize(
    pct_1 = quantile(rev, 0.99),
    pct_10 = quantile(rev, 0.9),
    pct_50 = quantile(rev, 0.5),
    n_1 = round(n() / 10),
    rev_1 = sum(rev[rev >= pct_1]) / sum(rev),
    rev_10 = sum(rev[rev >= pct_10]) / sum(rev),
  )

host_top_10_pct <- scales::percent(host_rev_data$rev_10, 0.1)
host_top_1_pct <- scales::percent(host_rev_data$rev_1, 0.1)
host_top_1_n <- scales::comma(host_rev_data$n_1, 10)
host_median <- scales::dollar(host_rev_data$pct_50, 100)

ml_active <- 
  monthly |> 
  filter(str_starts(property_ID, "ab-"), R + A > 0) |> 
  summarize(pct = mean(multi), .by = c(tourism, month))

ml_pct_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01"), 
         str_starts(property_ID, "ab-"), R + A > 0) |> 
  summarize(pct = mean(multi)) |> 
  pull(pct) |> 
  scales::percent(0.1)

ml_rev_pct_2023 <- 
  monthly |> 
  filter(month == yearmonth("2023-06-01"), 
         str_starts(property_ID, "ab-"), R + A > 0) |> 
  summarize(rev_pct = sum(revenue[multi]) / sum(revenue)) |> 
  pull(rev_pct) |> 
  scales::percent(0.1)


# Figure 3 ----------------------------------------------------------------

fig_3 <-
  host_deciles |> 
  select(-display_val, -display_percentile) |> 
  mutate_tourism() |> 
  ggplot(aes(position, value, group = decile, fill = decile)) +
  geom_area(colour = "white", lwd = 1.2) +
  facet_wrap(~ tourism, nrow = 2) +
  scale_y_continuous(name = "Host decile", label = scales::label_percent(1),
                     breaks = seq(0, 1, by = 0.1), #limits = c(0, 1),
                     sec.axis = sec_axis(~., 
                                         name = "% of total revenue",
                                         labels = derive(), 
                                         breaks = derive())) +
  scale_fill_gradientn(colours = revenue_colour) +
  theme_void() +
  theme(legend.position = "none",
        text = element_text(family = "Futura"),
        axis.text.y = element_text(hjust = 1),
        axis.title.y.left = element_text(
          angle = 90, margin = margin(0, 10, 0, 0)),
        axis.title.y.right = element_text(
          angle = 270, margin = margin(0, 0, 0, 10)),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_3.png", fig_3, width = 9, height = 5)


# Figure 4 ----------------------------------------------------------------

fig_4 <- 
  ml_active |> 
  mutate_tourism() |> 
  filter(month >= yearmonth("2017-07-01")) |> 
  ggplot() +
  geom_line(aes(month, pct, colour = tourism), lwd = 1) +
  facet_wrap(~tourism) +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  scale_y_continuous(name = NULL, labels = scales::percent) +
  scale_x_yearmonth(name = NULL) +
  theme_minimal() +
  theme(text = element_text(family = "Futura"),
        plot.background = element_rect(fill = "white", colour = "transparent"))

ggsave("output/figure_4.png", fig_4, width = 9, height = 5)


# Growth trends -----------------------------------------------------------

monthly_variation <-
  monthly |> 
  filter(str_starts(property_ID, "ab-")) |>
  summarize(active = sum(R + A), .by = c(tourism, month)) |> 
  arrange(tourism, month) |> 
  mutate(yoy = slide_dbl(active, ~{(.x[13] - .x[1]) / .x[1]}, .before = 12, 
                       .complete = FALSE), .by = tourism) |> 
  filter(!is.na(yoy), !is.infinite(yoy)) |> 
  filter(month >= yearmonth("2018-06-01"))

active_change_pct_2018 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A), .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2018] - active[year == 2017]) / 
              active[year == 2017]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2019 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A), .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2019] - active[year == 2018]) / 
              active[year == 2018]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2020 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A), .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2020] - active[year == 2019]) / 
              active[year == 2019]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2021 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A), .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2021] - active[year == 2020]) / 
              active[year == 2021]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2022 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A), .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2022] - active[year == 2021]) / 
              active[year == 2021]) |> 
  pull(pct) |> 
  scales::percent(0.1)

active_change_pct_2023 <- 
  monthly |> 
  filter(month(month) == 6) |>
  mutate(year = year(month)) |> 
  summarize(active = sum(R + A) / 30, .by = year) |> 
  arrange(year) |> 
  summarize(pct = (active[year == 2023] - active[year == 2022]) / 
              active[year == 2022]) |> 
  pull(pct) |> 
  scales::percent(0.1)


# Figure 5 ----------------------------------------------------------------

fig_5 <- 
  monthly_variation |> 
  mutate_tourism() |> 
  ggplot(aes(month, yoy, colour = tourism)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(lwd = 1, na.rm = TRUE) +
  scale_x_yearmonth(name = NULL) +
  scale_y_continuous(name = NULL, limits = c(-0.6, 0.6), oob = scales::squish,
                     labels = scales::percent) +
  scale_color_manual(values = col_palette[c(5, 6, 2, 7, 4, 8)], 
                     guide = "none") +
  facet_wrap(~tourism) +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor.x = element_blank(),
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_5.png", fig_5, width = 9, height = 5)
