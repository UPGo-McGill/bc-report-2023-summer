#### Chapter 0 #################################################################

source("R/01_startup.R")
CD <- qread("output/data/CD.qs")


# Figure 1 ----------------------------------------------------------------

fig_1 <- 
  CD |>
  group_by(tourism) |> 
  summarize(.groups = "drop") |> 
  mutate_tourism() |> 
  ggplot(aes(fill = tourism)) +
  geom_sf(colour = "transparent") +
  ggrepel::geom_label_repel(aes(label = tourism, geometry = geometry), stat = 
                              "sf_coordinates", family = "Futura", size = 3) +
  scale_fill_manual(name = NULL, values = col_palette[c(2, 4, 5, 6, 7, 8)]) +
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "white", colour = "transparent"),
        text = element_text(family = "Futura"))

ggsave("output/figure_1.png", fig_1, width = 9, height = 5)

