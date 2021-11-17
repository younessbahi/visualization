pacman::p_load(tidyverse, ggplot2, afrilearndata, ggtext, sysfonts)
showtext::showtext_auto()
sysfonts::font.add.google("Baloo 2")
font3 = 'Baloo 2'

afripop_df <-
  afripop2020 %>%
  as.data.frame(xy = TRUE) %>%
  rename(pop = 3) %>%
  filter(! is.na(pop)) %>%
  mutate(
    pop2 = case_when(
      pop <= 500 ~ pop,
      TRUE ~ 600
    )
  )

df_ <-
  data.frame(
    label        = c("<b>Africa</b>", "*Population density*", "<b>2020<b>"),
    x            = c(- 3.4, - 6.2, 5),
    y            = c(- 1, - 1.6, - .7),
    hjust        = c(.6, .5, - .9),
    vjust        = c(1.6, 4, - 0.07),
    angle        = c(0, 0, - 90),
    color        = c("#d3dadc", "#ffa72f", "#ff2223"),
    fill         = c(NA, NA, NA),
    label.colour = c(NA, NA, NA),
    size         = c(60, 20, 20)
  )


map_ <-
  ggplot(afripop_df) +
  geom_tile(
    aes(x, y, fill = pop2)
  ) +
  ggtext::geom_richtext(
    data = df_, aes(x, y),
    family = font3,
    label = df_$label,
    angle = df_$angle,
    color = df_$color,
    fill  = df_$fill,
    label.colour = df_$label.colour,
    size = df_$size,
    hjust = df_$hjust,
    vjust = df_$vjust
  ) +
  scale_fill_gradientn(
    colors = c("#36393c", "#ffa91e", "#fc4127", "#9d241c"),
    breaks = seq(0, 600, 100),
    labels = c(seq(0, 500, 100), ">600")
  ) +
  guides(
    fill = guide_colorbar(
      title          = "Population Concentration\n (per/km²)",
      label.position = "bottom",
      title.hjust    = .5
    )
  ) +
  labs(caption = "<b>Data: afrilearndata</b> ■ <i>by: Youness Bahi</i>") +
  coord_fixed() +
  theme_void(base_family = font3, base_size = 45) +
  theme(
    legend.position   = "bottom",
    legend.direction  = "horizontal",
    legend.key.width  = unit(3, "line"),
    legend.key.height = unit(.1, "line"),
    legend.title      = element_text(
      size       = 38,
      margin     = margin(- 5, 0, 10, - 20),
      color      = "grey85",
      lineheight = .5,
      family     = font3
    ),
    legend.text       = element_text(color = "grey85"),
    plot.background   = element_rect(fill = "#36393c", color = NA),
    plot.margin       = margin(10, 10, 10, 10),
    plot.caption      = element_markdown(color = "#d3dadc", family = font3),
    text              = element_text()
  )

ggsave(
  plot = map_,
  filename = "africa-pop-density-2020.png",
  device = 'png',
  height = 8,
  width = 7,
  dpi = 300
)
