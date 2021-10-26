pacman::p_load(conflicted, tidyverse, dplyr, sf, purrr, here, magrittr, patchwork, magick)
conflict_prefer("filter", "dplyr")
if (!require(victor)) {
	remotes::install_github("adam-gruer/victor")
	library(victor)
}


## LOADING DATA ----
citiesData <- readRDS('citiesData.RDS')

buildings <- map(citiesData, "building")


mapBuildings <-
             imap(
		             buildings,
		             ~ ggplot() +
		             geom_sf(data = filter(.x, height <= 30), fill = "#798992", alpha = .9, colour = NA) +
				             geom_sf(data = filter(.x, height > 30) %>%
											mutate(height_fct = cut(height, breaks = 5)),
									        aes(fill = height_fct), colour = NA
									) +
				             ggplot2::annotate("text", -Inf, Inf,
				                               label = toupper(.y),
				                               hjust = 0,
				                               vjust = 1,
				                               colour = "white",
				                               alpha = 0.6,
				                               size = 7
				             ) +
									guides(fill = guide_legend(title = NULL, override.aes = list(size = 1, alpha = 1))) +
									ggthemes::scale_fill_tableau(palette = "Jewel Bright", direction = -1) +
									theme_void() +
									theme(
											panel.background = element_rect(colour = NA, fill = "#203e4a"),
											plot.background = element_rect(colour = NA, fill = "#203e4a"),
											plot.margin = margin(0, 0, 3, 0),
											legend.key.size = unit(.1, 'cm'),
											legend.key.height = unit(.2, 'cm'), #change legend key height
											legend.key.width = unit(.9, 'cm'), #change legend key width
											legend.position = c(0.01, -0.01),
											legend.background = element_blank(),
											legend.key = element_blank(),
											legend.text = element_text(colour = "white"),
											legend.margin = margin(-15,0,3,0),
											legend.box.background = element_rect(color = NA, fill = NA),
											legend.direction = "horizontal",
											legend.justification = c(0, 0)
									)
             )
wrap_plots(maps)

ggsave("citiesBuildings.png", height = 5, width = 10, dpi = 300)
main_image <- magick::image_read("citiesBuildings.png")
main_image %>%
		magick::image_annotate(
				text = "Building Height Distribution",
				weight = 900,
				gravity = "northwest",
				color = "white",
				size = 80,
				location = "+20+20"
		) %>%
		magick::image_annotate(
				text = "Visualization By: Youness Bahi",
				gravity = "southeast",
				color = "white",
				size = 40,
				location = "+42+0"
		) %>%
		image_write(here("Dubai_buildings_height.png"), quality = 100)