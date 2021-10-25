pacman::p_load(conflicted, tidyverse, dplyr, sf, purrr, here, magrittr, patchwork, magick)
conflict_prefer("filter", "dplyr")
if (!require(victor)) {
	remotes::install_github("adam-gruer/victor")
	library(victor)
}

## IMPORT DATA VIA API ---
# dubai <-
# 		tribble(
# 				~city, ~lon, ~lat,
# 				"Dubai", 55.296249,25.276987
# 		) %>%
# 				purrr::transpose()
#
# names(dubai) <-
# 		purrr::map_chr(dubai, "city")
# cityData <- purrr::map(
# 		dubai,
# 		~ victor::spoils(.x$lon,
# 		                 .x$lat,
# 		                 zoom = 15,
# 		                 nrow = 25,
# 		                 ncol = 14
# 		)
# )
# saveRDS(cityData, 'cityData.RDS')

## LOADING DATA ----
cityData <- readRDS('cityData.RDS')

buildings <- cityData$Dubai$building %>%
		filter(height > 0) %>%
		mutate(height_fct = cut(height, breaks = 5, dig.lab = 3))

mapBuildings <-
		ggplot() +
		geom_sf(data = filter(buildings, height <= 30), fill = "#798992", alpha = .9, colour = NA) +
		geom_sf(data = filter(buildings, height > 30) %>%
				mutate(height_fct = cut(height, breaks = 5)),
		        aes(fill = height_fct), colour = NA
		) +
		guides(fill = guide_legend(title = NULL, override.aes = list(size = 1, alpha = 1))) +
		ggthemes::scale_fill_tableau(palette = "Jewel Bright", direction = -1) +
		theme_void() +
		theme(
				panel.background = element_rect(colour = NA, fill = "#0a0a0a"),
				plot.background = element_rect(colour = NA, fill = "#0a0a0a"),
				plot.margin = margin(0, 2, 0, 2),
				legend.key.size = unit(.2, 'cm'),
				legend.key.height = unit(.2, 'cm'), #change legend key height
				legend.key.width = unit(1, 'cm'), #change legend key width
				legend.position = "bottom" ,#c(0.01, 0.02),
				legend.background = element_blank(),
				legend.key = element_blank(),
				legend.text = element_text(colour = "white"),
				legend.margin = margin(-15,0,3,0),
				legend.box.background = element_rect(color = NA, fill = NA),
				legend.direction = "horizontal",
				legend.justification = c(0, 0)
		)

ggsave(plot = mapBuildings, here("dubai_buildings.png"), width = 8, height = 10, dpi = 300)

source('plot.R')
main_image <- magick::image_read("dubai_buildings.png")
plots <- magick::image_crop(main_image, "2200x3000+50+0")
legends <- magick::image_crop(main_image,geometry = "2000x245+0-200" , gravity = "northwest")


magick::image_append(c(plots, legends), stack = TRUE) %>%
		magick::image_annotate(
				text = "Dubai",
				weight = 900,
				gravity = "northwest",
				color = "#d9c09e",
				size = 120,
				location = "+20+20"
		) %>%
		magick::image_annotate(
				text = "City Building Height Distribution (> 30m)",
				gravity = "northwest",
				color = "#d9c09e",
				size = 45,
				location = "+20+150"
		) %>%
		magick::image_annotate(
				text = "Visualization By: Youness Bahi",
				gravity = "northeast",
				color = "#d3dadc",
				size = 35,
				location = "+20+150"
		) %>%
		magick::image_composite(
				composite_image = densityPlot,
				offset = "+0+300"
		) %>%
		image_write(here("Dubai_buildings_height.png"))











