vecHeight <- cityData$Dubai$building$height
tbl <- tibble::tibble(mean   = paste0(round(mean(vecHeight)), 'm'),
                      median = paste0(median(vecHeight), 'm'),
                      min    = paste0(min(vecHeight), 'm'),
                      max    = paste0(max(vecHeight), 'm'))

plotDensity <-
		ggplot() +
				geom_density(
						aes(x = vecHeight),
						#fill = "#f9d23c",
						col  = "#f9d23c",
						stat = "count"
				) +
				# geom_vline(xintercept = median(vecHeight),
				#            col = "red",
				#            alpha = .3,
				#            show.legend = TRUE) +
				ggplot2::annotate("text",
				                  x = median(vecHeight),
				                  y = 800,
				                  label = "---Median---",
				                  colour = "white",
				                  size = 1.4,
				                  angle = 90,
				                  alpha = 1) +
				labs(y = 'count', x = 'height') +
				scale_x_continuous(breaks = c(median(vecHeight), 30, 60, 90, 120, 300, 600, 827)) +
				hrbrthemes::theme_ipsum_rc() +
				theme(
						panel.background = element_blank(),
						plot.background = element_blank(),
						panel.grid.major = element_blank(),
						panel.grid.minor = element_blank(),
						axis.text.y = element_text(
								size = 7,
						),
						axis.text.x = element_text(
								size = 7,
						),
						text = element_text(
								size = 7,
								colour = 'white'
						)
				)

library(ggpp)
densityPlot <-
		plotDensity +
				ggpp::annotate(geom = "table",
				               x = 800,
				               y = 5000,
				               label = list(tbl),
				               color = 'white',
				               fill = "transparent",
				               size = 2
				)

ggsave(densityPlot, filename = "densityPlot.png",height = 2, width = 5)
densityPlot <- magick::image_read("densityPlot.png")
