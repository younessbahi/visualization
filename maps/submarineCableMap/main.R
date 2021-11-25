## ----This project may take several hours to render.
## ----Make sure you have the latest version of rayrender:
# options(
#   repos = c(
#     tylermorganwall = 'https://tylermorganwall.r-universe.dev',
#     CRAN = 'https://cloud.r-project.org'
#   )
# )

pacman::p_load(geojsonsf, sf, rayrender, dplyr, magick)

#Data source: https://github.com/telegeography/www.submarinecablemap.com
cables = geojsonsf::geojson_sf("cable-geo.json")

cablescene = list()
counter = 1
for (i in seq_along(cables$geometry)) {
  for (j in seq_along(cables$geometry[[i]])) {
    temp     = cables$geometry[[i]][[j]]
    cableval = data.frame(x = sinpi(temp[, 1] / 180) * cospi(temp[, 2] / 180),
                          y = sinpi(temp[, 2] / 180),
                          z = cospi(temp[, 1] / 180) * cospi(temp[, 2] / 180))
    #Don't lower start of line at the 180/0 longitude border
    if (abs(temp[1, 1] - 180) > 0.001 && abs(temp[1, 1] + 180) > 0.001) {
      cableval[1,] = cableval[1,] * 1 / 1.02
    }
    nr = nrow(temp)
    #Don't lower end of line at the 180/0 longitude border
    if (abs(temp[nr, 1] - 180) > 0.001 && abs(temp[nr, 1] + 180) > 0.001) {
      nr            = nrow(cableval)
      cableval[nr,] = cableval[nr,] * 1 / 1.02
    }
    cablescene[[counter]] = path(cableval, width = 0.005, material = rayrender::diffuse(color = cables$color[i]))
    counter               = counter + 1
  }
}
fullcablescene = do.call(rbind,cablescene)

dir.create("img")

for (i in seq(1, 720, by = 1)) {
  rayrender::group_objects(
    fullcablescene,
    scale = c(1, 1, 1) * 1.02
  ) %>%
    rayrender::add_object(
      rayrender::sphere(
        radius = 0.99,
        material = rayrender::diffuse(
          image_texture = "8k_earth_daymap.jpg"
        ),
        angle = c(0, - 90, 0)
      )
    ) %>%
    rayrender::group_objects(
      angle = c(0, - i / 2, 0)
    ) %>%
    rayrender::add_object(
      rayrender::sphere(
        y = 5,
        z = 5,
        x = 5,
        material = rayrender::light(
          intensity = 80,
          color = "lightblue"
        )
      )
    ) %>%
    rayrender::add_object(
      rayrender::sphere(
        y = 5,
        z = 5,
        x = - 5,
        material = light(
          intensity = 10,
          color = "orange"
        )
      )
    ) %>%
    rayrender::add_object(
      rayrender::sphere(
        y = - 10,
        material = light(
          intensity = 3,
          color = "white"
        )
      )
    ) %>%
    rayrender::render_scene(
      samples = 64,
      width = 1200,
      height = 1200,
      fov = 0,
      aperture = 0,
      ortho_dimensions = c(2.3, 2.3),
      sample_method = "sobol_blue",
      filename = sprintf("img/smallcables%d.png", i)
    )
}

imgs <- list.files("img", pattern = ".png$", full.names = TRUE)
order <- formatC(as.integer(stringr::str_extract(imgs, "[0-9]+")), width = 3)
imgList <- cbind(order, imgs) %>%
  data.frame %>%
  arrange(desc(order))

imgList$imgs %>%
  magick::image_read() %>% # reads each path file
  magick::image_join() %>% # joins image
  magick::image_animate(fps = 10) %>% # animates, can opt for number of loops
  magick::image_write("FileName.gif") # write to current dir
