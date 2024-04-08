# 1. LIBRARIES

install.packages("pacman")
pacman::p_load(
    geodata,
    sf,
    elevatr,
    terra,
    tidyverse,
    rayshader,
    scales
)

# 2. OSM RAILWAY DATA

main_path <- getwd()
macedonia_dir <- "macedonia_osm"
dir.create(macedonia_dir)
out_dir_macedonia <- paste0(
    main_path,
    "/", macedonia_dir
)
setwd(out_dir_macedonia)
getwd()

# download

options(timeout = 999)
url <- "https://download.geofabrik.de/europe/macedonia-latest-free.shp.zip"
destfile <- basename(url)

download.file(
    url = url,
    destfile = destfile,
    mode = "wb"
)

# unzip

zip_file <- list.files()
zip_name <- grep(
    "railways",
    unzip(
        destfile,
        list = TRUE
    )$Name,
    ignore.case = TRUE,
    value = TRUE
)

unzip(
    destfile,
    files = zip_name,
    exdir = out_dir_macedonia,
    overwrite = TRUE
)

list.files()

# load railways

rail_sf <- sf::st_read(
    "gis_osm_railways_free_1.shp"
)

# 3. COUNTRY BOUNDARIES

country_sf <- geodata::gadm(
    country = "MKD",
    level = 0,
    path = getwd()
) |>
sf::st_as_sf()

# 4. DIGITAL ELEVATION MODEL

elev <- elevatr::get_elev_raster(
    locations = country_sf,
    z = 8,
    clip = "locations"
)

crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"

elev_lambert <- elev |>
    terra::rast() |>
    terra::project(crs)

elmat <- elev_lambert |>
    rayshader::raster_to_matrix()

# 5. SIMPLIFY RAILS

country_rail <- rail_sf |>
    dplyr::filter(
        fclass %in% c(
            "rail",
            "narrow_gauge"
        )
    ) |>
    sf::st_intersection(
        country_sf
    ) |>
    sf::st_simplify(
        preserveTopology = TRUE,
        dTolerance = 1000
    ) |>
    sf::st_transform(crs = crs)

# 6. RENDER SCENE
#----------------

cols <- hcl.colors(
    n = 4,
    palette = "Blues 3",
    rev = FALSE
)

scales::show_col(
    cols, ncol = 4,
    labels = TRUE
)

elmat |>
    rayshader::height_shade(
        texture = colorRampPalette(
            cols[1:3]
        )(128)
    ) |>
    rayshader::add_overlay(
        rayshader::generate_line_overlay(
            geometry = country_rail,
            extent = elev_lambert,
            heightmap = elmat,
            color = "#F59F07",
            linewidth = 5
        ),
        alphalayer = 1
    ) |>
    rayshader::plot_3d(
        elmat,
        zscale = 15,
        solid = FALSE,
        shadow = TRUE,
        shadow_darkness = 1,
        background = "white",
        windowsize = c(800, 600),
        zoom = .55,
        phi = 89,
        theta = 0
    )

# 7. RENDER OBJECT
#-----------------

u <- "https://dl.polyhaven.org/file/ph-assets/HDRIs/hdr/4k/photo_studio_loft_hall_4k.hdr"
hdri_file <- basename(u)

download.file(
    url = u,
    destfile = hdri_file,
    mode = "wb"
)

img_name = "3d-railway-north-macedonia.png"

rayshader::render_highquality(
    filename = img_name,
    preview = TRUE,
    interactive = FALSE,
    light = TRUE,
    environment_light = hdri_file,
    intensity_env = .6,
    parallel = TRUE,
    line_radius = 3,
    width = 800 * 5,
    height = 600 * 5
)

