library(rgee)
library(tidyverse)
library(mapedit)
library(sf)
library(geojsonio)
library(purrr)
library(httr2)

library(mapview)
mapviewOptions(basemaps = c("Esri.WorldImagery", "OpenStreetMap"))

ee$Initialize(project = Sys.getenv("EE_PROJECT"))

#==========================================Lessons with Claude=======================================#

#=====Lesson 1========#

pt <- ee$Geometry$Point(c(-111.79, 43.82)) # gathers data for the square that this point is in

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$ # grabs the images from the copernicus satellite
  filterBounds(pt)$                                      # filters down to the square contianing this point
  filterDate("2025-06-01", "2025-07-01")                 # filters down to the images collected in this time range

print(s2)
s2$size()$getInfo()


dates <- s2$aggregate_array("system:time_start")$getInfo()
as.POSIXct(unlist(dates) / 1000, origin = "1970-01-01", tz = "America/Boise")

s2$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo() # shows the cloud coverage percentage in each image in the collection

ee_get_date_ic(s2) # Properly checks the dat on the images


#======Lesson 2=======#

farm <- ee$Geometry$Point(c(-111.882630, 43.828436))$buffer(500)

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(farm)$
  filterDate("2025-04-01", "2025-12-30")

s2$size()$getInfo()

cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")

mask_clouds <- function(img) {
  img$updateMask(img$select("cs_cdf")$gte(0.6)) # keep pixels >= 0.6 "clear"
}

add_ndvi <- function(img) {
  img$addBands(img$normalizedDifference(c("B8", "B4"))$rename("NDVI"))
}

s2_clean <- s2$
  linkCollection(cs_plus, list("cs_cdf"))$
  map(mask_clouds)$
  map(add_ndvi)

s2_clean$size()$getInfo()


field_mean <- function(img) {
  m <- img$select("NDVI")$reduceRegion(
    reducer = ee$Reducer$mean(), geometry = farm, scale = 10)
  img$set("ndvi", m$get("NDVI"))
}

add_date <- function(img) {
  d <- ee$Date(img$get("system:time_start"))$format("YYYY-MM-dd")
  img$set("new_date", d)
}

ts <- s2_clean$map(field_mean)$filter(ee$Filter$notNull(list("ndvi")))$map(add_date)


curve <- tibble(
  date = as.Date(unlist(ts$aggregate_array("new_date")$getInfo())),
  ndvi = unlist(ts$aggregate_array("ndvi")$getInfo())
)


plotly::ggplotly(
ggplot(curve, aes(date,ndvi)) +
  geom_point() +
  geom_line()
)

s2_clean$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo()

raw <- unlist(ts$aggregate_array("system:time_start")$getInfo())
head(raw)


#==================Whats in the field========================#

cdl <- ee$ImageCollection("USDA/NASS/CDL")$
  filterDate("2025-01-01", "2025-12-31")$
  first()$
  select("cropland")

crops <- cdl$reduceRegion(
  reducer = ee$Reducer$frequencyHistogram(),
  geometry = farm,
  scale = 30
)

raw <- unlist(crops$getInfo())
raw

crop_tbl <- enframe(raw, name="code", value="pixels") |> 
  mutate(code = as.integer(str_remove(code, "cropland\\.")))

cdl_lookup <- tribble(
  ~code, ~crop,
  1,   "Corn",
  21,  "Barley",
  23,  "Spring Wheat",
  24,  "Winter Wheat",
  28,  "Oats",
  30,  "Speltz",
  36,  "Alfalfa",
  37,  "Other Hay",
  43,  "Potatoes",
  61,  "Fallow/Idle",
  111, "Open Water",
  121, "Developed/Open Space",
  122, "Developed/Low Intensity",
  123, "Developed/Med Intensity",
  124, "Developed/High Intensity",
  152, "Shrubland",
  176, "Grass/Pasture",
  195, "Herbaceous Wetlands"
)

crop_tbl |> 
  group_by() |> 
  summarise(sum(pixels))

crop_tbl <- crop_tbl |> 
  left_join(cdl_lookup, by="code") |> 
  mutate(pct = pixels / 864 * 100)

ggplot(
  crop_tbl, aes(x=crop, y=pct)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  )


#==============================All again but with exact shapes====================#
drawn <- editMap()
field <- sf_as_ee(sf::st_geometry(drawn))

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(field)$
  filterDate("2025-04-01", "2025-12-30")

s2$size()$getInfo()

cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED")

mask_clouds <- function(img) {
  img$updateMask(img$select("cs_cdf")$gte(0.6)) # keep pixels >= 0.6 "clear"
}

add_ndvi <- function(img) {
  img$addBands(img$normalizedDifference(c("B8", "B4"))$rename("NDVI"))
}

s2_clean <- s2$
  linkCollection(cs_plus, list("cs_cdf"))$
  map(mask_clouds)$
  map(add_ndvi)

s2_clean$size()$getInfo()


field_mean <- function(img) {
  m <- img$select("NDVI")$reduceRegion(
    reducer = ee$Reducer$mean(), geometry = field, scale = 10)
  img$set("ndvi", m$get("NDVI"))
}

add_date <- function(img) {
  d <- ee$Date(img$get("system:time_start"))$format("YYYY-MM-dd")
  img$set("new_date", d)
}

start <- ee$Date("2025-04-01")
n_weeks <- 34

weekly <- ee$ImageCollection(
  ee$List$sequence(0, n_weeks - 1)$map(ee_utils_pyfunc(function(i) {
    w0 <- start$advance(ee$Number(i), "week")
    wk <- s2_clean$filterDate(w0, w0$advance(1, "week"))
    wk$select("NDVI")$median()$
      set("system:time_start", w0$millis())$
      set("n_imgs", wk$size())
  }))
)

weekly <- weekly$filter(ee$Filter$gt("n_imgs", 0))

ts <- weekly$map(field_mean)$filter(ee$Filter$notNull(list("ndvi")))$map(add_date)

curve <- tibble(
  date = as.Date(unlist(ts$aggregate_array("new_date")$getInfo())),
  ndvi = unlist(ts$aggregate_array("ndvi")$getInfo())
) |>
  mutate(week = as.integer(difftime(date, as.Date("2025-04-01"), units = "weeks")))

setdiff(0:33, curve$week)

View(curve)

plotly::ggplotly(
ggplot(curve, aes(date,ndvi)) +
  geom_point() +
  geom_line()
)

s2_clean$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo()

raw <- unlist(ts$aggregate_array("system:time_start")$getInfo())
head(raw)

#=======================Streamlined Pipeline================================#

cs_plus <- ee$ImageCollection("GOOGLE/CLOUD_SCORE_PLUS/V1/S2_HARMONIZED") 

mask_clouds <- function(img, ct) {
  img$updateMask(img$select("cs_cdf")$gte(ct)) # uses var ct (cloud thresh) to measure the limit of % of cloud cover
}

add_ndvi <- function(img) {
  img$addBands(img$normalizedDifference(c("B8", "B4"))$rename("NDVI"))
}

calc_field_mean <- function(img, geometry, scale) {
  m <- img$select("NDVI")$reduceRegion(
    reducer = ee$Reducer$mean(), geometry = geometry, scale = scale)
  img$set("ndvi", m$get("NDVI"))
}

add_date <- function(img) {
  d <- ee$Date(img$get("system:time_start"))$format("YYYY-MM-dd")
  img$set("new_date", d)
}

# Planning what to include and which
# previous year to pull from for the next

grain_mask <- function(cdl_year){
  cdl <- ee$ImageCollection("USDA/NASS/CDL")$
    filterDate(paste0(cdl_year, "-01-01"), paste0(cdl_year, "-12-31"))$
    first()$select("cropland")

  cdl$eq(23)$Or(cdl$eq(21))
}

drawn <- editMap()
field <- sf_as_ee(sf::st_geometry(drawn))

sf::st_write(drawn, "test_field.geojson") # Now you can just run field and it all works, no need to redraw.

field_curve <- function(geometry, start, n_weeks = 34, cloud_thresh = 0.6, scale = 10, cdl_year = NULL) {
  start_d <- ee$Date(start)
  end_d <- start_d$advance(n_weeks, "week")
  
  s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
    filterBounds(geometry)$
    filterDate(start_d, end_d)

  s2_clean <- s2$
    linkCollection(cs_plus, list("cs_cdf"))$
    map(function(img) mask_clouds(img,cloud_thresh))$
    map(add_ndvi)

  if (!is.null(cdl_year)) {
    grain <- grain_mask(cdl_year)

    s2_clean <- s2_clean$
      map(function(img) img$updateMask(grain))
  }

  weekly <- ee$ImageCollection(
    ee$List$sequence(0, n_weeks - 1)$map(ee_utils_pyfunc(function(i) {
      w0 <- start_d$advance(ee$Number(i), "week")
      wk <- s2_clean$filterDate(w0, w0$advance(1, "week"))
      wk$select("NDVI")$median()$
        set("system:time_start", w0$millis())$
        set("n_imgs", wk$size())
    }))
  )

  weekly <- weekly$filter(ee$Filter$gt("n_imgs", 0))

  with_mean <- weekly$
    map(function(img) calc_field_mean(img, geometry, scale))

  ts <- with_mean$
    filter(ee$Filter$notNull(list("ndvi")))$
    map(add_date)

  tibble(
    date = as.Date(unlist(ts$aggregate_array("new_date")$getInfo())),
    ndvi = unlist(ts$aggregate_array("ndvi")$getInfo())
    ) |>
  mutate(
    week = as.integer(difftime(date, as.Date(start), units = "weeks")),
    year = as.integer(format(as.Date(start), "%Y"))
  )
}

# Testing pipeline below

test <- field_curve(field, "2025-04-01")
nrow(test)

ggplot(
  test, aes(week, ndvi)
) +
  geom_line() +
  geom_point()


# Works!!!

# Checking if data goes back to 2017

field_curve(field, "2017-04-01") |> nrow()

c17 <- field_curve(field, "2017-04-01")
setdiff(0.33, c17$week)

# Looking at a county

counties <- ee$FeatureCollection("TIGER/2018/Counties")

madison <- counties$
  filter(ee$Filter$eq("STATEFP", "16"))$
  filter(ee$Filter$eq("NAME", "Madison"))

madison$size()$getInfo()

madison_geom <- madison$geometry()

county_2025 <- field_curve(madison_geom, "2025-04-01", scale = 250)

county_2025_masked <- field_curve(madison_geom, "2025-04-01", scale = 250, cdl_year = 2025)


ggplot(
  county_2025, aes(x = week, y = ndvi)
) +
  geom_line() +
  geom_point()

# single field vs county

all3 <- bind_rows(
  test |> mutate(source = "Test field"),
  county_2025 |> mutate(source = "County, unmasked"),
  county_2025_masked |> mutate(source = "County, grain only")
)

all3 <- read_csv("data/curves_2025_test.csv")

ggplot(all3, aes(week, ndvi, color = source)
  )  +
  geom_line() +
  geom_point()

# caching results so I dont have to do this all again


# write_csv(all3, "data/curves_2025_test.csv")

# ggsave("county_vs_field_2025.png")

## LOGAN!! Heres what you fina do Nov 5:
# Run masked county curve for 2020 to 2023, using each years own CDL (the prev year)
# And cache each one like above!

# Calculating multiple years at once

years <- 2020:2025

curves <- map(years, \(y) field_curve(field, paste0(y, "-04-01"))) |>
  list_rbind()

ggplot(curves, aes(week, ndvi, color = factor(year), group = year)) +
  geom_line() +
  geom_point()


counties <- ee$FeatureCollection("TIGER/2018/Counties")
madison <- counties$
  filter(ee$Filter$eq("STATEFP", "16"))$
  filter(ee$Filter$eq("NAME", "Madison"))
madison$size()$getInfo()
madison_geom <- madison$geometry()
# county_2025 <- field_curve(madison_geom, "2025-04-01", scale = 250)
# county_2025_masked <- field_curve(madison_geom, "2025-04-01", scale = 250, cdl_year = 2025)

county_years <- 2020:2025

county_curves <- map(county_years, \(y) field_curve(madison_geom, paste0(y, "-04-01"), scale=250, cdl_year = y-1)) |> 
  list_rbind()

county_test <- field_curve(madison_geom, "2025-04-01", scale = 250, cdl_year = 2024)

View(county_test)

ggplot(county_curves, aes(week, ndvi, color = factor(year), group = year)) +
  geom_line() +
  geom_point()


View(county_curves)

smooth_county_curves <- county_curves |> 
  group_by(year) |> 
  arrange(week) |> 
  mutate(
    smooth_ndvi = case_when(
      is.na(lag(ndvi)) ~ ndvi,
      is.na(lead(ndvi)) ~ ndvi,
      .default = (lag(ndvi) + ndvi + lead(ndvi)) - pmax(lag(ndvi), ndvi, lead(ndvi)) - pmin(lag(ndvi), ndvi, lead(ndvi))
    )
  ) |> 
  ungroup() |> 
  arrange(year, week)

?case_when()

View(smooth_county_curves)

write_csv(smooth_county_curves, "data/smooth_madison_county_curves_2020_2025.csv")

# ggsave("county_curves_2020_2025.png")

# Soooo cool! 2020 - 2025 there are some sweet patterns!

# Doing a crop match based on pixels

cdl_lookup <- tribble(
  ~code, ~crop,
  1,   "Corn",
  21,  "Barley",
  23,  "Spring Wheat",
  24,  "Winter Wheat",
  28,  "Oats",
  30,  "Speltz",
  36,  "Alfalfa",
  37,  "Other Hay",
  43,  "Potatoes",
  61,  "Fallow/Idle",
  111, "Open Water",
  121, "Developed/Open Space",
  122, "Developed/Low Intensity",
  123, "Developed/Med Intensity",
  124, "Developed/High Intensity",
  152, "Shrubland",
  176, "Grass/Pasture",
  195, "Herbaceous Wetlands"
)

field_crops <- function(geometry, year) {
  cdl <- ee$ImageCollection("USDA/NASS/CDL")$
    filterDate(paste0(year, "-01-01"), paste0(year, "-12-31"))$
    first()$select("cropland")

  raw <- cdl$reduceRegion(
    reducer = ee$Reducer$frequencyHistogram(),
    geometry = geometry, scale = 30
  )$getInfo() |> unlist()

  enframe(raw, name = "code", value = "pixels") |>
    mutate(code = as.integer(str_remove(code, "cropland\\.")),
           year = year) |>
    left_join(cdl_lookup, by = "code") |>
    mutate(pct = pixels / sum(pixels) * 100)
}

field_crop_details <- map(2020:2025, \(y) field_crops(field, y)) |> list_rbind() |>
  arrange(year, desc(pct))

View(field_crop_details)


#==================Whats in the field========================#

cdl <- ee$ImageCollection("USDA/NASS/CDL")$
  filterDate("2025-01-01", "2025-12-31")$
  first()$
  select("cropland")

crops <- cdl$reduceRegion(
  reducer = ee$Reducer$frequencyHistogram(),
  geometry = field,
  scale = 30
)

raw <- unlist(crops$getInfo())
raw

crop_tbl <- enframe(raw, name="code", value="pixels") |> 
  mutate(code = as.integer(str_remove(code, "cropland\\.")))

cdl_lookup <- tribble(
  ~code, ~crop,
  1,   "Corn",
  21,  "Barley",
  23,  "Spring Wheat",
  24,  "Winter Wheat",
  28,  "Oats",
  30,  "Speltz",
  36,  "Alfalfa",
  37,  "Other Hay",
  43,  "Potatoes",
  61,  "Fallow/Idle",
  111, "Open Water",
  121, "Developed/Open Space",
  122, "Developed/Low Intensity",
  123, "Developed/Med Intensity",
  124, "Developed/High Intensity",
  152, "Shrubland",
  176, "Grass/Pasture",
  195, "Herbaceous Wetlands"
)

crop_tbl |> 
  group_by() |> 
  summarise(sum(pixels))

crop_tbl <- crop_tbl |> 
  left_join(cdl_lookup, by="code") |> 
  mutate(pct = pixels / 171 * 100)

ggplot(
  crop_tbl, aes(x=crop, y=pct)
) +
  geom_col() +
  theme(
    axis.text.x = element_text(angle=45, hjust=1)
  )


#==================Using the NASS Quick Stats API========================

resp <- request("https://quickstats.nass.usda.gov/api/api_GET/") |>
  req_url_query(
    key = Sys.getenv("NASS_KEY"),
    source_desc = "SURVEY",
    commodity_desc = "WHEAT",
    class_desc = "SPRING, (EXCL DURUM)",
    statisticcat_desc = "YIELD",
    agg_level_desc = "COUNTY",
    state_alpha = "ID",
    county_name = "MADISON",
    year__GE = 2017,
    format = "JSON"
  ) |> 
  req_error(is_error = \(r) FALSE) |>
  req_perform()

yields <- resp |> resp_body_json() |> pluck("data") |> bind_rows()

View(yields)
