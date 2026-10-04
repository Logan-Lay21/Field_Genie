library(rgee)
library(tidyverse)
library(mapedit)
library(sf)
library(geojsonio)

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

.s2_clean$aggregate_array("CLOUDY_PIXEL_PERCENTAGE")$getInfo()

raw <- unlist(ts$aggregate_array("system:time_start")$getInfo())
head(raw)


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
