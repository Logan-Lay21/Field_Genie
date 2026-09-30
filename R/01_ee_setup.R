library(rgee)
ee$Initialize(project = Sys.getenv("EE_PROJECT"))



#==========================================Lessons with Claude=======================================#

pt <- ee$Geometry$Point(c(-111.79, 43.82))

s2 <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterBounds(pt)$
  filterDate("2025-06-01", "2025-07-01")

print(s2)
s2$size()$getInfo()


dates <- s2$aggregate_array("system:time_start")$getInfo()
as.POSIXct(unlist(dates) / 1000, origin = "1970-01-01", tz = )
