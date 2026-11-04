library(sf)
library(tidyverse)

path <- "/Users/loganlay/Field_Genie/data/2019 Yield data"

st_layers(path)

erikson <- st_read(path,
  layer = "Erikson 2019 HWSW Yield data")

erikson |> 
  st_drop_geometry() |> 
  glimpse()

names(erikson)

lims <- quantile(erikson$Yld_Vol_Dr, c(0.01, 0.99), na.rm = TRUE)

ggplot(erikson) +
  geom_sf(aes(color = Yld_Vol_Dr), size = 0.4) +
  scale_color_viridis_c(limits = lims, oob = scales::squish, name = "Yield (bu/ac)") +
  theme_minimal() +
  labs(title = "2019 Erikson HWSW yield monitor points")
