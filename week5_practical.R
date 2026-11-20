library(sf)
library(tidyverse)

# OSM data

OSM <- st_read("D:/uss/GIS/week5/greater-london-261116-free.shp/gis_osm_pois_free_1.shp")%>%
  st_transform(., 27700) %>%
  #select hotels only
  filter(fclass == 'hotel')

# Airbnb data
Airbnb <- read_csv("listings.csv") %>%
  # longitude is considered x value here, latitude is y
  st_as_sf(., coords = c("longitude", "latitude"), 
           crs = 4326) %>%
  st_transform(., 27700)%>%
  #select entire places that are available all year
  filter(room_type == 'Entire home/apt' & availability_365 =='365')


#London Borough data is already in 277000
Londonborough <- st_read("statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp")%>%
  st_transform(., 27700)

# load world cities
Worldcities <- st_read("World_Cities/World_Cities.shp") %>%
  st_transform(., 27700)

# filter based on key cities
Worldcities2 <- Worldcities %>%
  filter(CNTRY_NAME=='United Kingdom'&
           Worldcities$CITY_NAME=='Birmingham'|
           Worldcities$CITY_NAME=='London'|
           Worldcities$CITY_NAME=='Edinburgh')

# load UK outline
UK_outline <- st_read("gadm41_GBR_shp/gadm41_GBR_0.shp")%>%
  st_transform(., 27700)

Hotels <-  st_join(Londonborough, OSM)

Airbnbs <-  st_join(Londonborough, Airbnb)

head(Hotels)


Hotels_sum <- Hotels %>%
  # we need to list the columns we want to keep in the summarise
  group_by(., GSS_CODE, NAME)%>%
  # for each group count the number of rows and store in a column called accomodation count.
  summarise(`Accomodation count` = n())

Airbnb_sum <- Airbnbs %>%
  group_by(., GSS_CODE, NAME)%>%
  summarise(`Accomodation count` = n())

Hotels %>%
  filter(NAME == "Bexley")

Hotels_sum %>%
  filter(NAME == "Bexley")

Hotels_example <- st_contains(Londonborough, OSM)
Hotels_example

Accomodation_contained <- Londonborough %>%
  mutate(hotels_n = lengths(st_contains(., OSM))) %>%
  mutate(airbnbs_n = lengths(st_contains(., Airbnb)))

Accomodation_contained %>%
  filter(NAME == "Bexley")

install.packages("remotes")
remotes::install_github("r-tmap/tmap")

library(tmap)
tmap_mode("plot")

# set the shape
tm1 <- tm_shape(Accomodation_contained) + 
  # set the map layer
  # try changing this to tm_symbols()
  tm_polygons("hotels_n",
              col = "black", lwd=0.5, lty="dashed",)
tm1

tm_no_map_layer <- tm_shape(Accomodation_contained) + 
  # there is no tm_polygons() if we just want the map with no spatial data
  tm_borders(col = "darkblue")
tm_no_map_layer

tm_no_map_layer <- tm_shape(Accomodation_contained) + 
  # there is no tm_polygons() if we just want the map with no spatial data
  tm_borders(col = "darkblue")
tm_no_map_layer

tm1 <- tm_shape(Accomodation_contained) + 
  tm_polygons(fill ="hotels_n",
              col = "black", 
              lwd =0.5,
              lty="dashed",
              fill.chart = tm_chart_violin(),
              # above this was the same as before
              fill.scale = tm_scale_intervals(
                values="brewer.bu_pu",
                n=5,
                style="jenks"))
tm1

stats <- Accomodation_contained %>%
  st_drop_geometry() %>%
  dplyr::select(hotels_n, airbnbs_n) %>%  
  summarise(across(everything(), list(
    min = min,
    max = max,
    mean = mean,
    median = median,
    sd = sd
  )))
library(classInt)

# Get Jenks breaks for 5 classes
breaks <- Accomodation_contained%>%
  st_drop_geometry()%>%
  #need a numeric vector not a dataframe or tibble
  pull(airbnbs_n) %>%             
  classIntervals(., n = 5, style = "jenks")

breaks$brks

tm1 <- tm_shape(Accomodation_contained) +
  tm_polygons(
    fill = c("hotels_n", "airbnbs_n"),
    fill.scale = tm_scale_intervals(values = "brewer.blues", breaks=breaks$brks),
    # set the legend
    fill.legend = tm_legend(title="Accomodation count",
                            title.size=0.85,
                            size=0.8,
                            # plot outside of the main map
                            #explained below
                            position=tm_pos_out("right", 
                                                "center",
                                                pos.v = "center")),
    # all facets share the same legend
    fill.free = FALSE)+
  # make 2 rows for the facets
  tm_facets(nrow=2)

tm1

# Create the complete map layout

tm2 <- tm1 +
  tm_layout(
    panel.labels = c("Hotels", "Airbnb"),
    panel.show = TRUE,
    inner.margins = c(0.20, 0, 0, 0)
  ) +
  tm_compass(
    type = "arrow",
    size = 1.8,
    position = tm_pos_out(
      "right", "center",
      pos.h = -0.05,
      pos.v = 0.72
    )
  ) +
  tm_scalebar(
    text.size = 0.7,
    breaks = c(0, 10, 20),
    position = tm_pos_out(
      "right", "center",
      pos.h = 0.075,
      pos.v = 0.68
    )
  ) +
  tm_credits(
    "(c) OpenStreetMap contributors and Airbnb",
    size = 0.6,
    position = c("center", "bottom")
  )

tm2

tmap_save(tm2, "1_facet.png", width = 8, height = 6, units="in", dpi = 300)



qtm(UK_outline)
newbb <- c(xmin=-296000, ymin=5408, xmax=655696, ymax=1000000)

UK_outlinecrop <- UK_outline$geometry %>%
  st_crop(., newbb)

tm3 <- tm_shape(UK_outlinecrop)+ 
  tm_polygons(col="darkslategray1")+
  tm_layout(frame=FALSE)+
  tm_shape(Worldcities2) +
  #adds the city points
  tm_symbols(shape=20,
             fill = "orange",
             #outline colour
             col="black",
             size=0.8)+
  #add the city labels, x and y move the label around the point
  tm_text("CITY_NAME", xmod=-1, ymod=-0.5)
tm3


Londonbb <- st_bbox(Accomodation_contained,
                    crs = st_crs(Accomodation_contained))%>%
  # st_as_sfc coverts the coordinates to an sf object
  st_as_sfc()

tm4 <- tm3 +
  tm_shape(Londonbb)+ 
  tm_borders(col = "grey40", lwd = 3)+
  tm_layout(frame=FALSE,
            bg.color = "transparent")
tm4

library(ggplot2)

accom_long <- Accomodation_contained %>%
  # drop geometry for plotting
  st_drop_geometry() %>%  
  pivot_longer(
    cols = c(hotels_n, airbnbs_n),
    names_to = "accom_type",
    values_to = "count"
  )
violin <- accom_long %>%
  ggplot(aes(x = accom_type, 
             # number
             y = count, 
             # hotels or airbnb 
             fill = accom_type)) +
  geom_violin(trim = FALSE, 
              color = "grey30", 
              alpha = 0.8) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(
    title = "",
    x = "",
    y = "",
    fill = "black"
  ) +
  # change the labels from the column names to..
  scale_x_discrete(labels = c("hotels_n" = "Hotels", 
                              "airbnbs_n" = "Airbnbs"))+
  theme_minimal(base_size = 13)+
  theme(
    # no legend
    legend.position = "none",
    # all text to black
    text = element_text(color = "black"),         # set all text to black
    axis.text = element_text(color = "black"),    # axis tick labels
    axis.title = element_text(color = "black"),   # axis titles (if used)
    plot.title = element_text(color = "black")    # title (if used)
  )
library(grid)

# Open PNG device
png("3_facet.png", width = 8, height = 6, units="in", res = 300)

tm2
print(tm4, vp = viewport(x=0.73, y= 0.18, width = 0.3, height = 0.35))
print(violin, vp = viewport(x=0.67, y= 0.83, width = 0.25, height = 0.35))


# Close device
dev.off()
