######  Tidyverse tutorial

### SHORT DESCRIPTION ----


# Inputs: 
# Otputs: 

# Clean the environment if you need to do so
# rm(list = ls())


#### LIBRARIES ----

# tidyverse has: dplyr,ggplot2,tibble,readr,tidyr,purrr
# install.packages("tidyverse")
# install.packages("broom")
# install.packages("ggExtra")
# install.packages("maps")
# install.packages("RColorBrewer")

library(tidyverse)
library(broom)
# library(ggExtra)
# library(maps)
# library(RColorBrewer)

####  SCRIPT ----

## Set WD automatically to where the R scrip is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

## Check the wd
getwd()


## Load data and store them as tibble ----
#rm(list=ls())

df.vil <- dplyr::tbl_df(readRDS("./data/village_data.rds"))
class(df.vil)
df.liv <- dplyr::tbl_df(readRDS("./data/livelihood_data.rds"))
class(df.liv)

## SUBSET FOR CAMEROON

# df.vil <- subset(df.vil, LOCALITY_6 == "CAM")
# df.liv <- subset(df.liv, COUNTRY == "CAM") 
## Check which villages don't have the respective livelihood data and excludes these from the analysis

# df.vil$PK %in% df.liv$FK # full list of included and excluded

df.vil$PK[!df.vil$PK %in% df.liv$FK] # villages not included

df.vil_red <- df.vil %>%
              .[df.vil$PK %in% df.liv$FK, ] %>%  # villages included
              dplyr::select (.,PK,Longitude,Latitude) 
              
df.vil_red <- df.vil_red %>% rename(Longitude.vil = Longitude, Latitude.vil = Latitude)

## Merge using the coordinates the two points we want to check the distance from

df.liv_mer <- merge(df.liv,df.vil_red, by.x = "FK", by.y = "PK")


## Get the distance from the points

library(raster)

df.liv_mer <- df.liv_mer %>% 
              mutate(dista = pointDistance(cbind(Longitude, Latitude), cbind(Longitude.vil, Latitude.vil), lonlat = TRUE))


df.liv_mer <- df.liv_mer %>%
  group_by(., VIL_NAME) %>%
  mutate(avg_dist = mean(dista)) %>%
  ungroup()


saveRDS(df.liv_mer, "./data/liv_dist.rds")


library(sf)
library(tmap)

countries <- sf::st_read("K:/tpd/AdminBoundaries/WFP_world_boundarieds/wld_bnd_adm0_wfpge_un.shp")

country_sub <- countries %>%
              subset(., (adm0_name == "Cameroon" | adm0_name == "Democratic Republic of the Congo"))


## Simple basemap fill and borders
base_map <- tm_shape(country_sub) + 
  tm_fill(alpha = 0.3) + 
  tm_borders(alpha = 0.6)

## plots of all the points

# transform the tibble into a sf object

liv_mer_sf <- st_as_sf(df.liv_mer,coords = c('Longitude', 'Latitude'), crs = 4326)

all_map <- base_map +
tm_shape(liv_mer_sf) + 
  tm_dots(col = "avg_dist", alpha = 0.5)

tmap_mode("view")
all_map


write.csv(df.liv_mer, "df.liv_mer.csv")



                     