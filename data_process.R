
rm(list = ls())

# Set WD automatically to where the R scrip is
setwd(dirname(rstudioapi::getSourceEditorContext()$path))


df.vil = read.csv("./data/mfr_village_post13_all.csv", stringsAsFactors = F)
df.liv = read.csv("./data/mfr_livelihood_post13_all.csv", stringsAsFactors = F)

### MANIPULATE THE CSV AND TRANSFORM IT INTO A .RDS

## VILLAGE
df.vil <- tidyr::separate(data=df.vil,
                      col=wkt_geom,
                      into=c("delete", "Longitude","Latitude"),
                      sep=" ",
                      remove=FALSE)

df.vil$Latitude <- stringr::str_replace_all(df.vil$Latitude, "[)]", "")
df.vil$Longitude <- stringr::str_replace_all(df.vil$Longitude, "[()]", "")

df.vil$Latitude <- as.numeric(df.vil$Latitude)
df.vil$Longitude <- as.numeric(df.vil$Longitude)

village_data <-  subset(df.vil, select=-c(wkt_geom,delete,GID)) ## Delete the columns that we don't need ## in case you want to subset your dataset df[c(1:1000),]
saveRDS(village_data, "./data/village_data.rds")

rm(village_data) # keep the environment tidy

## LIVELIHOOD
df.liv <- tidyr::separate(data=df.liv,
                      col=wkt_geom,
                      into=c("delete", "Longitude","Latitude"),
                      sep=" ",
                      remove=FALSE)

df.liv$Latitude <- stringr::str_replace_all(df.liv$Latitude, "[)]", "")
df.liv$Longitude <- stringr::str_replace_all(df.liv$Longitude, "[()]", "")

df.liv$Latitude <- as.numeric(df.liv$Latitude)
df.liv$Longitude <- as.numeric(df.liv$Longitude)


#livelihood_data <-  subset(df.liv[c(1:1000),], select=-c(wkt_geom,delete)) ## in case you want to subset your dataset df[c(1:1000),]
livelihood_data <-  subset(df.liv, select=-c(wkt_geom,delete)) ## in case you want to subset your dataset df[c(1:1000),]
saveRDS(livelihood_data, "./data/livelihood_data.rds")

rm(livelihood_data) # keep the environment tidy

