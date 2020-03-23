setwd("~/covid19-suisse")
library(sf)
library(tidyverse)
library(viridis)
library(RColorBrewer)


coronaSpatial <- read.csv("0.CoronaSpatial.csv")

coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "ZH"] <- "1"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BE"] <- "2"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "LU"] <- "3"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "UR"] <- "4"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SZ"] <- "5"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "OW"] <- "6"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "NW"] <- "7"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GL"] <- "8"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "ZG"] <- "9"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "FR"] <- "10"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SO"] <- "11"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BS"] <- "12"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "BL"] <- "13"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SH"] <- "14"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AI"] <- "15"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AR"] <- "16"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "SG"] <- "17"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GR"] <- "18"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "AG"] <- "19"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "TG"] <- "20"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "TI"] <- "21"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "VD"] <- "22"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "VS"] <- "23"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "NE"] <- "24"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "GE"] <- "25"
coronaSpatial$KANTONSNUM[coronaSpatial$Canton == "JU"] <- "26"

names(coronaSpatial )[names(coronaSpatial ) == "KANTONSNUM"] <- "KTNR"

# read cantonal borders
canton_geo <- read_sf("g2k20.shp")

# read country borders
country_geo <- read_sf("g2l20.shp")

# read lakes
lake_geo <- read_sf("g2s20.shp")

#Data work
coronaSpatial$KTNR <- as.numeric(coronaSpatial$KTNR)
dat_merged <- merge(canton_geo, coronaSpatial, by="KTNR")
dat_merged$CasPositifs <- as.numeric(dat_merged$CasPositifs)


ggplot(
  data = dat_merged
) +
  geom_sf(
    mapping = aes(
      fill = CasPositifs,
    ),
    color = "white",
    size = 0.1
  ) +
  scale_fill_continuous(high = "#1b2576", low = "#5ab7db",
                        labels = c("250", "500", "750", "1000"),
                        name = "Cas positifs"
                        )+
  scale_size(guide = "legend"
             )+
  geom_sf(
    data = canton_geo,
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  geom_sf(
      data = lake_geo,
      fill = "#D6F1FF",
      color = "transparent"
  ) +
  labs(x = NULL,
        y = NULL,
        title = "Cas de coronavirus en Suisse",
        subtitle = "Nombre de personnes testées positives, état au 20 mars 2020") 




ggplot(data=canton_geo)
theme_map()