#Week 4 Homework
################

library(usethis)          #not needed but useful to create Github repository
library(here)             #for specifying paths
library(janitor)          #clean names functionality
library(countrycode)      #standardises country names and codes
library(sf)               #spatial features
library(dplyr)            #mutate, filter etc
library(tmap)             #thematic maps

######################## 

#PART 1 - LOAD DATA AND BOUNDARIES

#Load in spatial data (json format)

RawWorldMap <- st_read(here("World_Countries_(Generalized)_9029012925078512962.geojson"))

WorldMap <- RawWorldMap %>%
  clean_names()


plot(WorldMap$geometry)
head(WorldMap)



#WorldMapSimp <- st_simplify(WorldMap, dTolerance = 1000)
#plot(WorldMapSimp$geometry)


#Load in gender inequality data

RawGenderData <- read.csv(here("gender_inequality_index.csv"),
                          header = TRUE,
                          sep = ",")

#alt method for reading csv in
RawTest <- read.csv("gender_inequality_index.csv")

GenderData <- RawGenderData %>%
  clean_names()

########################

#PART 2 - GENDER INEQUALITY DIFFERENCE CALULATED

GenderSubset <- subset(GenderData, select = c(iso3, country, hdicode, region,
                                              gii_2010, gii_2019))

GenderSubsetDiff <- GenderSubset %>%
  mutate(gii_difference = (gii_2019 - gii_2010))

head(GenderSubsetDiff)

########################

#PART 3 - SORT OUT COUNTRY CODES

GenderSubsetDiffCCode <- GenderSubsetDiff %>%
  mutate(ISO2code = countrycode(iso3, "iso3c", "iso2c"))

GenderSubsetDiffCCode <- GenderSubsetDiffCCode %>%
  rename("iso" = "ISO2code")    #rename with dplyr - new column name = old column name

########################

#PART 4- JOIN DATA AND BOUNDARIES

GIIMap <- WorldMap %>%
  merge(.,
        GenderSubsetDiffCCode,
        by.x ="iso",
        by.y ="iso",
        all.x = TRUE,
        no.dups = TRUE)

########################

#PART 5 MAP

qtm(GIIMap, 
    fill = "gii_difference")

# interactive map of Gender Inequality Index (Differences between 2010 and 2019)
tmap_mode("view")
tm_shape(GIIMap) + 
  tm_polygons("gii_difference", 
              id = "country.x",
              #style="sd",
              breaks = c(-Inf,-0.5,-0.3,-0.1,0.1,0.3),
              palette="RdYlGn",
              midpoint=0,
              title="Diff. between GI Index 2010-2019",
              labels = c("0.1 to 0.3", "-0.1 to 0.1", "-0.3 to -0.1", "-0.5 to -0.3", "< -0.5"),  # reverse the labels
              fill_alpha = 0.9) + 
  #tm_compass(position = c("left", "bottom"),type = "arrow") + 
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(title = "Difference in GI Index 2010-2019")

########################

#Sources of Data

#https://hdr.undp.org/data-center/documentation-and-downloads accessed 27/10/2024
#https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0/explore?location=-2.688200%2C0.000000%2C1.41 accessed 27/10/24


#websites used

#https://sparkbyexamples.com/r-programming/dplyr-rename-column/
#https://rpubs.com/jenrichmond/clean_names
#https://stackoverflow.com/questions/54297542/r-tmap-changing-labels-in-view-mode


