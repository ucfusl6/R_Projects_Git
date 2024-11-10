# Ineqaulity task - week 4

### Read in data:

#Located within the Data folder:
  
#* HDI data from: https://hdr.undp.org/data-center/documentation-and-downloads
#* Shapefile from: https://hub.arcgis.com/datasets/2b93b06dc0dc4e809d3c8db5cb96ba69_0/explore?location=-2.688200%2C0.000000%2C1.41 

## Load packages

library(tidyverse)
library(sf)
library(here)
library(janitor)
library(countrycode)

## read in data


HDI <- read_csv(here::here("/Users/shinliu/Desktop/CASA0005/Week 4/prac 1/wk4_paer1","HDR23-24_Composite_indices_complete_time_series.csv"),
                locale = locale(encoding = "latin1"),
                na = " ", skip=0)

World <- st_read(here("/Users/shinliu/Desktop/CASA0005/Week 4/prac 1/wk4_paer1", "World_Countries_(Generalized)_9029012925078512962.geojson"))


## Column names

#* World shape has the column ISO2
#* HDI has ISO3
#* We could join on country name?
  
#  Select columns we want and change country name to country code, using the `countrycode` package

#We could also just join it without this on the country name column 

#Some notes:
  
#* COW codes = correlates of war codes
#* ISO = International Organization for Standardization with 2 or 3 letters...


HDIcols<- HDI %>%
  clean_names()%>%
  select(iso3, country, gii_2019, gii_2010)%>%
  mutate(difference=gii_2019-gii_2010)%>%
  #not needed here as we can now use the country name...but see below
  mutate(iso_code=countrycode(country, origin = 'country.name', destination = 'iso2c'))%>%
  mutate(iso_code2=countrycode(iso3, origin ='iso3c', destination = 'iso2c'))


## Join

#Join the csv to world shape file


Join_HDI <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            # change to "aff_iso" = "iso_code"
            by = c("iso" = "iso_code"))

# 261 if using "aff_iso", 251 if using "iso". Could filter out the NA values.


#If using affiliate code then Aruba (country) is part of the Netherlands (affiliate country).

#Aruba is a self-governing country within the Kingdom of the Netherlands.

#If using iso code then Aruba will have no data as it has no data in the HDI. 

#We could also use the country column to join...


Join_HDI_2 <- World %>% 
  clean_names() %>%
  left_join(., 
            HDIcols,
            by = c("country" = "country"))
#251

```

## Problems 

#* With Join_HDI there are 11 rows for Antarctica **if using aff_iso** - be sure to use iso column or remove the NA values as in the aff_iso Antarctica is NA. Within the HDI dataset there are a series of rows for collections (e.g. World and Very high human development counties). So this join is being done on the `NA` values. For example, the `aff_iso` for Antarctica is NA and the ISO code for Very high human development counties is also NA!
  
  
#* With Join_HDI_2 (that uses country name to join) the entries for other islands aren't joined as they don't have the same name...for example, Anguilla, Bermuda, Falkland Islands..... 

#These are The British Overseas Territories (BOTs), also known as the United Kingdom Overseas Territories (UKOTs), fourteen territories with a constitutional and historical link with the United Kingdom, but do not form part of the United Kingdom itself.

#Should the HDI value for the UK be assigned to the The British Overseas Territories ?
  

Join_HDI_GB<-Join_HDI %>%
  filter(aff_iso=="GB")

Join_HDI_2_GB<-Join_HDI_2 %>%
  filter(aff_iso=="GB")


#..next time we will make a map using different repos


## Making Maps
library(tmap)
library(tmaptools)
library(tidyverse)
library(dplyr)
#library(leafpop)
#library(leaflet)


tmap_mode("plot")

# set the breaks
# for our mapped data



# plot map
#Gender_Inq_map <- tm_shape(Join_HDI_2) + 
#  tm_polygons("difference", 
#              breaks=breaks,
#              palette="PuBu")+
#  tm_legend(show=FALSE)+
#  tm_layout(frame=FALSE)+
#  tm_credits("(a)", position=c(0,0.85), size=1.5)
breaks = c(-Inf,-0.5,-0.3,-0.1,0.1,0.3) 

Gender_Inq_map <- tm_shape(Join_HDI_2) + 
tm_polygons("difference",
            breaks = c(-Inf,-0.5,-0.3,-0.1,0.1,0.3),
            style="pretty",
            palette="Blues",
            midpoint=0,
            title="difference 2010-2019",
            labels = c("0.1 to 0.3", "-0.1 to 0.1", "-0.3 to -0.1", "-0.5 to -0.3", "< -0.5"),  # reverse the labels,
            fill_alpha = 0.9) +
  tm_scale_bar(position=c("left", "bottom"), text.size=0.6)+
  tm_layout(title = "difference in gender inequality", legend.position = c("right", "bottom"))

t
print(Gender_Inq_map)


