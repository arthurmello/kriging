library(geoR)

# to get lat and lon from Google
library(ggmap)
library(rgdal)

library(tidyverse)
library(DT)
library(knitr)
library(sp)
library(rgeos)
library(ggplot2)
library(ggthemes)
library(outliers)
library(maptools)

getCurrentFileLocation <-  function()
{
  this_file <- commandArgs() %>% 
    tibble::enframe(name = NULL) %>%
    tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
    dplyr::filter(key == "--file") %>%
    dplyr::pull(value)
  if (length(this_file)==0)
  {
    this_file <- rstudioapi::getSourceEditorContext()$path
  }
  return(dirname(this_file))
}

path = getCurrentFileLocation()
setwd(path)
db = read.csv('db.csv')

#converting date format (min = 2016-09-01, max=2017-08-31)
db$SALE.DATE = format(as.Date(db$SALE.DATE))
#db = subset(db,SALE.DATE <= as.Date('2016/12/31'))

#cleaning the data: replacing '-' in SALE.PRICE
db$SALE.PRICE <- as.character(db$SALE.PRICE)
db$SALE.PRICE[db$SALE.PRICE == "-"] = ""
db$SALE.PRICE <- as.integer(db$SALE.PRICE)

#cleaning the data: replacing '-' in GROSS.SQUARE.FEET
db$GROSS.SQUARE.FEET <- as.character(db$GROSS.SQUARE.FEET)
db$GROSS.SQUARE.FEET[db$GROSS.SQUARE.FEET == "-"] = ""
db$GROSS.SQUARE.FEET <- as.integer(db$GROSS.SQUARE.FEET)

#cleaning the data: replacing '-' in LAND.SQUARE.FEET
db$LAND.SQUARE.FEET <- as.character(db$LAND.SQUARE.FEET)
db$LAND.SQUARE.FEET[db$LAND.SQUARE.FEET == "-"] = ""
db$LAND.SQUARE.FEET <- as.integer(db$LAND.SQUARE.FEET)

#cleaning the data: removing lines for which SALES.PRICE is too low
#db = subset(db,is.na(SALE.PRICE)==FALSE)
db = subset(db,SALE.PRICE>10)

#cleaning the data: removing lines for which there is no information on GROSS.SQUARE.FEET
db = subset(db,is.na(GROSS.SQUARE.FEET)==FALSE)
db = subset(db,GROSS.SQUARE.FEET>0)

#we add price per square feet:
db$PRICE.SQUARE.FEET = db$SALE.PRICE/db$GROSS.SQUARE.FEET

#we create a function that returns the borough's name, and add it to the database
borough_name_fun = function(code){
  if(code == 1){
    b_name = 'Manhattan'
  } else if (code == 2){
    b_name = 'Bronx'
  } else if (code == 3){
    b_name = 'Brooklyn'
  } else if (code == 4){
    b_name = 'Queens'
  } else if (code == 5){
    b_name = 'Staten Island'
  } else {
    b_name = 'N/I'
  }
  return(b_name)
}
db$BOROUGHNAME = lapply(db$BOROUGH, borough_name_fun)

#we create a full address column
db$location = paste0(db$ADDRESS, ", ", db$BOROUGHNAME, ", ", db$ZIP.CODE , " - New York")

# the sample is too big for Google API, so for now, we take just 2500 entries, selected randomly
# and we set the seed to make our partition reproductible

db$ID <- seq.int(nrow(db))
db1 = subset(db,ID<=2499)
db2 = subset(db,ID>2499 & ID<=4999)
db3 = subset(db,ID>4999 & ID<=7499)
db4 = subset(db,ID>7499 & ID<=9999)
db5 = subset(db,ID>9999)

###
smp_size = floor(2500/12949 * nrow(db))
set.seed(123)
reduced_sample = sample(seq_len(nrow(db)), size = smp_size)
reduced_sample = db[reduced_sample, ]
db = reduced_sample

#We get longitude and latitude from google API
geo = geocode(location = db5$location, output="latlon", source="google")
db5$lon = geo$lon
db5$lat = geo$lat

x = rbind(db1,db2,db3,db4,db5)
x = subset(x,is.na(lon)==FALSE)
x <- apply(x,2,as.character)

#Creating final db
db = rbind(db1,db2,db3,db4,db5)
db = subset(db,is.na(lon)==FALSE)

#Saving into a new file
write.csv(x, file = 'db.csv')

