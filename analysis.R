# plots and maps
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(scales)

# spatial data
library(geoR)
library(sp)
library(sf)
library(gstat)
library(raster)
library(automap)

# other stuff
library(Metrics)
library(FNN)
library(caret)
library(rstudioapi)
library(readr)

# reading data
setwd(dirname(getActiveDocumentContext()$path))
db = read.csv('db.csv')

# Removing outliers
boxplot(db$PRICE.SQUARE.FEET)
db = subset(db, PRICE.SQUARE.FEET<2000)
db = subset(db, PRICE.SQUARE.FEET>100)
boxplot(db$PRICE.SQUARE.FEET)

# Converting factor columns
factor_cols = c('BOROUGH', 'TAX.CLASS.AT.TIME.OF.SALE')
db[factor_cols] <- lapply(db[factor_cols], factor)

# Plotting prices
mapnyc <- data.frame(latitude = db$lat,
                     longitude = db$lon,
                     price = db$PRICE.SQUARE.FEET)

Token_map_box = read_file("api_token.txt")

palette_rev <- rev(brewer.pal(10, "RdYlBu"))

pal <- colorNumeric(palette = palette_rev, domain = c(max(mapnyc$price):min(mapnyc$price)))
leaflet() %>%  addTiles(urlTemplate = Token_map_box) %>%
  addCircleMarkers(data = mapnyc, color = ~pal(price), radius = 2)

# Checking for data normality
hist(log(db$PRICE.SQUARE.FEET)) # use log
db$PRICE.SQUARE.FEET.log = log(db$PRICE.SQUARE.FEET)
hist(db$PRICE.SQUARE.FEET.log)

# Train/test split
smp_size <- floor(0.75 * nrow(db))

set.seed(123)
train_ind <- sample(seq_len(nrow(db)), size = smp_size)

train <- db[train_ind, ]
test <- db[-train_ind, ]

# Converting to spatialpointsdataframe
projection = "+proj=longlat +ellps=WGS84 +no_defs"
train_spdf = SpatialPointsDataFrame(train[28:29], train[-c(28,29)], proj4string = CRS(projection))
test_spdf = SpatialPointsDataFrame(test[28:29], test[-c(28,29)], proj4string = CRS(projection))
db_spdf = SpatialPointsDataFrame(db[28:29], db[-c(28,29)], proj4string = CRS(projection))

# Removing duplicate locations
train_spdf = train_spdf[-zerodist(train_spdf)[,1],]
test_spdf = test_spdf[-zerodist(test_spdf)[,1],]
db_spdf = db_spdf[-zerodist(db_spdf)[,1],]

# Choosing the best variogram
vario.fit = autofitVariogram(PRICE.SQUARE.FEET.log~1,
                             train_spdf,
                             model = c("Exp", "Sph"),
                             kappa = c(0, 0.01, 0.05, seq(0.2, 2, 0.1), 5, 10),
                             fix.values = c(NA),
                             start_vals = c(NA),
                             verbose = T)

fit.v = vario.fit$var_model

lzn.vgm <- variogram(PRICE.SQUARE.FEET.log~1, train_spdf) # calculates sample variogram values 
lzn.fit <- fit.variogram(lzn.vgm, model=fit.v, fit.kappa = TRUE) # fit model

plot(lzn.vgm, lzn.fit) # plot the sample values, along with the fit model

# Getting NY polygon
ny_polygon = st_read('data/borough_boundaries/geo_export_a88ff5b0-7fb1-479d-a22b-224328f6d976.shp')
ny_polygon = st_transform(ny_polygon, projection)

spd <- sf::as_Spatial(st_geometry(ny_polygon), IDs = as.character(1:nrow(ny_polygon)))
spd_data = ny_polygon
spd_data$geometry = NULL
spd_data <- as.data.frame(spd_data)
spd <- sp::SpatialPolygonsDataFrame(spd, data = spd_data)

grid <- makegrid(spd, cellsize = 0.01)
grid <- SpatialPoints(grid, proj4string = CRS(projection))
grid <- grid[spd, ]

# Kriging
heat = krige(PRICE.SQUARE.FEET.log ~ 1, locations = train_spdf, newdata = grid, model = fit.v)

heat %>% as.data.frame %>%
  ggplot(aes(x=x1, y=x2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()

# measure performance on test set
kriging_test_prediction = krige(PRICE.SQUARE.FEET.log ~ 1, locations = train_spdf,
                        newdata = test_spdf, model = fit.v)

kriging_error = rmse(test_spdf$PRICE.SQUARE.FEET, exp(kriging_test_prediction$var1.pred))

# K-nearest neighbours
k_vector = seq(1, 20)
k_error_vector = c()
for (k in k_vector)
{
  knn_model = knn.reg(train[c('lon','lat')], test = NULL,
                      train$PRICE.SQUARE.FEET.log, k = k)
  k_error = rmse(train$PRICE.SQUARE.FEET, exp(knn_model$pred))
  k_error_vector = c(k_error_vector, k_error)
  
}

plot(k_vector, k_error_vector, type="l") 

# optimal k seems to be around 14
knn_model = knn.reg(train[c('lon','lat')], test = test[c('lon','lat')],
                    train$PRICE.SQUARE.FEET.log, k = 14)

knn_error = rmse(test$PRICE.SQUARE.FEET, exp(knn_model$pred))

# final results
cat(paste('Kriging error:', kriging_error, '\nKNN error:', knn_error))
