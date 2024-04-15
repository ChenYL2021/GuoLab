
library(raster)
library(terra)
library(ncdf4)
# load("Z:/ck/xc/Plantation/pf.RData")
load("Z:/ck/xc/Plantation/pfAg.RData")


# Data preparation ---------------------------------

# Climate # ESSD
prep<-rast("D:/Greening/pre_1982_1984/pre_1982_1984.nc") # "Z:/ck/xc/Greening/pre_1982_1984/pre_1982_1984.nc"
plot(prep$pre_7)
dim(prep) # 4717 7680   36
proj4string(raster(prep)) # "+proj=longlat +datum=WGS84 +no_defs" # crs(prep) # Deprecated Proj.4 representation: +proj=longlat +datum=WGS84 +no_defs 
res(prep) # 0.008333333 0.008333333 # 1 km!!

# # China's border # from Ren Yu
library(sf)
china2<-read_sf("D:/Greening/国界/国界.shp") # "Z:/ck/xc/Greening/国界/国界/国界.shp" # readOGR("Z:/ck/xc/Greening/国界/国界/国界.shp") # vect("Z:/ck/xc/Greening/国界/国界/国界.shp")
# china2 = as_Spatial(china2)
plot(china2)
crs(china2) # Deprecated Proj.4 representation:
# +proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0
# +ellps=krass +units=m +no_defs # "PROJCRS[\"Krasovsky_1940_Albers\",\n    BASEGEOGCRS[\"Unknown datum based upon the Krassowsky 1940 ellipsoid\",\n        DATUM[\"Not specified (based on Krassowsky 1940 ellipsoid)\",\n            ELLIPSOID[\"Krassowsky 1940\",6378245,298.3,\n
# china2Pj<-project(china2, prep)
# crs(china2Pj) # "GEOGCRS[\"unknown\",\n    DATUM[\"World Geodetic System 1984\",\n        ELLIPSOID[\"WGS 84\",6378137,298.257223563,\n
# plot(china2Pj)

# Planted forests
wd<-"D:/Plantation/Final_PF_resample/Final_PF_resample" # "Z:/ck/xc/Plantation/Final_PF_resample/Final_PF_resample" # 1 km # wd<-"Z:/ck/xc/Plantation/Final_PF/Final_PF" # wd<-"Z:/ck/xc/Plantation/Final_PF_resample/Final_PF_resample"
# wd<-"Z:/ck/xc/Plantation/Final_PF/Final_PF" # 30 m
setwd(wd)
filenames <- list.files(wd)
filenames <- filenames[grep("[.]tif", filenames)] 
len<-length(filenames)
filenames
# "pf00.tif" "pf05.tif" "pf10.tif" "pf15.tif" "pf20.tif" "pf90.tif" "pf95.tif"

# # From 30 m to 1 km: project()
# From 1 km to 10 km: project()
prepAg<-aggregate(prep, fact=10, fun=mean, na.rm=TRUE) # 10 km
plot(prepAg$pre_1)
ext(prepAg) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2254610567952, 55.5587943901263 (xmin, xmax, ymin, ymax)
res(prepAg) # 0.08333333 0.08333333

# pf<-c() # c() # raster()
pfPj<-c()
for (i in c(6,7,1:5)){ # c(6,7,1:5) # 1:len
  ras<-rast(filenames[[i]]) # rast # raster
  # Replace NA and abnormal numbers
  if (i == 1|i == 2|i == 4){ # i == 2
    ras<-subst(ras, NA, 128) # NAflag(ras) <- 128 # doesn't work # ras[is.na(ras)==T]<-128 # need to be done before project to get the decimal
  }
  # if (i == 3|i == 5|i == 7){ # i == 2
  #   ras<-subst(ras, 128, NA) # NAflag(ras) <- 128 # doesn't work # ras[is.na(ras)==T]<-128 # need to be done before project to get the decimal
  # }
  if (i == 6){
    ras<-subst(ras, NA, 128)
    ras<-subst(ras, 22, 33) # twice
  }
  # pf<- c(pf, ras) # terra::add(pf)<-ras  # pf<-stack(pf, ras) # addLayer(pf, ras)
  rasPj<-project(ras, prepAg, method="bilinear") # project(ras, rasAg6) # default: , method="bilinear" for numerical
  plot(rasPj)
  # writeRaster(raster(rasPj), filename=paste0("Z:/ck/xc/Plantation/",i,"tif"),
  #             format="GTiff",  # specify output format - GeoTIFF
  #             overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
  #             NAflag=-9999) # set no data value to -9999
  # Put together
  pfPj<- c(pfPj, rasPj)
  rm(ras,rasPj)
  gc()
}
par(mfrow=c(2,2))
for(i in 1: len){plot(pf[[i]])}
for(i in 1: len){plot(pfPj[[i]])}

# # From 30 m to 10 km: aggregate()
# From 1 km to 10 km: aggregate()
pfAg<-c() # pfPj<-c() # c() # raster()
for (i in c(6,7,1:5)){ # c(6,7,1:5) # 1:len # by time sequence
  ras<-rast(filenames[[i]]) # rast # raster
  # Replace NA and abnormal numbers
  if (i == 6){
    ras<-subst(ras, NA, 128) # ras<-subst(ras, NA, 0) # ras<-subst(ras, NA, 128)
    ras<-subst(ras, 22, 33) # ras<-subst(ras, 22, 1) # ras<-subst(ras, 22, 33) # twice
    rasAg<-aggregate(ras, fact=10, fun=mean, na.rm=TRUE) # 30 m: fact=10*1000/30 # i=4, 5, ras extent is larger
    rasAg6<-rasAg
    # rasAg<-project(ras, rasAg6)
  }
  if (i == 1|i == 2|i == 4){
    ras<-subst(ras, NA, 128) # ras<-subst(ras, NA, 0) # ras<-subst(ras, NA, 128) # NAflag(ras) <- 128 # doesn't work # ras[is.na(ras)==T]<-128 # need to be done before project to get the decimal
    # ras<-subst(ras, 33, 1)
  }
  # if (i == 3|i == 5|i == 7){
    # ras<-subst(ras, 128, 0) 
    # ras<-subst(ras, 33, 1) # ras<-subst(ras, 128, NA)
  # }
  # pf<- c(pf, ras) # terra::add(pf)<-ras  # pf<-stack(pf, ras) # addLayer(pf, ras)
  # Aggregate / Reproject
  rasAg<-aggregate(ras, fact=10, fun=mean, na.rm=TRUE) # 30 m: fact=10*1000/30 # rasAg<-project(ras, rasAg6) # rasAg<-aggregate(ras, fact=10, fun=modal) # terra or raster # extent is the same as original # rasPj<-project(ras, prep) # default: , method="bilinear" for numerical
  if (i == 4|i == 5){
    # rasAg<-project(ras, rasAg6)
    rasCr<-crop(rasAg, ext(rasAg6)) # terra or raster # i=4, 5 have still a bit different extents with 6,7,1,2,3
    rasRe<-resample(rasCr, rasAg6, method="bilinear") # same extent with the others
    rasAg<-rasRe
  }
  plot(rasAg)
  writeRaster(raster(rasAg), filename=paste0("D:/Plantation/_10km/",i,"_10km.tif"), # "Z:/ck/xc/Plantation/" # rasPj
              format="GTiff",  # specify output format - GeoTIFF
              overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
              NAflag=-9999) # set no data value to -9999
  # Put together
  pfAg<-c(pfAg, rasAg) # pfPj<- c(pfPj, rasPj)
  rm(ras,rasAg) 
  gc()
}
rm(rasAg6,rasCr,rasRe)
gc()
par(mfrow=c(2,2))
# for(i in 1: len){plot(pf[[i]])}
# pfPj<-project(rast(pf), prep) # Error: cannot allocate vector of size 75.0 Gb
for(i in 1: len){plot(pfAg[[i]])} # for(i in 1: len){plot(pfPj[[i]])}


# Calculation ---------------------------------

# Matrix
pfChina <- lapply(pfAg, raster) # pfChina <- lapply(pfPj, raster)
# Not replacing pf or pfPj to pfAg thereafter, except for the output .tif names
pfChina <- stack(pfChina) # sapply(1:7, function(i){stack(pfChina[[i]])})
pfChina <- rasterToPoints(pfChina)

# Slope (first derivative)
# Go to NDVI3_v3.R for masking functions
slope <- function(df){
  df[is.na(df) | df=="Inf" | df=="NaN"] <- NA
  if (length(df)>1) { # !=0
    m <- lm(df ~ c(1:length(df))) # no need of ", na.rm=TRUE", R simply ignores the NA values when fitting the linear regression model
    return(c(summary(m)$coefficients[2], summary(m)$coefficients[,'Pr(>|t|)'][2]))
  }
  # else{
  #   return()
  # }
}
maskRaster2<-function(data, value){
  # # Mask
  # mask<-data
  # mask[mask<value]<-NA
  # plot(mask)
  # 
  # mask<-is.na(mask)==F
  # # plot(mask)
  # mask[mask!=1]<-NA
  # # plot(mask)
  # 
  # # Masked area
  # dataMask <- mask(data, mask)
  # plot(dataMask[[1]])
  # # summary(dataMask)
  
  # dataMask <- data
  data[data>=value] <- NA # dataMask
  # summary(dataMask)
  
  return(data) # dataMask
}

# Maksed area by raster
maskMatrix<-function(data, raster){
  # Mask
  mask<-raster # prov2
  mask<-is.na(mask)==F
  mask[mask!=1]<-NA
  plot(mask)
  
  # Masked area
  dataMask <- raster::mask(data, mask) # Will be used later
  # dataMask <- crop(data, mask) # has the same type, but a bit different values from dataMask?
  plot(dataMask[[1]])
  dataMatrix <- rasterToPoints(dataMask) # "matrix" "array" # nlayers(data)+2 columns, has x, y
  # dataMatrix <- as.data.frame(dataMask) # data.frame # nlayers(data) columns, doesn't have x, y
  
  return(list(dataMask=dataMask, dataMatrix=dataMatrix))
}

slopeRaster<-function(data, colname, colname2, sigThre){ # , colname, colname2
  # calculate slope
  # data[is.na(data) | data=="Inf"] <- NA
  # data <- data[apply(data, 1, function(x){any(!is.na(x))}),3:ncol(data)] # remove the rows that are all NAs
  result<-apply(data[,3:ncol(data)],1,slope) # ndviProv2[,3:36] # there are NaNs in the second row
  # result[is.na(result) | result=="Inf" | result=="NaN"] <- NA # doesn't matter
  slope_data<-result[1,]
  
  # point to raster
  slope_data<-as.data.frame(slope_data)
  colnames(slope_data)<- colname # "slope_ndviProv2"
  spdf_data<-SpatialPointsDataFrame(coords = as.matrix(data[,1:2]), slope_data,
                                    proj4string = CRS("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")) # proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
  pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
  ras_data<-raster(pix_data[, colname]) # RasterLayer # 'slope_ndviProv2'
  
  # calculate significance
  slope_data<-result[2,] # ndviProv2[,3:36]
  
  # point to raster
  slope_data<-as.data.frame(slope_data)
  colnames(slope_data)<-colname2 # "sig_slope_ndviProv2"
  spdf_data<-SpatialPointsDataFrame(coords = as.matrix(data[,1:2]), slope_data,
                                    proj4string = CRS("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")) # proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
  pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
  ras_data2<-raster(pix_data[, colname2]) # RasterLayer # 'slope_ndviProv2'
  
  ras_data2<-maskRaster2(ras_data2, sigThre) # P-value
  ras_data3<-maskMatrix(ras_data,ras_data2)[[1]]
  ras_data<-stack(ras_data,ras_data2,ras_data3)
  
  par(mfrow=c(1,3))
  plot(ras_data[[1]]) # raster::plot(ras_slope_data)
  plot(ras_data[[2]])
  plot(ras_data[[3]])
  # summary(ras_data)
  return(ras_data)
}

# pfChina_seq<-pfChina[seq(1,nrow(pfChina),10000),]
slope_pfChina<-slopeRaster(pfChina, "slope_pfChina","sig_slope_pfChina", 0.05) # Two layers
# slope_pfAgChina<-slopeRaster(pfAgChina, "slope_pfChina","sig_slope_pfChina", 0.05) 
# slope_pfPjChina<-slopeRaster(pfPjChina, "slope_pfChina","sig_slope_pfChina", 0.05) 
# writeRaster(slope_pfChina, # three layers
#             filename="Z:/ck/xc/Plantation/slope_pfAgChina.tif", # filename="Z:/ck/xc/Plantation/slope_pfChina.tif",
#             format="GTiff",  # specify output format - GeoTIFF
#             overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
#             NAflag=-9999) # set no data value to -9999

# Second derivative
slope2 <- function(df){
  df[is.na(df) | df=="Inf" | df=="NaN"] <- NA
  if (length(df)>1) { # !=0
    m <- lm(df ~ poly(c(1:length(df)),2)) # no need of ", na.rm=TRUE", R simply ignores the NA values when fitting the linear regression model
    return(c(summary(m)$coefficients[3], summary(m)$coefficients[,'Pr(>|t|)'][3]))
  }
  # else{
  #   return()
  # }
}
slopeRaster2<-function(data, colname, colname2, sigThre){ # , colname, colname2
  # calculate slope
  # data[is.na(data) | data=="Inf"] <- NA
  # data <- data[apply(data, 1, function(x){any(!is.na(x))}),3:ncol(data)] # remove the rows that are all NAs
  result<-apply(data[,3:ncol(data)],1,slope2) # ndviProv2[,3:36]
  slope_data<-result[1,]
  
  # point to raster
  slope_data<-as.data.frame(slope_data)
  colnames(slope_data)<- colname # "slope_ndviProv2"
  spdf_data<-SpatialPointsDataFrame(coords = as.matrix(data[,1:2]), slope_data,
                                    proj4string = CRS("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")) # proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
  pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
  ras_data<-raster(pix_data[, colname]) # RasterLayer # 'slope_ndviProv2'
  
  # calculate significance
  slope_data<-result[2,] # ndviProv2[,3:36]
  
  # point to raster
  slope_data<-as.data.frame(slope_data)
  colnames(slope_data)<-colname2 # "sig_slope_ndviProv2"
  spdf_data<-SpatialPointsDataFrame(coords = as.matrix(data[,1:2]), slope_data,
                                    proj4string = CRS("+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")) # proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
  pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
  ras_data2<-raster(pix_data[, colname2]) # RasterLayer # 'slope_ndviProv2'
  
  ras_data2<-maskRaster2(ras_data2, sigThre) # P-value
  ras_data3<-maskMatrix(ras_data,ras_data2)[[1]]
  ras_data<-stack(ras_data,ras_data2,ras_data3)
  
  par(mfrow=c(1,3))
  plot(ras_data[[1]]) # raster::plot(ras_slope_data)
  plot(ras_data[[2]])
  plot(ras_data[[3]])
  # summary(ras_data)
  return(ras_data)
  rm(result,slope_data,spdf_data,pix_data,ras_data2,ras_data3)
  gc()
}

# prep <- raster::brick("Z:/ck/xc/Greening//Yangkun/prec_CMFD_V0106_B-01_01yr_010deg_1979-2018.nc") # brick("D:/Greening/Yangkun/prec_CMFD_V0106_B-01_01yr_010deg_1979-2018.nc")
# prepChina <- rasterToPoints(prep)
# slope2_prep<-slopeRaster2(prepChina, "slope_pfChina","sig_slope_pfChina", 0.05) # Two layers

slope2_pfChina<-slopeRaster2(pfChina, "slope2_pfChina","sig_slope2_pfChina", 0.05) # Two layers
# slope2_pfAgChina<-slopeRaster2(pfAgChina, "slope2_pfChina","sig_slope2_pfChina", 0.05)
# slope2_pfPjChina<-slopeRaster2(pfPjChina, "slope2_pfChina","sig_slope2_pfChina", 0.05)
# writeRaster(slope2_pfChina, # three layers
#             filename="Z:/ck/xc/Plantation/slope2_pfAgChina.tif", # filename="Z:/ck/xc/Plantation/slope2_pfChina.tif",
#             format="GTiff",  # specify output format - GeoTIFF
#             overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
#             NAflag=-9999) # set no data value to -9999


# Plot ---------------------------------

slope_pfChina<-brick("Z:/ck/xc/Plantation/slope_pfChina.tif")
slope2_pfChina<-brick("Z:/ck/xc/Plantation/slope2_pfChina.tif")

par(mfrow=c(1,1))

library(ggplot2)
library(grid)
library(ggplotify)
theme<-  theme_bw()+
  theme(panel.grid=element_blank(), legend.position = c(0.55,0.05), legend.direction = "horizontal", legend.key.size=unit(0.3,'cm'),
        legend.text = element_text(size=8), legend.title = element_text(size=8), # legend.position = "none", panel.border = element_blank(),
        title = element_text(size=14), text = element_text(size=8, colour="black"), ## family = 'Times_New_Roman',face='bold'
        axis.ticks.length=unit(-0.05, "cm"), axis.title = element_text(size=14), 
        axis.text = element_text(margin=unit(c(0.5,0.5,0.5,0.5), "cm"), size=8, colour="black"),
        # plot.margin = margin(t = 0.2, r = 0.2, b = 0.2,l = 0.2, unit = "cm")
  )
theme2 <- theme_bw()+
  theme(panel.grid=element_blank(), panel.border = element_blank(), axis.line = element_line(colour="black"), 
        # legend.text = element_text(size=8), legend.title = element_text(size=8), legend.position = "none",
        axis.ticks.length=unit(-0.05, "cm"), axis.title = element_blank(),
        axis.text = element_text(colour="black", size=8),
        title = element_text(size=7)) # margin=unit(c(0.5,0.5,0.5,0.5), "cm"),


# - First derivative ---------------------------------

plot(slope_pfChina$slope_pfChina.1)
summary(slope_pfChina$slope_pfChina.1)
# slope_pfChina.1
# Min.          -20.35714
# 1st Qu.         0.00000
# Median          0.00000
# 3rd Qu.         0.00000
# Max.           15.65103
# NA's      1233429.00000
summary(slope_pfChina$slope_pfChina.1*(-1/95))
# slope_pfChina.1
# Min.      -1.647477e-01
# 1st Qu.    0.000000e+00
# Median     0.000000e+00
# 3rd Qu.    0.000000e+00
# Max.       2.142857e-01
# NA's       1.233429e+06


# slope_pfChinaPj<-brick("Z:/ck/xc/Plantation/slope_pfChinaPj.tif")
# slope2_pfChinaPj<-brick("Z:/ck/xc/Plantation/slope2_pfChinaPj.tif")

# For 10 km:
slope_pfChina@crs
# Coordinate Reference System:
#   Deprecated Proj.4 representation:
#   +proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass
# +units=m +no_defs 
# slope_pfChinaPj<-projectRaster(slope_pfChina, crs="+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")
slope_pfChinaPj<-slope_pfChina
# For 1 km:
# slope_pfChinaPj<-terra::project(rast(slope_pfChina), "+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")
# summary(slope_pfChinaPj$layer.1*(-1/95))
# # layer.1      
# # Min.   :-0.079  
# # 1st Qu.: 0.000  
# # Median : 0.000  
# # Mean   : 0.003  
# # 3rd Qu.: 0.000  
# # Max.   : 0.214  
# # NA's   :27436   
# # Warning message:
# # [summary] used a sample
summary(slope_pfChinaPj$slope_pfChina.1*(-1/95))
# slope_pfChina.1
# Min.         -0.2142857
# 1st Qu.       0.0000000
# Median        0.0000000
# 3rd Qu.       0.0000000
# Max.          0.2142857
# NA's          0.0000000
# test_spdf <- as(raster(slope_pfChinaPj)[[1]]*(-1/95), "SpatialPixelsDataFrame") # slope_pfChina
test_spdf <- as(slope_pfChinaPj[[1]]*(-1/95), "SpatialPixelsDataFrame") # as(slope_pfChinaPj[[1]], "SpatialPixelsDataFrame")
test_df <- as.data.frame(test_spdf)
colnames(test_df) <- c("value", "x", "y")
ras<-is.na(slope_pfChinaPj[[2]])==FALSE # ras<-is.na(raster(slope_pfChinaPj[[2]]))==FALSE # slope_pfChina
plot(ras)
ras2<-aggregate(ras, fact=10, fun=mean) # 70
plot(ras2>0.5)
test_df2<-as.data.frame(rasterToPoints(ras2>0.5)) # ras>0.5 # test_df2<-as.data.frame(rasterToPoints(ras2>0.5))
hist(test_df2$layer)
summary(test_df$value)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -0.152044  0.000000  0.000000  0.003034  0.000000  0.214286 
p<-1000 # 10, 100, 500, 1000
test_df$log_value<-ifelse(test_df$value>0,log10(1+test_df$value*p),-log10(1-test_df$value*p)) # log, log2
summary(test_df$log_value)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -2.1848  0.0000  0.0000  0.0884  0.0000  2.3330 
summary(test_df$x)
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3349057 -1486133   -70465   -70326  1348283  3196580 
summary(test_df$y)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1865367 2823003 3777559 3853055 4848355 6141625 
slope.pfChina <- ggplot() +
  geom_tile(data = test_df, aes(x = x, y = y, fill = log_value)) + # fill = value
  scale_fill_gradient2(low="red", high="blue", na.value = "White", # limits = c(-3.3,3.3), # limits = c(-1, 1), # mid = "#FFFFFF", midpoint = 0, breaks = seq(-0.4,0.4,0.01),
                       name=expression(paste("Trend in log"[10], " (1 + area * 10"^3,") (km"^2," yr"^-1,")")))+
  geom_sf(data = china2, color="black", fill=NA)+ # geom_sf(data = china[china$ADCODE99==230000|china$ADCODE99==150000|china$ADCODE99==220000|china$ADCODE99==210000
  #                      |china$ADCODE99==130000|china$ADCODE99==610000|china$ADCODE99==140000,], fill = NA)+
  geom_point(data = test_df2[test_df2$layer==1,], aes(x = x, y = y, fill = layer), size=0.2) +
  coord_sf(xlim=c(-3*10^6, 3*10^6), ylim=c(1.5*10^6, 6*10^6))+ # coord_sf(xlim=c(73, 135), ylim=c(15, 55))+
  # scale_x_discrete(limits=seq(75, 130, 10))+ # scale_x_discrete(limits=factor(seq(70, 135, 10)))+ # labels=
  # scale_y_discrete(limits=factor(seq(15, 55, 10)))+
  labs(x ="Longitude",y="Latitude", # x ='Longitude (°E)',y="Latitude (°N)",
       # title = expression(paste("First derivative"))
       )+ #,subtitle = "land use data",caption = 'Visualization by DataCharm'
  theme
slope.pfChina

lm_eqn1 <- function(df){
  l<-list()
  m <- lm(value ~ year, df) # df[df[,2]==i,]
  beita = summary(m)$coefficients[2,1] # coef(m)[2]
  sd = sd(df[,"value"])
  # r2 = summary(m)$r.squared
  # p = summary(m)$coefficients[2,4]
  eq <- substitute("β"~"="~beita~"sd"~"="~sd, # #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                   list(beita = format(beita, digits = 2), sd = format(sd, digits = 2))) # , r2 = format(r2, digits = 2)
  l<-as.character(as.expression(eq))
  return(l)
}

lm_eqn2 <- function(df){
  l<-list()
  m <- lm(value ~ year, df); # df[df[,2]==i,]
  # beita = summary(m)$coefficients[2,1] # coef(m)[2]
  r2 = summary(m)$r.squared
  p = summary(m)$coefficients[2,4]
  if (p<0.001){
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"<"~0.001, # #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), p = format(p, digits = 2))) # , r2 = format(r2, digits = 2)
  } else if (p<0.005){
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"<"~0.005, # ~italic("p")~"<"~0.005, #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), p = format(p, digits = 2))) # , r2 = format(r2, digits = 2)
  } else if (p<0.01){
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"<"~0.01, # ~italic("p")~"<"~0.01, #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), p = format(p, digits = 2))) # , r2 = format(r2, digits = 2)
  } else if (p<0.05){
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"<"~0.05, # ~italic("p")~"<"~0.05, #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), p = format(p, digits = 2))) # , r2 = format(r2, digits = 2)
  } else if (p<0.1){
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"<"~0.1, # ~italic("p")~"<"~0.1, #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), p = format(p, digits = 2))) # , r2 = format(r2, digits = 2)
  } else{
    eq <- substitute(italic(r)^2~"="~r2~italic("p")~"≥"~0.1, # ~italic("p")~">"~0.1, #italic(r)^2~"="~r2~italic("p")~"="~p~"n"~"="~n,
                     list(r2 = format(r2, digits = 2), r2 = format(r2, digits = 2))) # ,p = format(p, digits = 2)
  }
  l<-as.character(as.expression(eq))
  return(l)
}

data<-data.frame(1:7, apply((128-pfChina[,3:9])/95,2,mean,na.rm=T))
names(data)<-c("year","value")
data$year<-seq(1990,2020,5)
Annual.mean.pfChina<-ggplot(data=data,aes(x=year,y=value))+
  geom_line()+
  geom_smooth(method="lm", se=TRUE, formula=y~x, linetype="dashed", color="black", size=0.5) + # color="red" #aes(color=season),
  annotate("text", x = 2005, y = 0.020, size=2.5, label = lm_eqn2(data), parse=TRUE)+ # color="green3", # lm_eqn2 from NDVI3_v3.R
  annotate("text", x = 2005, y = 0.016, size=2.5, label = lm_eqn1(data), parse=TRUE)+ # color="green3", # lm_eqn1 from NDVI3_v3.R
  coord_cartesian(xlim=c(1990,2020), ylim=c(0.015,0.04))+ # , ylim=c(0.5,0.8)
  scale_x_discrete(limits=seq(1990,2020,10))+ # factor()
  labs(x="", y=expression(paste("")),
       title=expression(paste("Mean annual area (km"^2," km"^-2,")"))
  )+ # Mean annual NDVI
  theme2
Annual.mean.pfChina

setwd("Z:/ck/xc/Plantation")
tiff(filename="pfChina_withSig_aea.tif",res=300,height=1500,width=2000) # filename="pfChina_withSig.tif" # filename="pfChina.tif" # width=2000
grid.newpage()
grid.draw(slope.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
vp = viewport(x=.215, y=.215, width=.25, height=.25) # vp = viewport(x=.26, y=.23, width=.25, height=.25)
pushViewport(vp)
grid.draw(as.grob(Annual.mean.pfChina)) # ggplotify
upViewport()
dev.off()


# - Second derivative ---------------------------------

plot(slope2_pfChina$slope2_pfChina.1*(-1/95)*2) # 2a
summary(slope2_pfChina$slope2_pfChina.1*(-1/95)*2)
# slope2_pfChina.1
# Min.       -1.290707e+00
# 1st Qu.     0.000000e+00
# Median      0.000000e+00
# 3rd Qu.     0.000000e+00
# Max.        1.389288e+00
# NA's        7.690251e+06

# For 1 km:
# slope2_pfChinaPj<-terra::project(rast(slope2_pfChina), "+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs")
summary(slope2_pfChinaPj$slope2_pfChina.1*(-1/95)*2)
# slope2_pfChina.1
# Min.   :-1.09   # has changed?
# 1st Qu.: 0.00   
# Median : 0.00   
# Mean   : 0.01   
# 3rd Qu.: 0.00   
# Max.   : 1.09   
# NA's   :41368   
# Warning message:
# [summary] used a sample 

# For 10 km:
slope2_pfChinaPj<-slope2_pfChina
# slope2_pfChinaPj<-slope2_pfAgChina
# slope2_pfChinaPj<-slope2_pfPjChina
summary(slope2_pfChinaPj$slope2_pfChina.1*(-1/95)*2)
# slope2_pfChina.1
# Min.          -0.7095474
# 1st Qu.        0.0000000
# Median         0.0000000
# 3rd Qu.        0.0000000
# Max.           0.9666551
# NA's         886.0000000
test2_spdf <- as(slope2_pfChinaPj[[1]]*(-1/95)*2, "SpatialPixelsDataFrame") # test2_spdf <- as(raster(slope2_pfChinaPj[[1]])*(-1/95)*2, "SpatialPixelsDataFrame") # slope2_pfChina
test2_df <- as.data.frame(test2_spdf)
colnames(test2_df) <- c("value", "x", "y")
ras2_<-is.na(slope2_pfChinaPj[[2]])==FALSE # ras2_<-is.na(raster(slope2_pfChinaPj[[2]]))==FALSE # slope2_pfChina
plot(ras2_)
ras2_2<-aggregate(ras2_, fact=10, fun=mean) # ras2_2<-aggregate(ras2_, fact=50, fun=mean) # 70
plot(ras2_2>0.5)
test2_df2<-as.data.frame(rasterToPoints(ras2_2>0.5))
hist(test2_df2$layer)
summary(test2_df$value)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# -1.169206  0.000000  0.000000  0.007864  0.000000  1.195017 
p<-1000 # 10, 100, 500, 1000
test2_df$log_value<-ifelse(test2_df$value>0,log10(1+test2_df$value*p),-log10(1-test2_df$value*p)) # log, log2
summary(test2_df$log_value) # more distinguished color
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# -3.06826  0.00000  0.00000  0.08209  0.00000  3.07774  
slope2.pfChina <- ggplot() +
  geom_tile(data = test2_df, aes(x = x, y = y, fill = log_value)) + # fill = log_value # fill = value
  scale_fill_gradient2(low="red", high="blue", na.value = "White", # limits = c(-3.3,3.3), # limits = c(-1, 1), # mid = "#FFFFFF", midpoint = 0, breaks = seq(-0.4,0.4,0.01),
                       name=expression(paste("Trend in log"[10], " (1 + area change * 10"^3,") (km"^2," yr"^-2,")")))+ # name=expression(paste("Trend in area change (km"^2," yr"^-2,")")))+ # name=expression(paste("Trend in log"[10], " (area * 10"^3,") (km"^2," yr"^-2,")")))+ 
  geom_sf(data = china2, color="black", fill = NA)+ # geom_sf(data = china[china$ADCODE99==230000|china$ADCODE99==150000|china$ADCODE99==220000|china$ADCODE99==210000
  #                      |china$ADCODE99==130000|china$ADCODE99==610000|china$ADCODE99==140000,], fill = NA)+  
  geom_point(data = test2_df2[test2_df2$layer==1,], aes(x = x, y = y, fill = layer), size=0.2) +
  coord_sf(xlim=c(-3*10^6, 3*10^6), ylim=c(1.5*10^6, 6*10^6))+ # coord_sf(xlim=c(73, 135), ylim=c(15, 55))+
  # scale_x_continuous(limits=seq(75, 135, 10))+ # scale_x_discrete(limits=seq(70, 135, 10))+
  # scale_y_discrete(limits=seq(15, 55, 10))+
  labs(x ="Longitude",y="Latitude", # x ='Longitude (°E)',y="Latitude (°N)",
       # title = expression(paste("Second derivative")) # "First derivative"
  )+ #,subtitle = "land use data",caption = 'Visualization by DataCharm'
  theme
slope2.pfChina

data2<-data.frame(1:6, apply(((128-pfChina[,4:9])/95-(128-pfChina[,3:8])/95)/5,2,mean,na.rm=T))
names(data2)<-c("year","value")
data2$year<-seq(1995,2020,5)
Annual.mean2.pfChina<-ggplot(data=data2,aes(x=year,y=value))+
  geom_line()+
  geom_smooth(method="lm", se=TRUE, formula=y~x, linetype="dashed", color="black", size=0.5) + # color="red" #aes(color=season),
  annotate("text", x = 2007.74, y = 0.0000, size=2.5, label = lm_eqn2(data2), parse=TRUE)+ # color="green3", # lm_eqn2 from NDVI3_v3.R # y = 0.0020
  annotate("text", x = 2007.75, y = -0.0003, size=2.5, label = lm_eqn1(data2), parse=TRUE)+ # color="green3", # lm_eqn1 from NDVI3_v3.R # y = 0.0016
  coord_cartesian(xlim=c(1995,2020))+ # , ylim=c(-0.002,0.004)
  scale_x_discrete(limits=seq(2000,2020,10))+ # factor()
  # scale_y_discrete(factor(limits=seq(-0.001,0.002,0.0005)))+
  labs(x="", y=expression(paste("")),
       title=expression(paste("Mean annual \narea change (km"^2," km"^-2, " yr"^-1, ")"))
  )+ # Mean annual NDVI
  theme2
Annual.mean2.pfChina

setwd("Z:/ck/xc/Plantation")
tiff(filename="pfChina2_withSig_aea.tif",res=300,height=1500,width=2000) # filename="pfChina_withSig.tif" # filename="pfChina.tif" # width=2000
grid.newpage()
grid.draw(slope2.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
vp = viewport(x=.215, y=.215, width=.25, height=.25) # x=.215
pushViewport(vp)
grid.draw(as.grob(Annual.mean2.pfChina)) # ggplotify
upViewport()
dev.off()
# vp = viewport(x=.45, y=.7, width=.13, height=.2)
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean.pcor.temperatureDe)) # ggplotify
# upViewport()
# vp = viewport(x=.78, y=.7, width=.13, height=.2)
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean.non.plantation)) # ggplotify
# upViewport()
# dev.off()

# # eps
# setwd("Z:/ck/xc/Plantation")
# setEPS()
# postscript("pfChina2_withSig_aea.eps",height=1500,width=2000)
# grid.newpage()
# grid.draw(slope2.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
# vp = viewport(x=.215, y=.215, width=.25, height=.25) # x=.215
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean2.pfChina)) # ggplotify
# upViewport()
# dev.off()
# 
# # ggsave("pfChina2_withSig_aea.eps", height=1500,width=2000)
# 
# grid.newpage()
# grid.draw(slope2.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
# vp = viewport(x=.215, y=.215, width=.25, height=.25) # x=.215
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean2.pfChina)) # ggplotify
# upViewport()
# dev.copy2eps()
# 
# # pdf
# setwd("Z:/ck/xc/Plantation")
# pdf(filename="pfChina_withSig_aea.pdf",res=300,height=1500,width=2000) # filename="pfChina_withSig.tif" # filename="pfChina.tif" # width=2000
# grid.newpage()
# grid.draw(slope.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
# vp = viewport(x=.215, y=.215, width=.25, height=.25) # x=.215
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean.pfChina)) # ggplotify
# upViewport()
# dev.off()
# 
# pdf(filename="pfChina2_withSig_aea.pdf",res=300,height=1500,width=2000) # filename="pfChina_withSig.tif" # filename="pfChina.tif" # width=2000
# grid.newpage()
# grid.draw(slope2.pfChina) # grid.draw(slope.pfChina) # grid.draw((All.vegetation_MODIS+All.vegetation_GIMMS)/(All.vegetation_CAS+All.vegetation.plt_GIMMS))  # grid
# vp = viewport(x=.215, y=.215, width=.25, height=.25) # x=.215
# pushViewport(vp)
# grid.draw(as.grob(Annual.mean2.pfChina)) # ggplotify
# upViewport()
# dev.off()

# rm(test_spdf,ras,ras2,vp)
# save.image("D:/Plantation/pf_10km.RData") # save.image("Z:/ck/xc/Plantation/pf.RData")

# Significance layer
# spdf_data<-SpatialPointsDataFrame(coords = as.matrix(test_df2[,1:2]), test_df2,
#                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
# pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
# ras_data<-raster(pix_data[, "layer"]) # RasterLayer # 'slope_ndviProv2'
# writeRaster(ras_data, filename="Z:/ck/xc/Plantation/ras_data.tif",
#             format="GTiff",  # specify output format - GeoTIFF
#             overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
#             NAflag=-9999) # set no data value to -9999
# spdf_data<-SpatialPointsDataFrame(coords = as.matrix(test2_df2[,1:2]), test2_df2,
#                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) #SpatialPointsDataFrame
# pix_data<-SpatialPixelsDataFrame(spdf_data, spdf_data@data)  # SpatialPixelsDataFrame # pix will be used later
# ras_data<-raster(pix_data[, "layer"]) # RasterLayer # 'slope_ndviProv2'
# writeRaster(ras_data, filename="Z:/ck/xc/Plantation/ras2_data.tif",
#             format="GTiff",  # specify output format - GeoTIFF
#             overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
#             NAflag=-9999) # set no data value to -9999

write.csv(test_df2[test_df2$layer==1,], "Z:/ck/xc/Plantation/test_df2Pj_layersIs1.csv") # test_df2Ag_layersIs1.csv
write.csv(test2_df2[test2_df2$layer==1,], "Z:/ck/xc/Plantation/test2_df2Pj_layersIs1.csv") # test2_df2Ag_layersIs1.csv


# No change ---------------------------------

# For 1 km
# For 10 km
wd1<-"D:/Plantation/_10km" # "Z:/ck/xc/Plantation"                                        #設定tif檔案的檔案夾位置
setwd(wd1)
filez <- list.files(path = wd1, pattern = ".tif")                       #取得檔案夾下的所有的tif檔
filez <- filez[grep("_10km[.]tif", filez)] 
r.list <- list()                                                       #開一個空的list

for(i in 1:length(filez)){ # length(filez)                                       #將r.list中的raster載入
  r.list[[i]] <- raster(paste(wd1, "/", filez[i], sep = ""))  
}
r.list <- lapply(r.list, maskRaster2, 128)
m <- do.call(merge, r.list)                                            #將所有的r.list合併
# m <- maskRaster2(m, 128)
par(mfrow=c(1,1))
plot(m)
writeRaster(m, filename="D:/Plantation/all_10km.tif",                          #寫出tif檔                               
            format="GTiff",  # specify output format - GeoTIFF
            overwrite=TRUE, # CAUTION: if this is true, it will overwrite an
            # existing file
            NAflag=-9999) # set no data value to -9999

par(mfrow=c(1,1))
# slope_pfChinaPj_1 <- raster::mask(slope_pfChinaPj$slope_pfChina.1*(-1/95), slope_pfChinaPj[[2]]) # Remove the background (insignificant)
# plot(slope_pfChinaPj_1)
nonmaskMatrix2<-function(data, raster, value){
  # Mask
  raster[raster==value] <- NA
  mask<-raster # prov2
  mask<-is.na(mask)==F
  mask[mask!=1]<-NA
  # plot(mask)
  
  # Masked area
  dataMask <- raster::mask(data, mask) # Will be used later
  # dataMask <- crop(data, mask) # has the same type, but a bit different values from dataMask?
  # plot(dataMask[[1]])
  dataMatrix <- rasterToPoints(dataMask) # "matrix" "array" # nlayers(data)+2 columns, has x, y
  # dataMatrix <- as.data.frame(dataMask) # data.frame # nlayers(data) columns, doesn't have x, y
  
  return(list(dataMask=dataMask, dataMatrix=dataMatrix))
}
slope_pfChinaPj_1 <- nonmaskMatrix2(slope_pfChinaPj$slope_pfChina.1*(-1/95), slope_pfChinaPj$slope_pfChina.1, 0)[[1]]
plot(slope_pfChinaPj_1) # does not include 0, but not proper cuz there can be 0
slope_pfChinaPj_11 <- raster::mask(slope_pfChinaPj$slope_pfChina.1*(-1/95), m)
plot(slope_pfChinaPj_11) # includes 0, any pixel once had planted forests

plot(slope_pfChinaPj_11) # 0 (no change) is NA
plot(slope_pfChinaPj_11<0) # a few
plot(slope_pfChinaPj_11>0) # more
plot(slope_pfChinaPj_11==0) # all is not 0


# slope2_pfChinaPj_1 <- raster::mask(slope2_pfChinaPj$slope2_pfChina.1*(-1/95)*2, slope2_pfChinaPj[[2]]) # Remove the background (all 0)
slope2_pfChinaPj_1 <- nonmaskMatrix2(slope2_pfChinaPj$slope2_pfChina.1*(-1/95)*2, slope2_pfChinaPj$slope2_pfChina.1, 0)[[1]]
plot(slope2_pfChinaPj_1)
slope2_pfChinaPj_11 <- raster::mask(slope2_pfChinaPj$slope2_pfChina.1*(-1/95)*2, m)
plot(slope2_pfChinaPj_11) # includes 0

plot(slope2_pfChinaPj_11) # 0 (no change) is NA
plot(slope2_pfChinaPj_11<0) # a few
plot(slope2_pfChinaPj_11>0) # more
plot(slope2_pfChinaPj_11==0) # all is not 0
writeRaster(slope_pfChinaPj_11, # one layer! # slope_pfChinaPj_1$dataMask, # raster(slope_pfChinaPj_1), 
            filename="Z:/ck/xc/Plantation/slope_pfAgChinaPj_11.tif", # filename="Z:/ck/xc/Plantation/slope_pfChinaPj_1.tif",
            format="GTiff",  # specify output format - GeoTIFF
            overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
            NAflag=-9999) # set no data value to -9999
writeRaster(slope2_pfChinaPj_11, # one layer! # slope2_pfChinaPj_1$dataMask, # raster(slope2_pfChinaPj_1), 
            filename="Z:/ck/xc/Plantation/slope2_pfAgChinaPj_11.tif", # filename="Z:/ck/xc/Plantation/slope2_pfChinaPj_1.tif",
            format="GTiff",  # specify output format - GeoTIFF
            overwrite=TRUE, # CAUTION: if this is true, it will overwrite an existing file
            NAflag=-9999) # set no data value to -9999

save.image("D:/Plantation/pfAg_10km.RData") # save.image("Z:/ck/xc/Plantation/pfAg.RData")


# Other tries ---------------------------------

# proj4string(raster(ras)) # "+proj=aea +lat_0=0 +lon_0=105 +lat_1=25 +lat_2=47 +x_0=0 +y_0=0 +ellps=krass +units=m +no_defs"
# rasPj <- project(ras, rast(prep))
# rasPj2 <- projectRaster(raster(ras), crs=proj4string(raster(prep)))
# proj4string(raster(rasPj)) # "+proj=longlat +datum=WGS84 +no_defs" # crs(rasPj)
# proj4string(raster(rasPj2)) # 
# plot(rasPj) # circular sector
# plot(rasPj2)
# hist(rasPj)
# hist(rasPj2)
# summary(rasPj)
# # pf95      
# # Min.   : 33.0  
# # 1st Qu.:128.0  
# # Median :128.0  
# # Mean   :125.7  
# # 3rd Qu.:128.0  
# # Max.   :128.0  
# # NA's   :21293  
# # Warning message:
# # [summary] used a sample  
# summary(rasPj2)

plot(rasPj!=128)
rasPjnon128<-maskRaster2(rasPj,128) # from NDVI3_v3.R
plot(rasPjnon128)
hist(rasPjnon128)
summary(rasPjnon128)
# pf95 
# Min.   : 33.00  
# 1st Qu.: 59.42  
# Median :108.26  
# Mean   : 93.50  
# 3rd Qu.:126.40  
# Max.   :128.00  
# NA's   :94893   
# Warning message:
# [summary] used a sample 

res(rasPj) # 0.008333333 0.008333333 # 0.01053287 0.01053287: 1 km # ??
ext(rasPj) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2504610567952, 55.5587943901263 (xmin, xmax, ymin, ymax)
ext(pfPj[[4]]) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2504610567952, 55.5587943901263 (xmin, xmax, ymin, ymax)
ext(prep) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2504610567952, 55.5587943901263 (xmin, xmax, ymin, ymax)
# ext(china2Pj) # SpatExtent : 73.498959634335, 135.087385221879, 3.83384342231996, 53.5584987447372 (xmin, xmax, ymin, ymax)

# rasEt<-extend(rasPj, prep) # retain its own and extend (but not exactly!) to the other's wider extents
# ext(rasEt) # SpatExtent : 68.2181003692466, 136.523788501424, 15.3624547127573, 55.5559020392628  (xmin, xmax, ymin, ymax)
# plot(rasEt)
# rasCr<-crop(rasEt, prep)
# ext(rasCr) # SpatExtent : 72.1995267553627, 136.197269406266, 16.2472161318942, 55.5559020392628 (xmin, xmax, ymin, ymax) # a bit different from ext(prep)
# plot(rasCr)

# rasRe<-projectRaster(raster(rasCr), raster(prep), method="ngb") # still works
# ext(rasRe) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2504610567952, 55.5587943901263 (xmin, xmax, ymin, ymax) # the same as ext(prep)
# plot(rasRe)

# ras4Pj<-project(rast(filenames[[4]]),"+proj=longlat +datum=WGS84 +no_defs")
# ras4Et<-extend(ras4Pj, prep) # retain its own and extend (but not exactly!) to the other's wider extents
# ext(ras4Et) # SpatExtent : 48.4484413778749, 155.102804408993, 8.63581680488636, 59.0544363213417 (xmin, xmax, ymin, ymax)
# plot(ras4Et)
# ras4Cr<-crop(ras4Et, prep)
# ext(ras4Cr) # SpatExtent : 72.1954941895696, 136.194492765825, 16.2501875228324, 55.5556542456375 (xmin, xmax, ymin, ymax) # a bit different from ext(prep)
# plot(ras4Cr)
# ras4Re<-projectRaster(raster(ras4Cr), raster(prep), method="ngb") # still works
# ext(ras4Re) # SpatExtent : 72.1960450229045, 136.196045022846, 16.2504610567952, 55.5587943901263 (xmin, xmax, ymin, ymax) # the same as ext(prep)
# plot(ras4Re)
# pf4<-c(ras4Re,rasRe) # need the same extent, otherwise "Error: [rast] extents do not match"
