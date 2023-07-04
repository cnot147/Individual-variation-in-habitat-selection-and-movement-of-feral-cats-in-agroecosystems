library(adehabitatHR)
library(sf)
library(rgdal)
library(sp)
library(tidyverse)


# Read the csv file

## List filenames to be merged. 
filenames <- list.files(path="/CleanGPSdata/",pattern="*.csv")

## Print filenames to be merged
print(filenames)

## Full path to csv filenames
fullpath=file.path("/CleanGPSdata",filenames)

## Print Full Path to the files
print(fullpath)

## Merge listed files from the path above
Cat <- do.call("rbind",lapply(fullpath,FUN=function(files){ read.csv(files)}))



Cat.sp <- Cat[, c("id", "Latitude", "Longitude")] 


#convert latlong to xy

head(cat.xy)

cat.dec = SpatialPoints(cbind(Cat$Longitude, Cat$Latitude), proj4string=CRS("+proj=longlat"))
cat.xy <- spTransform(cat.dec, CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"))

cat.xy <- cbind(Cat.sp$id, cat.xy$coords.x1, cat.xy$coords.x2)
cat.xy <- as.data.frame(cat.xy)

# Create a SpatialPointsDataFrame by defining the coordinates - EPSG:2193
coordinates(cat.xy ) <- c("V2", "V3")


proj4string(cat.xy)  <- CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m")

# MCP

cat.mcp <- mcp(cat.xy, percent = 100)

cat.mcp

summary(cat.mcp)

hrs <- mcp.area(cat.xy, percent = seq(50, 100, by = 5))

mcp.area(cat.xy, percent = 95)

write.xlsx(hrs, "C:/Users/cnot147/OneDrive - The University of Auckland/PhD/Chapter 2 Resource Selection/Home Range/MCP.xlsx") 

#KDE
# 5. Domain                 HERE GRID IS INCREASED 50 AT X AND Y!!
x <- seq(-10000, 10000, by=1.) # resolution is the pixel size you desire 
y <- seq(-10000, 10000, by=1.)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)

str(cat.xy)

# 6. Kernel Density
kernel.ref <- kernelUD(cat.xy, h = "href")
kernal <-kernel.area(kernel.ref)
image(kernel.ref)


# 7. Get the Volum
vud_points <- getvolumeUD(kernel.ref)

# 8. Get contour
levels <- c(50, 75, 95)
list <- vector(mode="list", length = 2)

list[[1]] <- as.image.SpatialGridDataFrame(vud_points[[1]])
list[[2]] <- as.image.SpatialGridDataFrame(vud_points[[2]])
list[[3]] <- as.image.SpatialGridDataFrame(vud_points[[3]])
list[[4]] <- as.image.SpatialGridDataFrame(vud_points[[4]])
list[[5]] <- as.image.SpatialGridDataFrame(vud_points[[5]])
list[[6]] <- as.image.SpatialGridDataFrame(vud_points[[6]])
list[[7]] <- as.image.SpatialGridDataFrame(vud_points[[7]])
list[[8]] <- as.image.SpatialGridDataFrame(vud_points[[8]])
list[[9]] <- as.image.SpatialGridDataFrame(vud_points[[9]])
list[[10]] <- as.image.SpatialGridDataFrame(vud_points[[10]])
list[[11]] <- as.image.SpatialGridDataFrame(vud_points[[11]])
list[[12]] <- as.image.SpatialGridDataFrame(vud_points[[12]])
list[[13]] <- as.image.SpatialGridDataFrame(vud_points[[13]])
list[[14]] <- as.image.SpatialGridDataFrame(vud_points[[14]])


#Import home range and cat id data
HRV <- read.csv("C:/Users/cnot1/OneDrive - The University of Auckland/PhD/Chapter 2 Resource Selection/Home Range/Catdata.csv")

with(HRV, shapiro.test(X95KDE[Sex == "Male"]))
with(HRV, shapiro.test(X95KDE[Sex == "Female"]))
with(HRV, shapiro.test(X50KDE[Sex == "Male"]))
with(HRV, shapiro.test(X50KDE[Sex == "Female"]))


#not normally distributed

#two-samples Wilcoxon test
res <- wilcox.test(X95KDE ~ Sex, data = HRV,
                   exact = FALSE,conf.int = TRUE)
res

res <- wilcox.test(X50KDE ~ Sex, data = HRV,
                   exact = FALSE,conf.int = TRUE)
res

#two-samples Wilcoxon test
res <- wilcox.test(X95MCP ~ Sex, data = HRV,
                   exact = FALSE,conf.int = TRUE)
res

res <- wilcox.test(X50MCP ~ Sex, data = HRV,
                   exact = FALSE,conf.int = TRUE)
res
