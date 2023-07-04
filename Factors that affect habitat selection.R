#Packages
library(lubridate)
library(raster)
library(amt)
library(tidyverse)
library(move)
library(sp)
library(rgdal)
library(ggplot2)
library(ggpubr)
library(AICcmodavg)

#Landuse raster

simp.landuse <- raster("Simple_landuse.tif")

Vegetation <- simp.landuse == 1
Other <- simp.landuse == 2
Pasture <- simp.landuse == 3

names(Vegetation) <- "Vegetation"
names(Pasture) <- "Pasture"
names(Other) <- "Other"

#Cat
Cat <- read.csv("Cat_data_NZTM.csv")

Cat$id <- as.factor(Cat$id)

#' Make timestamp a date/time variable 


Cat$GMT_Time <- dmy_hm(as.character(Cat$GMT_Time), tz ="GMT")

trk <- mk_track(Cat, .x=POINT_X, .y=POINT_Y, .t = GMT_Time, id = id, 
                crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

# All cats: step selection function
m1 <- dat_all  %>% 
  mutate(ssf = lapply(dat_all, function(x) {
    x %>% track_resample(rate = minutes(10), tolerance = minutes(2)) %>%
      filter_min_n_burst(min_n = 3) %>%
      steps_by_burst() %>% random_steps(n=10) %>%
      extract_covariates(Forest) %>%
      extract_covariates(Pasture)%>%
      extract_covariates(Forest_diffuse) %>%
      extract_covariates(Shrub) %>%
      fit_issf(case_ ~  Forest+Pasture+Forest_diffuse+Shrub +strata(step_id_))   
  }))


#To run each step selection function for each cat and extract coefficients from the model

##Cat 1

dat1 <- trk %>% 
  filter(id == "1") 

ssf_SLU1 <- dat1 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU1<-as_tibble(ssf_SLU1)

SLU.cat1 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU1)

summary(SLU.cat1 )

w1 <- summary(SLU.cat1 $model)$coefficients
w1 <- cbind(Landuse = rownames(w1), w1)

w1.df <- as.data.frame(w1)
w1.df <- w1.df %>%
  add_column(id = c("1","1"),  .before = "coef")


##Cat 2

dat2 <- trk %>% 
  filter(id == "2") 

ssf_SLU2 <- dat2 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU2<-as_tibble(ssf_SLU2)

SLU.cat2 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU2)

summary(SLU.cat2)

w2 <- summary(SLU.cat2$model)$coefficients
w2 <- cbind(Landuse = rownames(w2), w2)

w2.df <- as.data.frame(w2)
w2.df <- w2.df %>%
  add_column(id = c("2","2"),  .before = "coef")

##Cat 3

dat3 <- trk %>% 
  filter(id == "3") 

ssf_SLU3 <- dat3 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU3<-as_tibble(ssf_SLU3)

SLU.cat3 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU3)

summary(SLU.cat3 )

w3 <- summary(SLU.cat3$model)$coefficients
w3 <- cbind(Landuse = rownames(w3), w3)

w3.df <- as.data.frame(w3)
w3.df <- w3.df %>%
  add_column(id = c("3","3"),  .before = "coef")

##Cat 4

dat4 <- trk %>% 
  filter(id == "4") 

ssf_SLU4 <- dat4 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU4<-as_tibble(ssf_SLU4)

SLU.cat4 <- fit_issf(case_ ~ Vegetation +strata(step_id_), 
                     data = ssf_SLU4)

summary(SLU.cat4 )

w4 <- summary(SLU.cat4 $model)$coefficients
w4 <- cbind(Landuse = rownames(w4), w4)

w4.df <- as.data.frame(w4)
w4.df <- w4.df %>%
  add_column(id = c("4"),  .before = "coef")

##Cat 5

dat5 <- trk %>% 
  filter(id == "5") 

ssf_SLU5 <- dat5 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU5<-as_tibble(ssf_SLU5)

SLU.cat5 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU5)

summary(SLU.cat5)

w5 <- summary(SLU.cat5 $model)$coefficients
w5 <- cbind(Landuse = rownames(w5), w5)

w5.df <- as.data.frame(w5)
w5.df <- w5.df %>%
  add_column(id = c("5","5"),  .before = "coef")

##Cat 6

dat6 <- trk %>% 
  filter(id == "6") 

ssf_SLU6 <- dat6 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU6<-as_tibble(ssf_SLU6)

SLU.cat6 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU6)

summary(SLU.cat6 )

w6<- summary(SLU.cat6 $model)$coefficients
w6 <- cbind(Landuse = rownames(w6), w6)

w6.df <- as.data.frame(w6)
w6.df <- w6.df %>%
  add_column(id = c("6","6"),  .before = "coef")

##Cat 7

dat7 <- trk %>% 
  filter(id == "7") 

ssf_SLU7 <- dat7 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU7<-as_tibble(ssf_SLU7)

SLU.cat7 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU7)

summary(SLU.cat7 )

w7 <- summary(SLU.cat7$model)$coefficients
w7 <- cbind(Landuse = rownames(w7), w7)

w7.df <- as.data.frame(w7)
w7.df <- w7.df %>%
  add_column(id = c("7","7"),  .before = "coef")

##Cat 8

dat8 <- trk %>% 
  filter(id == "8") 

ssf_SLU8 <- dat8 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU8<-as_tibble(ssf_SLU8)

SLU.cat8 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU8)

summary(SLU.cat8)

w8 <- summary(SLU.cat8$model)$coefficients
w8 <- cbind(Landuse = rownames(w8), w8)

w8.df <- as.data.frame(w8)
w8.df <- w8.df %>%
  add_column(id = c("8","8"),  .before = "coef")

##Cat 9

dat9 <- trk %>% 
  filter(id == "9") 

ssf_SLU9 <- dat9 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU9<-as_tibble(ssf_SLU9)

SLU.cat9 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                     data = ssf_SLU9)

summary(SLU.cat9)

w9 <- summary(SLU.cat9$model)$coefficients
w9 <- cbind(Landuse = rownames(w9), w9)

w9.df <- as.data.frame(w9)
w9.df <- w9.df %>%
  add_column(id = c("9","9"),  .before = "coef")

##Cat 10

dat10 <- trk %>% 
  filter(id == "10") 

ssf_SLU10 <- dat10 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU10<-as_tibble(ssf_SLU10)

SLU.cat10 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                      data = ssf_SLU10)

summary(SLU.cat10 )

w10 <- summary(SLU.cat10 $model)$coefficients
w10 <- cbind(Landuse = rownames(w10), w10)

w10.df <- as.data.frame(w10)
w10.df <- w10.df %>%
  add_column(id = c("10","10"),  .before = "coef")

##Cat 11

dat11 <- trk %>% 
  filter(id == "11") 

ssf_SLU11 <- dat11 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU11<-as_tibble(ssf_SLU11)

SLU.cat11 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                      data = ssf_SLU11)

summary(SLU.cat11 )

w11 <- summary(SLU.cat11 $model)$coefficients
w11 <- cbind(Landuse = rownames(w11), w11)

w11.df <- as.data.frame(w11)
w11.df <- w11.df %>%
  add_column(id = c("11","11"),  .before = "coef")

##Cat 12

dat1 <- trk %>% 
  filter(id == "12") 

ssf_SLU12 <- dat12 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU12<-as_tibble(ssf_SLU12)

SLU.cat12 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                      data = ssf_SLU12)

summary(SLU.cat12 )

w12 <- summary(SLU.cat12 $model)$coefficients
w12 <- cbind(Landuse = rownames(w12), w12)

w12.df <- as.data.frame(w12)
w12.df <- w12.df %>%
  add_column(id = c("12","12"),  .before = "coef")

##Cat 13

dat13 <- trk %>% 
  filter(id == "13") 

ssf_SLU13 <- dat13 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU13<-as_tibble(ssf_SLU13)

SLU.cat13 <- fit_issf(case_ ~ Vegetation+Pasture +strata(step_id_), 
                      data = ssf_SLU13)

summary(SLU.cat13 )

w13 <- summary(SLU.cat13 $model)$coefficients
w13 <- cbind(Landuse = rownames(w13), w13)

w13.df <- as.data.frame(w13)
w13.df <- w13.df %>%
  add_column(id = c("13","13"),  .before = "coef")

##Cat 14

dat14 <- trk %>% 
  filter(id == "14") 

ssf_SLU14 <- dat14 %>% 
  track_resample(rate = minutes(10), tolerance = minutes(2)) %>% 
  filter_min_n_burst(3) %>% 
  steps_by_burst() %>%
  time_of_day(include.crepuscule = FALSE) %>%
  random_steps(n = 100) %>%
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)%>%
  extract_covariates(Other) %>%
  mutate(cos_ta_ = cos(ta_), 
         log_sl_ = log(sl_+0.1))


ssf_SLU14<-as_tibble(ssf_SLU14)

SLU.cat14 <- fit_issf(case_ ~ Vegetation +strata(step_id_), 
                      data = ssf_SLU14)

summary(SLU.cat14 )

w14 <- summary(SLU.cat14 $model)$coefficients
w14 <- cbind(Landuse = rownames(w14), w14)

w14.df <- as.data.frame(w14)
w14.df <- w14.df %>%
  add_column(id = c("14"),  .before = "coef")


###Landscape characteristic metrics######
land <- read.csv("run1.land.csv")
class <- read.csv("run2.class.csv")

str(land)

lmetrics <- land %>% dplyr::select(EuclideanNearestNeighdist_AM, Edge_Density, Patch_AREA_weightedMean, Patch_AREA_Median)

cl <- dplyr::filter(class, TYPE == " cls_1 ") %>% dplyr::select(FRAC_AM, CLUMPY, ED,  AREA_AM,  AREA_MD,  ENN_AM)

cl$ENN_AM <- as.numeric(cl$ENN_AM)
metrics <- cbind(lmetrics,cl)

##Data exploration #####

##Look at data for outliers
par(mfrow= c (1,2), mar = c(5,4,2,1))
boxplot(veg$coef,  ylab = "coef")
dotchart(veg$coef, xlab = "coef",
         ylab = "Cat")

library(lattice)
Z <- cbind(metrics$EuclideanNearestNeighdist_AM, metrics$Edge_Density,  metrics$Patch_AREA_weightedMean, metrics$Patch_AREA_Median,
           metrics$FRAC_AM,  metrics$CLUMPY)

colnames(Z) <- c("EuclideanNearestNeigh", "Edge Density", "Patch Area weighted mean", "Patch_AREA_Median",
                 "FRAC_AM", "CLUMPY")

dotplot(as.matrix(Z), groups = FALSE,
        strip = strip.custom(bg = 'white',
                             par.strip.text = list(cex = 0.8)),
        scales = list(x = list(relation = "free"),
                      y = list(relation = "free"),
                      draw = FALSE),
        col = 1, cex  = 0.5, pch = 16,
        xlab = "Value of the variable",
        ylab = "Order of the data from text file")

metric.cor <-cor(scaled.cl)


scaled.metrics <- scale(metrics)

scaled.cl <- scale(cl)


#Cat data
HRV <- read.csv("Catdata.csv")


#Put all data into one dataframe
allmetrics <- cbind(veg,scaled.metrics, HRV)


#Six canidate models for factors affecting habitat selection
coef.mod <- list()

coef.mod[[1]]<-lm(coef~ 1, data =allmetrics)
coef.mod[[2]]<-lm(coef~ ED+ AREA_AM ,  data =allmetrics)
coef.mod[[3]]<-lm(coef~ ED+ AREA_AM +Rabbit, data =allmetrics)

coef.mod[[4]]<-lm(coef~ ED, data =allmetrics)
coef.mod[[5]]<-lm(coef~ AREA_AM, data =allmetrics)

coef.mod[[6]]<-lm(coef~ Rabbit, data =allmetrics)


Modnames <- c("Intercept", "ED+ AREA_AM", 
              "ED+ AREA_AM + Rabbit",  
              "Rabbit", 
              "ED","AREA_AM")


coef.aicc.table <-aictab(cand.set = coef.mod, modnames = Modnames)


#Testing model assumptions


simout1  <-  simulateResiduals(coef.mod[[1]],  n=250)
plot(simout1) 

simout2  <-  simulateResiduals(coef.mod[[2]],  n=250)
plot(simout2) 

simout3  <-  simulateResiduals(coef.mod[[3]],  n=250)
plot(simout3) 

simout4  <-  simulateResiduals(coef.mod[[4]],  n=250)
plot(simout4) 

simout5  <-  simulateResiduals(coef.mod[[5]],  n=250)
plot(simout5) 

simout6  <-  simulateResiduals(coef.mod[[6]],  n=250)
plot(simout6) 
