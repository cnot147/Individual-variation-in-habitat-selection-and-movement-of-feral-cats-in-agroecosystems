#Cat
Cat <- read.csv("Cat_data_NZTM.csv")

Cat$id <- as.factor(Cat$id)

#' Make timestamp a date/time variable 


Cat$GMT_Time <- dmy_hm(as.character(Cat$GMT_Time), tz ="GMT")
trk <- mk_track(Cat, .x=POINT_X, .y=POINT_Y, .t = GMT_Time, id = id, 
                crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

#Cat 1##########

dat1 <- trk %>% 
  filter(id == "1") 


step_dat1 <- dat1 %>% 
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


step_dat1$hour <- hour(step_dat1$t2_)  
step_dat1$tod <- ifelse(step_dat1$hour<7 | 19<step_dat1$hour, "Day","Night")

step_dat1 <- step_dat1 %>% filter(case_ == "TRUE")
step_Vegetation_dat1 <- step_dat1 %>% filter(Vegetation == "1")
step_pasture_dat1 <- step_dat1 %>% filter(Pasture == "1")

#Cat 2##########

dat2 <- trk %>% 
  filter(id == "2") 


step_dat2 <- dat2 %>% 
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



step_dat2$hour <- hour(step_dat2$t2_)  
step_dat2$tod <- ifelse(step_dat2$hour<7 | 19<step_dat2$hour, "Day","Night")

step_dat2 <- step_dat2 %>% filter(case_ == "TRUE")
step_Vegetation_dat2 <- step_dat2 %>% filter(Vegetation == "1")
step_pasture_dat2 <- step_dat2 %>% filter(Pasture == "1")

#Cat 3##########

dat3 <- trk %>% 
  filter(id == "3") 

step_dat3 <- dat3 %>% 
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


step_dat3$hour <- hour(step_dat3$t2_)  
step_dat3$tod <- ifelse(step_dat3$hour<7 | 19<step_dat3$hour, "Day","Night")

step_dat3 <- step_dat3 %>% filter(case_ == "TRUE")
step_Vegetation_dat3 <- step_dat3 %>% filter(Vegetation == "1")
step_pasture_dat3 <- step_dat3 %>% filter(Pasture == "1")

#Cat 4##########

dat4 <- trk %>% 
  filter(id == "4") 

step_dat4 <- dat4 %>% 
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


step_dat4$hour <- hour(step_dat4$t2_)  
step_dat4$tod <- ifelse(step_dat4$hour<7 | 19<step_dat4$hour, "Day","Night")

step_dat4 <- step_dat4 %>% filter(case_ == "TRUE")
step_Vegetation_dat4 <- step_dat4 %>% filter(Vegetation == "1")
step_pasture_dat4 <- step_dat4 %>% filter(Pasture == "1")
#Cat 5##########

dat5 <- trk %>% 
  filter(id == "5") 

step_dat5 <- dat5 %>% 
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



step_dat5$hour <- hour(step_dat5$t2_)  
step_dat5$tod <- ifelse(step_dat5$hour<7 | 19<step_dat5$hour, "Day","Night")

step_dat5 <- step_dat5 %>% filter(case_ == "TRUE")
step_Vegetation_dat5 <- step_dat5 %>% filter(Vegetation == "1")
step_pasture_dat5 <- step_dat5 %>% filter(Pasture == "1")
#Cat 6##########

dat6 <- trk %>% 
  filter(id == "6") 

step_dat6 <- dat6 %>% 
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


step_dat6$hour <- hour(step_dat6$t2_)  
step_dat6$tod <- ifelse(step_dat6$hour<7 | 19<step_dat6$hour, "Day","Night")

step_dat6 <- step_dat6 %>% filter(case_ == "TRUE")
step_Vegetation_dat6 <- step_dat6 %>% filter(Vegetation == "1")
step_pasture_dat6 <- step_dat6 %>% filter(Pasture == "1")
#Cat 7##########

dat7 <- trk %>% 
  filter(id == "7") 


step_dat7 <- dat7 %>% 
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

step_dat7$hour <- hour(step_dat7$t2_)  
step_dat7$tod <- ifelse(step_dat7$hour<7 | 19<step_dat7$hour, "Day","Night")

step_dat7 <- step_dat7 %>% filter(case_ == "TRUE")
step_Vegetation_dat7 <- step_dat7 %>% filter(Vegetation == "1")
step_pasture_dat7 <- step_dat7 %>% filter(Pasture == "1")

#Cat 8##########

dat8 <- trk %>% 
  filter(id == "8") 


step_dat8 <- dat8 %>% 
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


step_dat8$hour <- hour(step_dat8$t2_)  
step_dat8$tod <- ifelse(step_dat8$hour<7 | 19<step_dat8$hour, "Day","Night")

step_dat8 <- step_dat8 %>% filter(case_ == "TRUE")
step_Vegetation_dat8 <- step_dat8 %>% filter(Vegetation == "1")
step_pasture_dat8 <- step_dat8 %>% filter(Pasture == "1")
#Cat 9##########

dat9 <- trk %>% 
  filter(id == "9") 


step_dat9  <- dat9 %>% 
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


step_dat9$hour <- hour(step_dat9$t2_)  
step_dat9$tod <- ifelse(step_dat9$hour<7 | 19<step_dat9$hour, "Day","Night")

step_dat9 <- step_dat9 %>% filter(case_ == "TRUE")
step_Vegetation_dat9 <- step_dat9 %>% filter(Vegetation == "1")
step_pasture_dat9 <- step_dat9 %>% filter(Pasture == "1")

#Cat 10##########

dat10 <- trk %>% 
  filter(id == "10") 


step_dat10 <- dat10 %>% 
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


step_dat10$hour <- hour(step_dat10$t2_)  
step_dat10$tod <- ifelse(step_dat10$hour<7 | 19<step_dat10$hour, "Day","Night")

step_dat10 <- step_dat10 %>% filter(case_ == "TRUE")
step_Vegetation_dat10 <- step_dat10 %>% filter(Vegetation == "1")
step_pasture_dat10 <- step_dat10 %>% filter(Pasture == "1")

#Cat 11##########

dat11 <- trk %>% 
  filter(id == "11") 

step_dat11 <- dat11 %>% 
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


step_dat11$hour <- hour(step_dat11$t2_)  
step_dat11$tod <- ifelse(step_dat11$hour<7 | 19<step_dat11$hour, "Day","Night")

step_dat11 <- step_dat11 %>% filter(case_ == "TRUE")
step_Vegetation_dat11 <- step_dat11 %>% filter(Vegetation == "1")
step_pasture_dat11 <- step_dat11 %>% filter(Pasture == "1")
#Cat 12##########

dat12 <- trk %>% 
  filter(id == "12") 


step_dat12 <- dat12 %>% 
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


step_dat12$hour <- hour(step_dat12$t2_)  
step_dat12$tod <- ifelse(step_dat12$hour<7 | 19<step_dat12$hour, "Day","Night")

step_dat12 <- step_dat12 %>% filter(case_ == "TRUE")
step_Vegetation_dat12 <- step_dat12 %>% filter(Vegetation == "1")
step_pasture_dat12 <- step_dat12 %>% filter(Pasture == "1")
#Cat 13##########

dat13 <- trk %>% 
  filter(id == "13") 


step_dat13 <- dat13 %>% 
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


step_dat13$hour <- hour(step_dat13$t2_)  
step_dat13$tod <- ifelse(step_dat13$hour<7 | 19<step_dat13$hour, "Day","Night")

step_dat13 <- step_dat13 %>% filter(case_ == "TRUE")
step_Vegetation_dat13 <- step_dat13 %>% filter(Vegetation == "1")
step_pasture_dat13 <- step_dat13 %>% filter(Pasture == "1")
#Cat 14##########

dat14 <- trk %>% 
  filter(id == "14") 

step_dat14 <- dat14 %>% 
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


step_dat14$hour <- hour(step_dat14$t2_)  
step_dat14$tod <- ifelse(step_dat14$hour<7 | 19<step_dat14$hour, "Day","Night")

step_dat14 <- step_dat14 %>% filter(case_ == "TRUE")
step_Vegetation_dat14 <- step_dat14 %>% filter(Vegetation == "1")
step_pasture_dat14 <- step_dat14 %>% filter(Pasture == "1")

#All Data###########


step_dat1$id = 1
step_dat2$id = 2
step_dat3$id = 3
step_dat4$id = 4
step_dat5$id = 5
step_dat6$id = 6
step_dat7$id = 7
step_dat8$id = 8
step_dat9$id = 9
step_dat10$id = 10
step_dat11$id = 11
step_dat12$id = 12
step_dat13$id = 13
step_dat14$id = 14



step_dat_all <- rbind(step_dat1,step_dat2, step_dat3,step_dat4,step_dat5,step_dat6,step_dat7,step_dat8,step_dat9,step_dat10,step_dat11,step_dat12,step_dat13,step_dat14)



#IMPORT DATA interfix differences ############

step_dat_all <- readRDS("step_dat_all.rds")

step_Vegetation_all <- step_dat_all %>% filter(Vegetation == "1")
step_pasture_all <- step_dat_all %>% filter(Pasture == "1")



#Kolmogorov-Smirnov test###########


#Difference between Pasture and Vegetation ###########


ks.test(step_pasture_all$sl_, step_Vegetation_all$sl_)