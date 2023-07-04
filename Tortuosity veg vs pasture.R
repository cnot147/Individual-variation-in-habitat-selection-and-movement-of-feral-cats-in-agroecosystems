
#Cat
Cat <- read.csv("Cat_data_NZTM.csv")

Cat$id <- as.factor(Cat$id)

#' Make timestamp a date/time variable 


Cat$GMT_Time <- dmy_hm(as.character(Cat$GMT_Time), tz ="GMT")

trk <- mk_track(Cat, .x=POINT_X, .y=POINT_Y, .t = GMT_Time, id = id, 
                crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

#Calculate tortuosity

#Cat 1##########

dat1 <- trk %>% 
  filter(id == "1") 


Bssf_dat1 <- dat1 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)

Bssf_dat1$hour <- hour(Bssf_dat1$t_)  
Bssf_dat1$tod <- ifelse(Bssf_dat1$hour<7 | 19<Bssf_dat1$hour, "Day","Night")



# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg1 <-Bssf_dat1 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg1)


d.df1 = seg1 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df1 = mk_track(as.data.frame(d.df1), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df1$segment))
n = length(segments)
sinu1 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df1$segment)))) {
  sinu1[i,1] = trk.df1 %>% 
    filter(segment == na.omit(unique(d.df1$segment))[i]) %>%
    sinuosity()
}


sinu1<- as.data.frame(sinu1)

sinu1 <- sinu1 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu1)
sinu1$V1<-as.numeric(sinu1$V1)
sinu1_Vegetation <- sinu1 %>% filter(text %in% "Vegetation segment ")
sinu1_pasture <- sinu1 %>% filter(text %in% "Pasture segment ")  

#Cat 2##########

dat2 <- trk %>% 
  filter(id == "2") 


Bssf_dat2 <- dat2 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat2$hour <- hour(Bssf_dat2$t_)  
Bssf_dat2$tod <- ifelse(Bssf_dat2$hour<7 | 19<Bssf_dat2$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg2 <-Bssf_dat2 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg2)


d.df2 = seg2 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df2 = mk_track(as.data.frame(d.df2), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df2$segment))
n = length(segments)
sinu2 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df2$segment)))) {
  sinu2[i,1] = trk.df2 %>% 
    filter(segment == na.omit(unique(d.df2$segment))[i]) %>%
    sinuosity()
}


sinu2<- as.data.frame(sinu2)

sinu2 <- sinu2 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu2)
sinu2$V1<-as.numeric(sinu2$V1)
sinu2_Vegetation <- sinu2 %>% filter(text %in% "Vegetation segment ")
sinu2_pasture <- sinu2 %>% filter(text %in% "Pasture segment ")  

#Cat 3##########

dat3 <- trk %>% 
  filter(id == "3") 


Bssf_dat3 <- dat3 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat3$hour <- hour(Bssf_dat3$t_)  
Bssf_dat3$tod <- ifelse(Bssf_dat3$hour<7 | 19<Bssf_dat3$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg3 <-Bssf_dat3 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg3)


d.df3 = seg3 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df3 = mk_track(as.data.frame(d.df3), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df3$segment))
n = length(segments)
sinu3 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df3$segment)))) {
  sinu3[i,1] = trk.df3 %>% 
    filter(segment == na.omit(unique(d.df3$segment))[i]) %>%
    sinuosity()
}


sinu3<- as.data.frame(sinu3)

sinu3 <- sinu3 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu3)
sinu3$V1<-as.numeric(sinu3$V1)
sinu3_Vegetation <- sinu3 %>% filter(text %in% "Vegetation segment ")
sinu3_pasture <- sinu3 %>% filter(text %in% "Pasture segment ")  

#Cat 4##########

dat4 <- trk %>% 
  filter(id == "4") 


Bssf_dat4 <- dat4 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat4$hour <- hour(Bssf_dat4$t_)  
Bssf_dat4$tod <- ifelse(Bssf_dat4$hour<7 | 19<Bssf_dat4$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg4 <-Bssf_dat4 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg4)


d.df4 = seg4 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df4 = mk_track(as.data.frame(d.df4), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df4$segment))
n = length(segments)
sinu4 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df4$segment)))) {
  sinu4[i,1] = trk.df4 %>% 
    filter(segment == na.omit(unique(d.df4$segment))[i]) %>%
    sinuosity()
}


sinu4<- as.data.frame(sinu4)

sinu4 <- sinu4 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu4)
sinu4$V1<-as.numeric(sinu4$V1)
sinu4_Vegetation <- sinu4 %>% filter(text %in% "Vegetation segment ")
sinu4_pasture <- sinu4 %>% filter(text %in% "Pasture segment ")  

#Cat 5##########

dat5 <- trk %>% 
  filter(id == "5") 


Bssf_dat5 <- dat5 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat5$hour <- hour(Bssf_dat5$t_)  
Bssf_dat5$tod <- ifelse(Bssf_dat5$hour<7 | 19<Bssf_dat5$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg5 <-Bssf_dat5 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg5)


d.df5 = seg5 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df5 = mk_track(as.data.frame(d.df5), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df5$segment))
n = length(segments)
sinu5 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df5$segment)))) {
  sinu5[i,1] = trk.df5 %>% 
    filter(segment == na.omit(unique(d.df5$segment))[i]) %>%
    sinuosity()
}


sinu5<- as.data.frame(sinu5)

sinu5 <- sinu5 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu5)
sinu5$V1<-as.numeric(sinu5$V1)
sinu5_Vegetation <- sinu5 %>% filter(text %in% "Vegetation segment ")
sinu5_pasture <- sinu5 %>% filter(text %in% "Pasture segment ")  

#Cat 6##########

dat6 <- trk %>% 
  filter(id == "6") 


Bssf_dat6 <- dat6 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat6$hour <- hour(Bssf_dat6$t_)  
Bssf_dat6$tod <- ifelse(Bssf_dat6$hour<7 | 19<Bssf_dat6$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg6 <-Bssf_dat6 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg6)


d.df6 = seg6 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df6 = mk_track(as.data.frame(d.df6), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df6$segment))
n = length(segments)
sinu6 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df6$segment)))) {
  sinu6[i,1] = trk.df6 %>% 
    filter(segment == na.omit(unique(d.df6$segment))[i]) %>%
    sinuosity()
}


sinu6<- as.data.frame(sinu6)

sinu6 <- sinu6 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu6)
sinu6$V1<-as.numeric(sinu6$V1)
sinu6_Vegetation <- sinu6 %>% filter(text %in% "Vegetation segment ")
sinu6_pasture <- sinu6 %>% filter(text %in% "Pasture segment ")  


#Cat 7##########

dat7 <- trk %>% 
  filter(id == "7") 


Bssf_dat7 <- dat7 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat7$hour <- hour(Bssf_dat7$t_)  
Bssf_dat7$tod <- ifelse(Bssf_dat7$hour<7 | 19<Bssf_dat7$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg7 <-Bssf_dat7 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg7)


d.df7 = seg7 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df7 = mk_track(as.data.frame(d.df7), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df7$segment))
n = length(segments)
sinu7 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df7$segment)))) {
  sinu7[i,1] = trk.df7 %>% 
    filter(segment == na.omit(unique(d.df7$segment))[i]) %>%
    sinuosity()
}


sinu7<- as.data.frame(sinu7)

sinu7 <- sinu7 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu7)
sinu7$V1<-as.numeric(sinu7$V1)
sinu7_Vegetation <- sinu7 %>% filter(text %in% "Vegetation segment ")
sinu7_pasture <- sinu7 %>% filter(text %in% "Pasture segment ")  


#Cat 8##########

dat8 <- trk %>% 
  filter(id == "8") 


Bssf_dat8 <- dat8 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat8$hour <- hour(Bssf_dat8$t_)  
Bssf_dat8$tod <- ifelse(Bssf_dat8$hour<7 | 19<Bssf_dat8$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg8 <-Bssf_dat8 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg8)


d.df8 = seg8 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df8 = mk_track(as.data.frame(d.df8), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df8$segment))
n = length(segments)
sinu8 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df8$segment)))) {
  sinu8[i,1] = trk.df8 %>% 
    filter(segment == na.omit(unique(d.df8$segment))[i]) %>%
    sinuosity()
}


sinu8<- as.data.frame(sinu8)

sinu8 <- sinu8 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu8)
sinu8$V1<-as.numeric(sinu8$V1)
sinu8_Vegetation <- sinu8 %>% filter(text %in% "Vegetation segment ")
sinu8_pasture <- sinu8 %>% filter(text %in% "Pasture segment ")  


#Cat 9##########

dat9 <- trk %>% 
  filter(id == "9") 


Bssf_dat9 <- dat9 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat9$hour <- hour(Bssf_dat9$t_)  
Bssf_dat9$tod <- ifelse(Bssf_dat9$hour<7 | 19<Bssf_dat9$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg9 <-Bssf_dat9 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg9)


d.df9 = seg9 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df9 = mk_track(as.data.frame(d.df9), .x=x_, .y=y_, .t = t_, segment = segment, 
                   crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df9$segment))
n = length(segments)
sinu9 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df9$segment)))) {
  sinu9[i,1] = trk.df9 %>% 
    filter(segment == na.omit(unique(d.df9$segment))[i]) %>%
    sinuosity()
}


sinu9<- as.data.frame(sinu9)

sinu9 <- sinu9 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu9)
sinu9$V1<-as.numeric(sinu9$V1)
sinu9_Vegetation <- sinu9 %>% filter(text %in% "Vegetation segment ")
sinu9_pasture <- sinu9 %>% filter(text %in% "Pasture segment ")  


#Cat 10##########

dat10 <- trk %>% 
  filter(id == "10") 


Bssf_dat10 <- dat10 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat10$hour <- hour(Bssf_dat10$t_)  
Bssf_dat10$tod <- ifelse(Bssf_dat10$hour<7 | 19<Bssf_dat10$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg10 <-Bssf_dat10 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg10)


d.df10 = seg10 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df10 = mk_track(as.data.frame(d.df10), .x=x_, .y=y_, .t = t_, segment = segment, 
                    crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df10$segment))
n = length(segments)
sinu10 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df10$segment)))) {
  sinu10[i,1] = trk.df10 %>% 
    filter(segment == na.omit(unique(d.df10$segment))[i]) %>%
    sinuosity()
}


sinu10<- as.data.frame(sinu10)

sinu10 <- sinu10 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu10)
sinu10$V1<-as.numeric(sinu10$V1)
sinu10_Vegetation <- sinu10 %>% filter(text %in% "Vegetation segment ")
sinu10_pasture <- sinu10 %>% filter(text %in% "Pasture segment ")  


#Cat 11##########

dat11 <- trk %>% 
  filter(id == "11") 


Bssf_dat11 <- dat11 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)

Bssf_dat11$hour <- hour(Bssf_dat11$t_)  
Bssf_dat11$tod <- ifelse(Bssf_dat11$hour<7 | 19<Bssf_dat11$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg11 <-Bssf_dat11 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg11)


d.df11 = seg11 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df11 = mk_track(as.data.frame(d.df11), .x=x_, .y=y_, .t = t_, segment = segment, 
                    crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df11$segment))
n = length(segments)
sinu11 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df11$segment)))) {
  sinu11[i,1] = trk.df11 %>% 
    filter(segment == na.omit(unique(d.df11$segment))[i]) %>%
    sinuosity()
}


sinu11<- as.data.frame(sinu11)

sinu11 <- sinu11 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu11)
sinu11$V1<-as.numeric(sinu11$V1)
sinu11_Vegetation <- sinu11 %>% filter(text %in% "Vegetation segment ")
sinu11_pasture <- sinu11 %>% filter(text %in% "Pasture segment ")  


#Cat 12##########

dat12 <- trk %>% 
  filter(id == "12") 


Bssf_dat12 <- dat12 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat12$hour <- hour(Bssf_dat12$t_)  
Bssf_dat12$tod <- ifelse(Bssf_dat12$hour<7 | 19<Bssf_dat12$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg12 <-Bssf_dat12 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg12)


d.df12 = seg12 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df12 = mk_track(as.data.frame(d.df12), .x=x_, .y=y_, .t = t_, segment = segment, 
                    crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df12$segment))
n = length(segments)
sinu12 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df12$segment)))) {
  sinu12[i,1] = trk.df12 %>% 
    filter(segment == na.omit(unique(d.df12$segment))[i]) %>%
    sinuosity()
}


sinu12<- as.data.frame(sinu12)

sinu12 <- sinu12 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu12)
sinu12$V1<-as.numeric(sinu12$V1)
sinu12_Vegetation <- sinu12 %>% filter(text %in% "Vegetation segment ")
sinu12_pasture <- sinu12 %>% filter(text %in% "Pasture segment ")  


#Cat 13##########

dat13 <- trk %>% 
  filter(id == "13") 


Bssf_dat13 <- dat13 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat13$hour <- hour(Bssf_dat13$t_)  
Bssf_dat13$tod <- ifelse(Bssf_dat13$hour<7 | 19<Bssf_dat13$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg13 <-Bssf_dat13 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg13)


d.df13 = seg13 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df13 = mk_track(as.data.frame(d.df13), .x=x_, .y=y_, .t = t_, segment = segment, 
                    crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df13$segment))
n = length(segments)
sinu13 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df13$segment)))) {
  sinu13[i,1] = trk.df13 %>% 
    filter(segment == na.omit(unique(d.df13$segment))[i]) %>%
    sinuosity()
}


sinu13<- as.data.frame(sinu13)

sinu13 <- sinu13 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu13)
sinu13$V1<-as.numeric(sinu13$V1)
sinu13_Vegetation <- sinu13 %>% filter(text %in% "Vegetation segment ")
sinu13_pasture <- sinu13 %>% filter(text %in% "Pasture segment ")  


#Cat 14##########

dat14 <- trk %>% 
  filter(id == "14") 


Bssf_dat14 <- dat14 %>% 
  extract_covariates(Vegetation) %>%
  extract_covariates(Pasture)


Bssf_dat14$hour <- hour(Bssf_dat14$t_)  
Bssf_dat14$tod <- ifelse(Bssf_dat14$hour<7 | 19<Bssf_dat14$hour, "Day","Night")


# define function to get run data
run_func = function(x, lab) {
  # get run data
  rr = rle(x)
  # set a flag to identify true runs when Vegetation/pasture/shrub = 1 or run > 5
  run_flag = ifelse(rr$values == 1 & rr$lengths > 4, 1, 0)
  # label Vegetation/pasture/shrub runs using above logic and rep labels app lengths
  run_x_lab = ifelse(run_flag == 0, NA, paste(lab, cumsum(run_flag)))
  rep(run_x_lab, rr$lengths)
}

# update data.frame with new variables
seg14 <-Bssf_dat14 %>%
  mutate(run_Vegetation = run_func(Vegetation, lab = "Vegetation segment"),
         run_pasture = run_func(Pasture, lab = "Pasture segment"))

class(seg14)


d.df14 = seg14 %>%
  pivot_longer(c("run_Vegetation", "run_pasture"), names_to = "segment_type", values_to = "segment")

trk.df14 = mk_track(as.data.frame(d.df14), .x=x_, .y=y_, .t = t_, segment = segment, 
                    crs = CRS("+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +units=m + datum=WGS84"))

segments = na.omit(unique(d.df14$segment))
n = length(segments)
sinu14 = cbind(NA, segments)
for(i in 1:length(na.omit(unique(d.df14$segment)))) {
  sinu14[i,1] = trk.df14 %>% 
    filter(segment == na.omit(unique(d.df14$segment))[i]) %>%
    sinuosity()
}


sinu14<- as.data.frame(sinu14)

sinu14 <- sinu14 %>%
  separate(segments, 
           into = c("text", "num"), 
           sep = "(?<=[A-Z a-z])(?=[0-9])"
  )

str(sinu14)
sinu14$V1<-as.numeric(sinu14$V1)
sinu14_Vegetation <- sinu14 %>% filter(text %in% "Vegetation segment ")
sinu14_pasture <- sinu14 %>% filter(text %in% "Pasture segment ")  


sinu_all <- rbind(sinu1,sinu2, sinu3,sinu4,sinu5,sinu6,sinu7,sinu8,sinu9,sinu10,sinu11,sinu12,sinu13,sinu14)
Q <- quantile(sinu_all$V1, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(sinu_all$V1)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range???

eliminated <- subset(sinu_all, sinu_all$V1 > (Q[1] - 1.5*iqr) & sinu_all$V1 < (Q[2]+1.5*iqr))
sinu_all <-eliminated

#Kolmogorov-Smirnov test###########

#Difference between Pasture and Vegetation - tortuosity###########
sinu_veg <-  sinu_all %>% filter(text == "Vegetation segment ")
sinu_pas <-  sinu_all %>% filter(text == "Pasture segment ")    

ks.test(sinu_pas$V1, sinu_veg$V1)

