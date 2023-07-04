
library(survival) # for running the clogit (standard conditional logit)
library(MASS) # for mvrnorm
library(dplyr) # for data manipulations
library(ezknitr) # for creating html file
library(ggplot2)
library(KernSmooth)
library(devtools)

library(githubinstall)
githubinstall("uhcplots")

library(uhcplots)


library(splitTools)

ssf_dat_all <- readRDS("ssf_dat_all3.rds")



ssf_dat_all$tod <- as.factor(ssf_dat_all$tod)


##Subset data

ids <- splitTools::partition(
  ssf_dat_all$step_id_,
  p = c(train = 0.75, test = 0.02),
  type = "grouped"
)

mdat.train <- ssf_dat_all[ids$train, ]

mdat.test <- ssf_dat_all[ids$test, ]

##Specify Parameters########
#Habitat  model####
form1r <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub+sl_+strata(step_id_))
form2r <- ~ Forest+Pasture+Forest_diffuse+Shrub+sl_-1
textplotr <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+sl_+strata(step_id_)))

##Fit SSF models###########
###Full####

ssf.train.hab <- clogit(form1r, data=mdat.train)

summary(ssf.train.hab)

##UHC plots#########

head(design.mat.test.hab)[,-6]
head(z)
head(mdat.test)

###Full model####
design.mat.test.hab <- model.matrix(form2r, data=mdat.test)

z <- z.r <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub+sl_-1, 
                         data = mdat.test)[,-6]
xchoice.hab <- uhcsimstrat(nsims = 1000,
                           xmat = design.mat.test.hab[,-6], 
                           stratum = mdat.test$step_id_, 
                           fit_ssf = ssf.train.hab,
                           z = z.r) 


denshats.Forest.hab <- uhcdenscalc(rand_sims = xchoice.hab[,,1], 
                                   dat = z.r[mdat.test$case_==1,1], 
                                   avail = z.r[mdat.test$case_==0,1]) 
denshats.Pasture.hab <- uhcdenscalc(rand_sims=xchoice.hab[,,2], 
                                    dat=z.r[mdat.test$case_==1,2], 
                                    avail=z.r[mdat.test$case_==0,2])  
denshats.Forest_diffuse.hab <- uhcdenscalc(rand_sims=xchoice.hab[,,3], 
                                           dat=z.r[mdat.test$case_==1,3], 
                                           avail=z.r[mdat.test$case_==0,3])  
denshats.shrub.hab <- uhcdenscalc(rand_sims=xchoice.hab[,,4], 
                                  dat=z.r[mdat.test$case_==1,4], 
                                  avail=z.r[mdat.test$case_==0,4]) 

##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.")
par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.hab$densdat, 
            densrand = denshats.Forest.hab$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.hab$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.hab$densdat,
            densrand = denshats.Pasture.hab$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.hab$densavail) 

mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.hab$densdat, 
            densrand = denshats.Forest_diffuse.hab$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.hab$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.shrub.hab$densdat, 
            densrand = denshats.shrub.hab$densrand,
            includeAvail = TRUE, 
            densavail = denshats.shrub.hab$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)





##Habitat + tod:Habitat model####

textplot1 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1



##Fit SSF models###########
###Full####

ssf.train.full <- clogit(form1a, data=mdat.train)

summary(ssf.train.full)

##UHC plots#########

head(design.mat.test.full)[,-6]
head(z)
head(mdat.test)

###Full model####
design.mat.test.full <- model.matrix(form2a, data=mdat.test)

z <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                  data = mdat.test)[,-6]
xchoice.full <- uhcsimstrat(nsims = 1000,
                            xmat = design.mat.test.full[,-6], 
                            stratum = mdat.test$step_id_, 
                            fit_ssf = ssf.train.full,
                            z = z) 

#z <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub+sl_-1, data = mdat.test)


#xchoice.full <- uhcsimstrat(nsims = 1000,
#                             xmat = design.mat.test.full, 
#                            stratum = mdat.test$step_id_, 
#                           fit_ssf = ssf.train.full,
#                          z = z)    

denshats.Forest.full <- uhcdenscalc(rand_sims = xchoice.full[,,1], 
                                    dat = z[mdat.test$case_==1,1], 
                                    avail = z[mdat.test$case_==0,1]) 
denshats.Pasture.full <- uhcdenscalc(rand_sims=xchoice.full[,,2], 
                                     dat=z[mdat.test$case_==1,2], 
                                     avail=z[mdat.test$case_==0,2])  
denshats.Forest_diffuse.full <- uhcdenscalc(rand_sims=xchoice.full[,,3], 
                                            dat=z[mdat.test$case_==1,3], 
                                            avail=z[mdat.test$case_==0,3])  
denshats.shrub.full <- uhcdenscalc(rand_sims=xchoice.full[,,4], 
                                   dat=z[mdat.test$case_==1,4], 
                                   avail=z[mdat.test$case_==0,4]) 
denshats.night.Forest.full <- uhcdenscalc(rand_sims=xchoice.full[,,6], 
                                          dat=z[mdat.test$case_==1,6],
                                          avail=z[mdat.test$case_==0,6])

denshats.night.Pasture.full <- uhcdenscalc(rand_sims=xchoice.full[,,7], 
                                           dat=z[mdat.test$case_==1,7],
                                           avail=z[mdat.test$case_==0,7])

denshats.night.Forest_diffuse.full <- uhcdenscalc(rand_sims=xchoice.full[,,8], 
                                                  dat=z[mdat.test$case_==1,8],
                                                  avail=z[mdat.test$case_==0,8])

denshats.night.shrub.full <- uhcdenscalc(rand_sims=xchoice.full[,,9], 
                                         dat=z[mdat.test$case_==1,9], 
                                         avail=z[mdat.test$case_==0,9]) 



##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full$densdat, 
            densrand = denshats.Forest.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full$densdat,
            densrand = denshats.Pasture.full$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full$densdat, 
            densrand = denshats.Forest_diffuse.full$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.shrub.full$densdat, 
            densrand = denshats.shrub.full$densrand,
            includeAvail = TRUE, 
            densavail = denshats.shrub.full$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full$densdat, 
            densrand = denshats.sl.full$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full$densdat, 
            densrand = denshats.night.Forest.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full$densdat, 
            densrand = denshats.night.Pasture.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full$densdat, 
            densrand = denshats.night.Forest_diffuse.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.shrub.full$densdat, 
            densrand = denshats.night.shrub.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.shrub.full$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.2 <- recordPlot()


##############################
##Specify Parameters########

#habitat + sex:habitat ####
textplots <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1s <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +Sex:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2s <- ~ Forest+Pasture+Forest_diffuse+Shrub+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1



##Fit SSF models###########
###Full####

ssf.train.sex <- clogit(form1s, data=mdat.train)

summary(ssf.train.sex)

##UHC plots#########

head(design.mat.test.sex)[,-6]
head(z.s)
head(mdat.test)

###Full model####
design.mat.test.sex <- model.matrix(form2s, data=mdat.test)

z.s <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +Sex:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test)[,-6]
xchoice.sex <- uhcsimstrat(nsims = 1000,
                           xmat = design.mat.test.sex[,-6], 
                           stratum = mdat.test$step_id_, 
                           fit_ssf = ssf.train.sex,
                           z = z.s) 



denshats.Forest.sex <- uhcdenscalc(rand_sims = xchoice.sex[,,1], 
                                   dat = z.s[mdat.test$case_==1,1], 
                                   avail = z.s[mdat.test$case_==0,1]) 
denshats.Pasture.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,2], 
                                    dat=z.s[mdat.test$case_==1,2], 
                                    avail=z.s[mdat.test$case_==0,2])  
denshats.Forest_diffuse.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,3], 
                                           dat=z.s[mdat.test$case_==1,3], 
                                           avail=z.s[mdat.test$case_==0,3])  
denshats.shrub.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,4], 
                                  dat=z.s[mdat.test$case_==1,4], 
                                  avail=z.s[mdat.test$case_==0,4]) 
denshats.sex.Forest.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,6], 
                                       dat=z.s[mdat.test$case_==1,6],
                                       avail=z.s[mdat.test$case_==0,6])

denshats.sex.Pasture.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,7], 
                                        dat=z.s[mdat.test$case_==1,7],
                                        avail=z.s[mdat.test$case_==0,7])
denshats.sex.Forest_diffuse.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,8], 
                                               dat=z.s[mdat.test$case_==1,8],
                                               avail=z.s[mdat.test$case_==0,8])

denshats.sex.shrub.sex <- uhcdenscalc(rand_sims=xchoice.sex[,,9], 
                                      dat=z.s[mdat.test$case_==1,9], 
                                      avail=z.s[mdat.test$case_==0,9]) 



##Create UHC plot ####

#Hab:sex model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.sex$densdat, 
            densrand = denshats.Forest.sex$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.sex$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.sex$densdat,
            densrand = denshats.Pasture.sex$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.sex$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.sex$densdat, 
            densrand = denshats.Forest_diffuse.sex$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.sex$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.shrub.sex$densdat, 
            densrand = denshats.shrub.sex$densrand,
            includeAvail = TRUE, 
            densavail = denshats.shrub.sex$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)




uhcdensplot(densdat = denshats.sex.Forest.sex$densdat, 
            densrand = denshats.sex.Forest.sex$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Forest.sex$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Forest", cex=1.4)

uhcdensplot(densdat = denshats.sex.Pasture.sex$densdat, 
            densrand = denshats.sex.Pasture.sex$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Pasture.sex$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Pasture", cex=1.4)

uhcdensplot(densdat = denshats.sex.Forest_diffuse.sex$densdat, 
            densrand = denshats.sex.Forest_diffuse.sex$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Forest_diffuse.sex$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.sex.shrub.sex$densdat, 
            densrand = denshats.sex.shrub.sex$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.shrub.sex$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplots, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.sex.2 <- recordPlot()




##habitat + tod:habitat + sex:habitat####
textplot1 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+Sex:(Forest+Pasture+Forest_diffuse+Shrub) +tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))


form1b <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2b <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1
textplotb <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))




##Fit SSF models###########
###Full####

ssf.train.full.b <- clogit(form1b, data=mdat.train)

summary(ssf.train.full.b)

##UHC plots#########

head(design.mat.test.full)[,-6]
head(z)
head(mdat.test)

###Full model####
design.mat.test.full.b <- model.matrix(form2b, data=mdat.test)

z.b <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +Sex:(Forest+Pasture+Forest_diffuse+Shrub)+tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test)[,-6]
xchoice.full.b <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.b[,-6], 
                              stratum = mdat.test$step_id_, 
                              fit_ssf = ssf.train.full.b,
                              z = z.b) 

denshats.Forest.full.b <- uhcdenscalc(rand_sims = xchoice.full.b[,,1], 
                                      dat = z.b[mdat.test$case_==1,1], 
                                      avail = z.b[mdat.test$case_==0,1]) 
denshats.Pasture.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,2], 
                                       dat=z.b[mdat.test$case_==1,2], 
                                       avail=z.b[mdat.test$case_==0,2])  
denshats.Forest_diffuse.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,3], 
                                              dat=z.b[mdat.test$case_==1,3], 
                                              avail=z.b[mdat.test$case_==0,3])  
denshats.shrub.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,4], 
                                     dat=z.b[mdat.test$case_==1,4], 
                                     avail=z.b[mdat.test$case_==0,4]) 
denshats.night.Forest.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,6], 
                                            dat=z.b[mdat.test$case_==1,6],
                                            avail=z.b[mdat.test$case_==0,6])

denshats.night.Pasture.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,7], 
                                             dat=z.b[mdat.test$case_==1,7],
                                             avail=z.b[mdat.test$case_==0,7])
denshats.night.Forest_diffuse.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,8], 
                                                    dat=z.b[mdat.test$case_==1,8],
                                                    avail=z.b[mdat.test$case_==0,8])

denshats.night.shrub.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,9], 
                                           dat=z.b[mdat.test$case_==1,9], 
                                           avail=z.b[mdat.test$case_==0,9]) 




denshats.sex.Forest.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,10], 
                                          dat=z.b[mdat.test$case_==1,10],
                                          avail=z.b[mdat.test$case_==0,10])

denshats.sex.Pasture.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,11], 
                                           dat=z.b[mdat.test$case_==1,11],
                                           avail=z.b[mdat.test$case_==0,11])
denshats.sex.Forest_diffuse.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,12], 
                                                  dat=z.b[mdat.test$case_==1,12],
                                                  avail=z.b[mdat.test$case_==0,12])

denshats.sex.shrub.full.b <- uhcdenscalc(rand_sims=xchoice.full.b[,,13], 
                                         dat=z.b[mdat.test$case_==1,13], 
                                         avail=z.b[mdat.test$case_==0,13]) 


##Create UHC plot####

###Full model#####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.", "J.", "K.", "L.")
par(mfrow=c(3,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.b$densdat, 
            densrand = denshats.Forest.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.b$densdat,
            densrand = denshats.Pasture.full.b$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.b$densdat, 
            densrand = denshats.Forest_diffuse.full.b$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)


uhcdensplot(densdat = denshats.shrub.full.b$densdat, 
            densrand = denshats.shrub.full.b$densrand,
            includeAvail = TRUE, 
            densavail = denshats.shrub.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.night.Forest.full.b$densdat, 
            densrand = denshats.night.Forest.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.b$densdat, 
            densrand = denshats.night.Pasture.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.b$densdat, 
            densrand = denshats.night.Forest_diffuse.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest_diffuse", cex=1.4)
uhcdensplot(densdat = denshats.night.shrub.full.b$densdat, 
            densrand = denshats.night.shrub.full.b$densrand,
            includeAvail = TRUE, 
            densavail = denshats.night.shrub.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sex.Forest.full.b$densdat, 
            densrand = denshats.sex.Forest.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Sex Forest", cex=1.4)

uhcdensplot(densdat = denshats.sex.Pasture.full.b$densdat, 
            densrand = denshats.sex.Pasture.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Pasture.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Sex Pasture", cex=1.4)

uhcdensplot(densdat = denshats.sex.Forest_diffuse.full.b$densdat, 
            densrand = denshats.sex.Forest_diffuse.full.b$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Forest_diffuse.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Sex Forestdiffuse", cex=1.4)

uhcdensplot(densdat = denshats.sex.shrub.full.b$densdat, 
            densrand = denshats.sex.shrub.full.b$densrand,
            includeAvail = TRUE,
            densavail = denshats.sex.shrub.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Sex Shrub", cex=1.4)


mtext(outer=T, side=3, line=3,  textplotb, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)



uhcdensplot(densdat = denshats.sl.full.b$densdat, 
            densrand = denshats.sl.full.b$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.b$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

plot.uhc.full <- recordPlot()



##Hab + tod:hab + sex:hab + sex:tod:hab#################


textploto <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1o <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2o <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub) +Sex:tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1




##Fit SSF models###########
###Full####

ssf.train.full.o <- clogit(form1o, data=mdat.train)

summary(ssf.train.full.o)

##UHC plots#########

head(design.mat.test.full.o)[,-6]
head(z)
head(mdat.test)

###Full model####
design.mat.test.full.o <- model.matrix(form2o, data=mdat.test)

z.o <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+Sex:(Forest+Pasture+Forest_diffuse+Shrub) +Sex:tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1, 
                    data = mdat.test)[,-6]
xchoice.full.o <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.o[,-6], 
                              stratum = mdat.test$step_id_, 
                              fit_ssf = ssf.train.full.o,
                              z = z.o) 

denshats.Forest.full.o <- uhcdenscalc(rand_sims = xchoice.full.o[,,1], 
                                      dat = z.o[mdat.test$case_==1,1], 
                                      avail = z.o[mdat.test$case_==0,1]) 
denshats.Pasture.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,2], 
                                       dat=z.o[mdat.test$case_==1,2], 
                                       avail=z.o[mdat.test$case_==0,2])  
denshats.Forest_diffuse.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,3], 
                                              dat=z.o[mdat.test$case_==1,3], 
                                              avail=z.o[mdat.test$case_==0,3])  
denshats.shrub.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,4], 
                                     dat=z.o[mdat.test$case_==1,4], 
                                     avail=z.o[mdat.test$case_==0,4]) 
denshats.night.Forest.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,6], 
                                            dat=z.o[mdat.test$case_==1,6],
                                            avail=z.o[mdat.test$case_==0,6])

denshats.night.Pasture.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,7], 
                                             dat=z.o[mdat.test$case_==1,7],
                                             avail=z.o[mdat.test$case_==0,7])
denshats.night.Forest_diffuse.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,8], 
                                                    dat=z.o[mdat.test$case_==1,8],
                                                    avail=z.o[mdat.test$case_==0,8])

denshats.night.shrub.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,9], 
                                           dat=z.o[mdat.test$case_==1,9], 
                                           avail=z.o[mdat.test$case_==0,9]) 




denshats.sex.Forest.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,10], 
                                          dat=z.o[mdat.test$case_==1,10],
                                          avail=z.o[mdat.test$case_==0,10])

denshats.sex.Pasture.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,11], 
                                           dat=z.o[mdat.test$case_==1,11],
                                           avail=z.o[mdat.test$case_==0,11])
denshats.sex.Forest_diffuse.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,12], 
                                                  dat=z.o[mdat.test$case_==1,12],
                                                  avail=z.o[mdat.test$case_==0,12])

denshats.sex.shrub.full.o <- uhcdenscalc(rand_sims=xchoice.full.o[,,13], 
                                         dat=z.o[mdat.test$case_==1,13], 
                                         avail=z.o[mdat.test$case_==0,13]) 

denshats.male.night.Forest.full <- uhcdenscalc(rand_sims=xchoice.full.o[,,14], 
                                               dat=z.o[mdat.test$case_==1,14],
                                               avail=z.o[mdat.test$case_==0,14])

denshats.male.night.Pasture.full <- uhcdenscalc(rand_sims=xchoice.full.o[,,15], 
                                                dat=z.o[mdat.test$case_==1,15],
                                                avail=z.o[mdat.test$case_==0,15])

denshats.male.night.Forest_diffuse.full <- uhcdenscalc(rand_sims=xchoice.full.o[,,16], 
                                                       dat=z.o[mdat.test$case_==1,16],
                                                       avail=z.o[mdat.test$case_==0,16])

denshats.male.night.shrub.full <- uhcdenscalc(rand_sims=xchoice.full.o[,,17], 
                                              dat=z.o[mdat.test$case_==1,17], 
                                              avail=z.o[mdat.test$case_==0,17]) 


#denshats.sl.full <- uhcdenscalc(rand_sims=xchoice.full[,,5], 
dat=z[mdat.test$case_==1,5],
avail=z[mdat.test$case_==0,5]) 


##Create UHC plot####

###Full model#####
panlabs1 <- c("A)","B)", "C)", "D)", "E)", "F)", "G)", "H", "I", "J", "K", "L", "M", "N", "O", "P")
par(mfrow=c(4,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.o$densdat, 
            densrand = denshats.Forest.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.o$densdat,
            densrand = denshats.Pasture.full.o$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.o$densdat, 
            densrand = denshats.Forest_diffuse.full.o$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)


uhcdensplot(densdat = denshats.shrub.full.o$densdat, 
            densrand = denshats.shrub.full.o$densrand,
            includeAvail = TRUE, 
            densavail = denshats.shrub.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.night.Forest.full.o$densdat, 
            densrand = denshats.night.Forest.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.o$densdat, 
            densrand = denshats.night.Pasture.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest_diffuse.full.o$densdat, 
            densrand = denshats.night.Forest_diffuse.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest_diffuse", cex=1.4)
uhcdensplot(densdat = denshats.night.shrub.full.o$densdat, 
            densrand = denshats.night.shrub.full.o$densrand,
            includeAvail = TRUE, 
            densavail = denshats.night.shrub.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)


uhcdensplot(densdat = denshats.sex.Forest.full.o$densdat, 
            densrand = denshats.sex.Forest.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Forest.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Forest", cex=1.4)

uhcdensplot(densdat = denshats.sex.Pasture.full.o$densdat, 
            densrand = denshats.sex.Pasture.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Pasture.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[10], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Pasture", cex=1.4)
uhcdensplot(densdat = denshats.sex.Forest_diffuse.full.o$densdat, 
            densrand = denshats.sex.Forest_diffuse.full.o$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sex.Forest_diffuse.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[11], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Forest_diffuse", cex=1.4)
uhcdensplot(densdat = denshats.sex.shrub.full.o$densdat, 
            densrand = denshats.sex.shrub.full.o$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sex.shrub.full.o$densavail) 
mtext(side=3, line=1,  panlabs1[12], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Male Shrub", cex=1.4)



uhcdensplot(densdat = denshats.male.night.Forest.full$densdat, 
            densrand = denshats.male.night.Forest.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.male.night.Forest.full$densavail) 
mtext(side=3, line=1,  panlabs1[13], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Male Forest", cex=1.4)

uhcdensplot(densdat = denshats.male.night.Pasture.full$densdat, 
            densrand = denshats.male.night.Pasture.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.male.night.Pasture.full$densavail) 
mtext(side=3, line=1,  panlabs1[14], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Male Pasture", cex=1.4)
uhcdensplot(densdat = denshats.male.night.Forest_diffuse.full$densdat, 
            densrand = denshats.male.night.Forest_diffuse.full$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.male.night.Forest_diffuse.full$densavail) 
mtext(side=3, line=1,  panlabs1[15], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Male Forest_diffuse", cex=1.4)
uhcdensplot(densdat = denshats.male.night.shrub.full$densdat, 
            densrand = denshats.male.night.shrub.full$densrand,
            includeAvail = TRUE, 
            densavail = denshats.male.night.shrub.full$densavail) 
mtext(side=3, line=1,  panlabs1[16], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Male Shrub", cex=1.4)



mtext(outer=T, side=3, line=3,  textploto, cex=1.8)


plot.uhc.full <- recordPlot()


