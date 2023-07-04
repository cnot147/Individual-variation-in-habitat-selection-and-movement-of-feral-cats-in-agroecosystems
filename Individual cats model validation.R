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

ssf_dat_all <- readRDS("ssf_dat_all2.rds")


ssf_dat_all$tod <- as.factor(ssf_dat_all$tod)
str(ssf_dat_all)

#Cat 1########
dat1 <- ssf_dat_all %>% 
  filter(id == "1") 

##Subset data

ids1 <- splitTools::partition(
  dat1$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.1 <- dat1[ids1$train, ]
mdat.test.1 <- dat1[ids1$test, ]

##Specify Parameters########

###Full model####
textplot1.1 <- (expression(y %~% Forest+Pasture+Forest_diffuse+tod:(Forest+Pasture+Forest_diffuse)+sl_+strata(step_id_)))
form1a.1 <- (case_ ~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse)+sl_+strata(step_id_))
form2a.1 <- ~ Forest+Pasture+Forest_diffuse+tod:(Forest+Pasture+Forest_diffuse)+sl_-1


###Reduced model 1####

textplot2.1 <- (expression(y %~% Forest+Pasture+Forest_diffuse+ sl_+ strata(step_id_)))
form1b.1 <- (case_ ~ Forest+Pasture+Forest_diffuse+ sl_+ strata(step_id_))
form2b.1 <- ~ Forest+Pasture+Forest_diffuse+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.1 <- clogit(form1a.1, data=mdat.train.1)

ssf.train.full.1

###Reduced Model####
ssf.train.reduc1.1 <- clogit(form1b.1, data=mdat.train.1)

ssf.train.reduc1.1


##UHC plots#########
head(design.mat.test.full.1)
head(design.mat.test.full.1[,-5])
head(z.1)
head(mdat.test.1)

###Full model####
design.mat.test.full.1 <- model.matrix(form2a.1, data=mdat.test.1)
z.1 <- model.matrix(~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse) +sl_-1, 
                    data = mdat.test.1)[,-5]
xchoice.full.1 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.1[,-5], 
                              stratum = mdat.test.1$step_id_, 
                              fit_ssf = ssf.train.full.1,
                              z = z.1)    

denshats.Forest.full.1 <- uhcdenscalc(rand_sims = xchoice.full.1[,,1], 
                                      dat = z.1[mdat.test.1$case_==1,1], 
                                      avail = z.1[mdat.test.1$case_==0,1]) 
denshats.Pasture.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,2], 
                                       dat=z.1[mdat.test.1$case_==1,2], 
                                       avail=z.1[mdat.test.1$case_==0,2])  
denshats.Forest_diffuse.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,3], 
                                              dat=z.1[mdat.test.1$case_==1,3], 
                                              avail=z.1[mdat.test.1$case_==0,3])   
denshats.sl.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,4], 
                                  dat=z.1[mdat.test.1$case_==1,4],
                                  avail=z.1[mdat.test.1$case_==0,4]) 
denshats.night.Forest.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,5], 
                                            dat=z.1[mdat.test.1$case_==1,5],
                                            avail=z.1[mdat.test.1$case_==0,5])

denshats.night.Pasture.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,6], 
                                             dat=z.1[mdat.test.1$case_==1,6],
                                             avail=z.1[mdat.test.1$case_==0,6])
denshats.night.Forest_diffuse.full.1 <- uhcdenscalc(rand_sims=xchoice.full.1[,,7], 
                                                    dat=z.1[mdat.test.1$case_==1,7],
                                                    avail=z.1[mdat.test.1$case_==0,7])



###Reduced model#####

design.mat.test.reduc1.1 <- model.matrix(form2b.1, data=mdat.test.1)
z.1 <- model.matrix(~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse) +sl_-1, 
                    data = mdat.test.1)[,-5]
xchoice.reduc1.1 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.1, 
                                stratum = mdat.test.1$step_id_, 
                                fit_ssf = ssf.train.reduc1.1,
                                z = z.1)    

denshats.Forest.reduc1.1 <- uhcdenscalc(rand_sims = xchoice.reduc1.1[,,1], 
                                        dat = z.1[mdat.test.1$case_==1,1], 
                                        avail = z.1[mdat.test.1$case_==0,1]) 
denshats.Pasture.reduc1.1 <- uhcdenscalc(rand_sims=xchoice.reduc1.1[,,2], 
                                         dat=z.1[mdat.test.1$case_==1,2], 
                                         avail=z.1[mdat.test.1$case_==0,2])  
denshats.Forest_diffuse.reduc1.1 <- uhcdenscalc(rand_sims=xchoice.reduc1.1[,,3], 
                                                dat=z.1[mdat.test.1$case_==1,3], 
                                                avail=z.1[mdat.test.1$case_==0,3]) 
denshats.sl.reduc1.1 <- uhcdenscalc(rand_sims=xchoice.reduc1.1[,,4], 
                                    dat=z.1[mdat.test.1$case_==1,4],
                                    avail=z.1[mdat.test.1$case_==0,4])

##Create UHC plot####

###Full model#####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.1$densdat, 
            densrand = denshats.Forest.full.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.1$densdat,
            densrand = denshats.Pasture.full.1$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.1$densdat, 
            densrand = denshats.Forest_diffuse.full.1$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)


uhcdensplot(densdat = denshats.sl.full.1$densdat, 
            densrand = denshats.sl.full.1$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest.full.1$densdat, 
            densrand = denshats.night.Forest.full.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.1$densdat, 
            densrand = denshats.night.Pasture.full.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest_diffuse.full.1$densdat, 
            densrand = denshats.night.Forest_diffuse.full.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.1$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot1.1, cex=1.8)



plot.uhc.full.1 <- recordPlot()

###Reduced plot####
panlabs2 <- c("E)","F)", "G)")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.1$densdat, 
            densrand = denshats.Forest.reduc1.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.1$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)
uhcdensplot(densdat = denshats.Pasture.reduc1.1$densdat,
            densrand = denshats.Pasture.reduc1.1$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.1$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.1$densdat, 
            densrand = denshats.Forest_diffuse.reduc1.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.1$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.1$densdat, 
            densrand = denshats.sl.reduc1.1$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.1$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.1, cex=1.8)

plot.uhc.reduc.1 <- recordPlot()

##UHC Plot - Difference between observed and predicted#####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.1$densdat, 
                densrand = denshats.Forest.full.1$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.1$densdat, 
                densrand = denshats.Pasture.full.1$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.1$densdat, 
                densrand = denshats.night.Forest.full.1$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.1$densdat, 
                densrand = denshats.night.Pasture.full.1$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.1$densdat, 
                densrand = denshats.Forest.reduc1.1$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.1$densdat, 
                densrand = denshats.Pasture.reduc1.1$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.1 <- recordPlot()

#Cat 2####
dat2 <- ssf_dat_all %>% 
  filter(id == "2")

##Subset data#####

ids2 <- splitTools::partition(
  dat2$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.2 <- dat2[ids2$train, ]
mdat.test.2 <- dat2[ids2$test, ]

##Specify Parameters########

###Full model####
textplot1.2 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a.2 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a.2 <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1


###Reduced model 1####

textplot2.2 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_)))
form1b.2 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_))
form2b.2 <- ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.2 <- clogit(form1a.2, data=mdat.train.2)
ssf.train.full.2

###Reduced Model####
ssf.train.reduc1.2 <- clogit(form1b.2, data=mdat.train.2)
ssf.train.reduc1.2


##UHC plots#########

head(design.mat.test.full.2[,-6])
head(z.2)
###Full model####
design.mat.test.full.2 <- model.matrix(form2a.2, data=mdat.test.2)
z.2 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.2)[,-6]
xchoice.full.2 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.2[,-6], 
                              stratum = mdat.test.2$step_id_, 
                              fit_ssf = ssf.train.full.2,
                              z = z.2)    

denshats.Forest.full.2 <- uhcdenscalc(rand_sims = xchoice.full.2[,,1], 
                                      dat = z.2[mdat.test.2$case_==1,1], 
                                      avail = z.2[mdat.test.2$case_==0,1]) 
denshats.Pasture.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,2], 
                                       dat=z.2[mdat.test.2$case_==1,2], 
                                       avail=z.2[mdat.test.2$case_==0,2])  


denshats.Forest_diffuse.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,3], 
                                              dat=z.2[mdat.test.2$case_==1,3], 
                                              avail=z.2[mdat.test.2$case_==0,3]) 

denshats.Shrub.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,4], 
                                     dat=z.2[mdat.test.2$case_==1,4], 
                                     avail=z.2[mdat.test.2$case_==0,4]) 

denshats.sl.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,5], 
                                  dat=z.2[mdat.test.2$case_==1,5], 
                                  avail=z.2[mdat.test.2$case_==0,5]) 

denshats.night.Forest.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,6], 
                                            dat=z.2[mdat.test.2$case_==1,6],
                                            avail=z.2[mdat.test.2$case_==0,6])

denshats.night.Pasture.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,7], 
                                             dat=z.2[mdat.test.2$case_==1,7],
                                             avail=z.2[mdat.test.2$case_==0,7])

denshats.night.Forest_diffuse.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,8], 
                                                    dat=z.2[mdat.test.2$case_==1,8],
                                                    avail=z.2[mdat.test.2$case_==0,8])

denshats.night.Shrub.full.2 <- uhcdenscalc(rand_sims=xchoice.full.2[,,9], 
                                           dat=z.2[mdat.test.2$case_==1,9],
                                           avail=z.2[mdat.test.2$case_==0,9])


###Reduced model####

design.mat.test.reduc1.2 <- model.matrix(form2b.2, data=mdat.test.2)
z.2 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.2)[,-6]
xchoice.reduc1.2 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.2, 
                                stratum = mdat.test.2$step_id_, 
                                fit_ssf = ssf.train.reduc1.2,
                                z = z.2)    

denshats.Forest.reduc1.2 <- uhcdenscalc(rand_sims = xchoice.reduc1.2[,,1], 
                                        dat = z.2[mdat.test.2$case_==1,1], 
                                        avail = z.2[mdat.test.2$case_==0,1]) 

denshats.Pasture.reduc1.2 <- uhcdenscalc(rand_sims=xchoice.reduc1.2[,,2], 
                                         dat=z.2[mdat.test.2$case_==1,2], 
                                         avail=z.2[mdat.test.2$case_==0,2])  

denshats.Forest_diffuse.reduc1.2 <- uhcdenscalc(rand_sims=xchoice.reduc1.2[,,3], 
                                                dat=z.2[mdat.test.2$case_==1,3], 
                                                avail=z.2[mdat.test.2$case_==0,3]) 

denshats.Shrub.reduc1.2 <- uhcdenscalc(rand_sims=xchoice.reduc1.2[,,4], 
                                       dat=z.2[mdat.test.2$case_==1,4],
                                       avail=z.2[mdat.test.2$case_==0,4])

denshats.sl.reduc1.2 <- uhcdenscalc(rand_sims=xchoice.reduc1.2[,,5], 
                                    dat=z.2[mdat.test.2$case_==1,5],
                                    avail=z.2[mdat.test.2$case_==0,5])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A)","B)", "C)", "D)", "E)", "F)", "G)", "H)", "I)")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.2$densdat, 
            densrand = denshats.Forest.full.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.2$densdat,
            densrand = denshats.Pasture.full.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.2$densdat, 
            densrand = denshats.Forest_diffuse.full.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.2$densdat, 
            densrand = denshats.Shrub.full.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.2$densdat, 
            densrand = denshats.sl.full.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.2$densdat, 
            densrand = denshats.night.Forest.full.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.2$densdat, 
            densrand = denshats.night.Pasture.full.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.2$densdat, 
            densrand = denshats.night.Forest_diffuse.full.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.2$densdat, 
            densrand = denshats.night.Shrub.full.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.2$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.2, cex=1.8)

plot.uhc.full.2 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("J)","K)", "L)","M)", "N)")

par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.2$densdat, 
            densrand = denshats.Forest.reduc1.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.2$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.reduc1.2$densdat,
            densrand = denshats.Pasture.reduc1.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.2$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.2$densdat,
            densrand = denshats.Forest_diffuse.reduc1.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.2$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.2$densdat,
            densrand = denshats.Shrub.reduc1.2$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.2$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.2$densdat, 
            densrand = denshats.sl.reduc1.2$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.2$densavail) 
mtext(side=3, line=1,  panlabs2[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.2, cex=1.8)

plot.uhc.reduc.2 <- recordPlot()
##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.2$densdat, 
                densrand = denshats.Forest.full.2$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.2$densdat, 
                densrand = denshats.Pasture.full.2$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.2$densdat, 
                densrand = denshats.night.Forest.full.2$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.2$densdat, 
                densrand = denshats.night.Pasture.full.2$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.2$densdat, 
                densrand = denshats.Forest.reduc1.2$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.2$densdat, 
                densrand = denshats.Pasture.reduc1.2$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)


plot.uhc.differences.2 <- recordPlot()


#Cat 3####
dat3 <- ssf_dat_all %>% 
  filter(id == "3") 

##Subset data######

ids3 <- splitTools::partition(
  dat3$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.3 <- dat3[ids3$train, ]
mdat.test.3 <- dat3[ids3$test, ]

##Specify Parameters########

###Full model####
textplot1.3 <- (expression(y %~% Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_)))
form1a.3 <- (case_ ~ Forest+Pasture +Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_))
form2a.3 <- ~ Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.3 <- (expression(y %~% Forest+Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.3 <- (case_ ~ Forest+Pasture+Shrub+ sl_+ strata(step_id_))
form2b.3 <- ~ Forest+Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.3 <- clogit(form1a.3, data=mdat.train.3)
ssf.train.full.3

###Reduced Model####
ssf.train.reduc1.3 <- clogit(form1b.3, data=mdat.train.3)
ssf.train.reduc1.3


##UHC plots#########

head(design.mat.test.full[,-4])

###Full model####
design.mat.test.full.3 <- model.matrix(form2a.3, data=mdat.test.3)
z.3 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.3)[,-5]
xchoice.full.3 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.3[,-5], 
                              stratum = mdat.test.3$step_id_, 
                              fit_ssf = ssf.train.full.3,
                              z = z.3)    

denshats.Forest.full.3 <- uhcdenscalc(rand_sims = xchoice.full.3[,,1], 
                                      dat = z.3[mdat.test.3$case_==1,1], 
                                      avail = z.3[mdat.test.3$case_==0,1]) 
denshats.Pasture.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,2], 
                                       dat=z.3[mdat.test.3$case_==1,2], 
                                       avail=z.3[mdat.test.3$case_==0,2])  
denshats.Shrub.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,3], 
                                     dat=z.3[mdat.test.3$case_==1,3], 
                                     avail=z.3[mdat.test.3$case_==0,3])   
denshats.sl.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,4], 
                                  dat=z.3[mdat.test.3$case_==1,4],
                                  avail=z.3[mdat.test.3$case_==0,4]) 
denshats.night.Forest.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,5], 
                                            dat=z.3[mdat.test.3$case_==1,5],
                                            avail=z.3[mdat.test.3$case_==0,5])

denshats.night.Pasture.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,6], 
                                             dat=z.3[mdat.test.3$case_==1,6],
                                             avail=z.3[mdat.test.3$case_==0,6])
denshats.night.Shrub.full.3 <- uhcdenscalc(rand_sims=xchoice.full.3[,,7], 
                                           dat=z.3[mdat.test.3$case_==1,7],
                                           avail=z.3[mdat.test.3$case_==0,7])




###Reduced model####

design.mat.test.reduc1.3 <- model.matrix(form2b.3, data=mdat.test.3)
z.3 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.3)[,-5]
xchoice.reduc1.3 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.3, 
                                stratum = mdat.test.3$step_id_, 
                                fit_ssf = ssf.train.reduc1.3,
                                z = z.3)    

denshats.Forest.reduc1.3 <- uhcdenscalc(rand_sims = xchoice.reduc1.3[,,1], 
                                        dat = z.3[mdat.test.3$case_==1,1], 
                                        avail = z.3[mdat.test.3$case_==0,1]) 
denshats.Pasture.reduc1.3 <- uhcdenscalc(rand_sims=xchoice.reduc1.3[,,2], 
                                         dat=z.3[mdat.test.3$case_==1,2], 
                                         avail=z.3[mdat.test.3$case_==0,2])  

denshats.Shrub.reduc1.3 <- uhcdenscalc(rand_sims=xchoice.reduc1.3[,,3], 
                                       dat=z.3[mdat.test.3$case_==1,3], 
                                       avail=z.3[mdat.test.3$case_==0,3]) 
denshats.sl.reduc1.3 <- uhcdenscalc(rand_sims=xchoice.reduc1.3[,,4], 
                                    dat=z.3[mdat.test.3$case_==1,4],
                                    avail=z.3[mdat.test.3$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A)","B)", "C)", "D)", "E)", "F)", "G)")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.3$densdat, 
            densrand = denshats.Forest.full.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.3$densdat,
            densrand = denshats.Pasture.full.3$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.3$densdat, 
            densrand = denshats.Shrub.full.3$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.full.3$densdat, 
            densrand = denshats.sl.full.3$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.3$densdat, 
            densrand = denshats.night.Forest.full.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.3$densdat, 
            densrand = denshats.night.Pasture.full.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.3$densdat, 
            densrand = denshats.night.Shrub.full.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.3$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)


mtext(outer=T, side=3, line=3,  textplot1.3, cex=1.8)



plot.uhc.full.3 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H)","I)", "J)", "K)")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.3$densdat, 
            densrand = denshats.Forest.reduc1.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.3$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.reduc1.3$densdat,
            densrand = denshats.Pasture.reduc1.3$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.3$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.3$densdat, 
            densrand = denshats.Shrub.reduc1.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.3$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)


uhcdensplot(densdat = denshats.sl.reduc1.3$densdat, 
            densrand = denshats.sl.reduc1.3$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.3$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.3, cex=1.8)

plot.uhc.reduc.3 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.3$densdat, 
                densrand = denshats.Forest.full.3$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.3$densdat, 
                densrand = denshats.Pasture.full.3$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.3$densdat, 
                densrand = denshats.night.Forest.full.3$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.3$densdat, 
                densrand = denshats.night.Pasture.full.3$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.3$densdat, 
                densrand = denshats.Forest.reduc1.3$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.3$densdat, 
                densrand = denshats.Pasture.reduc1.3$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.3 <- recordPlot()


#Cat 4####
dat4 <- ssf_dat_all %>% 
  filter(id == "4") 

##Subset data######

ids4 <- splitTools::partition(
  dat4$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.4 <- dat4[ids4$train, ]
mdat.test.4 <- dat4[ids4$test, ]

##Specify Parameters########

###Full model####
textplot1.4 <- (expression(y %~% Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_)))
form1a.4 <- (case_ ~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_))
form2a.4 <- ~ Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.4 <- (expression(y %~% Forest+Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.4 <- (case_ ~ Forest+Pasture+Shrub+ sl_+ strata(step_id_))
form2b.4 <- ~ Forest+Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.4 <- clogit(form1a.4, data=mdat.train.4)
ssf.train.full.4

###Reduced Model####
ssf.train.reduc1.4 <- clogit(form1b.4, data=mdat.train.4)

ssf.train.reduc1.4


##UHC plots#########

head(design.mat.test.full.4[,-5])
head(z.4)
###Full model####
design.mat.test.full.4 <- model.matrix(form2a.4, data=mdat.test.4)
z.4 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture) +sl_-1, 
                    data = mdat.test.4)[,-5]
xchoice.full.4 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.4[,-5], 
                              stratum = mdat.test.4$step_id_, 
                              fit_ssf = ssf.train.full.4,
                              z = z.4)    

denshats.Forest.full.4 <- uhcdenscalc(rand_sims = xchoice.full.4[,,1], 
                                      dat = z.4[mdat.test.4$case_==1,1], 
                                      avail = z.4[mdat.test.4$case_==0,1]) 
denshats.Pasture.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,2], 
                                       dat=z.4[mdat.test.4$case_==1,2], 
                                       avail=z.4[mdat.test.4$case_==0,2])  

denshats.Shrub.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,3], 
                                     dat=z.4[mdat.test.4$case_==1,3], 
                                     avail=z.4[mdat.test.4$case_==0,3])

denshats.sl.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,4], 
                                  dat=z.4[mdat.test.4$case_==1,4], 
                                  avail=z.4[mdat.test.4$case_==0,4])
denshats.night.Forest.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,5], 
                                            dat=z.4[mdat.test.4$case_==1,5],
                                            avail=z.4[mdat.test.4$case_==0,5])

denshats.night.Pasture.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,6], 
                                             dat=z.4[mdat.test.4$case_==1,6],
                                             avail=z.4[mdat.test.4$case_==0,6])

denshats.night.Shrub.full.4 <- uhcdenscalc(rand_sims=xchoice.full.4[,,7], 
                                           dat=z.4[mdat.test.4$case_==1,7],
                                           avail=z.4[mdat.test.4$case_==0,7])

###Reduced model####

design.mat.test.reduc1.4 <- model.matrix(form2b.4, data=mdat.test.4)
z.4 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.4)[,-5]
xchoice.reduc1.4 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.4, 
                                stratum = mdat.test.4$step_id_, 
                                fit_ssf = ssf.train.reduc1.4,
                                z = z.4)    

denshats.Forest.reduc1.4 <- uhcdenscalc(rand_sims = xchoice.reduc1.4[,,1], 
                                        dat = z.4[mdat.test.4$case_==1,1], 
                                        avail = z.4[mdat.test.4$case_==0,1]) 

denshats.Pasture.reduc1.4 <- uhcdenscalc(rand_sims=xchoice.reduc1.4[,,2], 
                                         dat=z.4[mdat.test.4$case_==1,2], 
                                         avail=z.4[mdat.test.4$case_==0,2])  

denshats.Shrub.reduc1.4 <- uhcdenscalc(rand_sims=xchoice.reduc1.4[,,3], 
                                       dat=z.4[mdat.test.4$case_==1,3], 
                                       avail=z.4[mdat.test.4$case_==0,3]) 
denshats.sl.reduc1.4 <- uhcdenscalc(rand_sims=xchoice.reduc1.4[,,4], 
                                    dat=z.4[mdat.test.4$case_==1,4],
                                    avail=z.4[mdat.test.4$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A)","B)", "C)", "D)", "E)", "F)", "G)")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.4$densdat, 
            densrand = denshats.Forest.full.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)



uhcdensplot(densdat = denshats.Pasture.full.4$densdat,
            densrand = denshats.Pasture.full.4$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.4$densdat, 
            densrand = denshats.Shrub.full.4$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.full.4$densdat, 
            densrand = denshats.sl.full.4$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.4$densdat, 
            densrand = denshats.night.Forest.full.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.4$densdat, 
            densrand = denshats.night.Pasture.full.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.4$densdat, 
            densrand = denshats.night.Shrub.full.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.4$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.4, cex=1.8)

plot.uhc.full.4 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H)","I)", "J)", "K)")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.4$densdat, 
            densrand = denshats.Forest.reduc1.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.4$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "Density")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10),col=c("Black", "red", "gray"), bty="n", cex=1.8) 



uhcdensplot(densdat = denshats.Pasture.reduc1.4$densdat,
            densrand = denshats.Pasture.reduc1.4$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.4$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.4$densdat, 
            densrand = denshats.Shrub.reduc1.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.4$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.4$densdat, 
            densrand = denshats.sl.reduc1.4$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.4$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.4, cex=1.8)

plot.uhc.reduc.4 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.4$densdat, 
                densrand = denshats.Forest.full.4$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.4$densdat, 
                densrand = denshats.Pasture.full.4$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.4$densdat, 
                densrand = denshats.night.Forest.full.4$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.4$densdat, 
                densrand = denshats.night.Pasture.full.4$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.4$densdat, 
                densrand = denshats.Forest.reduc1.4$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.4$densdat, 
                densrand = denshats.Pasture.reduc1.4$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.4 <- recordPlot()

#Cat 5####
dat5 <- ssf_dat_all %>% 
  filter(id == "5") 

##Subset data######

ids5 <- splitTools::partition(
  dat5$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.5 <- dat5[ids5$train, ]
mdat.test.5 <- dat5[ids5$test, ]

##Specify Parameters########

###Full model####
textplot1.5 <- (expression(y %~% Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_)))
form1a.5 <- (case_ ~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_))
form2a.5 <- ~ Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.5 <- (expression(y %~% Forest+Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.5 <- (case_ ~ Forest+Pasture+Shrub+ sl_+ strata(step_id_))
form2b.5 <- ~ Forest+Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.5 <- clogit(form1a.5, data=mdat.train.5)

ssf.train.full.5

###Reduced Model####
ssf.train.reduc1.5 <- clogit(form1b.5, data=mdat.train.5)

ssf.train.reduc1.5


##UHC plots#########

head(design.mat.test.full[,-4])

###Full model####
design.mat.test.full.5 <- model.matrix(form2a.5, data=mdat.test.5)
z.5 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.5)[,-5]
xchoice.full.5 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.5[,-5], 
                              stratum = mdat.test.5$step_id_, 
                              fit_ssf = ssf.train.full.5,
                              z = z.5)    

denshats.Forest.full.5 <- uhcdenscalc(rand_sims = xchoice.full.5[,,1], 
                                      dat = z.5[mdat.test.5$case_==1,1], 
                                      avail = z.5[mdat.test.5$case_==0,1]) 
denshats.Pasture.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,2], 
                                       dat=z.5[mdat.test.5$case_==1,2], 
                                       avail=z.5[mdat.test.5$case_==0,2])  
denshats.Shrub.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,3], 
                                     dat=z.5[mdat.test.5$case_==1,3], 
                                     avail=z.5[mdat.test.5$case_==0,3]) 
denshats.sl.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,4], 
                                  dat=z.5[mdat.test.5$case_==1,4], 
                                  avail=z.5[mdat.test.5$case_==0,4])
denshats.night.Forest.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,5], 
                                            dat=z.5[mdat.test.5$case_==1,5],
                                            avail=z.5[mdat.test.5$case_==0,5])

denshats.night.Pasture.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,5], 
                                             dat=z.5[mdat.test.5$case_==1,5],
                                             avail=z.5[mdat.test.5$case_==0,5])

denshats.night.Shrub.full.5 <- uhcdenscalc(rand_sims=xchoice.full.5[,,6], 
                                           dat=z.5[mdat.test.5$case_==1,6],
                                           avail=z.5[mdat.test.5$case_==0,6])


###Reduced model####

design.mat.test.reduc1.5 <- model.matrix(form2b.5, data=mdat.test.5)
z.5 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.5)[,-5]
xchoice.reduc1.5 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.5, 
                                stratum = mdat.test.5$step_id_, 
                                fit_ssf = ssf.train.reduc1.5,
                                z = z.5)    

denshats.Forest.reduc1.5 <- uhcdenscalc(rand_sims = xchoice.reduc1.5[,,1], 
                                        dat = z.5[mdat.test.5$case_==1,1], 
                                        avail = z.5[mdat.test.5$case_==0,1]) 
denshats.Pasture.reduc1.5 <- uhcdenscalc(rand_sims=xchoice.reduc1.5[,,2], 
                                         dat=z.5[mdat.test.5$case_==1,2], 
                                         avail=z.5[mdat.test.5$case_==0,2])  
denshats.Shrub.reduc1.5 <- uhcdenscalc(rand_sims=xchoice.reduc1.5[,,3], 
                                       dat=z.5[mdat.test.5$case_==1,3], 
                                       avail=z.5[mdat.test.5$case_==0,3]) 
denshats.sl.reduc1.5 <- uhcdenscalc(rand_sims=xchoice.reduc1.5[,,4], 
                                    dat=z.5[mdat.test.5$case_==1,4],
                                    avail=z.5[mdat.test.5$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.5$densdat, 
            densrand = denshats.Forest.full.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.5$densdat,
            densrand = denshats.Pasture.full.5$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.5$densdat, 
            densrand = denshats.Shrub.full.5$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.full.5$densdat, 
            densrand = denshats.sl.full.5$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.5$densdat, 
            densrand = denshats.night.Forest.full.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.5$densdat, 
            densrand = denshats.night.Pasture.full.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)
uhcdensplot(densdat = denshats.night.Shrub.full.5$densdat, 
            densrand = denshats.night.Shrub.full.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.5$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.5, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.5 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H.","I.", "J.", "K.")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.5$densdat, 
            densrand = denshats.Forest.reduc1.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.5$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10),col=c("Black", "red", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Pasture.reduc1.5$densdat,
            densrand = denshats.Pasture.reduc1.5$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.5$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.5$densdat, 
            densrand = denshats.Shrub.reduc1.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.5$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.5$densdat, 
            densrand = denshats.sl.reduc1.5$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.5$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot2.5, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.5 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.5$densdat, 
                densrand = denshats.Forest.full.5$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.5$densdat, 
                densrand = denshats.Pasture.full.5$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.5$densdat, 
                densrand = denshats.night.Forest.full.5$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.5$densdat, 
                densrand = denshats.night.Pasture.full.5$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.5$densdat, 
                densrand = denshats.Forest.reduc1.5$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.5$densdat, 
                densrand = denshats.Pasture.reduc1.5$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)


plot.uhc.differences.5 <- recordPlot()


#Cat 6####
dat6 <- ssf_dat_all %>% 
  filter(id == "6") 

##Subset data######

ids6 <- splitTools::partition(
  dat6$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.6 <- dat6[ids6$train, ]
mdat.test.6 <- dat6[ids6$test, ]

##Specify Parameters########

###Full model####
textplot1.6 <- (expression(y %~% Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_)))
form1a.6 <- (case_ ~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_))
form2a.6 <- ~ Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.6 <- (expression(y %~% Forest+Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.6 <- (case_ ~ Forest+Pasture+Shrub+ sl_+ strata(step_id_))
form2b.6 <- ~ Forest+Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.6 <- clogit(form1a.6, data=mdat.train.6)

ssf.train.full.6

###Reduced Model####
ssf.train.reduc1.6 <- clogit(form1b.6, data=mdat.train.6)

ssf.train.reduc1.6


##UHC plots#########

head(design.mat.test.full.6[,-5])

###Full model####
design.mat.test.full.6 <- model.matrix(form2a.6, data=mdat.test.6)
z.6 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.6)[,-5]
xchoice.full.6 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.6[,-5], 
                              stratum = mdat.test.6$step_id_, 
                              fit_ssf = ssf.train.full.6,
                              z = z.6)    

denshats.Forest.full.6 <- uhcdenscalc(rand_sims = xchoice.full.6[,,1], 
                                      dat = z.6[mdat.test.6$case_==1,1], 
                                      avail = z.6[mdat.test.6$case_==0,1]) 
denshats.Pasture.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,2], 
                                       dat=z.6[mdat.test.6$case_==1,2], 
                                       avail=z.6[mdat.test.6$case_==0,2])  
denshats.Shrub.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,3], 
                                     dat=z.6[mdat.test.6$case_==1,3], 
                                     avail=z.6[mdat.test.6$case_==0,3])
denshats.sl.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,4], 
                                  dat=z.6[mdat.test.6$case_==1,4], 
                                  avail=z.6[mdat.test.6$case_==0,4]) 
denshats.night.Forest.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,5], 
                                            dat=z.6[mdat.test.6$case_==1,5],
                                            avail=z.6[mdat.test.6$case_==0,5])

denshats.night.Pasture.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,6], 
                                             dat=z.6[mdat.test.6$case_==1,6],
                                             avail=z.6[mdat.test.6$case_==0,6])

denshats.night.Shrub.full.6 <- uhcdenscalc(rand_sims=xchoice.full.6[,,7], 
                                           dat=z.6[mdat.test.6$case_==1,7],
                                           avail=z.6[mdat.test.6$case_==0,7])


###Reduced model####

design.mat.test.reduc1.6 <- model.matrix(form2b.6, data=mdat.test.6)
z.6 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                    data = mdat.test.6)[,-5]
xchoice.reduc1.6 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.6, 
                                stratum = mdat.test.6$step_id_, 
                                fit_ssf = ssf.train.reduc1.6,
                                z = z.6)    

denshats.Forest.reduc1.6 <- uhcdenscalc(rand_sims = xchoice.reduc1.6[,,1], 
                                        dat = z.6[mdat.test.6$case_==1,1], 
                                        avail = z.6[mdat.test.6$case_==0,1]) 
denshats.Pasture.reduc1.6 <- uhcdenscalc(rand_sims=xchoice.reduc1.6[,,2], 
                                         dat=z.6[mdat.test.6$case_==1,2], 
                                         avail=z.6[mdat.test.6$case_==0,2])  
denshats.Shrub.reduc1.6 <- uhcdenscalc(rand_sims=xchoice.reduc1.6[,,3], 
                                       dat=z.6[mdat.test.6$case_==1,3], 
                                       avail=z.6[mdat.test.6$case_==0,3]) 
denshats.sl.reduc1.6 <- uhcdenscalc(rand_sims=xchoice.reduc1.6[,,4], 
                                    dat=z.6[mdat.test.6$case_==1,4],
                                    avail=z.6[mdat.test.6$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.6$densdat, 
            densrand = denshats.Forest.full.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"),lty=c(1, 2, 1), lwd=c(2,3, 10),col=c("Black", "red", "gray"), bty="n", cex=1.8)  



uhcdensplot(densdat = denshats.Pasture.full.6$densdat,
            densrand = denshats.Pasture.full.6$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.6$densdat, 
            densrand = denshats.Shrub.full.6$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.full.6$densdat, 
            densrand = denshats.sl.full.6$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.6$densdat, 
            densrand = denshats.night.Forest.full.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.6$densdat, 
            densrand = denshats.night.Pasture.full.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.6$densdat, 
            densrand = denshats.night.Shrub.full.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.6$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.6, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.6 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H.","I.", "J.", "K.")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.6$densdat, 
            densrand = denshats.Forest.reduc1.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.6$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10),col=c("Black", "red", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Pasture.reduc1.6$densdat,
            densrand = denshats.Pasture.reduc1.6$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.6$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.6$densdat, 
            densrand = denshats.Shrub.reduc1.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.6$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.6$densdat, 
            densrand = denshats.sl.reduc1.6$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.6$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot2.6, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.6 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.6$densdat, 
                densrand = denshats.Forest.full.6$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.6$densdat, 
                densrand = denshats.Pasture.full.6$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.6$densdat, 
                densrand = denshats.night.Forest.full.6$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.6$densdat, 
                densrand = denshats.night.Pasture.full.6$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.6$densdat, 
                densrand = denshats.Forest.reduc1.6$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.6$densdat, 
                densrand = denshats.Pasture.reduc1.6$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)


plot.uhc.differences.6 <- recordPlot()

#Cat 7####
dat7 <- ssf_dat_all %>% 
  filter(id == "7") 

##Subset data######

ids7 <- splitTools::partition(
  dat7$step_id_,
  p = c(train = 0.75, test = 0.15),
  type = "grouped"
)

mdat.train.7 <- dat7[ids7$train, ]
mdat.test.7 <- dat7[ids7$test, ]

##Specify Parameters########

###Full model####
textplot1.7 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a.7 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a.7 <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1


###Reduced model 1####

textplot2.7 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_)))
form1b.7 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_))
form2b.7 <- ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.7 <- clogit(form1a.7, data=mdat.train.7)

ssf.train.full.7

###Reduced Model####
ssf.train.reduc1.7 <- clogit(form1b.7, data=mdat.train.7)

ssf.train.reduc1.7


##UHC plots#########

head(design.mat.test.full.7[,-6])
head(z.7)
###Full model####
design.mat.test.full.7 <- model.matrix(form2a.7, data=mdat.test.7)
z.7 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.7)[,-6]
xchoice.full.7 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.7[,-6], 
                              stratum = mdat.test.7$step_id_, 
                              fit_ssf = ssf.train.full.7,
                              z = z.7)    

denshats.Forest.full.7 <- uhcdenscalc(rand_sims = xchoice.full.7[,,1], 
                                      dat = z.7[mdat.test.7$case_==1,1], 
                                      avail = z.7[mdat.test.7$case_==0,1]) 
denshats.Pasture.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,2], 
                                       dat=z.7[mdat.test.7$case_==1,2], 
                                       avail=z.7[mdat.test.7$case_==0,2])  


denshats.Forest_diffuse.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,3], 
                                              dat=z.7[mdat.test.7$case_==1,3], 
                                              avail=z.7[mdat.test.7$case_==0,3]) 

denshats.Shrub.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,4], 
                                     dat=z.7[mdat.test.7$case_==1,4], 
                                     avail=z.7[mdat.test.7$case_==0,4]) 

denshats.sl.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,5], 
                                  dat=z.7[mdat.test.7$case_==1,5], 
                                  avail=z.7[mdat.test.7$case_==0,5]) 

denshats.night.Forest.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,6], 
                                            dat=z.7[mdat.test.7$case_==1,6],
                                            avail=z.7[mdat.test.7$case_==0,6])

denshats.night.Pasture.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,7], 
                                             dat=z.7[mdat.test.7$case_==1,7],
                                             avail=z.7[mdat.test.7$case_==0,7])

denshats.night.Forest_diffuse.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,8], 
                                                    dat=z.7[mdat.test.7$case_==1,8],
                                                    avail=z.7[mdat.test.7$case_==0,8])

denshats.night.Shrub.full.7 <- uhcdenscalc(rand_sims=xchoice.full.7[,,9], 
                                           dat=z.7[mdat.test.7$case_==1,9],
                                           avail=z.7[mdat.test.7$case_==0,9])


###Reduced model####

head(z.7)

design.mat.test.reduc1.7 <- model.matrix(form2b.7, data=mdat.test.7)
z.7 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.7)[,-6]
xchoice.reduc1.7 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.7, 
                                stratum = mdat.test.7$step_id_, 
                                fit_ssf = ssf.train.reduc1.7,
                                z = z.7)    

denshats.Forest.reduc1.7 <- uhcdenscalc(rand_sims = xchoice.reduc1.7[,,1], 
                                        dat = z.7[mdat.test.7$case_==1,1], 
                                        avail = z.7[mdat.test.7$case_==0,1]) 

denshats.Pasture.reduc1.7 <- uhcdenscalc(rand_sims=xchoice.reduc1.7[,,2], 
                                         dat=z.7[mdat.test.7$case_==1,2], 
                                         avail=z.7[mdat.test.7$case_==0,2])  

denshats.Forest_diffuse.reduc1.7 <- uhcdenscalc(rand_sims=xchoice.reduc1.7[,,3], 
                                                dat=z.7[mdat.test.7$case_==1,3], 
                                                avail=z.7[mdat.test.7$case_==0,3]) 

denshats.Shrub.reduc1.7 <- uhcdenscalc(rand_sims=xchoice.reduc1.7[,,4], 
                                       dat=z.7[mdat.test.7$case_==1,4],
                                       avail=z.7[mdat.test.7$case_==0,4])

denshats.sl.reduc1.7 <- uhcdenscalc(rand_sims=xchoice.reduc1.7[,,5], 
                                    dat=z.7[mdat.test.7$case_==1,5],
                                    avail=z.7[mdat.test.7$case_==0,5])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.7$densdat, 
            densrand = denshats.Forest.full.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.7$densdat,
            densrand = denshats.Pasture.full.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.7$densdat, 
            densrand = denshats.Forest_diffuse.full.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.7$densdat, 
            densrand = denshats.Shrub.full.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.7$densdat, 
            densrand = denshats.sl.full.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.7$densdat, 
            densrand = denshats.night.Forest.full.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.7$densdat, 
            densrand = denshats.night.Pasture.full.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.7$densdat, 
            densrand = denshats.night.Forest_diffuse.full.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.7$densdat, 
            densrand = denshats.night.Shrub.full.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.7$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.7, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.7 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("J.","K.", "L.","M.", "N.")

par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.7$densdat, 
            densrand = denshats.Forest.reduc1.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.7$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)

uhcdensplot(densdat = denshats.Pasture.reduc1.7$densdat,
            densrand = denshats.Pasture.reduc1.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.7$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.7$densdat,
            densrand = denshats.Forest_diffuse.reduc1.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.7$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.7$densdat,
            densrand = denshats.Shrub.reduc1.7$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.7$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.7$densdat, 
            densrand = denshats.sl.reduc1.7$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.7$densavail) 
mtext(side=3, line=1,  panlabs2[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.7, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.7 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.7$densdat, 
                densrand = denshats.Forest.full.7$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.7$densdat, 
                densrand = denshats.Pasture.full.7$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.7$densdat, 
                densrand = denshats.night.Forest.full.7$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.7$densdat, 
                densrand = denshats.night.Pasture.full.7$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.7$densdat, 
                densrand = denshats.Forest.reduc1.7$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.7$densdat, 
                densrand = denshats.Pasture.reduc1.7$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.7 <- recordPlot()

#Cat 8####
dat8 <- ssf_dat_all %>% 
  filter(id == "8") 

##Subset data######

ids8 <- splitTools::partition(
  dat8$step_id_,
  p = c(train = 0.75, test = 0.15),
  type = "grouped"
)

mdat.train.8 <- dat8[ids8$train, ]
mdat.test.8 <- dat8[ids8$test, ]

##Specify Parameters########

###Full model####
textplot1.8 <- (expression(y %~% Forest+Pasture+Forest_diffuse+tod:(Forest+Pasture+Forest_diffuse)+sl_+strata(step_id_)))
form1a.8 <- (case_ ~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse)+sl_+strata(step_id_))
form2a.8 <- ~ Forest+Pasture+Forest_diffuse+tod:(Forest+Pasture+Forest_diffuse)+sl_-1


###Reduced model 1####

textplot2.8 <- (expression(y %~% Forest+Pasture+Forest_diffuse+ sl_+ strata(step_id_)))
form1b.8 <- (case_ ~ Forest+Pasture+Forest_diffuse+ sl_+ strata(step_id_))
form2b.8 <- ~ Forest+Pasture+Forest_diffuse+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.8 <- clogit(form1a.8, data=mdat.train.8)

ssf.train.full.8

###Reduced Model####
ssf.train.reduc1.8 <- clogit(form1b.8, data=mdat.train.8)

ssf.train.reduc1.8


##UHC plots#########

head(design.mat.test.full.8[,-5])
head(z.8)


###Full model####
design.mat.test.full.8 <- model.matrix(form2a.8, data=mdat.test.8)
z.8 <- model.matrix(~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse) +sl_-1, 
                    data = mdat.test.8)[,-5]
xchoice.full.8 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.8[,-5], 
                              stratum = mdat.test.8$step_id_, 
                              fit_ssf = ssf.train.full.8,
                              z = z.8)    

denshats.Forest.full.8 <- uhcdenscalc(rand_sims = xchoice.full.8[,,1], 
                                      dat = z.8[mdat.test.8$case_==1,1], 
                                      avail = z.8[mdat.test.8$case_==0,1]) 
denshats.Pasture.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,2], 
                                       dat=z.8[mdat.test.8$case_==1,2], 
                                       avail=z.8[mdat.test.8$case_==0,2])  
denshats.Forest_diffuse.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,3], 
                                              dat=z.8[mdat.test.8$case_==1,3], 
                                              avail=z.8[mdat.test.8$case_==0,3])   
denshats.sl.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,4], 
                                  dat=z.8[mdat.test.8$case_==1,4],
                                  avail=z.8[mdat.test.8$case_==0,4]) 
denshats.night.Forest.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,5], 
                                            dat=z.8[mdat.test.8$case_==1,5],
                                            avail=z.8[mdat.test.8$case_==0,5])

denshats.night.Pasture.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,6], 
                                             dat=z.8[mdat.test.8$case_==1,6],
                                             avail=z.8[mdat.test.8$case_==0,6])
denshats.night.Forest_diffuse.full.8 <- uhcdenscalc(rand_sims=xchoice.full.8[,,7], 
                                                    dat=z.8[mdat.test.8$case_==1,7],
                                                    avail=z.8[mdat.test.8$case_==0,7])



###Reduced model#####

design.mat.test.reduc1.8 <- model.matrix(form2b.8, data=mdat.test.8)
z.8 <- model.matrix(~ Forest+Pasture+Forest_diffuse +tod:(Forest+Pasture+Forest_diffuse) +sl_-1, 
                    data = mdat.test.8)[,-5]
xchoice.reduc1.8 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.8, 
                                stratum = mdat.test.8$step_id_, 
                                fit_ssf = ssf.train.reduc1.8,
                                z = z.8)    

denshats.Forest.reduc1.8 <- uhcdenscalc(rand_sims = xchoice.reduc1.8[,,1], 
                                        dat = z.8[mdat.test.8$case_==1,1], 
                                        avail = z.8[mdat.test.8$case_==0,1]) 
denshats.Pasture.reduc1.8 <- uhcdenscalc(rand_sims=xchoice.reduc1.8[,,2], 
                                         dat=z.8[mdat.test.8$case_==1,2], 
                                         avail=z.8[mdat.test.8$case_==0,2])  
denshats.Forest_diffuse.reduc1.8 <- uhcdenscalc(rand_sims=xchoice.reduc1.8[,,3], 
                                                dat=z.8[mdat.test.8$case_==1,3], 
                                                avail=z.8[mdat.test.8$case_==0,3]) 
denshats.sl.reduc1.8 <- uhcdenscalc(rand_sims=xchoice.reduc1.8[,,4], 
                                    dat=z.8[mdat.test.8$case_==1,4],
                                    avail=z.8[mdat.test.8$case_==0,4])

##Create UHC plot####

###Full model#####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.8$densdat, 
            densrand = denshats.Forest.full.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.8$densdat,
            densrand = denshats.Pasture.full.8$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.8$densdat, 
            densrand = denshats.Forest_diffuse.full.8$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)


uhcdensplot(densdat = denshats.sl.full.8$densdat, 
            densrand = denshats.sl.full.8$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest.full.8$densdat, 
            densrand = denshats.night.Forest.full.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.8$densdat, 
            densrand = denshats.night.Pasture.full.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest_diffuse.full.8$densdat, 
            densrand = denshats.night.Forest_diffuse.full.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.8$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot1.8, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)


plot.uhc.full.8 <- recordPlot()

###Reduced plot####
panlabs2 <- c("H.","I.", "J.", "K.")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.8$densdat, 
            densrand = denshats.Forest.reduc1.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.8$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
#      lty=c(1, 2, 1), lwd=c(2,3, 10), 
#     col=c("Black", "red", "gray"), bty="n", cex=1.8)
uhcdensplot(densdat = denshats.Pasture.reduc1.8$densdat,
            densrand = denshats.Pasture.reduc1.8$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.8$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.8$densdat, 
            densrand = denshats.Forest_diffuse.reduc1.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.8$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.8$densdat, 
            densrand = denshats.sl.reduc1.8$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.8$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.8, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.8 <- recordPlot()

##UHC Plot - Difference between observed and predicted#####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.8$densdat, 
                densrand = denshats.Forest.full.8$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.8$densdat, 
                densrand = denshats.Pasture.full.8$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.8$densdat, 
                densrand = denshats.night.Forest.full.8$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.8$densdat, 
                densrand = denshats.night.Pasture.full.8$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.8$densdat, 
                densrand = denshats.Forest.reduc1.8$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.8$densdat, 
                densrand = denshats.Pasture.reduc1.8$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.8 <- recordPlot()


#Cat 9####
dat9 <- ssf_dat_all %>% 
  filter(id == "9") 

##Subset data######

ids9 <- splitTools::partition(
  dat9$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.9 <- dat9[ids9$train, ]
mdat.test.9 <- dat9[ids9$test, ]

##Specify Parameters########

###Full model####
textplot1.9 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a.9 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a.9 <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1


###Reduced model 1####

textplot2.9 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_)))
form1b.9 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_))
form2b.9 <- ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.9 <- clogit(form1a.9, data=mdat.train.9)

ssf.train.full.9

###Reduced Model####
ssf.train.reduc1.9 <- clogit(form1b.9, data=mdat.train.9)

ssf.train.reduc1.9


##UHC plots#########

head(design.mat.test.full.9[,-6])
head(z.9)
###Full model####
design.mat.test.full.9 <- model.matrix(form2a.9, data=mdat.test.9)
z.9 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.9)[,-6]
xchoice.full.9 <- uhcsimstrat(nsims = 1000,
                              xmat = design.mat.test.full.9[,-6], 
                              stratum = mdat.test.9$step_id_, 
                              fit_ssf = ssf.train.full.9,
                              z = z.9)    

denshats.Forest.full.9 <- uhcdenscalc(rand_sims = xchoice.full.9[,,1], 
                                      dat = z.9[mdat.test.9$case_==1,1], 
                                      avail = z.9[mdat.test.9$case_==0,1]) 
denshats.Pasture.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,2], 
                                       dat=z.9[mdat.test.9$case_==1,2], 
                                       avail=z.9[mdat.test.9$case_==0,2])  


denshats.Forest_diffuse.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,3], 
                                              dat=z.9[mdat.test.9$case_==1,3], 
                                              avail=z.9[mdat.test.9$case_==0,3]) 

denshats.Shrub.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,4], 
                                     dat=z.9[mdat.test.9$case_==1,4], 
                                     avail=z.9[mdat.test.9$case_==0,4]) 

denshats.sl.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,5], 
                                  dat=z.9[mdat.test.9$case_==1,5], 
                                  avail=z.9[mdat.test.9$case_==0,5]) 

denshats.night.Forest.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,6], 
                                            dat=z.9[mdat.test.9$case_==1,6],
                                            avail=z.9[mdat.test.9$case_==0,6])

denshats.night.Pasture.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,7], 
                                             dat=z.9[mdat.test.9$case_==1,7],
                                             avail=z.9[mdat.test.9$case_==0,7])

denshats.night.Forest_diffuse.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,8], 
                                                    dat=z.9[mdat.test.9$case_==1,8],
                                                    avail=z.9[mdat.test.9$case_==0,8])

denshats.night.Shrub.full.9 <- uhcdenscalc(rand_sims=xchoice.full.9[,,9], 
                                           dat=z.9[mdat.test.9$case_==1,9],
                                           avail=z.9[mdat.test.9$case_==0,9])


###Reduced model####

design.mat.test.reduc1.9 <- model.matrix(form2b.9, data=mdat.test.9)
z.9 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                    data = mdat.test.9)[,-5]
xchoice.reduc1.9 <- uhcsimstrat(nsims = 1000,
                                xmat = design.mat.test.reduc1.9, 
                                stratum = mdat.test.9$step_id_, 
                                fit_ssf = ssf.train.reduc1.9,
                                z = z.9)    

denshats.Forest.reduc1.9 <- uhcdenscalc(rand_sims = xchoice.reduc1.9[,,1], 
                                        dat = z.9[mdat.test.9$case_==1,1], 
                                        avail = z.9[mdat.test.9$case_==0,1]) 

denshats.Pasture.reduc1.9 <- uhcdenscalc(rand_sims=xchoice.reduc1.9[,,2], 
                                         dat=z.9[mdat.test.9$case_==1,2], 
                                         avail=z.9[mdat.test.9$case_==0,2])  

denshats.Forest_diffuse.reduc1.9 <- uhcdenscalc(rand_sims=xchoice.reduc1.9[,,3], 
                                                dat=z.9[mdat.test.9$case_==1,3], 
                                                avail=z.9[mdat.test.9$case_==0,3]) 

denshats.Shrub.reduc1.9 <- uhcdenscalc(rand_sims=xchoice.reduc1.9[,,4], 
                                       dat=z.9[mdat.test.9$case_==1,4],
                                       avail=z.9[mdat.test.9$case_==0,4])

denshats.sl.reduc1.9 <- uhcdenscalc(rand_sims=xchoice.reduc1.9[,,5], 
                                    dat=z.9[mdat.test.9$case_==1,5],
                                    avail=z.9[mdat.test.9$case_==0,5])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.9$densdat, 
            densrand = denshats.Forest.full.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.9$densdat,
            densrand = denshats.Pasture.full.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.9$densdat, 
            densrand = denshats.Forest_diffuse.full.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.9$densdat, 
            densrand = denshats.Shrub.full.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.9$densdat, 
            densrand = denshats.sl.full.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.9$densdat, 
            densrand = denshats.night.Forest.full.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.9$densdat, 
            densrand = denshats.night.Pasture.full.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.9$densdat, 
            densrand = denshats.night.Forest_diffuse.full.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.9$densdat, 
            densrand = denshats.night.Shrub.full.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.9$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.9, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.9 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("J.","K.", "L.","M.", "N.")

par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.9$densdat, 
            densrand = denshats.Forest.reduc1.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.9$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
#      lty=c(1, 2, 1), lwd=c(2,3, 10), 
#     col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.reduc1.9$densdat,
            densrand = denshats.Pasture.reduc1.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.9$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.9$densdat,
            densrand = denshats.Forest_diffuse.reduc1.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.9$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.9$densdat,
            densrand = denshats.Shrub.reduc1.9$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.9$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.9$densdat, 
            densrand = denshats.sl.reduc1.9$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.9$densavail) 
mtext(side=3, line=1,  panlabs2[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.9, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.9 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.9$densdat, 
                densrand = denshats.Forest.full.9$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.9$densdat, 
                densrand = denshats.Pasture.full.9$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.9$densdat, 
                densrand = denshats.night.Forest.full.9$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.9$densdat, 
                densrand = denshats.night.Pasture.full.9$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.9$densdat, 
                densrand = denshats.Forest.reduc1.9$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.9$densdat, 
                densrand = denshats.Pasture.reduc1.9$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.9 <- recordPlot()

#Cat 10####
dat10 <- ssf_dat_all %>% 
  filter(id == "10") 

##Subset data######

ids10 <- splitTools::partition(
  dat10$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.10 <- dat10[ids10$train, ]
mdat.test.10 <- dat10[ids10$test, ]

##Specify Parameters########

###Full model####
textplot1.10 <- (expression(y %~% Pasture+Forest_diffuse+Shrub+tod:(Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a.10 <- (case_ ~ Pasture+Forest_diffuse+Shrub +tod:(Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a.10 <- ~ Pasture+Forest_diffuse+Shrub+tod:(Pasture+Forest_diffuse+Shrub)+sl_-1


###Reduced model 1####

textplot2.10 <- (expression(y %~% Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_)))
form1b.10 <- (case_ ~ Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_))
form2b.10 <- ~ Pasture+Forest_diffuse+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.10 <- clogit(form1a.10, data=mdat.train.10)

ssf.train.full.10

###Reduced Model####
ssf.train.reduc1.10 <- clogit(form1b.10, data=mdat.train.10)

ssf.train.reduc1.10


##UHC plots#########

head(design.mat.test.full.10[,-6])
head(z.10)
###Full model####
design.mat.test.full.10 <- model.matrix(form2a.10, data=mdat.test.10)
z.10 <- model.matrix(~ Pasture+Forest_diffuse+Shrub +tod:(Pasture+Forest_diffuse+Shrub) +sl_-1, 
                     data = mdat.test.10)[,-6]
xchoice.full.10 <- uhcsimstrat(nsims = 1000,
                               xmat = design.mat.test.full.10[,-6], 
                               stratum = mdat.test.10$step_id_, 
                               fit_ssf = ssf.train.full.10,
                               z = z.10)    



denshats.Pasture.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,1], 
                                        dat=z.10[mdat.test.10$case_==1,1], 
                                        avail=z.10[mdat.test.10$case_==0,1])  


denshats.Forest_diffuse.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,2], 
                                               dat=z.10[mdat.test.10$case_==1,2], 
                                               avail=z.10[mdat.test.10$case_==0,2]) 

denshats.Shrub.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,3], 
                                      dat=z.10[mdat.test.10$case_==1,3], 
                                      avail=z.10[mdat.test.10$case_==0,3]) 

denshats.sl.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,4], 
                                   dat=z.10[mdat.test.10$case_==1,4], 
                                   avail=z.10[mdat.test.10$case_==0,4]) 


denshats.night.Pasture.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,5], 
                                              dat=z.10[mdat.test.10$case_==1,5],
                                              avail=z.10[mdat.test.10$case_==0,5])

denshats.night.Forest_diffuse.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,6], 
                                                     dat=z.10[mdat.test.10$case_==1,6],
                                                     avail=z.10[mdat.test.10$case_==0,6])

denshats.night.Shrub.full.10 <- uhcdenscalc(rand_sims=xchoice.full.10[,,7], 
                                            dat=z.10[mdat.test.10$case_==1,7],
                                            avail=z.10[mdat.test.10$case_==0,7])


###Reduced model####

design.mat.test.reduc1.10 <- model.matrix(form2b.10, data=mdat.test.10)
z.10 <- model.matrix(~ Pasture+Forest_diffuse+Shrub +tod:(Pasture+Forest_diffuse+Shrub) +sl_-1, 
                     data = mdat.test.10)[,-5]
xchoice.reduc1.10 <- uhcsimstrat(nsims = 1000,
                                 xmat = design.mat.test.reduc1.10, 
                                 stratum = mdat.test.10$step_id_, 
                                 fit_ssf = ssf.train.reduc1.10,
                                 z = z.10)    


denshats.Pasture.reduc1.10 <- uhcdenscalc(rand_sims=xchoice.reduc1.10[,,1], 
                                          dat=z.10[mdat.test.10$case_==1,1], 
                                          avail=z.10[mdat.test.10$case_==0,1])  

denshats.Forest_diffuse.reduc1.10 <- uhcdenscalc(rand_sims=xchoice.reduc1.10[,,2], 
                                                 dat=z.10[mdat.test.10$case_==1,2], 
                                                 avail=z.10[mdat.test.10$case_==0,2]) 

denshats.Shrub.reduc1.10 <- uhcdenscalc(rand_sims=xchoice.reduc1.10[,,3], 
                                        dat=z.10[mdat.test.10$case_==1,3],
                                        avail=z.10[mdat.test.10$case_==0,3])

denshats.sl.reduc1.10 <- uhcdenscalc(rand_sims=xchoice.reduc1.10[,,4], 
                                     dat=z.10[mdat.test.10$case_==1,4],
                                     avail=z.10[mdat.test.10$case_==0,4])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Pasture.full.10$densdat, 
            densrand = denshats.Pasture.full.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Forest_diffuse.full.10$densdat, 
            densrand = denshats.Forest_diffuse.full.10$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.10$densdat, 
            densrand = denshats.Shrub.full.10$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.10$densdat, 
            densrand = denshats.sl.full.10$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)


uhcdensplot(densdat = denshats.night.Pasture.full.10$densdat, 
            densrand = denshats.night.Pasture.full.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.10$densdat, 
            densrand = denshats.night.Forest_diffuse.full.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.10$densdat, 
            densrand = denshats.night.Shrub.full.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.10$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.10, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.10 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H.","I.", "J.","K.")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Pasture.reduc1.10$densdat, 
            densrand = denshats.Pasture.reduc1.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.10$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Red", "Black", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.10$densdat,
            densrand = denshats.Forest_diffuse.reduc1.10$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.10$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.10$densdat,
            densrand = denshats.Shrub.reduc1.10$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.10$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.10$densdat, 
            densrand = denshats.sl.reduc1.10$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.10$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.10, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.10 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Pasture.full.10$densdat, 
                densrand = denshats.Pasture.full.10$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.10$densdat, 
                densrand = denshats.Pasture.full.10$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.10$densdat, 
                densrand = denshats.night.Forest.full.10$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.10$densdat, 
                densrand = denshats.night.Pasture.full.10$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.10$densdat, 
                densrand = denshats.Forest.reduc1.10$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.10$densdat, 
                densrand = denshats.Pasture.reduc1.10$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.10 <- recordPlot()



#Cat 11####
dat11 <- ssf_dat_all %>% 
  filter(id == "11") 

##Subset data######

ids11 <- splitTools::partition(
  dat11$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.11 <- dat11[ids11$train, ]
mdat.test.11 <- dat11[ids11$test, ]

##Specify Parameters########

###Full model####
textplot1.11 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_)))
form1a.11 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_+strata(step_id_))
form2a.11 <- ~ Forest+Pasture+Forest_diffuse+Shrub+tod:(Forest+Pasture+Forest_diffuse+Shrub)+sl_-1


###Reduced model 1####

textplot2.11 <- (expression(y %~% Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_)))
form1b.11 <- (case_ ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_+ strata(step_id_))
form2b.11 <- ~ Forest+Pasture+Forest_diffuse+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.11 <- clogit(form1a.11, data=mdat.train.11)

ssf.train.full.11

###Reduced Model####
ssf.train.reduc1.11 <- clogit(form1b.11, data=mdat.train.11)

ssf.train.reduc1.11


##UHC plots#########

head(design.mat.test.full.11[,-6])
head(z.11)
###Full model####
design.mat.test.full.11 <- model.matrix(form2a.11, data=mdat.test.11)
z.11 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                     data = mdat.test.11)[,-6]
xchoice.full.11 <- uhcsimstrat(nsims = 1000,
                               xmat = design.mat.test.full.11[,-6], 
                               stratum = mdat.test.11$step_id_, 
                               fit_ssf = ssf.train.full.11,
                               z = z.11)    

denshats.Forest.full.11 <- uhcdenscalc(rand_sims = xchoice.full.11[,,1], 
                                       dat = z.11[mdat.test.11$case_==1,1], 
                                       avail = z.11[mdat.test.11$case_==0,1]) 
denshats.Pasture.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,2], 
                                        dat=z.11[mdat.test.11$case_==1,2], 
                                        avail=z.11[mdat.test.11$case_==0,2])  


denshats.Forest_diffuse.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,3], 
                                               dat=z.11[mdat.test.11$case_==1,3], 
                                               avail=z.11[mdat.test.11$case_==0,3]) 

denshats.Shrub.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,4], 
                                      dat=z.11[mdat.test.11$case_==1,4], 
                                      avail=z.11[mdat.test.11$case_==0,4]) 

denshats.sl.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,5], 
                                   dat=z.11[mdat.test.11$case_==1,5], 
                                   avail=z.11[mdat.test.11$case_==0,5]) 

denshats.night.Forest.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,6], 
                                             dat=z.11[mdat.test.11$case_==1,6],
                                             avail=z.11[mdat.test.11$case_==0,6])

denshats.night.Pasture.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,7], 
                                              dat=z.11[mdat.test.11$case_==1,7],
                                              avail=z.11[mdat.test.11$case_==0,7])

denshats.night.Forest_diffuse.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,8], 
                                                     dat=z.11[mdat.test.11$case_==1,8],
                                                     avail=z.11[mdat.test.11$case_==0,8])

denshats.night.Shrub.full.11 <- uhcdenscalc(rand_sims=xchoice.full.11[,,9], 
                                            dat=z.11[mdat.test.11$case_==1,9],
                                            avail=z.11[mdat.test.11$case_==0,9])


###Reduced model####

design.mat.test.reduc1.11 <- model.matrix(form2b.11, data=mdat.test.11)
z.11 <- model.matrix(~ Forest+Pasture+Forest_diffuse+Shrub +tod:(Forest+Pasture+Forest_diffuse+Shrub) +sl_-1, 
                     data = mdat.test.11)[,-6]
xchoice.reduc1.11 <- uhcsimstrat(nsims = 1000,
                                 xmat = design.mat.test.reduc1.11, 
                                 stratum = mdat.test.11$step_id_, 
                                 fit_ssf = ssf.train.reduc1.11,
                                 z = z.11)    

denshats.Forest.reduc1.11 <- uhcdenscalc(rand_sims = xchoice.reduc1.11[,,1], 
                                         dat = z.11[mdat.test.11$case_==1,1], 
                                         avail = z.11[mdat.test.11$case_==0,1]) 

denshats.Pasture.reduc1.11 <- uhcdenscalc(rand_sims=xchoice.reduc1.11[,,2], 
                                          dat=z.11[mdat.test.11$case_==1,2], 
                                          avail=z.11[mdat.test.11$case_==0,2])  

denshats.Forest_diffuse.reduc1.11 <- uhcdenscalc(rand_sims=xchoice.reduc1.11[,,3], 
                                                 dat=z.11[mdat.test.11$case_==1,3], 
                                                 avail=z.11[mdat.test.11$case_==0,3]) 

denshats.Shrub.reduc1.11 <- uhcdenscalc(rand_sims=xchoice.reduc1.11[,,4], 
                                        dat=z.11[mdat.test.11$case_==1,4],
                                        avail=z.11[mdat.test.11$case_==0,4])

denshats.sl.reduc1.11 <- uhcdenscalc(rand_sims=xchoice.reduc1.11[,,5], 
                                     dat=z.11[mdat.test.11$case_==1,5],
                                     avail=z.11[mdat.test.11$case_==0,5])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.", "H.", "I.")
par(mfrow=c(3,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.11$densdat, 
            densrand = denshats.Forest.full.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.11$densdat,
            densrand = denshats.Pasture.full.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.full.11$densdat, 
            densrand = denshats.Forest_diffuse.full.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.11$densdat, 
            densrand = denshats.Shrub.full.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.11$densdat, 
            densrand = denshats.sl.full.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.11$densdat, 
            densrand = denshats.night.Forest.full.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)

uhcdensplot(densdat = denshats.night.Pasture.full.11$densdat, 
            densrand = denshats.night.Pasture.full.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest_diffuse.full.11$densdat, 
            densrand = denshats.night.Forest_diffuse.full.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest_diffuse.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[8], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.11$densdat, 
            densrand = denshats.night.Shrub.full.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.11$densavail) 
mtext(side=3, line=1,  panlabs1[9], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.11, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.11 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("J.","K.", "L.","M.", "N.")

par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.11$densdat, 
            densrand = denshats.Forest.reduc1.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.11$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
#      lty=c(1, 2, 1), lwd=c(2,3, 10), 
#     col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.reduc1.11$densdat,
            densrand = denshats.Pasture.reduc1.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.11$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Forest_diffuse.reduc1.11$densdat,
            densrand = denshats.Forest_diffuse.reduc1.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Forest_diffuse.reduc1.11$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Forest diffuse", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.11$densdat,
            densrand = denshats.Shrub.reduc1.11$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.11$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.11$densdat, 
            densrand = denshats.sl.reduc1.11$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.11$densavail) 
mtext(side=3, line=1,  panlabs2[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.11, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.11 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.11$densdat, 
                densrand = denshats.Forest.full.11$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.11$densdat, 
                densrand = denshats.Pasture.full.11$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.11$densdat, 
                densrand = denshats.night.Forest.full.11$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.11$densdat, 
                densrand = denshats.night.Pasture.full.11$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.11$densdat, 
                densrand = denshats.Forest.reduc1.11$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.11$densdat, 
                densrand = denshats.Pasture.reduc1.11$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.11 <- recordPlot()

#Cat 12####
dat12 <- ssf_dat_all %>% 
  filter(id == "12") 

##Subset data######

ids12 <- splitTools::partition(
  dat12$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.12 <- dat12[ids12$train, ]
mdat.test.12 <- dat12[ids12$test, ]

##Specify Parameters########

###Full model####
textplot1.12 <- (expression(y %~% Pasture+Shrub+tod:(Pasture+Shrub)+sl_+strata(step_id_)))
form1a.12 <- (case_ ~ Pasture+Shrub +tod:(Pasture+Shrub)+sl_+strata(step_id_))
form2a.12 <- ~ Pasture+Shrub+tod:(Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.12 <- (expression(y %~% Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.12 <- (case_ ~ Pasture+Shrub+ sl_+ strata(step_id_))
form2b.12 <- ~ Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.12 <- clogit(form1a.12, data=mdat.train.12)

ssf.train.full.12

###Reduced Model####
ssf.train.reduc1.12 <- clogit(form1b.12, data=mdat.train.12)

ssf.train.reduc1.12


##UHC plots#########

head(design.mat.test.full.12[,-6])
head(z.12)
###Full model####
design.mat.test.full.12 <- model.matrix(form2a.12, data=mdat.test.12)
z.12 <- model.matrix(~ Pasture+Shrub +tod:(Pasture+Shrub) +sl_-1, 
                     data = mdat.test.12)[,-4]
xchoice.full.12 <- uhcsimstrat(nsims = 1000,
                               xmat = design.mat.test.full.12[,-4], 
                               stratum = mdat.test.12$step_id_, 
                               fit_ssf = ssf.train.full.12,
                               z = z.12)    



denshats.Pasture.full.12 <- uhcdenscalc(rand_sims=xchoice.full.12[,,1], 
                                        dat=z.12[mdat.test.12$case_==1,1], 
                                        avail=z.12[mdat.test.12$case_==0,1])  



denshats.Shrub.full.12 <- uhcdenscalc(rand_sims=xchoice.full.12[,,2], 
                                      dat=z.12[mdat.test.12$case_==1,2], 
                                      avail=z.12[mdat.test.12$case_==0,2]) 

denshats.sl.full.12 <- uhcdenscalc(rand_sims=xchoice.full.12[,,3], 
                                   dat=z.12[mdat.test.12$case_==1,3], 
                                   avail=z.12[mdat.test.12$case_==0,3]) 


denshats.night.Pasture.full.12 <- uhcdenscalc(rand_sims=xchoice.full.12[,,4], 
                                              dat=z.12[mdat.test.12$case_==1,4],
                                              avail=z.12[mdat.test.12$case_==0,4])


denshats.night.Shrub.full.12 <- uhcdenscalc(rand_sims=xchoice.full.12[,,5], 
                                            dat=z.12[mdat.test.12$case_==1,5],
                                            avail=z.12[mdat.test.12$case_==0,5])


###Reduced model####

design.mat.test.reduc1.12 <- model.matrix(form2b.12, data=mdat.test.12)
z.12 <- model.matrix(~ Pasture+Shrub +tod:(Pasture+Shrub) +sl_-1, 
                     data = mdat.test.12)[,-4]
xchoice.reduc1.12 <- uhcsimstrat(nsims = 1000,
                                 xmat = design.mat.test.reduc1.12, 
                                 stratum = mdat.test.12$step_id_, 
                                 fit_ssf = ssf.train.reduc1.12,
                                 z = z.12)    


denshats.Pasture.reduc1.12 <- uhcdenscalc(rand_sims=xchoice.reduc1.12[,,1], 
                                          dat=z.12[mdat.test.12$case_==1,1], 
                                          avail=z.12[mdat.test.12$case_==0,1])  


denshats.Shrub.reduc1.12 <- uhcdenscalc(rand_sims=xchoice.reduc1.12[,,2], 
                                        dat=z.12[mdat.test.12$case_==1,2],
                                        avail=z.12[mdat.test.12$case_==0,2])

denshats.sl.reduc1.12 <- uhcdenscalc(rand_sims=xchoice.reduc1.12[,,3], 
                                     dat=z.12[mdat.test.12$case_==1,3],
                                     avail=z.12[mdat.test.12$case_==0,3])


##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.")
par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Pasture.full.12$densdat, 
            densrand = denshats.Pasture.full.12$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.12$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), lty=c(1, 2, 1), lwd=c(2,3, 10), col=c("Black", "red", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Shrub.full.12$densdat, 
            densrand = denshats.Shrub.full.12$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.12$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)



uhcdensplot(densdat = denshats.sl.full.12$densdat, 
            densrand = denshats.sl.full.12$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.12$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)


uhcdensplot(densdat = denshats.night.Pasture.full.12$densdat, 
            densrand = denshats.night.Pasture.full.12$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.12$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.12$densdat, 
            densrand = denshats.night.Shrub.full.12$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.12$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)

mtext(outer=T, side=3, line=3,  textplot1.12, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.12 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("F.","G.", "H.")

par(mfrow=c(1,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Pasture.reduc1.12$densdat, 
            densrand = denshats.Pasture.reduc1.12$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.12$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Red", "Black", "gray"), bty="n", cex=1.8)


uhcdensplot(densdat = denshats.Shrub.reduc1.12$densdat,
            densrand = denshats.Shrub.reduc1.12$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.12$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.12$densdat, 
            densrand = denshats.sl.reduc1.12$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.12$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.12, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.12 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Pasture.full.12$densdat, 
                densrand = denshats.Pasture.full.12$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.12$densdat, 
                densrand = denshats.Pasture.full.12$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.12$densdat, 
                densrand = denshats.night.Forest.full.12$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.12$densdat, 
                densrand = denshats.night.Pasture.full.12$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.12$densdat, 
                densrand = denshats.Forest.reduc1.12$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.12$densdat, 
                densrand = denshats.Pasture.reduc1.12$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.12 <- recordPlot()




#Cat 13####
dat13 <- ssf_dat_all %>% 
  filter(id == "13") 

##Subset data######

ids13 <- splitTools::partition(
  dat13$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.13 <- dat13[ids13$train, ]
mdat.test.13 <- dat13[ids13$test, ]

##Specify Parameters########

###Full model####
textplot1.13 <- (expression(y %~% Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_)))
form1a.13 <- (case_ ~ Forest+Pasture +Shrub+tod:(Forest+Pasture+Shrub)+sl_+strata(step_id_))
form2a.13 <- ~ Forest+Pasture+Shrub+tod:(Forest+Pasture+Shrub)+sl_-1


###Reduced model 1####

textplot2.13 <- (expression(y %~% Forest+Pasture+Shrub+ sl_+ strata(step_id_)))
form1b.13 <- (case_ ~ Forest+Pasture+Shrub+ sl_+ strata(step_id_))
form2b.13 <- ~ Forest+Pasture+Shrub+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.13 <- clogit(form1a.13, data=mdat.train.13)

ssf.train.full.13

###Reduced Model####
ssf.train.reduc1.13 <- clogit(form1b.13, data=mdat.train.13)

ssf.train.reduc1.13


##UHC plots#########

head(design.mat.test.full.13[,-5])

###Full model####
design.mat.test.full.13 <- model.matrix(form2a.13, data=mdat.test.13)
z.13 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                     data = mdat.test.13)[,-5]
xchoice.full.13 <- uhcsimstrat(nsims = 1000,
                               xmat = design.mat.test.full.13[,-5], 
                               stratum = mdat.test.13$step_id_, 
                               fit_ssf = ssf.train.full.13,
                               z = z.13)    

denshats.Forest.full.13 <- uhcdenscalc(rand_sims = xchoice.full.13[,,1], 
                                       dat = z.13[mdat.test.13$case_==1,1], 
                                       avail = z.13[mdat.test.13$case_==0,1]) 
denshats.Pasture.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,2], 
                                        dat=z.13[mdat.test.13$case_==1,2], 
                                        avail=z.13[mdat.test.13$case_==0,2])  
denshats.Shrub.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,3], 
                                      dat=z.13[mdat.test.13$case_==1,3], 
                                      avail=z.13[mdat.test.13$case_==0,3])   
denshats.sl.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,4], 
                                   dat=z.13[mdat.test.13$case_==1,4],
                                   avail=z.13[mdat.test.13$case_==0,4]) 
denshats.night.Forest.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,5], 
                                             dat=z.13[mdat.test.13$case_==1,5],
                                             avail=z.13[mdat.test.13$case_==0,5])

denshats.night.Pasture.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,6], 
                                              dat=z.13[mdat.test.13$case_==1,6],
                                              avail=z.13[mdat.test.13$case_==0,6])
denshats.night.Shrub.full.13 <- uhcdenscalc(rand_sims=xchoice.full.13[,,7], 
                                            dat=z.13[mdat.test.13$case_==1,7],
                                            avail=z.13[mdat.test.13$case_==0,7])




###Reduced model####

design.mat.test.reduc1.13 <- model.matrix(form2b.13, data=mdat.test.13)
z.13 <- model.matrix(~ Forest+Pasture+Shrub +tod:(Forest+Pasture+Shrub) +sl_-1, 
                     data = mdat.test.13)[,-5]
xchoice.reduc1.13 <- uhcsimstrat(nsims = 1000,
                                 xmat = design.mat.test.reduc1.13, 
                                 stratum = mdat.test.13$step_id_, 
                                 fit_ssf = ssf.train.reduc1.13,
                                 z = z.13)    

denshats.Forest.reduc1.13 <- uhcdenscalc(rand_sims = xchoice.reduc1.13[,,1], 
                                         dat = z.13[mdat.test.13$case_==1,1], 
                                         avail = z.13[mdat.test.13$case_==0,1]) 
denshats.Pasture.reduc1.13 <- uhcdenscalc(rand_sims=xchoice.reduc1.13[,,2], 
                                          dat=z.13[mdat.test.13$case_==1,2], 
                                          avail=z.13[mdat.test.13$case_==0,2])  

denshats.Shrub.reduc1.13 <- uhcdenscalc(rand_sims=xchoice.reduc1.13[,,3], 
                                        dat=z.13[mdat.test.13$case_==1,3], 
                                        avail=z.13[mdat.test.13$case_==0,3]) 
denshats.sl.reduc1.13 <- uhcdenscalc(rand_sims=xchoice.reduc1.13[,,4], 
                                     dat=z.13[mdat.test.13$case_==1,4],
                                     avail=z.13[mdat.test.13$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.", "F.", "G.")
par(mfrow=c(2,4), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.13$densdat, 
            densrand = denshats.Forest.full.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
#legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
#      lty=c(1, 2, 1), lwd=c(2,3, 10), 
#     col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.full.13$densdat,
            densrand = denshats.Pasture.full.13$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.full.13$densdat, 
            densrand = denshats.Shrub.full.13$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Shrub.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)

uhcdensplot(densdat = denshats.sl.full.13$densdat, 
            densrand = denshats.sl.full.13$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)

uhcdensplot(densdat = denshats.night.Forest.full.13$densdat, 
            densrand = denshats.night.Forest.full.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.13$densdat, 
            densrand = denshats.night.Pasture.full.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[6], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)

uhcdensplot(densdat = denshats.night.Shrub.full.13$densdat, 
            densrand = denshats.night.Shrub.full.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Shrub.full.13$densavail) 
mtext(side=3, line=1,  panlabs1[7], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Shrub", cex=1.4)


mtext(outer=T, side=3, line=3,  textplot1.13, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)


plot.uhc.full.13 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("H.","I.", "J.", "K.")

par(mfrow=c(2,2), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.13$densdat, 
            densrand = denshats.Forest.reduc1.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.13$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)

uhcdensplot(densdat = denshats.Pasture.reduc1.13$densdat,
            densrand = denshats.Pasture.reduc1.13$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.13$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.Shrub.reduc1.13$densdat, 
            densrand = denshats.Shrub.reduc1.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Shrub.reduc1.13$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Shrub", cex=1.4)


uhcdensplot(densdat = denshats.sl.reduc1.13$densdat, 
            densrand = denshats.sl.reduc1.13$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.13$densavail) 
mtext(side=3, line=1,  panlabs2[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2.13, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.13 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.13$densdat, 
                densrand = denshats.Forest.full.13$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.13$densdat, 
                densrand = denshats.Pasture.full.13$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.13$densdat, 
                densrand = denshats.night.Forest.full.13$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.13$densdat, 
                densrand = denshats.night.Pasture.full.13$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.13$densdat, 
                densrand = denshats.Forest.reduc1.13$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.13$densdat, 
                densrand = denshats.Pasture.reduc1.13$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.13 <- recordPlot()


#Cat 14####
dat14 <- ssf_dat_all %>% 
  filter(id == "14") 

##Subset data######

ids14 <- splitTools::partition(
  dat14$step_id_,
  p = c(train = 0.75, test = 0.25),
  type = "grouped"
)

mdat.train.14 <- dat14[ids14$train, ]
mdat.test.14 <- dat14[ids14$test, ]

##Specify Parameters########

###Full model####
textplot1.14 <- (expression(y %~% Forest+Pasture+tod:(Forest+Pasture)+sl_+strata(step_id_)))
form1a.14 <- (case_ ~ Forest+Pasture +tod:(Forest+Pasture)+sl_+strata(step_id_))
form2a.14 <- ~ Forest+Pasture+tod:Forest+ tod:Pasture+sl_-1


###Reduced model 1####

textplot2.14 <- (expression(y %~% Forest+Pasture+ sl_+ strata(step_id_)))
form1b.14 <- (case_ ~ Forest+Pasture+ sl_+ strata(step_id_))
form2b.14 <- ~ Forest+Pasture+ sl_-1



##Fit SSF models###########
###Full####

ssf.train.full.14 <- clogit(form1a.14, data=mdat.train.14)

ssf.train.full.14

###Reduced Model####
ssf.train.reduc1.14 <- clogit(form1b.14, data=mdat.train.14)

ssf.train.reduc1.14


##UHC plots#########

head(design.mat.test.full.14[,-4])
head(z.14)
###Full model####
design.mat.test.full.14 <- model.matrix(form2a.14, data=mdat.test.14)
z.14 <- model.matrix(~ Forest+Pasture +tod:(Forest+Pasture) +sl_-1, 
                     data = mdat.test.14)[,-4]
xchoice.full.14 <- uhcsimstrat(nsims = 1000,
                               xmat = design.mat.test.full.14[,-4], 
                               stratum = mdat.test.14$step_id_, 
                               fit_ssf = ssf.train.full.14,
                               z = z.14)    

denshats.Forest.full.14 <- uhcdenscalc(rand_sims = xchoice.full.14[,,1], 
                                       dat = z.14[mdat.test.14$case_==1,1], 
                                       avail = z.14[mdat.test.14$case_==0,1]) 
denshats.Pasture.full.14 <- uhcdenscalc(rand_sims=xchoice.full.14[,,2], 
                                        dat=z.14[mdat.test.14$case_==1,2], 
                                        avail=z.14[mdat.test.14$case_==0,2])  
denshats.sl.full.14 <- uhcdenscalc(rand_sims=xchoice.full.14[,,3], 
                                   dat=z.14[mdat.test.14$case_==1,3], 
                                   avail=z.14[mdat.test.14$case_==0,3]) 
denshats.night.Forest.full.14 <- uhcdenscalc(rand_sims=xchoice.full.14[,,4], 
                                             dat=z.14[mdat.test.14$case_==1,4],
                                             avail=z.14[mdat.test.14$case_==0,4])

denshats.night.Pasture.full.14 <- uhcdenscalc(rand_sims=xchoice.full.14[,,5], 
                                              dat=z.14[mdat.test.14$case_==1,5],
                                              avail=z.14[mdat.test.14$case_==0,5])



###Reduced model####

design.mat.test.reduc1.14 <- model.matrix(form2b.14, data=mdat.test.14)
z.14 <- model.matrix(~ Forest+Pasture +tod:(Forest+Pasture) +sl_-1, 
                     data = mdat.test.14)[,-5]
xchoice.reduc1.14 <- uhcsimstrat(nsims = 1000,
                                 xmat = design.mat.test.reduc1.14, 
                                 stratum = mdat.test.14$step_id_, 
                                 fit_ssf = ssf.train.reduc1.14,
                                 z = z.14)    

denshats.Forest.reduc1.14 <- uhcdenscalc(rand_sims = xchoice.reduc1.14[,,1], 
                                         dat = z.14[mdat.test.14$case_==1,1], 
                                         avail = z.14[mdat.test.14$case_==0,1]) 
denshats.Pasture.reduc1.14 <- uhcdenscalc(rand_sims=xchoice.reduc1.14[,,2], 
                                          dat=z.14[mdat.test.14$case_==1,2], 
                                          avail=z.14[mdat.test.14$case_==0,2])  
denshats.sl.reduc1.14 <- uhcdenscalc(rand_sims=xchoice.reduc1.14[,,3], 
                                     dat=z.14[mdat.test.14$case_==1,3], 
                                     avail=z.14[mdat.test.14$case_==0,3]) 
denshats.night.Pasture.reduc1.14 <- uhcdenscalc(rand_sims=xchoice.reduc1.14[,,4], 
                                                dat=z.14[mdat.test.14$case_==1,4],
                                                avail=z.14[mdat.test.14$case_==0,4])

##Create UHC plot ####

###Full model####
panlabs1 <- c("A.","B.", "C.", "D.", "E.")
par(mfrow=c(2,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.full.14$densdat, 
            densrand = denshats.Forest.full.14$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.full.14$densavail) 
mtext(side=3, line=1,  panlabs1[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)
uhcdensplot(densdat = denshats.Pasture.full.14$densdat,
            densrand = denshats.Pasture.full.14$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.full.14$densavail) 
mtext(side=3, line=1,  panlabs1[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)
uhcdensplot(densdat = denshats.sl.full.14$densdat, 
            densrand = denshats.sl.full.14$densrand,
            includeAvail = TRUE, 
            densavail = denshats.sl.full.14$densavail) 
mtext(side=3, line=1,  panlabs1[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
uhcdensplot(densdat = denshats.night.Forest.full.14$densdat, 
            densrand = denshats.night.Forest.full.14$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Forest.full.14$densavail) 
mtext(side=3, line=1,  panlabs1[4], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Forest", cex=1.4)
uhcdensplot(densdat = denshats.night.Pasture.full.14$densdat, 
            densrand = denshats.night.Pasture.full.14$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.night.Pasture.full.14$densavail) 
mtext(side=3, line=1,  panlabs1[5], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Night Pasture", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot1.14, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.full.14 <- recordPlot()

###Reduced plot#####
panlabs2 <- c("F.","G.", "H.")

par(mfrow=c(1,3), mar=c(4,2,2,2), oma=c(3, 4, 7, 0), bty="L")
# First, the density plots
uhcdensplot(densdat = denshats.Forest.reduc1.14$densdat, 
            densrand = denshats.Forest.reduc1.14$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.Forest.reduc1.14$densavail) 
mtext(side=3, line=1,  panlabs2[1], cex=1.2, ad=0)
mtext(outer=F, side=2, line=3, "")
mtext(outer=F, side=3, line=1, "Forest", cex=1.4)
legend(0.3, 10.5, c("Available", "Used", "Predicted"), 
       lty=c(1, 2, 1), lwd=c(2,3, 10), 
       col=c("Black", "red", "gray"), bty="n", cex=1.8)
uhcdensplot(densdat = denshats.Pasture.reduc1.14$densdat,
            densrand = denshats.Pasture.reduc1.14$densrand,
            includeAvail = TRUE, 
            densavail = denshats.Pasture.reduc1.14$densavail) 
mtext(side=3, line=1,  panlabs2[2], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "Pasture", cex=1.4)

uhcdensplot(densdat = denshats.sl.reduc1.14$densdat, 
            densrand = denshats.sl.reduc1.14$densrand, 
            includeAvail = TRUE, 
            densavail = denshats.sl.reduc1.14$densavail) 
mtext(side=3, line=1,  panlabs2[3], cex=1.2, ad=0)
mtext(outer=F, side=3, line=1, "sl", cex=1.4)
mtext(outer=T, side=3, line=3,  textplot2, cex=1.8)
mtext(outer=T, side=2, line=1,  "Density", cex=1.8)
plot.uhc.reduc.14 <- recordPlot()

##UHC Plot - Difference between observed and predicted####

par(mfrow=c(2,4), mar=c(2,2,2,2), oma=c(3, 3, 5, 0), bty="L")
###Full Model####
uhcdiffdensplot(densdat = denshats.Forest.full.14$densdat, 
                densrand = denshats.Forest.full.14$densrand) 
mtext(side=3, line=1,  panlabs1[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.full.14$densdat, 
                densrand = denshats.Pasture.full.14$densrand)
mtext(side=3, line=1,  panlabs1[2], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Forest.full.14$densdat, 
                densrand = denshats.night.Forest.full.14$densrand)
mtext(side=3, line=1,  panlabs1[3], cex=1, ad=0)
uhcdiffdensplot(densdat = denshats.night.Pasture.full.14$densdat, 
                densrand = denshats.night.Pasture.full.14$densrand) 
mtext(side=3, line=1,  panlabs1[4], cex=1, ad=0)

###Reduced Model 1#####
uhcdiffdensplot(densdat = denshats.Forest.reduc1.14$densdat, 
                densrand = denshats.Forest.reduc1.14$densrand) 
mtext(side=3, line=1,  panlabs2[1], cex=1, ad=0)
mtext(outer=F, side=2, line=3, "Observed-Predicted")
uhcdiffdensplot(densdat = denshats.Pasture.reduc1.14$densdat, 
                densrand = denshats.Pasture.reduc1.14$densrand)
mtext(side=3, line=1,  panlabs2[3], cex=1, ad=0)

plot.uhc.differences.14 <- recordPlot()
