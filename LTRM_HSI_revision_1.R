#load all packages for sorting, general exploration, analyses, an dpossible figures
library(dplyr)
library(plyr)
library(tidyr)
library(car)
library(lmerTest)
library(lme4)
library(MASS)
library(ggplot2)
library(ggExtra)
library(gridExtra)
library(scales)
library(nlme)
library(plyr)
library(rpart)
library(randomForest)
library(gbm)
library(dismo)
library(caret)
library(mlbench)
library(vip)
library(pdp)
library(randomForestExplainer)
library(vivid)
library(sna)
library(vivid)
library(intergraph)
library(cowplot)
library(VSURF) 
library(ggRandomForests)
library(randomForestSRC)
#set correct WD

getwd()
setwd("XXXXXX")

#reead in CSV file
VSURFSUB<-read.csv( "VSURFSUB.csv", header = T)

#setseed
# Setting a seed
set.seed(123)

#meanHSI VSURF process

hsi.vsurf <- VSURF(VSURFSUB[,5:19], VSURFSUB[,4], mtry = 100)

plot(hsi.vsurf, step = "pred", imp.sd = FALSE, var.names = TRUE) #view selected variable under predcited category in VSURF package

table(hsi.vsurf$varselect.pred)

#run random forest model with variables selected in VSURF
fitHSIoNOSC_SRC <- rfsrc(meanHSIo~num_outl+FDD+pct_outl+CHLcal+avg_fetch+dis_z_score+WDP+pct_aqveg+ZICE+econ, data=VSURFSUB, importance = TRUE) 
print(fitHSIoNOSC_SRC)


#GGPLOT RFSRCF
#create theme
#create theme for figures
my_theme = theme(axis.title.x = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size = 10, face = "bold"),
                 axis.title.y = element_text(size = 12, face = "bold"),
                 axis.text.y = element_text(size = 10))

#prepare interface between RFSRC and FGGplot
varsel_hsi<-var.select(fitHSIoNOSC_SRC)

gg_md<-gg_minimal_vimp(varsel_hsi)

plot(gg_md)

gg_hsi<-gg_variable(fitHSIoNOSC_SRC)

gg_v<-gg_variable(fitHSIoNOSC_SRC)

xvar<-gg_md$names

plot(gg_v, panel = TRUE)



partial_hsio2<-plot.variable(fitHSIoNOSC_SRC,
                             xvar = xvar,
                             partial = TRUE, sorted = TRUE,
                             show.plots = FALSE)

gg_p <- gg_partial(partial_hsio2)

hsio_partialplot<-plot(gg_p, panel=TRUE)+my_theme+stat_smooth(se =FALSE,  size = 1, method = loess)+geom_point(size = 1.5)+ labs(x="", y = "Partial effect on HSIo")

hsio_partialplot


#temp VSURF selection and SRC random forest models

temp.vsurf <- VSURF(VSURFSUB[,5:19], VSURFSUB[,1], mtry = 100)

table(temp.vsurf)

plot(temp.vsurf, step = "pred", imp.sd = FALSE, var.names = TRUE)

fitTEMPNOSC_SRC <- rfsrc(TEMP.no.negatives.2~FDD+pct_outl+dis_z_score, data=VSURFSUB, importance = TRUE) 
print(fitTEMPNOSC_SRC)

#GGPLOT RFSRCF temp
varsel_temp<-var.select(fitTEMPNOSC_SRC)

gg_md_temp<-gg_minimal_vimp(varsel_temp)

plot(gg_md_temp)

gg_temp<-gg_variable(fitTEMPNOSC_SRC)

gg_v_temp<-gg_variable(fitTEMPNOSC_SRC)

xvar_temp<-gg_md_temp$names

plot(gg_v_temp, panel = TRUE)



partial_temp<-plot.variable(fitTEMPNOSC_SRC,
                            xvar = xvar,
                            partial = TRUE, sorted = TRUE,
                            show.plots = FALSE)

gg_p_temp <- gg_partial(partial_temp)

temp_partialplot<-plot(gg_p_temp, panel=TRUE)+my_theme+stat_smooth(se =FALSE,  size = 1, method = loess)+geom_point(size = 1.5)+ labs(x="", y = "Partial effect on temp. (C)")

#DO VSURF and SRC
DO.vsurf <- VSURF(VSURFSUB[,5:19], VSURFSUB[,2], mtry = 100)

summary(DO.vsurf)

plot(DO.vsurf)

plot(DO.vsurf, step = "pred", imp.sd = FALSE, var.names = TRUE)

fitDONOSC_SRC <- rfsrc(DO~FDD+CHLcal+ZSNOW+dis_z_score+pct_outl+ZICE+pct_aqveg+avg_fetch+sdi+WDP, data=VSURFSUB, importance = TRUE) 
print(fitDONOSC_SRC)

#GGPLOT RFSRCF DO
varsel_DO<-var.select(fitDONOSC_SRC)

gg_md_DO<-gg_minimal_vimp(varsel_DO)

plot(gg_md_DO)

gg_DO<-gg_variable(fitDONOSC_SRC)

gg_v_DO<-gg_variable(fitDONOSC_SRC)

xvar_DO<-gg_md_DO$names

plot(gg_v_DO, panel = TRUE)



partial_DO<-plot.variable(fitDONOSC_SRC,
                          xvar = xvar_DO,
                          partial = TRUE, sorted = TRUE,
                          show.plots = FALSE)

gg_p_DO <- gg_partial(partial_DO)

do_partialplot<-plot(gg_p_DO, panel=TRUE)+my_theme+stat_smooth(se =FALSE,  size = 1, method = loess)+geom_point(size = 1.5)+ labs(x="", y = "Partial effect on DO (mg/L)")

do_partialplot

#velocity vsurf and src
vel.vsurf <- VSURF(VSURFSUB[,5:19], VSURFSUB[,3], mtry = 100)

summary(vel.vsurf)

plot(vel.vsurf)

plot(vel.vsurf, step = "pred", imp.sd = FALSE, var.names = TRUE)

fitVELNOSC_SRC <- rfsrc(vel_cm_s~FDD+WDP+pct_outl+dis_z_score, data=VSURFSUB, importance = TRUE) 
print(fitVELNOSC_SRC)

#GGPLOT RFSRCF VEL
varsel_VEL<-var.select(fitVELNOSC_SRC)

gg_md_VEL<-gg_minimal_vimp(varsel_VEL)

plot(gg_md_VEL)

gg_VEL<-gg_variable(fitVELNOSC_SRC)

gg_v_VEL<-gg_variable(fitVELNOSC_SRC)

xvar_VEL<-gg_md_VEL$names

plot(gg_v_VEL, panel = TRUE)



partial_VEL<-plot.variable(fitVELNOSC_SRC,
                           xvar = xvar,
                           partial = TRUE, sorted = TRUE,
                           show.plots = FALSE)

gg_p_VEL <- gg_partial(partial_VEL)

vel_partialplot<-plot(gg_p_VEL, panel=TRUE)+my_theme+geom_smooth(se =FALSE,  size = 1, method = loess)+geom_point(size = 1.5)+ labs(x="", y = "Partial effect on velocity (cm/s)")

vel_partialplot


