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

#set working directory and import data - replace file directory an dfile names
setwd("XXXXXX")

newdataNOSC<-read.csv( "CCCCC", header = T)

#create theme for figures
my_theme = theme(axis.title.x = element_text(size = 12, face = "bold"),
                 axis.text.x = element_text(size = 10, face = "bold"),
                 axis.title.y = element_text(size = 12, face = "bold"),
                 axis.text.y = element_text(size = 10))

#convert fahrneheit to celsius
newdataNOSC$Air_temp<-((newdataNOSC$Air_temp)-32)*0.556

#random forest for HSIo as a continous variable


fitHSIoNOSC <- randomForest(HSIo~FDD+Discharge+Air_temp+Aquatic_conn+AWD+Chl_a+Latitude+Eff_conn+Ice_depth+Snow_depth+Lotic_conn+Max_depth, data=newdataNOSC, type = "prob", localImp = TRUE)
print(fitHSIoNOSC)

vimp<-(vip(fitHSIoNOSC, bar = FALSE, horizontal = TRUE)+my_theme)


vimp

#partial dependence plots from random forest modelling HSIo

p1<-partial(fitHSIoNOSC, pred.var = "Air_temp", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

airtemp_pdp<-p1+my_theme+labs(y= "M.E.", x = "Air temperature (C)")+geom_line(size = 2)

p2<-partial(fitHSIoNOSC, pred.var = "Max_depth", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

Max_depth_pdp<-p2+my_theme+labs(y= "M.E.", x = "Maximum depth (m)")+geom_line(size = 2)

p3<-partial(fitHSIoNOSC, pred.var = "Chl_a", plot = TRUE, rug = TRUE, plot.engine = "ggplot2")

chl_pdp_pdp<-p3+my_theme+labs(y= "M.E.", x = "Chl a (ug/L)")+geom_line(size = 2)

p4<-partial(fitHSIoNOSC, pred.var = "Lotic_conn", plot = TRUE, rug = TRUE, plot.engine = "ggplot2")

lotic_pdp<-p4+my_theme+labs(y= "M.E.", x = "Adjacent lotic habitat (%)")+geom_line(size = 2)

p5<-partial(fitHSIoNOSC, pred.var = "FDD", plot = TRUE, rug = TRUE,plot.engine = "ggplot2")

fdd_pdp<-p5+my_theme+labs(y= "M.E.", x = "Freezing degree days")+geom_line(size = 2)

p6<-partial(fitHSIoNOSC, pred.var = "Discharge", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

dis_pdp<-p6+my_theme+labs(y= "M.E.", x = "Discharge (cfs)")+geom_line(size = 2)

p7<-partial(fitHSIoNOSC, pred.var = "AWD", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

AWD_pdp<-p7+my_theme+labs(y= "M.E.", x = "Available water depth (m)")+geom_line(size = 2)

p8<-partial(fitHSIoNOSC, pred.var = "Ice_depth", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

icedepth_pdp<-p8+my_theme+labs(y= "M.E.", x = "Ice thickness (cm)")+geom_line(size = 2)

p9<-partial(fitHSIoNOSC, pred.var = "Aquatic_conn", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

conn_aquatic_pdp<-p9+my_theme+labs(y= "M.E.", x = "Adjacent aquatic habitat (%)")+geom_line(size = 2)

p10<-partial(fitHSIoNOSC, pred.var = "Eff_conn", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

econ_pdp<-p10+my_theme+labs(y= "M.E.", x = "Effective connections")+geom_line(size = 2)

p11<-partial(fitHSIoNOSC, pred.var = "Latitude", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

lat_pdp<-p11+my_theme+labs(y= "M.E.", x = "Latitude (Northing)")+geom_line(size = 2)

p12<-partial(fitHSIoNOSC, pred.var = "Snow_depth", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

Snow_pdp<-p12+my_theme+labs(y= "M.E.", x = "Snow depth (cm)")+geom_line(size = 2)

middle_row<-plot_grid(chl_pdp_pdp, fdd_pdp, lotic_pdp, airtemp_pdp, dis_pdp, AWD_pdp,
                      labels=c("b","c","d","e", "f", "g"),
                      nrow=2, ncol=3, align="hv")
middle_row

ggsave("Figure8_Season_2panel_052623.png", height=12, width=9, units="in")

#actual vs. predicted plot from random forest modelling HSIo
#create data frame with actual HSIo values from all sites AND with the predicted HSIo values using the random forest model for all sites 
actualAndPredictedData = data.frame(actualValue = newdataNOSC$HSIo, 
                                    predictedValue = predict(fitHSIoNOSC,newdataNOSC))
#create plot
plot12<-ggplot(actualAndPredictedData, aes(x = actualValue, y = predictedValue))+geom_point(size = 2)

pred_actual_RF<-plot12+geom_abline(intercept = 0, slope = 1, color = "red", size = 1.5)+my_theme+xlab("Actual overall HSI")+ylab("Predicted overall HSI")+my_theme


plot_grid(pred_actual_RF, middle_row, vimp,
          labels = c('a', '', 'h'),
          nrow=3, ncol=1)

#merged pdp for SI

PDP_SI<-plot_grid(lat_pdp, icedepth_pdp,conn_aquatic_pdp,econ_pdp,Snow_pdp, Max_depth_pdp,
                  labels=c("a","b","c","d", "e", "f"),
                  nrow=3, ncol=2, align="hv")
PDP_SI



#check for interactions in random forest modelling HSIo
#dataframes used to look for interactions within parameter-level models

dfinter<-newdataNOSC[c("HSIo","Max_depth","Lotic_conn", "Latitude", "Discharge","Eff_conn", "Ice_depth", "AWD", "Air_temp", "Aquatic_conn", "Snow_depth", "Chl_a", "FDD")]

#HSIO interactions
rf_HSIo  <- vivi(fit = fitHSIoNOSC, 
                 data = dfinter, 
                 response = "HSIo",
                 gridSize = 10,
                 importanceType = "%IncMSE",
                 nmax = 100,
                 reorder = TRUE,
                 class = 1,
                 predictFun = NULL)
viviHeatmap(mat = rf_HSIo)

HSIo_inter<-viviNetwork(mat = rf_HSIo, intLim= c(0.1,1))
HSIo_inter

#MODELING drivers of the parameters that determine HSIo (Temp, DO, and flow velocity)

#random forest model for water temperature
fittempC <- randomForest(newdataNOSC$Water_temp~Discharge+Aquatic_conn+AWD+Latitude+Aquatic_conn+Lotic_conn+Air_temp+FDD+Max_depth, data=newdataNOSC, type = "prob", localImp = TRUE)
print(fittempC)

importance(fittempC)
varImpPlot(fittempC)

#partial dependence plots from key independent variables from random forest modeling water temperature

#air temperature 
t1<-partial(fittempC, pred.var = "Air_temp", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

airtemp_Tpdp<-t1+my_theme+labs(y= "M.E.", x = "Air temperature (C)")+geom_line(size = 2)
airtemp_Tpdp

#available water depth
t2<-partial(fittempC, pred.var = "AWD", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

AWD_Tpdp<-t2+my_theme+labs(y= "M.E.", x = "Available water depth (m)")+geom_line(size = 2)
AWD_Tpdp

#discharge
t3<-partial(fittempC, pred.var = "Discharge", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

dis_Tpdp<-t3+my_theme+labs(y= "M.E.", x = "Discharge (cfs)")+geom_line(size = 2)
dis_Tpdp

#lotic connectivity
t4<-partial(fittempC, pred.var = "Lotic_conn", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

lotic_Tpdp<-t4+my_theme+labs(y= "M.E.", x = "Adjacent lotic habitat (%)")+geom_line(size = 2)
lotic_Tpdp

#FDD
t5<-partial(fittempC, pred.var = "FDD", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

FDD_Tpdp<-t4+my_theme+labs(y= "M.E.", x = "Freezing degree days")+geom_line(size = 2)
FDD_Tpdp

#Latitude
t6<-partial(fittempC, pred.var = "Latitude", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

Lat_Tpdp<-t4+my_theme+labs(y= "M.E.", x = "Latitude (Northing)")+geom_line(size = 2)
Lat_Tpdp

#merge top 4 pdp plots for combined figure
Temp_RF_pdp<-plot_grid(FDD_Tpdp, airtemp_Tpdp, AWD_Tpdp , lotic_Tpdp,
                       labels=c("a","b","c","d"),
                       nrow=2, ncol=2, align="hv")
Temp_RF_pdp

#random forest model for dissolved oxygen
fitDOC <- randomForest(newdataNOSC$DO~Discharge+Chl_a+AWD+Latitude+Aquatic_conn+Lotic_conn+Air_temp+FDD+Max_depth+Ice_depth+Snow_depth, data=newdataNOSC, type = "prob", localImp = TRUE)
print(fitDOC)

importance(fitDOC)
varImpPlot(fitDOC)

#partial dependance plots for notable independent varaibles from random forest modeling DO

#chl a
DO1<-partial(fitDOC, pred.var = "Chl_a", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

chl_DOpdp<-DO1+my_theme+labs(y= "M.E.", x = "Chl a (ug/l)")+geom_line(size = 2)
chl_DOpdp

#FDD
DO2<-partial(fitDOC, pred.var = "FDD", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

fdd_DOpdp<-DO2+my_theme+labs(y= "M.E.", x = "FDD")+geom_line(size = 2)
fdd_DOpdp

#snow
DO3<-partial(fitDOC, pred.var = "Snow_depth", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

snow_DOpdp<-DO3+my_theme+labs(y= "M.E.", x = "Snow depth (cm)")+geom_line(size = 2)
snow_DOpdp

#discharge
DO4<-partial(fitDOC, pred.var = "Discharge", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

dis_DOpdp<-DO4+my_theme+labs(y= "M.E.", x = "Discharge (cfs)")+geom_line(size = 2)
dis_DOpdp

#latitude
DO4<-partial(fitDOC, pred.var = "Latitude", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

Lat_DOpdp<-DO4+my_theme+labs(y= "M.E.", x = "Latitude (Northing)")+geom_line(size = 2)
Lat_DOpdp

#merge top 4 pdp plots for combined figure
DO_RF_pdp<-plot_grid(chl_DOpdp, fdd_DOpdp , snow_DOpdp, dis_DOpdp,
                     labels=c("e","f","g","h"),
                     nrow=2, ncol=2, align="hv")
DO_RF_pdp


#random forest model for flow velocity
fitvelC <- randomForest(newdataNOSC$Velocity~Discharge+Aquatic_conn+AWD+Latitude+Eff_conn+Lotic_conn+Max_depth, data=newdataNOSC, type = "prob", localImp = TRUE)
print(fitvelC)
varImpPlot(fitvelC)


#partial dependence plots for notable independent variables from random forest modeling velocity
#AWD
V1<-partial(fitvelC, pred.var = "AWD", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

AWD_Vpdp<-V1+my_theme+labs(y= "M.E.", x = "Available water depth (m)")+geom_line(size = 2)
AWD_Vpdp

#Discharge
V2<-partial(fitvelC, pred.var = "Discharge", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

dis_Vpdp<-V2+my_theme+labs(y= "M.E.", x = "Discharge (cfs))")+geom_line(size = 2)
dis_Vpdp

#aquatic connectivity
V3<-partial(fitvelC, pred.var = "Aquatic_conn", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

aqua_Vpdp<-V3+my_theme+labs(y= "M.E.", x = "Adjacent aquatic habitat (%)")+geom_line(size = 2)
aqua_Vpdp


#latitude
V4<-partial(fitvelC, pred.var = "Latitude", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

lat_Vpdp<-V4+my_theme+labs(y= "M.E.", x = "Latitude (Northing)")+geom_line(size = 2)
lat_Vpdp

#max depth
V5<-partial(fitvelC, pred.var = "Max_depth", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

MaxD_Vpdp<-V5+my_theme+labs(y= "M.E.", x = "Maximum depth (m)")+geom_line(size = 2)
MaxD_Vpdp

#lotic connectivity
V6<-partial(fitvelC, pred.var = "Lotic_conn", plot = TRUE,rug = TRUE, plot.engine = "ggplot2")

LotD_Vpdp<-V6+my_theme+labs(y= "M.E.", x = "Adjacent lotic habitat (%)")+geom_line(size = 2)
LotD_Vpdp

#merge previous 4 pdp plots for combined figure
vel_RF_pdp<-plot_grid(AWD_Vpdp,dis_Vpdp,aqua_Vpdp,  lat_Vpdp,
                      labels=c("i","j","k","l"),
                      nrow=2, ncol=2, align="hv")

vel_RF_pdp

#MErge all parameter pdpS

parameter_RF_pdp<-plot_grid(Temp_RF_pdp, DO_RF_pdp, vel_RF_pdp,
                            
                            nrow=3, ncol=1, align="hv")

parameter_RF_pdp

#dataframes used to look for interactions within parameter-level models

dfinter<-newdataNOSC[c("HSIo","Max_depth","Lotic_conn", "Latitude", "Discharge","Eff_conn", "Ice_depth", "AWD", "Air_temp", "Aquatic_conn", "Snow_depth", "Chl_a", "FDD")]
dfinterVEL<-newdataNOSC[c("Velocity","AWD","Discharge", "Latitude", "Aquatic_conn","Max_depth", "Lotic_conn", "Eff_conn")]
dfinterDO<-newdataNOSC[c("DO","Discharge","Chl_a","AWD","Latitude","Aquatic_conn","Lotic_conn","Air_temp","FDD","Max_depth","Ice_depth","Snow_depth")]
dfinterTEMP<-newdataNOSC[c('Water_temp','Discharge','AWD','Latitude','Aquatic_conn','Lotic_conn','Air_temp','FDD','Max_depth')]

#checking for interactions within the temperature RF model

dfinterTEMP<-newdataNOSC[c('Water_temp','Discharge','AWD','Latitude','Aquatic_conn','Lotic_conn','Air_temp','FDD','Max_depth')]

dfinterTEMP

rf_fit_temp  <- vivi(fit = fittempC, 
                     data = dfinterTEMP, 
                     response = "Water_temp",
                     gridSize = 10,
                     importanceType = "%IncMSE",
                     nmax = 100,
                     reorder = TRUE,
                     class = 1,
                     predictFun = NULL)
viviHeatmap(mat = rf_fit_temp)

viviNetwork(mat = rf_fit_temp)

temp_inter<-viviNetwork(mat = rf_fit_temp, intLim= c(0.1,1))
temp_inter

#checking for interactions within the DO RF model
rf_fit_DO  <- vivi(fit = fitDOC, 
                   data = dfinterDO, 
                   response = "DO",
                   gridSize = 10,
                   importanceType = "%IncMSE",
                   nmax = 100,
                   reorder = TRUE,
                   class = 1,
                   predictFun = NULL)
viviHeatmap(mat = rf_fit_DO)

viviNetwork(mat = rf_fit_DO)

DO_inter<-viviNetwork(mat = rf_fit_DO, intLim= c(0.1,1))
DO_inter

#checking for interactions within the velocity RF model

rf_fit_vel  <- vivi(fit = fitvelC, 
                    data = dfinterVEL, 
                    response = "Velocity",
                    gridSize = 10,
                    importanceType = "%IncMSE",
                    nmax = 100,
                    reorder = TRUE,
                    class = 1,
                    predictFun = NULL)
viviHeatmap(mat = rf_fit_vel)

viviNetwork(mat = rf_fit_vel)

vel_inter<-viviNetwork(mat = rf_fit_vel, intLim= c(0.1,1))
vel_inter

#MErge interaction plots


interaction_plots<-plot_grid(HSIo_inter, temp_inter, DO_inter, vel_inter,
                             labels=c("a","b","c","d"),
                             
                             nrow=2, ncol=2, align="hv")
interaction_plots


#CORRELATION PLOT

library(ggcorrplot)
newdataNOSC
dfcorr<-newdataNOSC[c("Max_depth","Lotic_conn", "Latitude", "Discharge","Eff_conn", "Ice_depth", "AWD", "Air_temp", "Aquatic_conn", "Snow_depth", "Chl_a", "FDD")]
corr<- round(cor(dfcorr, method = "spearman"), 1)




ggcorr<-ggcorrplot(corr, insig = "blank", lab = TRUE, type = "lower", colors = c("red", "white", "blue"))

ggcorr+theme(axis.text.x = element_text(size = 12),
             axis.text.y = element_text(size = 12))




