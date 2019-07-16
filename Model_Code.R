## OST-P TRIAD Pedestrian Fatality ##
## Analyze fatalities by tract using FARS, ACS, HPMS data ##
## Authors: Dana Peck, Ted Mansfield

#### load libraries and data ####
require(foreign)
require(MASS)
require(ggplot2)
require(boot)
require(pscl)
require(pls)
require(psych)
require(nonnest2)
require(SemiParSampleSel)
library(car)
library(corrgram)
require(AER)
library(scales)
library(dplyr)
library(lubridate)
library(reshape2)
require(stats)
require(tidyverse)
library(ggthemes)
library(glmmTMB)

# Set up Working directory
#setwd("C:/Users/Ted Mansfield/Desktop/Model/R")
setwd("C:/Users/Coleman.Shepard.ctr/DOT/Pedestrian Fatalities/Pedestrian-Fatality-Risk-Project-master")

# Load data
Data <- read.csv("Model_Input_Data.csv") #Note: tracts with zero land area not included

# Variable names
vars <- c("GEOID",
          "PF12","PF13","PF14","PF15","PF16","PF1216",
          "LandArea","HabArea",
          "ResPop","DayPop","AvePop","ResPopDen","AvePopDen",
          "Urb50","UnHabArea","UnHabPer",
          "Tribal50","Tribal33", "NSP50","NSP33",
          "LPQPer","LPHPer",
          "FC12_VMT","FC3_VMT","FC4_VMT","FC5_VMT","FC6_VMT","FC56_VMT",
          "EmpOff","EmpRet","EmpInd","EmpSer","EmpEnt","EmpTrWa","EmpOff_SQ","EmpRet_SQ","EmpInd_SQ","EmpSer_SQ","EmpEnt_SQ","EmpTrWa_SQ","ActEnt",
          "M_18","M_18_24","M_25_34","M_35_44","M_45_54","M_55_64","M_65",
          "F_18","F_18_24","F_25_34","F_35_44","F_45_54","F_55_64","F_65",
          "All_18","All_18_24","All_25_34","All_35_44","All_45_54","All_55_64","All_65",
          "M","F",
          "Prop_White","Prop_Black","Prop_Hisp","Prop_Asian","Prop_Other",
          "Prop_TransitToWork","Prop_WalkToWork",
          "Zero_Veh","Rent","HH_INC",
          "ID_auto","ID_mm3",
          "State","Census_R","Census_D","Fed_Div","UrbanArea","CSA","CBSA",
          "Complete_Streets","Ped_Crossing","Open_Container","Ignit_Inter"
) 

# Data frames
df <- Data 
names(Data) <- vars
Data$Urb50 <- as.factor(Data$Urb50)
Data$Tribal50 <- as.factor(Data$Tribal50)
Data$Tribal33 <- as.factor(Data$Tribal33)
Data$NSP50 <- as.factor(Data$NSP50)
Data$NSP33 <- as.factor(Data$NSP33)
Data$State <- as.factor(Data$State)
Data$Census_R <- as.factor(Data$Census_R)
Data$Census_D <- as.factor(Data$Census_D)
Data$Fed_Div <- as.factor(Data$Fed_Div)
Data$UrbanArea <- as.factor(Data$UrbanArea)
Data$CBSA <- as.factor(Data$CBSA)
Data$CSA <- as.factor(Data$CSA)
Data$Complete_Streets <- as.factor(Data$Complete_Streets)
Data$Ped_Crossing <- as.factor(Data$Ped_Crossing)
Data$Open_Container <- as.factor(Data$Open_Container)
Data$Ignit_Inter <- as.factor(Data$Ignit_Inter)

# Split Urban/Rural
Data_urb <- subset(Data, Data$Urb50==1)
Data_rur <- subset(Data, Data$Urb50==0)

# Re-scale Urban Variables
Data_urb$HabArea <- log(Data_urb$HabArea+1)
Data_urb$ResPopDen <- Data_urb$ResPopDen/1000
Data_urb$AvePopDen <- Data_urb$AvePopDen/1000
Data_urb$FC12_VMT <- (Data_urb$FC12_VMT/10000)/Data_urb$LandArea
Data_urb$FC3_VMT <- (Data_urb$FC3_VMT/10000)/Data_urb$LandArea
Data_urb$FC4_VMT <- (Data_urb$FC4_VMT/10000)/Data_urb$LandArea
Data_urb$FC5_VMT <- (Data_urb$FC5_VMT/10000)/Data_urb$LandArea
Data_urb$FC6_VMT <- (Data_urb$FC6_VMT/10000)/Data_urb$LandArea
Data_urb$FC56_VMT <- (Data_urb$FC56_VMT/10000)/Data_urb$LandArea
Data_urb$EmpOff <- (Data_urb$EmpOff/100)/Data_urb$LandArea
Data_urb$EmpRet <- (Data_urb$EmpRet/100)/Data_urb$LandArea
Data_urb$EmpInd <- ((Data_urb$EmpInd+Data_urb$EmpTrWa)/100)/Data_urb$LandArea
Data_urb$EmpSer <- (Data_urb$EmpSer/100)/Data_urb$LandArea
Data_urb$EmpEnt <- (Data_urb$EmpEnt/100)/Data_urb$LandArea
Data_urb$EmpTrWa <- (Data_urb$EmpTrWa/100)/Data_urb$LandArea
Data_urb$EmpOff_SQ <- (((Data_urb$EmpOff_SQ)^2)/100)^.5/Data_urb$LandArea
Data_urb$EmpRet_SQ <- (((Data_urb$EmpRet_SQ)^2)/100)^.5/Data_urb$LandArea
Data_urb$EmpInd_SQ <- ((((Data_urb$EmpInd_SQ)^2)/100)+(((Data_urb$EmpTrWa_SQ)^2)/100))^.5/Data_urb$LandArea
Data_urb$EmpSer_SQ <- (((Data_urb$EmpSer_SQ)^2)/100)^.5/Data_urb$LandArea
Data_urb$EmpEnt_SQ <- (((Data_urb$EmpEnt_SQ)^2)/100)^.5/Data_urb$LandArea
Data_urb$EmpTrWa_SQ <- (((Data_urb$EmpTrWa_SQ)^2)/100)^.5/Data_urb$LandArea
Data_urb$ActEnt <- Data_urb$ActEnt/10
Data_urb$HH_INC <- Data_urb$HH_INC/1000
Data_urb$ID_auto <- Data_urb$ID_auto/640
Data_urb$ID_mm3 <- Data_urb$ID_mm3/640

# Re-scale Rural Variables
Data_rur$HabArea <- log(Data_rur$HabArea+1)
Data_rur$ResPopDen <- Data_rur$ResPopDen/100
Data_rur$AvePopDen <- Data_rur$AvePopDen/100
Data_rur$FC12_VMT <- (Data_rur$FC12_VMT/1000)/Data_rur$LandArea
Data_rur$FC3_VMT <- (Data_rur$FC3_VMT/1000)/Data_rur$LandArea
Data_rur$FC4_VMT <- (Data_rur$FC4_VMT/1000)/Data_rur$LandArea
Data_rur$FC5_VMT <- (Data_rur$FC5_VMT/1000)/Data_rur$LandArea
Data_rur$FC6_VMT <- (Data_rur$FC6_VMT/1000)/Data_rur$LandArea
Data_rur$FC56_VMT <- (Data_rur$FC56_VMT/1000)/Data_rur$LandArea
Data_rur$EmpOff <- (Data_rur$EmpOff/1)/Data_rur$LandArea
Data_rur$EmpRet <- (Data_rur$EmpRet/1)/Data_rur$LandArea
Data_rur$EmpInd <- ((Data_rur$EmpInd+Data_rur$EmpTrWa)/1)/Data_rur$LandArea
Data_rur$EmpSer <- (Data_rur$EmpSer/1)/Data_rur$LandArea
Data_rur$EmpEnt <- (Data_rur$EmpEnt/1)/Data_rur$LandArea
Data_rur$EmpTrWa <- (Data_rur$EmpTrWa/1)/Data_rur$LandArea
Data_rur$EmpOff_SQ <- (((Data_rur$EmpOff_SQ)^2)/1)^.5/Data_rur$LandArea
Data_rur$EmpRet_SQ <- (((Data_rur$EmpRet_SQ)^2)/1)^.5/Data_rur$LandArea
Data_rur$EmpInd_SQ <- ((((Data_rur$EmpInd_SQ)^2)/1)+(((Data_rur$EmpTrWa_SQ)^2)/1))^.5/Data_rur$LandArea
Data_rur$EmpSer_SQ <- (((Data_rur$EmpSer_SQ)^2)/1)^.5/Data_rur$LandArea
Data_rur$EmpEnt_SQ <- (((Data_rur$EmpEnt_SQ)^2)/1)^.5/Data_rur$LandArea
Data_rur$EmpTrWa_SQ <- (((Data_rur$EmpTrWa_SQ)^2)/1)^.5/Data_rur$LandArea
Data_rur$ActEnt <- Data_rur$ActEnt/10
Data_rur$HH_INC <- Data_rur$HH_INC/1000
Data_rur$ID_auto <- Data_rur$ID_auto/640
Data_rur$ID_mm3 <- Data_rur$ID_mm3/640

# Drop outlier observation
Data_rur <- subset(Data_rur, Data_rur$FC3_VMT<900)

# Urban model
urb_null = glmmTMB(PF1216 ~1 , zi= ~1 , data=Data_urb, family=nbinom2, offset = (AvePop/50000), verbose = TRUE,
                   control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))) 

urb_nb = glmmTMB(PF1216 ~ 
                   ResPopDen +
                   FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                   EmpSer_SQ + EmpEnt + ActEnt + 
                   HH_INC + Prop_Black + Prop_Hisp + Prop_Asian + Prop_Other +
                   HabArea + 
                   M_18_24 + M_45_54 + M_55_64 + M_65 + F_45_54 + F_55_64 + F_65 +
                   (1|CSA),
                      data=Data_urb, family=nbinom2, offset = (AvePop/50000), verbose = TRUE,
                      control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

urb_zinb <- glmmTMB(PF1216 ~ 
                      ResPopDen +
                      FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                      EmpSer_SQ + EmpEnt + ActEnt + 
                      HH_INC + Prop_Black + Prop_Hisp + Prop_Asian + Prop_Other +
                      HabArea + 
                      M_18_24 + M_45_54 + M_55_64 + M_65 + F_45_54 + F_55_64 + F_65 +
                      (1|CSA),
                    zi= ~ Prop_TransitToWork + ID_auto + ID_mm3 + 
                      FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                      EmpOff_SQ + EmpRet_SQ + EmpInd_SQ + EmpSer_SQ + 
                      HabArea + State,
                    data=Data_urb, family=nbinom2, offset = (AvePop/50000), verbose = TRUE,
                    control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

urb_zinb_RP <- glmmTMB(PF1216 ~ 
                             ResPopDen +
                             FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                             EmpSer_SQ + EmpEnt + ActEnt + 
                             HH_INC + Prop_Black + Prop_Hisp + Prop_Asian + Prop_Other +
                             HabArea + 
                             M_18_24 + M_45_54 + M_55_64 + M_65 + F_45_54 + F_55_64 + F_65 +
                             (0+FC12_VMT|CSA) + (0+FC3_VMT|CSA) + (0+FC4_VMT|CSA) + (0+FC56_VMT|CSA) + 
                             (0+EmpSer_SQ|CSA) + (0+ActEnt|CSA) + (1|CSA),
                           zi= ~ Prop_TransitToWork + ID_auto + ID_mm3 + 
                             FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                             EmpOff_SQ + EmpRet_SQ + EmpInd_SQ + EmpSer_SQ + 
                             HabArea + State,
                           data=Data_urb, family=nbinom2, offset = (AvePop/50000), verbose = TRUE,
                           control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

ranef <- ranef(urb_zinb_RP)
write.csv(ranef$cond, 'urb_ranef_cond.csv')

zinb_obs_pred <- as.data.frame(cbind(Data_urb$PF1216, fitted(urb_zinb_4RP_noZI)))
plot(zinb_obs_pred$V1, zinb_obs_pred$V2)
mean(zinb_obs_pred$V2[zinb_obs_pred$V1==0])
mean(zinb_obs_pred$V2[zinb_obs_pred$V1>0])

zinb_obs_pred_RP <- as.data.frame(cbind(Data_urb$PF1216, fitted(urb_zinb_4RP_noZI_RP)))
plot(zinb_obs_pred_RP$V1, zinb_obs_pred_RP$V2)
mean(zinb_obs_pred_RP$V2[zinb_obs_pred_RP$V1==0])
mean(zinb_obs_pred_RP$V2[zinb_obs_pred_RP$V1>0])

# LR ChiSquare
lr_urb_nb <- lrtest(urb_null,urb_nb)
lr_urb_zinb <- lrtest(urb_null,urb_zinb)
lr_urb_zinb_RP <- lrtest(urb_null,urb_zinb_RP)
lr_urb_zinb_vs_nb <- lrtest(urb_nb,urb_zinb_RP)
lr_urb_zinb_RP_vs_nb <- lrtest(urb_nb,urb_zinb_RP)
lr_urb_zinb_RP_vs_zinb <- lrtest(urb_zinb,urb_zinb_RP)

# Nagelkerke R2
urb_R2_nb <- (1-exp(-as.numeric(lr_urb_nb$Chisq[2])/nrow(Data_urb)))/(1-exp(2*as.numeric(logLik(urb_null)/nrow(Data_urb))))
urb_R2_zinb <- (1-exp(-as.numeric(lr_urb_zinb$Chisq[2])/nrow(Data_urb)))/(1-exp(2*as.numeric(logLik(urb_null)/nrow(Data_urb))))
urb_R2_zinb_RP <- (1-exp(-as.numeric(lr_urb_zinb_RP$Chisq[2])/nrow(Data_urb)))/(1-exp(2*as.numeric(logLik(urb_null)/nrow(Data_urb))))

mean(fitted(urb)[Data_urb$PF1216==0])
mean(fitted(urb)[Data_urb$PF1216>0])
plot((Data_urb$PF1216), fitted(urb))

# rural model
rur_null <- glmmTMB(PF1216 ~ 1, zi= ~ 1,
                    data=Data_rur, family=nbinom2, offset = (AvePop/25000), verbose = TRUE,
                    control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

rur_nb <- glmmTMB(PF1216 ~ 
                    ID_auto + ID_mm3 +
                    FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                    EmpOff_SQ + EmpRet + EmpInd_SQ + EmpSer_SQ + EmpEnt + ActEnt +
                    HabArea +
                    HH_INC + Prop_Black + Prop_Hisp + Prop_Other +
                    All_18 + All_18_24 + All_25_34 + All_35_44 + All_55_64 + M + (1|CSA),
                  data=Data_rur, family=nbinom2, offset = (AvePop/25000), verbose = TRUE,
                  control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

rur_zinb <- glmmTMB(PF1216 ~ 
                      ID_auto + ID_mm3 +
                      FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                      EmpOff_SQ + EmpRet + EmpInd_SQ + EmpSer_SQ + EmpEnt + ActEnt +
                      HabArea +
                      HH_INC + Prop_Black + Prop_Hisp + Prop_Other +
                      All_18 + All_18_24 + All_25_34 + All_35_44 + All_55_64 + M + (1|CSA),
                    zi= ~
                      Prop_WalkToWork +
                      FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT +
                      EmpRet + EmpSer_SQ + EmpEnt + 
                      Tribal33 + HabArea + 
                      State,
                    data=Data_rur, family=nbinom2, offset = (AvePop/25000), verbose = TRUE,
                    control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

rur_zinb_RP <- glmmTMB(PF1216 ~ 
                         ID_auto + ID_mm3 +
                         FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT + 
                         EmpOff_SQ + EmpRet + EmpInd_SQ + EmpSer_SQ + EmpEnt + ActEnt +
                         HabArea +
                         HH_INC + Prop_Black + Prop_Hisp + Prop_Other +
                         All_18 + All_18_24 + All_25_34 + All_35_44 + All_55_64 + M +
                         (0+FC12_VMT|CSA) + (0+FC3_VMT|CSA) + (0+FC4_VMT|CSA) + 
                         (0+EmpOff_SQ|CSA) + (0+EmpInd_SQ|CSA) + (0+EmpSer_SQ|CSA) + (0+ActEnt|CSA) + (1|CSA),
                       zi= ~
                         Prop_WalkToWork +
                         FC12_VMT + FC3_VMT + FC4_VMT + FC56_VMT +
                         EmpRet + EmpSer_SQ + EmpEnt + 
                         Tribal33 + HabArea + 
                         State,
                       data=Data_rur, family=nbinom2, offset = (AvePop/25000), verbose = TRUE,
                       control = glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3)))

vif(rur)

rur_ranef <- ranef(rur_zinb_RP)
write.csv(rur_ranef$cond, 'rur_ranef_cond.csv')

rur_zinbRP_obs_pred <- as.data.frame(cbind(Data_rur$PF1216, fitted(rur_zinb_RP)))
plot(rur_zinbRP_obs_pred$V1, rur_zinbRP_obs_pred$V2)
mean(rur_zinbRP_obs_pred$V2[rur_zinbRP_obs_pred$V1==0])
mean(rur_zinbRP_obs_pred$V2[rur_zinbRP_obs_pred$V1>0])

# LR ChiSquare
lr_rur_nb <- lrtest(rur_null,rur_nb)
lr_rur_zinb <- lrtest(rur_null,rur_zinb)
lr_rur_zinb_RP <- lrtest(rur_null,rur_zinb_RP)
lr_rur_zinb_vs_nb <- lrtest(rur_nb,rur_zinb_RP)
lr_rur_zinb_RP_vs_nb <- lrtest(rur_nb,rur_zinb_RP)
lr_rur_zinb_RP_vs_zinb <- lrtest(rur_zinb,rur_zinb_RP)

# Nagelkerke R2
rur_R2_par <- (1-exp(-as.numeric(lr_rur_par$Chisq[2])/nrow(Data_rur)))/(1-exp(2*as.numeric(logLik(rur_null)/nrow(Data_rur))))
rur_R2_full <- (1-exp(-as.numeric(lr_rur_full$Chisq[2])/nrow(Data_rur)))/(1-exp(2*as.numeric(logLik(rur_null)/nrow(Data_rur))))

mean(fitted(rur)[Data_rur$PF1216==0])
mean(fitted(rur)[Data_rur$PF1216>0])
plot((Data_rur$PF1216), fitted(rur))

#Descripitve Stats
mean(Data_urb$PF1216)
median(Data_urb$PF1216)
sd(Data_urb$PF1216)
mean(Data_rur$PF1216)
median(Data_rur$PF1216)
sd(Data_rur$PF1216)

mean(Data_urb$ResPopDen)
median(Data_urb$ResPopDen)
sd(Data_urb$ResPopDen)
mean(Data_rur$ResPopDen)
median(Data_rur$ResPopDen)
sd(Data_rur$ResPopDen)

mean(Data_urb$AvePopDen)
median(Data_urb$AvePopDen)
sd(Data_urb$AvePopDen)
mean(Data_rur$AvePopDen)
median(Data_rur$AvePopDen)
sd(Data_rur$AvePopDen)

mean(Data_urb$Prop_WalkToWork)
median(Data_urb$Prop_WalkToWork)
sd(Data_urb$Prop_WalkToWork)
mean(Data_rur$Prop_WalkToWork)
median(Data_rur$Prop_WalkToWork)
sd(Data_rur$Prop_WalkToWork)

mean(Data_urb$Prop_TransitToWork)
median(Data_urb$Prop_TransitToWork)
sd(Data_urb$Prop_TransitToWork)
mean(Data_rur$Prop_TransitToWork)
median(Data_rur$Prop_TransitToWork)
sd(Data_rur$Prop_TransitToWork)

mean(Data_urb$ID_auto)
median(Data_urb$ID_auto)
sd(Data_urb$ID_auto)
mean(Data_rur$ID_auto)
median(Data_rur$ID_auto)
sd(Data_rur$ID_auto)

mean(Data_urb$ID_mm3)
median(Data_urb$ID_mm3)
sd(Data_urb$ID_mm3)
mean(Data_rur$ID_mm3)
median(Data_rur$ID_mm3)
sd(Data_rur$ID_mm3)

mean(Data_urb$FC12_VMT)
median(Data_urb$FC12_VMT)
sd(Data_urb$FC12_VMT)
mean(Data_rur$FC12_VMT)
median(Data_rur$FC12_VMT)
sd(Data_rur$FC12_VMT)

mean(Data_urb$FC3_VMT)
median(Data_urb$FC3_VMT)
sd(Data_urb$FC3_VMT)
mean(Data_rur$FC3_VMT)
median(Data_rur$FC3_VMT)
sd(Data_rur$FC3_VMT)

mean(Data_urb$FC4_VMT)
median(Data_urb$FC4_VMT)
sd(Data_urb$FC4_VMT)
mean(Data_rur$FC4_VMT)
median(Data_rur$FC4_VMT)
sd(Data_rur$FC4_VMT)

mean(Data_urb$FC5_VMT)
median(Data_urb$FC5_VMT)
sd(Data_urb$FC5_VMT)
mean(Data_rur$FC5_VMT)
median(Data_rur$FC5_VMT)
sd(Data_rur$FC5_VMT)

mean(Data_urb$PerWithinQuarterMI_LocalPark)
median(Data_urb$PerWithinQuarterMI_LocalPark)
sd(Data_urb$PerWithinQuarterMI_LocalPark)
mean(Data_rur$PerWithinQuarterMI_LocalPark)
median(Data_rur$PerWithinQuarterMI_LocalPark)
sd(Data_rur$PerWithinQuarterMI_LocalPark)

mean(Data_urb$ActEnt)
median(Data_urb$ActEnt)
sd(Data_urb$ActEnt)
mean(Data_rur$ActEnt)
median(Data_rur$ActEnt)
sd(Data_rur$ActEnt)

mean(Data_urb$EmpOff)
median(Data_urb$EmpOff)
sd(Data_urb$EmpOff)
mean(Data_rur$EmpOff)
median(Data_rur$EmpOff)
sd(Data_rur$EmpOff)

mean(Data_urb$EmpRet)
median(Data_urb$EmpRet)
sd(Data_urb$EmpRet)
mean(Data_rur$EmpRet)
median(Data_rur$EmpRet)
sd(Data_rur$EmpRet)

mean(Data_urb$EmpInd)
median(Data_urb$EmpInd)
sd(Data_urb$EmpInd)
mean(Data_rur$EmpInd)
median(Data_rur$EmpInd)
sd(Data_rur$EmpInd)

mean(Data_urb$EmpSer)
median(Data_urb$EmpSer)
sd(Data_urb$EmpSer)
mean(Data_rur$EmpSer)
median(Data_rur$EmpSer)
sd(Data_rur$EmpSer)

mean(Data_urb$EmpEnt)
median(Data_urb$EmpEnt)
sd(Data_urb$EmpEnt)
mean(Data_rur$EmpEnt)
median(Data_rur$EmpEnt)
sd(Data_rur$EmpEnt)

mean(Data_urb$EmpTrWa)
median(Data_urb$EmpTrWa)
sd(Data_urb$EmpTrWa)
mean(Data_rur$EmpTrWa)
median(Data_rur$EmpTrWa)
sd(Data_rur$EmpTrWa)

mean(Data_urb$HH_INC)
median(Data_urb$HH_INC)
sd(Data_urb$HH_INC)
mean(Data_rur$HH_INC)
median(Data_rur$HH_INC)
sd(Data_rur$HH_INC)

mean(Data_urb$Prop_White)
median(Data_urb$Prop_White)
sd(Data_urb$Prop_White)
mean(Data_rur$Prop_White)
median(Data_rur$Prop_White)
sd(Data_rur$Prop_White)

mean(Data_urb$Prop_Black)
median(Data_urb$Prop_Black)
sd(Data_urb$Prop_Black)
mean(Data_rur$Prop_Black)
median(Data_rur$Prop_Black)
sd(Data_rur$Prop_Black)

mean(Data_urb$Prop_Hisp)
median(Data_urb$Prop_Hisp)
sd(Data_urb$Prop_Hisp)
mean(Data_rur$Prop_Hisp)
median(Data_rur$Prop_Hisp)
sd(Data_rur$Prop_Hisp)

mean(Data_urb$Prop_Asian)
median(Data_urb$Prop_Asian)
sd(Data_urb$Prop_Asian)
mean(Data_rur$Prop_Asian)
median(Data_rur$Prop_Asian)
sd(Data_rur$Prop_Asian)

mean(Data_urb$Prop_Other)
median(Data_urb$Prop_Other)
sd(Data_urb$Prop_Other)
mean(Data_rur$Prop_Other)
median(Data_rur$Prop_Other)
sd(Data_rur$Prop_Other)

mean(Data_urb$Prop_Old)
median(Data_urb$Prop_Old)
sd(Data_urb$Prop_Old)
mean(Data_rur$Prop_Old)
median(Data_rur$Prop_Old)
sd(Data_rur$Prop_Old)

mean(Data_urb$Rent)
median(Data_urb$Rent)
sd(Data_urb$Rent)
mean(Data_rur$Rent)
median(Data_rur$Rent)
sd(Data_rur$Rent)

mean(Data_urb$Zero_Veh)
median(Data_urb$Zero_Veh)
sd(Data_urb$Zero_Veh)
mean(Data_rur$Zero_Veh)
median(Data_rur$Zero_Veh)
sd(Data_rur$Zero_Veh)

mean(Data_urb$LandArea)
median(Data_urb$LandArea)
sd(Data_urb$LandArea)
mean(Data_rur$LandArea)
median(Data_rur$LandArea)
sd(Data_rur$LandArea)

mean(Data_urb$HabArea)
median(Data_urb$HabArea)
sd(Data_urb$HabArea)
mean(Data_rur$HabArea)
median(Data_rur$HabArea)
sd(Data_rur$HabArea)

mean(Data_urb$F_18)
median(Data_urb$F_18)
sd(Data_urb$F_18)
mean(Data_rur$F_18)
median(Data_rur$F_18)
sd(Data_rur$F_18)

mean(Data_urb$F_18_24)
median(Data_urb$F_18_24)
sd(Data_urb$F_18_24)
mean(Data_rur$F_18_24)
median(Data_rur$F_18_24)
sd(Data_rur$F_18_24)

mean(Data_urb$F_25_34)
median(Data_urb$F_25_34)
sd(Data_urb$F_25_34)
mean(Data_rur$F_25_34)
median(Data_rur$F_25_34)
sd(Data_rur$F_25_34)

mean(Data_urb$F_35_44)
median(Data_urb$F_35_44)
sd(Data_urb$F_35_44)
mean(Data_rur$F_35_44)
median(Data_rur$F_35_44)
sd(Data_rur$F_35_44)

mean(Data_urb$F_45_54)
median(Data_urb$F_45_54)
sd(Data_urb$F_45_54)
mean(Data_rur$F_45_54)
median(Data_rur$F_45_54)
sd(Data_rur$F_45_54)

mean(Data_urb$F_55_64)
median(Data_urb$F_55_64)
sd(Data_urb$F_55_64)
mean(Data_rur$F_55_64)
median(Data_rur$F_55_64)
sd(Data_rur$F_55_64)

mean(Data_urb$F_65)
median(Data_urb$F_65)
sd(Data_urb$F_65)
mean(Data_rur$F_65)
median(Data_rur$F_65)
sd(Data_rur$F_65)

mean(Data_urb$M_18)
median(Data_urb$M_18)
sd(Data_urb$M_18)
mean(Data_rur$M_18)
median(Data_rur$M_18)
sd(Data_rur$M_18)

mean(Data_urb$M_18_24)
median(Data_urb$M_18_24)
sd(Data_urb$M_18_24)
mean(Data_rur$M_18_24)
median(Data_rur$M_18_24)
sd(Data_rur$M_18_24)

mean(Data_urb$M_25_34)
median(Data_urb$M_25_34)
sd(Data_urb$M_25_34)
mean(Data_rur$M_25_34)
median(Data_rur$M_25_34)
sd(Data_rur$M_25_34)

mean(Data_urb$M_35_44)
median(Data_urb$M_35_44)
sd(Data_urb$M_35_44)
mean(Data_rur$M_35_44)
median(Data_rur$M_35_44)
sd(Data_rur$M_35_44)

mean(Data_urb$M_45_54)
median(Data_urb$M_45_54)
sd(Data_urb$M_45_54)
mean(Data_rur$M_45_54)
median(Data_rur$M_45_54)
sd(Data_rur$M_45_54)

mean(Data_urb$M_55_64)
median(Data_urb$M_55_64)
sd(Data_urb$M_55_64)
mean(Data_rur$M_55_64)
median(Data_rur$M_55_64)
sd(Data_rur$M_55_64)

mean(Data_urb$M_65)
median(Data_urb$M_65)
sd(Data_urb$M_65)
mean(Data_rur$M_65)
median(Data_rur$M_65)
sd(Data_rur$M_65)

#Export data and fitted values
write.csv(fitted(urb_zinb_RP),"urb_fitted_v3.csv")
write.csv(Data_urb,"urb_data.csv")
write.csv(fitted(rur_zinb_RP),"rur_fitted_v3.csv")
write.csv(Data_rur,"rur_data.csv")

#Plot MEs
##NOTE: MEs were estimated using Monte Carlo simulation usign Analytica software
MEs_FC <- as.data.frame(read.csv("MEs_FC.csv"))
MEs_BE <- as.data.frame(read.csv("MEs_BE.csv"))

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

dodge <- position_dodge(width = 0.9)

MEs_urban_FC <- subset(MEs_FC, MEs_FC$Group=="Urban")
MEs_rural_FC <- subset(MEs_FC, MEs_FC$Group=="Rural")
MEs_urban_BE <- subset(MEs_BE[3:9,])
MEs_rural_BE <- subset(MEs_BE[12:17,])
MEs_urban_BE$Variable <- factor(MEs_urban_BE$Variable, levels = MEs_urban_BE$Variable[order(row.names(MEs_urban_BE))])
MEs_rural_BE$Variable <- factor(MEs_rural_BE$Variable, levels = MEs_rural_BE$Variable[order(row.names(MEs_rural_BE))])
limits_urban_FC <- aes(ymax = MEs_urban_FC$Max, ymin = MEs_urban_FC$Min)
limits_rural_FC <- aes(ymax = MEs_rural_FC$Max, ymin = MEs_rural_FC$Min)
limits_urban_BE <- aes(ymax = MEs_urban_BE$Max, ymin = MEs_urban_BE$Min)
limits_rural_BE <- aes(ymax = MEs_rural_BE$Max, ymin = MEs_rural_BE$Min)

urban_FC <- ggplot(data = MEs_urban_FC, aes(x = Functional.Classification, y = Mean, fill=factor(Functional.Classification))) +
  geom_bar(stat = "identity", position = dodge, show.legend=F) +
  geom_errorbar(limits_urban_FC, position = dodge, width = 0.25, size=0.1) +
  labs(title = "Urban Tracts, Traffic Density Variables", x = "Roadway Functional Classification", y = expression("    Change in ped. fatality rate per\n10,000 unit change in VMT density")) +
  scale_fill_discrete(name = "Roadway Functional Classification") + scale_fill_grey(start = 0.35, end = 0.9) +
  theme(panel.background = element_rect(fill= "white"), plot.background = element_rect(fill= "white"), plot.title = element_text(size = rel(0.75), hjust = 0.5), axis.title.x = element_text(size = rel(0.6)), axis.title.y = element_text(size = rel(0.6)), axis.text = element_text(size = rel(0.5))) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.line = element_line(size = 0.1, colour = "black")) +
  theme(axis.ticks = element_line(colour = "black", size=0.1)) +
  theme(axis.title.y = element_text(margin = margin(t = 2, r = 2, b = 2, l = 0))) +
  scale_y_continuous(limits = c(0,1.5), breaks=c(0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5), expand = c(0,0)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.3), "cm"))

rural_FC <- ggplot(data = MEs_rural_FC, aes(x = Functional.Classification, y = Mean, fill=factor(Functional.Classification))) +
  geom_bar(stat = "identity", position = dodge, show.legend=F) +
  geom_errorbar(limits_rural_FC, position = dodge, width = 0.25, size=0.1) +
  labs(title = "Rural Tracts, Traffic Density Variables", x = "Roadway Functional Classification", y = expression("  Change in ped. fatality rate per\n1,000 unit change in VMT density")) +
  scale_fill_discrete(name = "Roadway Functional Classification") + scale_fill_grey(start = 0.35, end = 0.9) +
  theme(panel.background = element_rect(fill= "white"), plot.background = element_rect(fill= "white"), plot.title = element_text(size = rel(0.75), hjust = 0.5), axis.title.x = element_text(size = rel(0.6)), axis.title.y = element_text(size = rel(0.6)), axis.text = element_text(size = rel(0.5))) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.line = element_line(size = 0.1, colour = "black")) +
  theme(axis.ticks = element_line(colour = "black", size=0.1)) +
  theme(axis.title.y = element_text(margin = margin(t = 2, r = 2, b = 2, l = 0))) +
  scale_y_continuous(limits = c(0,1.25), breaks=c(0, 0.25, 0.5, 0.75, 1.0, 1.25), expand = c(0,0)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.3), "cm"))

urban_BE <- ggplot(data = MEs_urban_BE, aes(x = Variable, y = Mean, fill=factor(Variable))) +
  geom_bar(stat = "identity", position = dodge, show.legend=F) +
  geom_errorbar(limits_urban_BE, position = dodge, width = 0.25, size=0.1) +
  labs(title = "Urban Tracts, Population and Employment Density Variables", x = "Population/Employment Category", y = expression("Change in ped. fatality rate\n per unit change in density")) +
  scale_x_discrete(labels = c("Residential","Office","Retail","Industrial","Service","Entertainment","Activity\nmix index")) +
  scale_fill_discrete(name = "Population/Employment Category") + scale_fill_grey(start = 0.35, end = 0.9) +
  theme(panel.background = element_rect(fill= "white"), plot.background = element_rect(fill= "white"), plot.title = element_text(size = rel(0.75), hjust = 0.5), axis.title.x = element_text(size = rel(0.6)), axis.title.y = element_text(size = rel(0.6)), axis.text = element_text(size = rel(0.5))) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.line = element_line(size = 0.1, colour = "black")) +
  theme(axis.ticks = element_line(colour = "black", size=0.1)) +
  theme(axis.title.y = element_text(margin = margin(t = 2, r = 2, b = 2, l = 0))) +
  scale_y_continuous(limits = c(-1,3), breaks=c(-1, -0.5, 0, 0.5, 1.0, 1.5, 2.0, 2.5, 3.0), expand = c(0,0)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.3), "cm"))

rural_BE <- ggplot(data = MEs_rural_BE, aes(x = Variable, y = Mean, fill=factor(Variable))) +
  geom_bar(stat = "identity", position = dodge, show.legend=F) +
  geom_errorbar(limits_rural_BE, position = dodge, width = 0.25, size=0.1) +
  labs(title = "Rural Tracts, Population and Employment Density Variables", x = "Population/Employment Category", y = expression("Change in ped. fatality rate\n per unit change in density")) +
  scale_x_discrete(labels = c("Office","Retail","Industrial","Service","Entertainment","Activity\nmix index")) +
  scale_fill_discrete(name = "Population/Employment Category") + scale_fill_grey(start = 0.35, end = 0.9) +
  theme(panel.background = element_rect(fill= "white"), plot.background = element_rect(fill= "white"), plot.title = element_text(size = rel(0.75), hjust = 0.5), axis.title.x = element_text(size = rel(0.6)), axis.title.y = element_text(size = rel(0.6)), axis.text = element_text(size = rel(0.5))) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.line = element_line(size = 0.1, colour = "black")) +
  theme(axis.ticks = element_line(colour = "black", size=0.1)) +
  theme(axis.title.y = element_text(margin = margin(t = 2, r = 2, b = 2, l = 0))) +
  scale_y_continuous(limits = c(-.25,.2), breaks=c(-0.25, -0.2, -0.15, -0.10, -0.05, 0, 0.05, 0.10, 0.15, 0.20), expand = c(0,0)) +
  theme(plot.margin = unit(c(0.1,0.1,0.1,0.3), "cm"))

urban_FC
ggsave("test_FC.png", width=2.7, height=2.25, dpi=1600)
rural_FC
ggsave("test_FC2.png", width=2.7, height=2.25, dpi=1600)
urban_BE
ggsave("test_BE.png", width=3.8, height=2.25, dpi=1600)
rural_BE
ggsave("test_BE2.png", width=4.0, height=2.25, dpi=1600)

#Histogram
Data_hist <- Data[c("Urb50", "PF1216", "PF12", "PF13", "PF14", "PF15", "PF16")]
levels(Data_hist$Urb50)[levels(Data_hist$Urb50)=='0'] <- 'Rural'
levels(Data_hist$Urb50)[levels(Data_hist$Urb50)=='1'] <- 'Urban'
Data_hist$Urb50 <- ordered(Data_hist$Urb50)
Data_hist$Urb50 <- factor(Data_hist$Urb50, levels= c("Urban","Rural"))
Data_hist$PF1216[Data_hist$PF1216>10] <- 10

hist_1216 <- ggplot(data = Data_hist, aes(x=Data_hist$PF1216, fill=Data_hist$Urb50)) + 
  geom_histogram(binwidth = .5, position='dodge') +
  labs(x="Pedestrian Fatality Count, 2012 to 2016", y="Frequency", fill="Tract classification") + 
  scale_x_continuous(limits = c(-.5,10), expand = c(0, 0), breaks = seq(0, 10, by = 1),
                     labels = c("0","1","2","3","4","5","6","7","8","9","10+")) +
  scale_y_continuous(limits = c(0, 37500), expand = c(0, 0), breaks = seq(0, 37500, by = 2500), label = comma) +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill= "white"), plot.background = element_rect(fill= "white"),
        axis.title.x = element_text(size = rel(0.65)),
        axis.title.y = element_text(size = rel(0.65)),
        axis.text = element_text(size = rel(0.65))) +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank()) +
  theme(axis.line = element_line(size = 0.1, colour = "black")) +
  theme(axis.ticks = element_line(colour = "black", size=0.1)) +
  theme(legend.position = c(0.8, 0.8)) +
  theme(legend.title = element_text(size= rel(0.65))) + 
  theme(legend.text = element_text(size= rel(0.65))) + scale_fill_grey() +
  theme(legend.key.size = unit(1,"line")) + 
  theme(plot.margin = unit(c(0.1,0.3,0.1,0.1), "cm"))

hist_1216
ggsave("hist.png", width=6, height=3, dpi=1600)

#For Vuong
sd(log(predict(urb_zinb))-log(predict(urb_nb)))


