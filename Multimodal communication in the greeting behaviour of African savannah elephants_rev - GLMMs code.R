#Multimodal communication in the greeting behaviour of African savannah elephants - GLMMs code

#Required packages
library(lme4)
library(Matrix)
library(car)
library(MuMIn)

#GLMM1: Does the use of body acts when the recipient is attending or not vary between modalities?----
##Sort data
setwd("~/Desktop/PhD/Analysis/Datasets/Descriptives") 
xdata=read.table("Multi-signal_dataset_analyses_grt_clean.csv", header=T, sep=";", fill=T, dec=".", stringsAsFactors=T)
xdata_bodysignal=subset(xdata, Signal=="Gesture") #subset to only body signals
xdata_bodysignal_2=subset(xdata_bodysignal, ! Signaller %in% c("MS")) #Exclude MS (Masuwe) because she has no signals in tactile modality
xdata_bodysignal_3=subset(xdata_bodysignal_2, ! Rcp_Visual_att %in% c("Unclear", "Unk", "Out_of_sight")) #Exclude when recipient visual attention unclear
str(xdata_bodysignal_3) 
levels(xdata_bodysignal_3$Signal_modality)
levels(xdata_bodysignal_3$Rcp_Visual_att)
xdata_bodysignal_6=subset(xdata_bodysignal_3, ! Signal_modality %in% c("Unk")) #Exclude where modality of signal is unknown (tail signals and Reach_Touch)
str(xdata_bodysignal_6)

xdata_bodysignal_6=droplevels(xdata_bodysignal_6) #drop levels of categorical variables with no observations
levels(xdata_bodysignal_6$Signal_modality) 
levels(xdata_bodysignal_6$Rcp_Visual_att) 

xdata_bodysignal_6$Signal_modality <- relevel(xdata_bodysignal_6$Signal_modality, ref = "Tactile") #relevel Tactile to baseline
xdata_bodysignal_6$Rcp_Visual_att <- relevel(xdata_bodysignal_6$Rcp_Visual_att, ref = "No_VA") #relevel No_VA to baseline

xdata=xdata_bodysignal_6 

##Check if data balanced within random effects
table(xdata$Signaller) 
table(xdata$Com_number) 

##Check frequencies of response within possible random effects
table(xdata$Rcp_Visual_att, xdata$Signaller) 
table(xdata$Rcp_Visual_att, xdata$Com_number) 

##Check frequencies of response variable
table(xdata$Rcp_Visual_att) 
table(xdata$Rcp_Visual_att, xdata$Signal_modality) 

##Dummy code response
xdata$Rcp_Visual_att=as.numeric(xdata$Rcp_Visual_att=="VA") 

##Relevel Signal modality 
xdata$Signal_modality=relevel(xdata$Signal_modality, ref="Tactile") 
str(xdata)

##Choose random slopes
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/diagnostic_fcns.r") 
xx.fe.re=fe.re.tab(fe.model="Rcp_Visual_att ~ Signal_modality",
  re="(1|Signaller)", data=xdata) #this function 
#will remove NAs and dummy code variables and return a list with 2 components: summary and data;
#summary was used to choose random slopes as indicated by Roger Mundry (2021)

xx.fe.re$summary #include random slope of Signal modality

##Center factors for random slope
t.data=xx.fe.re$data
str(t.data)
t.data$Signal_modality.Audible=t.data$Signal_modality.Audible-mean(t.data$Signal_modality.Audible) 
t.data$Signal_modality.Visual=t.data$Signal_modality.Visual-mean(t.data$Signal_modality.Visual) 

##Fit model
full=glmer(Rcp_Visual_att ~ Signal_modality +
  (1+Signal_modality.Audible+Signal_modality.Visual|Signaller),
  data=t.data, family=binomial) #model converged but Singularity message: 
#may indicate that random intercepts and slopes may be correlated

summary(full)$varcor #check correlations 

ll.old=logLik(full) #stored ll.old for comparison below with ll of full model without correlations

full.wc=glmer(Rcp_Visual_att ~ Signal_modality +
  (1+Signal_modality.Audible+Signal_modality.Visual||Signaller),
  data=t.data, family=binomial) #isSingular message

round(ll.old, 3); logLik(full.wc) #check if ll differ

summary(full.wc)$varcor #check if SD of random slopes close to 0

full.wrs=glmer(Rcp_Visual_att ~ Signal_modality +
  (1|Signaller),
  data=t.data, family=binomial) 

logLik(full)
logLik(full.wc)
logLik(full.wrs) #ll don't vary much without correlations, so we can exclude the 
                  #correlations but keep random slopes to avoide type I error

full=full.wc 

##Assumptions
#BLUPs assumption 
ranef.diagn.plot(full) 

#Model stability 
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/glmm_stability.r") #function provided by Roger Mundry
mstab1=glmm.model.stab(model.res=full, contr=contr) 

table(mstab1$detailed$lme4.warnings) 
table(mstab1$detailed$opt.warnings)  

m.stab.plot(mstab$summary[, -1]) 
mstab1=round(mstab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

##Full null model comparison
null=glmer(Rcp_Visual_att ~
  (1+Signal_modality.Audible+Signal_modality.Visual||Signaller),
  data=t.data, family=binomial) 
Chisq_Model1=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_Model1=round(Chisq_Model1, 3)

##Results 
#Coeffs
CoeffsModel1=round(summary(full)$coefficients, 3) 

install.packages("multcomp")
library(multcomp) #for contexts p-values
tests1=cftest(full) 

#inverse logit transformation for probabilities
exp(fixef(full)["(Intercept)"])/
  (1+exp(fixef(full)["(Intercept)"])) 

exp(fixef(full)["Signal_modalityAudible"])/
  (1+exp(fixef(full)["Signal_modalityAudible"])) 

exp(fixef(full)["Signal_modalityVisual"])/
  (1+exp(fixef(full)["Signal_modalityVisual"])) 

#CIs 
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/boot_glmm.r") #function provided by Roger Mundry
boot.resD=boot.glmm.pred(model.res=full, excl.warnings=F,
                         nboots=1000, para=F)
cis1=round(boot.resD$ci.estimates, 3)

#Effect sizes 
library(MuMIn)
r.sq1=r.squaredGLMM(object=full) 
r.sq1_res=round(r.sq1, 3)


#GLMM2: Do individual and social factors affect the order of vocalisations and gestures in multicomponent combinations by elephants during greeting?----
xdata=read.table("Multi-signal combinations GLMMs_data.csv", header=T, sep=";", fill=T, dec=",", stringsAsFactors=T)
str(xdata)

##Check Signaller and Com_number 
table(xdata$Signaller)
table(xdata$Com_number) 

##Check if random effects needed
xx=aggregate(x=1:nrow(xdata), by=xdata[, c("Signaller", "Com_number")],
  FUN=length) 
head(xx)

##Check observations x Com_number 
table(xdata$NN_Index, xdata$Com_number)
table(xdata$Signaller_sex, xdata$Com_number)

##Check frequencies of response variable
table(xdata$Signal_1) 
table(xdata$Signal_1, xdata$Signaller) 
table(xdata$Signal_1, xdata$Com_number) 
table(xdata$Signal_1, xdata$Signaller_sex) 
table(xdata$Signal_1, xdata$Sex_Dyad) 
table(xdata$Signal_1, xdata$NN_Index) 

##Check if one sex x indvidual
xx=table(xdata$Signaller, xdata$Signaller_sex)
range(apply(X=xx>0, MARGIN=1, FUN=sum)) 

##Check Nearest-neighbour index distribution 
xdata$NN_Index=as.numeric(xdata$NN_Index) #convert to numeric
hist(xdata$NN_Index) 
str(xdata)

##Dummy code response
xdata$Signal_1=as.numeric(xdata$Signal_1=="Vocalisation") 
str(xdata)

##Sort categorical data for random slopes
xdata$Signaller_sex=relevel(xdata$Signaller_sex, ref="Female") #female as baseline
xdata$Sex_Dyad=relevel(xdata$Sex_Dyad, ref="Same") #same as baseline
str(xdata)

##Choose random slopes
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #function provided by Roger Mundry

xx.fe.re=fe.re.tab(fe.model="Signal_1 ~ Signaller_sex*Sex_Dyad + NN_Index",
  re="(1|Signaller)+(1|Com_number)", data=xdata) 

xx.fe.re$summary #include random slopes for NN, Sex dyad within Signaller; 
                 #include no random slopes within Com_number.

##Center factors for random slopes
t.data=xx.fe.re$data 
str(t.data)
t.data$Signaller_sex.Male=t.data$Signaller_sex.Male-mean(t.data$Signaller_sex.Male) 
t.data$Sex_Dyad.Different=t.data$Sex_Dyad.Different-mean(t.data$Sex_Dyad.Different)

##Z-transform covariates to include in model
t.data$z.NN_Index=as.vector(scale(t.data$NN_Index))
str(t.data) 

##Fit model
full=glmer(Signal_1 ~ Signaller_sex*Sex_Dyad + z.NN_Index +
  (1+z.NN_Index+Sex_Dyad.Different|Signaller) + (1|Com_number),
  data=t.data, family=binomial)  #model converged but Singularity message: 
  #may indicate that random intercepts and slopes may be correlated  

summary(full)$varcor #check correlations

ll.old=logLik(full) #stored ll.old for comparison below with ll of full model without correlations

full.wc=glmer(Signal_1 ~ Signaller_sex*Sex_Dyad + z.NN_Index +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial) #fit model without correlations: 
                                            #converged but still isSingular message

round(ll.old, 3); logLik(full.wc) #ll don't vary much without correlations, so we can exclude the correlations 

full=full.wc 

##Assumptions
###BLUPs assumption 
ranef.diagn.plot(full) 

###Model stability
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/glmm_stability.r") #function by Roger Mundry (2021)
m.stab=glmm.model.stab(model.res=full) 
mstab2=round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
xx=lm(Signal_1 ~ Signaller_sex + Sex_Dyad + z.NN_Index,
  data=t.data)
vif1=vif(xx) 
vif1=round(vif1, 3)  

##Full null model comparison 
null=glmer(Signal_1 ~ Signaller_sex + Sex_Dyad +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial)
Chisq_Model2=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_Model2=round(Chisq_Model2, 3)


#Model 3: Do individual and social factors affect the use of combinations of Rumble and Ear-Flapping by elephants during greeting?----
xdata=read.table("Multi-signal combinations GLMMs_data.csv", header=T, sep=";", 
  fill=T, dec=",", stringsAsFactors=T)
str(xdata)

##Add a column for Combination variable based on values in Signal_record_1 and Signal_record_2
##based on Signal_record_1 and Signal_record_2
xdata$Combination <- ifelse(xdata$Signal_record_1=="Rumble" & xdata$Signal_record_2 == "Ear-Flapping", 
 "Rumble_Ear-Flapping", ifelse(xdata$Signal_record_1=="Ear-Flapping" & xdata$Signal_record_2 == "Rumble", 
 "Rumble_Ear-Flapping", "Other"))
xdata <- xdata[, c("Combination", names(xdata)[-55])]

##Check Signaller and Com_number 
table(xdata$Signaller) 
table(xdata$Com_number) 

##Check if random effects needed
xx=aggregate(x=1:nrow(xdata), by=xdata[, c("Signaller", "Com_number")],
  FUN=length) 
head(xx) 

##Check observations x Com_number 
table(xdata$NN_Index, xdata$Com_number)
table(xdata$Signaller_sex, xdata$Com_number)

##Dummy code response
xdata$Combination=as.numeric(xdata$Combination=="Rumble_Ear-Flapping") #Rumble_Ear-Flapping=1

##Check frequencies of response variable
table(xdata$Combination) 
table(xdata$Combination, xdata$Signaller) 
table(xdata$Combination, xdata$Com_number) 
table(xdata$Combination, xdata$Signaller_age) 
table(xdata$Combination, xdata$Signaller_sex) 
table(xdata$Combination, xdata$Sex_Dyad) 
table(xdata$Combination, xdata$NN_Index) 

##Check if one sex x indvidual
xx=table(xdata$Signaller, xdata$Signaller_sex)
range(apply(X=xx>0, MARGIN=1, FUN=sum)) 

##Check Nearest-neighbour index distribution 
xdata$NN_Index=as.numeric(xdata$NN_Index) #convert to numeric
hist(xdata$NN_Index) 
str(xdata)

##Sort categorical data for random slopes
xdata$Signaller_sex=relevel(xdata$Signaller_sex, ref="Female") #female as baseline
xdata$Sex_Dyad=relevel(xdata$Sex_Dyad, ref="Same") #same as baseline
str(xdata)

##Choose random slopes
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #Function provided by Roger Mundry (2021)

xx.fe.re=fe.re.tab(fe.model="Combination ~ Signaller_sex*Sex_Dyad + NN_Index",
  re="(1|Signaller)+(1|Com_number)", data=xdata) #this function 
  #will remove NAs and dummy code variables and return a list with 2 components: summary and data;
  #summary was used to choose random slopes as indicated by Roger Mundry 

xx.fe.re$summary #include: NN, Sex dyad within Signaller; No random slopes within Com_number.

##Center factors for random slopes
t.data=xx.fe.re$data 
str(t.data)
t.data$Signaller_sex.Male=t.data$Signaller_sex.Male-mean(t.data$Signaller_sex.Male) 
t.data$Sex_Dyad.Different=t.data$Sex_Dyad.Different-mean(t.data$Sex_Dyad.Different)

##Z-transform covariates to include in model
t.data$z.NN_Index=as.vector(scale(t.data$NN_Index))
str(t.data) 

##Fit model
full=glmer(Combination ~ Signaller_sex*Sex_Dyad + z.NN_Index +
  (1+z.NN_Index+Sex_Dyad.Different|Signaller) + (1|Com_number),
  data=t.data, family=binomial) #model converged but Singularity message: 
  #may indicate that random intercepts and slopes may be correlated  

summary(full)$varcor #check correlations

ll.old=logLik(full) #stored ll.old for comparison below with ll of full model without correlations

full.wc=glmer(Combination ~ Signaller_sex*Sex_Dyad + z.NN_Index +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial) #fit model without correlations: converged but 
                                            #still isSingular message

round(ll.old, 3); logLik(full.wc) #ll similar so can exclude the correlations

full=full.wc 

##Assumptions
###BLUPs assumption 
ranef.diagn.plot(full) 

###Model stability
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/glmm_stability.r") #function by Roger Mundry 
m.stab=glmm.model.stab(model.res=full) 

mstab3=round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
xx=lm(Combination ~ Signaller_sex + Sex_Dyad + z.NN_Index,
  data=t.data)
vif3=vif(xx) 
vif3=round(vif2, 3) 

##Full null model comp
null=glmer(Combination ~ Signaller_sex + Sex_Dyad +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial) 
Chisq_Model3=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_Model3_res=round(Chisq_Model3, 3)

##Results
###Coefficients
round(summary(full)$coefficients, 3) 
tests3=as.data.frame(drop1(full, test="Chisq")) 
tests3=round(tests3, 3)

##Confidence Intervals
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/boot_glmm.r") #Function provided by Roger Mundry (2021)
boot.res3=boot.glmm.pred(model.res=full, excl.warnings=F,
  nboots=1000, para=F)
cis3=round(boot.res3$ci.estimates, 3)

##Effect sizes 
r.sq3=r.squaredGLMM(object=full) 
r.sq3=round(r.sq3, 3)

##Plot effect of interaction between signaller and sex dyad on 
#the probability of use of Rumble and Ear-flapping in combination

###Creat plot baseline
to.plot=aggregate(x=xdata$Combination,
  by=xdata[, c("Signaller_sex", "Sex_Dyad")],
  FUN=mean)

barplot(to.plot$x) #plot barplot

par(mar=c(3, 3, 0.5, 0.2), mgp=c(1.7, 0.3, 0), tcl=-0.15,
  las=1, lwd=3) #set parameters
barplot(to.plot$x, col="white", ylim=c(0, 1), yaxs="i",
  ylab="Probability of Rumble + Ear-flapping")

x.at=barplot(to.plot$x, col="white", ylim=c(0, 1), yaxs="i",
  ylab="Probability of Rumble + Ear-flapping")
x.at #adding labels at the x-axis

mtext(text=to.plot$Signaller_sex, side=1, line=0.2, at=x.at)
mtext(text=c("Same Sex", "Different Sex"), side=1, line=1.4,
  at=c(mean(x.at[1:2]), mean(x.at[3:4]))) #add text positions

###Add model lines in plot
####Extract coefficients from model
plot.res.int=glmer(Combination ~ Signaller_sex*Sex_Dyad + z.NN_Index +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial)
coefs=fixef(plot.res.int) 

####Create probability values based on coefs
fv=rep(NA, times=4) #create vector with 4 entries for fixed values calculations for 4 terms
fv[1]=coefs["(Intercept)"]  
fv[2]=coefs["(Intercept)"]+coefs["Signaller_sexMale"]
fv[3]=coefs["(Intercept)"]+coefs["Sex_DyadDifferent"]
fv[4]=coefs["(Intercept)"]+coefs["Signaller_sexMale"]+coefs["Sex_DyadDifferent"] +
  coefs["Signaller_sexMale:Sex_DyadDifferent"]
fv 
fv=exp(fv)/(1+exp(fv)) #convert fixed values into probabilities
fv

hll=mean(diff(x.at[, 1]))/4 #create segment half length of width of bar
segments(x0=x.at-hll, x1=x.at+hll, y0=fv, y1=fv, lwd=3) #add segments depicting fixed values to bars

###Add Confidence intervals to plot
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/boot_glmm.r") #function provided by Roger Mundry
boot.plot.res.int=boot.glmm.pred(model.res=plot.res.int, 
  excl.warnings=F, nboots=1000, para=F, resol=4, level=0.95, 
  use="Signaller_sexMale:Sex_DyadDifferent") #get bootstrapped CIs for interaction
arrows(x0=x.at, x1=x.at, y0=boot.plot.res.int$ci.predicted$lower.cl, 
  y1=boot.plot.res.int$ci.predicted$upper.cl, code=3, len=0.1, angle=90) #add bootstrapped cis to plot

dev.copy2pdf(file="Plot_Rumble-Ear-flapping_Interaction_Sex-Sex_Dyad2.pdf", out.type = "pdf")


#Supplementary Information GLMM: Do tail body acts rely on the state of visual attention of the recipient?----
str(xdata_bodysignal_3) #data from above excluding data from Masuwe and where recipient visual attention unclear or unknown  
levels(xdata_bodysignal_3$Signal_modality)
levels(xdata_bodysignal_3$Rcp_Visual_att)
xdata_bodysignal_7=subset(xdata_bodysignal_3, ! Signal_modality %in% c("Audible", "Tactile")) #Keep signals with visual or unknown modality (i.e., tail body acts)
xdata_bodysignal_7=subset(xdata_bodysignal_7, ! Signal_record %in% c("Trunk-Reach_Touch_Unc")) #Exclude Trunk-Reach_Touch_Unc because modality unknown
xdata_bodysignal_7=droplevels(xdata_bodysignal_7) #drop levels of categorical variables with no observations

table(xdata_bodysignal_7$Signal_record, xdata_bodysignal_7$Signal_modality) #check modalities x signal record
xdata=xdata_bodysignal_7 
str(xdata)

xdata$Signal_record2 <- ifelse(xdata$Signal_record %in% c("Tail-on-Side","Tail-Raise", 
  "Tail-Stiff", "Tail-Waggling"), "Tail", "Non-Tail") #Add a new column with tail body acts as "Tail" and other visual gestures as "Non-tail"
table(xdata$Signal_record, xdata$Signal_record2) 
str(xdata)

##Check if data balanced within random effects
table(xdata$Signaller) #ok
table(xdata$Com_number) #ok

##Check frequencies of response within possible random effects
table(xdata$Signal_record2, xdata$Signaller) #ok 
table(xdata$Signal_record2, xdata$Com_number) #many No_VA=0 so don´t include as random effect

##Check frequencies of variables
table(xdata$Rcp_Visual_att) 
table(xdata$Rcp_Visual_att, xdata$Signal_record2) 

##Dummy code response
xdata$Signal_record2=as.numeric(xdata$Signal_record2=="Tail")

##Relevel Recipient visual attention
xdata$Rcp_Visual_att=relevel(xdata$Rcp_Visual_att, ref="No_VA") #No_VA as baseline
str(xdata)

##Choose random slopes
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/diagnostic_fcns.r") 
xx.fe.re=fe.re.tab(fe.model="Signal_record2 ~ Rcp_Visual_att",
  re="(1|Signaller)", data=xdata) 

xx.fe.re$summary #include random slope for Rcp_Visual_att within Signaller

##Center factors to include random slope
t.data=xx.fe.re$data
str(t.data)
t.data$Rcp_Visual_att.VA=t.data$Rcp_Visual_att.VA-mean(t.data$Rcp_Visual_att.VA) 

##Fit model
full=glmer(Signal_record2 ~ Rcp_Visual_att +
  (1+Rcp_Visual_att.VA|Signaller),
  data=t.data, family=binomial) #converged but isSingular message

summary(full)$varcor #check correlations

ll.old=logLik(full) #store ll.old for comparison below with ll of full model without correlations

full.wc=glmer(Signal_record2 ~ Rcp_Visual_att +
  (1+Rcp_Visual_att.VA||Signaller),
  data=t.data, family=binomial) ##converged but isSingular message again

round(ll.old, 3); logLik(full.wc) #ll similar 

summary(full.wc)$varcor #check if SD of random slopes close to 0

full=full.wc ##ll don't vary much without correlations, so we can exclude the 
#correlations but keep random slopes to avoide type I error

##Assumptions
#BLUPs assumption
ranef.diagn.plot(full) 

#Model stability 
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/glmm_stability.r") #function provided by Roger Mundry
mstabS=glmm.model.stab(model.res=full)

table(mstabS$detailed$lme4.warnings) 
table(mstabS$detailed$opt.warnings) 

m.stab.plot(mstabS$summary[, -1]) 
mstabE=round(mstabS$summary[, -1], 3) 

##Full null model comparison
null=glmer(Signal_record2 ~ 
  (1+Rcp_Visual_att.VA||Signaller),
  data=t.data, family=binomial) 
Chisq_ModelS=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_ModelS=round(Chisq_ModelE, 3)

##Results 
#Coeffs
CoeffsModelS=round(summary(full)$coefficients, 3) 
CoeffsModelS

testsS=as.data.frame(drop1(full, test="Chisq")) 
testsS_res=round(testsE, 3)

##Inverse logit transform for probabilities
exp(fixef(full)["(Intercept)"])/
  (1+exp(fixef(full)["(Intercept)"])) 

exp(fixef(full)["Rcp_Visual_attVA"])/
  (1+exp(fixef(full)["Rcp_Visual_attVA"])) 

#CIs 
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/boot_glmm.r") #function provided by Roger Mundry
boot.resE=boot.glmm.pred(model.res=full, excl.warnings=F,
  nboots=1000, para=F)
cisS=round(boot.resE$ci.estimates, 3)

#Effect sizes 
library(MuMIn)
full.rsq=glmer(Signal_record2 ~ Rcp_Visual_att +
  (1|Signaller)+(0+Rcp_Visual_att.VA||Signaller),
  data=t.data, family=binomial) 
r.sqS=r.squaredGLMM(object=full.rsq) 
r.sqS_res=round(r.sqS, 3)




