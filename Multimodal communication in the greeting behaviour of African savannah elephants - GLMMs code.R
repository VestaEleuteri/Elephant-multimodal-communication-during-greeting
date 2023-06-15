#Multimodal communication in the greeting behaviour of African savannah elephants - GLMMs code

#Required packages
library(lme4)
library(Matrix)
library(car)
library(MuMIn)


#Model 1: Do individual and social factors affect the order of multi-signal combinations by elephants during greeting?----
xdata=read.table("Multi-signal combinations GLMMs_data.csv", header=T, sep=";", 
  fill=T, dec=",", stringsAsFactors=T)
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
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/diagnostic_fcns.r") #function provided by Roger Mundry (2021)

xx.fe.re=fe.re.tab(fe.model="Signal_1 ~ Signaller_sex*Sex_Dyad + NN_Index",
  re="(1|Signaller)+(1|Com_number)", data=xdata) #this function 
  #will remove NAs and dummy code variables and return a list with 2 components: summary and data;
  #summary was used to choose random slopes as indicated by Roger Mundry (2021)

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
mstab1=round(m.stab$summary[, -1], 3) 
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
Chisq_Model1=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_Model1=round(Chisq_Model1, 3)


#Model 2: Do individual and social factors affect the use of combinations of Rumble and Ear-Flapping by elephants during greeting?----
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

##Check if needed random effects needed
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
  #summary was used to choose random slopes as indicated by Roger Mundry (2021)

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
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/glmm_stability.r") #function by Roger Mundry (2021)
m.stab=glmm.model.stab(model.res=full) 

mstab2=round(m.stab$summary[, -1], 3) 
m.stab.plot(m.stab$summary[, -1]) 

###Collinearity
xx=lm(Combination ~ Signaller_sex + Sex_Dyad + z.NN_Index,
  data=t.data)
vif2=vif(xx) 
vif2=round(vif2, 3) 

##Full null model comp
null=glmer(Combination ~ Signaller_sex + Sex_Dyad +
  (1+z.NN_Index+Sex_Dyad.Different||Signaller) + (1|Com_number),
  data=t.data, family=binomial) 
Chisq_Model2=as.data.frame(anova(null, full, test="Chisq")) 
Chisq_Model2_res=round(Chisq_Model2, 3)

##Results
###Coefficients
round(summary(full)$coefficients, 3) 
tests2=as.data.frame(drop1(full, test="Chisq")) 
tests2=round(tests2, 3)

##Confidence Intervals
source("~/Desktop/R courses 2022/Mundry R course 2020/course material/functions/boot_glmm.r") #Function provided by Roger Mundry (2021)
boot.res2=boot.glmm.pred(model.res=full, excl.warnings=F,
  nboots=1000, para=F)
cis2=round(boot.res2$ci.estimates, 3)

##Effect sizes 
r.sq2=r.squaredGLMM(object=full) 
r.sq2=round(r.sq2, 3)

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
