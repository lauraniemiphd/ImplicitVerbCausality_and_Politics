
#Importing the dataset 
TC <- read.csv("~/Desktop/R/TrumpClintonR.csv", stringsAsFactors=FALSE)

######variable creation/labeling using original dataset "TC" ##################

#assigning the variables and making them into factors where each number corresponds to a category
TC$Wave <-factor(TC$Wave, levels=c(1,2,3), labels=c("Red States 10/10-10/17/16", "All States 10/13/16", "Post Election 11/9-11/14/16"))
TC$PartyBelong <-factor(TC$PartyBelong, levels=c(1,2,3,4,5), labels=c("Republican", "Democrat", "Independent", "Other", "None"))
TC$PartyUsual <-factor(TC$PartyUsual, levels=c(1,2,3,4,5), labels=c("Republican", "Democrat", "Independent", "Other", "None"))
TC$WhoVote <-factor(TC$WhoVote, levels=c(1,2,3,4), labels=c("Trump", "Clinton", "Other", "Not voting"))
TC$TrumpClinton <-factor(TC$TrumpClinton, levels=c(0,1), labels=c("Clinton voter", "Trump voter"))
TC$Edu <-factor(TC$Edu, levels=c(1,2,3,4,5,6,7,8), labels=c("some high school", "high school", "some college", "college degree", "graduate degree", "doctoral degree", "professorial degree", "other"))
TC$EngNative <-factor(TC$EngNative, levels=c(1,2), labels=c("Yes", "No"))
TC$EngYears <-factor(TC$EngYears, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("less than 2 years", "2 years", "3 years", "4 years", "5 years", "6 years", "7 years", "8 years", "9 years", "10 years", "11 years", "12+ years"))
TC$Ethnicity <-factor(TC$Ethnicity, levels=c(0,1,2,3,4,5,6,7), labels=c("Unknown", "White", "Black", "Asian", "Native American", "Mixed", "Hispanic", "Middle Eastern"))
TC$Gender <-factor(TC$Gender, levels=c(1,2,3), labels=c("Male", "Female", "Other"))

###subsetting original data set by "study' for writing methods sections
orig2<-subset(TC, TC$Wave=="Post Election 11/9-11/14/16")
orig1<-subset(TC, TC$Wave!="Post Election 11/9-11/14/16")
summary(is.na(orig1$WhoVote))
summary(is.na(orig2$WhoVote))

###############exclusions#################


#Excluding people who didn't vote for either Trump or Clinton
TC2 <- TC[(TC$WhoVote=="Trump" | TC$WhoVote=="Clinton" ),]

summary(TC)
by(TC$WhoVote, TC$Wave,summary)
by(TC2$WhoVote, TC2$Wave,summary)
by(TC3$WhoVote, TC3$Wave,summary)
summary(TC3$WhoVote)
summary(TC3$Wave)
summary(is.na(TC$WhoVote))

distinct(study1$WorkerID)

library(dplyr)
install.packages("tidyr")
library(tidyr)
COUNT(study1$PartyBelong)
summarise(n_distinct(study1$PartyBelong))

#checking that the exclusion worked
table(TC2$WhoVote)

#Excluding people who weren't paying attention
TC3 <- TC2[(TC2$AttentionCheck==6 | TC2$AttentionCheck==7 ),]
orig2b<-subset(TC2, TC2$Wave=="Post Election 11/9-11/14/16")
orig1b<-subset(TC2, TC2$Wave!="Post Election 11/9-11/14/16")
summary(as.factor(TC2$AttentionCheck))
summary(as.factor(TC$AttentionCheck))
summary(as.factor(orig1$AttentionCheck))
summary(as.factor(orig2$AttentionCheck))
summary(TC3$Gender)
summary(orig1$WhoVote)
summary(orig2$WhoVote)


#Checking that the exclusion worked
table(TC3$AttentionCheck)

#Viewing the data
summary(TC3)
View(TC3)
names(TC3)


#making dataset without: object bias averages Gina made, "commments" variable, sentence completion, Male Female IC task
TC4<-TC3[c(1:9,14:66, 71:81)]
names(TC4)
summary(TC4)

####subsetting by 'study' (study 1= waves 2&3, study 2= wave 4) for wide-format dataset#####
table(TC4$Wave)
s2wide<-subset(TC4, TC4$Wave=="Post Election 11/9-11/14/16")
s1wide<-subset(TC4, TC4$Wave!="Post Election 11/9-11/14/16")

########reshaping into long-format dataset############

install.packages("reshape2")
library(reshape2)
#'melting' participant-level data
test<-melt(TC4, id.vars = c(1:14, 63:73), na.rm=TRUE, 
           variable.name = "trial", value.name = "response")
head(test$thing.verb)
tail(test)
remove(thing, test2)
tail(test)

#creating variables to denote verb name, verb order, and verb set
thing <- colsplit(test$trial, "_", c("order", "set", "verb"))
thing$order<-as.factor(thing$order)
thing$verb<-as.factor(thing$verb)
str(thing$verb)
levels(thing$verb)

#adding order, verb, and set to 'melted' dataset
test2<-cbind(thing,test)
test2<-cbind(test, thing)
tail(test2)
str(test2)
test2$order<-as.factor(test2$order)
test2$verb<-as.factor(test2$verb)
str(test2$order)
str(test2$verb)
levels(test2$verb)
View(test2)


######subsetting long-format data#####

#by study
study2<-subset(test2, test2$Wave=="Post Election 11/9-11/14/16")
study1<-subset(test2, test2$Wave!="Post Election 11/9-11/14/16")

#by order within each study
Tfirst1 <- subset(study1, order=="TC")
Cfirst1 <- subset(study1, order=="CT")
Tfirst2 <- subset(study2, order=="TC")
Cfirst2 <- subset(study2, order=="CT")


#####variable creation in long-form dataset######

#positive verbs: 5,6, 11, 13,14, 15, 20, 23
#negative verbs: 1,2,3,4,7,8,9, 10, 12, 16, 17, 18, 19, 21,22, 24

#creating verb cat for positive vs negative verbs
library(car)
test2$verbcat = recode(test2$verb, '5=0; 6=0; 11=0; 13=0; 14=0; 15=0; 20=0; 23=0; else=1')
test2$verbcat = recode(test2$verb, '"Comforted"=0; "Complimented"=0; "Impressed"=0; "Inspired"=0; 
                       "Interested"=0; "Praised"=0; "Forgave"=0; "Thanked"=0; else=1')
str(test2$verbcat)
levels(test2$verbcat)
head(test2$verbcat)
tail(test2)
names(test2)

#the catagory labeled first is the comparison or control, the one labeled second is the treatment
test2$verbcat <-factor(test2$verbcat, levels=c(0,1), labels=c("Positive", "Negative"))
test2$TrumpClinton <-factor(test2$TrumpClinton, levels=c(0,1), labels=c("Clinton", "Trump"))
test2$objectbias <-factor(test2$response, levels=c(0,1), labels=c("subject", "object"))
table(test2$WhoVote, test2$TrumpClinton)
head(test2$objectbias)

###############descriptives and data visualization############

means <-aggregate(test2$response~ test2$verbcat+test2$TrumpClinton, data=test2, mean)
means <-aggregate(Tfirst1$response~ Tfirst1$verbcat+Tfirst1$TrumpClinton, data=Tfirst1, mean)
means <-aggregate(Cfirst1$response~ Cfirst1$verbcat+Cfirst1$TrumpClinton, data=Cfirst1, mean)
means

xtabs(formula=response~verbcat+TrumpClinton, data=Tfirst1)
table(Tfirst1$verbcat, Tfirst1$TrumpClinton)

xtabs(formula=response~verbcat+TrumpClinton, data=Cfirst1)
table(Cfirst1$verbcat, Cfirst1$TrumpClinton)

plot(tapply(Tfirst$response, Tfirst$TrumpClinton, mean))
table(Tfirst$objectbias[Tfirst$TrumpClinton=="Clinton"])
prop.table(table(Tfirst$objectbias[Tfirst$TrumpClinton=="Clinton"]))

table(s1wide$Gender)
table(s2wide$Gender)
table(TC4$Gender)

table(s2wide$Gender)

table(s1wide$Ethnicity)
table(s1wide$Edu)
table(s2wide$Edu)
table(s1wide$TrumpClinton)
prop.table(table(s1wide$Ethnicity))
table(s2wide$Ethnicity)
table(TC4$Ethnicity)
mean(s1wide$Age, na.rm=TRUE)
sd(s1wide$Age, na.rm=TRUE)

levels(TC4$Edu)
table(TC4$Edu)
TC4$Edu2<-TC4$Edu
#creating Education variable that excludes 8="other" so that edu is a continuous variable
#where higher numbers mean more education
TC4<-TC4[TC4$Edu2!="other",]
levels(TC4$Edu2)
TC4$Edu2<-factor(TC4$Edu2)

table(s2wide$Ethnicity)
table(s2wide$Edu2)
table(s2wide$TrumpClinton)
mean(s2wide$Age, na.rm=TRUE)
sd(s2wide$Age, na.rm=TRUE)

#comparing Trump and Clinton voters
t.test(s1wide$Age~s1wide$TrumpClinton, var.equal=T)
chisq.test(s1wide$Gender, s1wide$TrumpClinton)


####looking at individual waves using only Trump blanks Clinton format data####

#subsetting by wave
Twave1<-subset(Tfirst, Tfirst$Wave=="Red States 10/1/-10/17/16")
Twave2<-subset(Tfirst, Tfirst$Wave=="All States 10/13/16")
Twave3<-subset(Tfirst, Tfirst$Wave=="Post Election 11/9-11/14/16")

str(Tfirst$Wave)
levels(Tfirst$Wave)
table(Twave3$Wave)
names(test2)

table(study2$Wave)

##testing differences between waves
summary(test2)
summary(aov(test2$Age~test2$Wave))
summary(aov(test2$Liberalism~test2$Wave))
summary(aov(test2$Religiousity~test2$Wave))
summary(aov(as.numeric(test2$Edu)~test2$Wave))

tapply(test2$Age, test2$Wave, mean)
tapply(test2$Liberalism, test2$Wave, mean)
tapply(test2$Religiousity, test2$Wave, mean)
tapply(as.numeric(test2$Edu), test2$Wave, mean)


library(Hmisc)
chiSquare(test2$Gender2~test2$Wave)
prop.table(table(test2$Gender2, test2$Wave))
chiSquare(test2$TrumpClinton~test2$Wave)
prop.table(table(test2$TrumpClinton, test2$Wave))
chiSquare(test2$Ethnicity~test2$Wave)
prop.table(table(test2$Ethnicity, test2$Wave))

####### notes on regression models###########

#iv's are TrumpClinton (Clinton is comparison, Trump is main), verbcat (positive is comparison, negative is main)
#random effects are verb and WorkerID, dv is response 
#(because R messed up comparison for object bias, made it 1 vs 2 even though I entered it as 0 vs 1)

#######main regression models with event type (POS=1, NEG=0)#########

####Trump blanks Clinton#####

##study 1
model1<-glmer(response~(TrumpClinton+Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpClinton*Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
summary(model1)
summary(model2)

##study 2
model3<-glmer(response~(TrumpClinton+Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model4<-glmer(response~(TrumpClinton*Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model3)
summary(model4)

######Clinton blanks Trump#####

##study 1
model5<-glmer(response~(TrumpClinton+Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model6<-glmer(response~(TrumpClinton*Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
summary(model5)
summary(model6)

##study 2
model7<-glmer(response~(TrumpClinton+Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model8<-glmer(response~(TrumpClinton*Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model7)
summary(model8)

#confidence intervals, using r code from Matt Stanley
confint(model1, parm="beta_",method="Wald")
confint(model2, parm="beta_",method="Wald")
confint(model3, parm="beta_",method="Wald")
confint(model5, parm="beta_",method="Wald")
confint(model6, parm="beta_",method="Wald")
confint(model7, parm="beta_",method="Wald")
confint(model8, parm="beta_",method="Wald")

########Breaking down the Interaction starting with event type(1=pos)#######

###Trump blanks Clinton#####

#effect of voter (1=trump) among negative verbs; effect of verb(1=pos) among clinton voters 
model1<-glmer(response~(TrumpVoter*Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
#effect of voter (1=trump) among positive verbs; effect of verb(1=pos) among clinton voters 
model2<-glmer(response~(TrumpVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
#effect of voter (1=clinton) among negative verbs; effect of verb(1=pos) among trump voters
model3<-glmer(response~(ClintonVoter*Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model1)
summary(model2)
summary(model3)

####Clinton blanks Trump####

#effect of voter (1=trump) among negative verbs; effect of verb(1=pos) among clinton voters 
model4<-glmer(response~(TrumpVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
#effect of voter (1=trump) among positive verbs; effect of verb(1=pos) among clinton voters 
model5<-glmer(response~(TrumpVoter*Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
#effect of voter (1=clinton) among negative verbs; effect of verb(1=pos) among trump voters
model6<-glmer(response~(ClintonVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model4)
summary(model5)
summary(model6)

########main regression models##################

install.packages("lme4")
library(lme4)

####Trump blanks Clinton#####

##study 1
model1<-glmer(response~(TrumpClinton+verbcat)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpClinton*verbcat)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
summary(model1)
summary(model2)

##study 2
model3<-glmer(response~(TrumpClinton+verbcat)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model4<-glmer(response~(TrumpClinton*verbcat)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model3)
summary(model4)


######Clinton blanks Trump#####

##study 1
model5<-glmer(response~(TrumpClinton+verbcat)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model6<-glmer(response~(TrumpClinton*verbcat)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
summary(model5)
summary(model6)

##study 2
model7<-glmer(response~(TrumpClinton+verbcat)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model8<-glmer(response~(TrumpClinton*verbcat)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model7)
summary(model8)

confint(model1, parm="beta_",method="Wald")
confint(model2, parm="beta_",method="Wald")
confint(model3, parm="beta_",method="Wald")
confint(model4, parm="beta_",method="Wald")
confint(model5, parm="beta_",method="Wald")
confint(model6, parm="beta_",method="Wald")
confint(model7, parm="beta_",method="Wald")
confint(model8, parm="beta_",method="Wald")

#comparing CI's get with Matt code and with sjPlot code
sjt.glmer(model3, exp.coef=FALSE,ci.hyphen = ", ", file = "ci_trumpobject1.doc")
sjt.glmer(model4, exp.coef=FALSE,ci.hyphen = ", ", file = "ci_trumpobject2.doc")
sjt.glmer(model3, exp.coef=FALSE,ci.hyphen = ", ", file = "ci_clintonobject1.doc")
sjt.glmer(model4, exp.coef=FALSE,ci.hyphen = ", ", file = "ci_clintonobject2.doc")

#########Variable manipulation to use in regressions###########

#releveling variables OVERALL (then re-make subsets) so can look at full interaction
#in relevel(), ref means what is the comparison group, the control group
test2$TrumpVoter<-test2$TrumpClinton
levels(test2$TrumpVoter) #Clinton is comparison, Trump is main
test2$ClintonVoter <-relevel(test2$TrumpVoter, ref="Trump") 
levels(test2$ClintonVoter)#Trump is comparison, Clinton is main

test2$Negative<-test2$verbcat
levels(test2$Negative) #positive is comparison, negative is main
test2$Positive<-relevel(test2$Negative, ref="Negative")
levels(test2$Positive) #negative is comparison, positive is main

#making gender variable one with only 2 levels, trying a better way than before
levels(test2$Gender)
test2$Gender2<-test2$Gender
test2<-test2[test2$Gender2!="Other",]
levels(test2$Gender2)
test2$Gender2<-factor(test2$Gender2)
table(test2$Gender2)
names(test2)

########Breaking down the Interaction#######
library(lme4)

#always first check to see if using study 1 or study 2 datasets

###Trump blanks Clinton#####
model<-glmer(response~(TrumpVoter+Negative)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model1<-glmer(response~(TrumpVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpVoter*Positive)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model3<-glmer(response~(ClintonVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model)
summary(model1)
summary(model2)
summary(model3)

####Clinton blanks Trump####
model4<-glmer(response~(TrumpVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model5<-glmer(response~(TrumpVoter*Positive)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model6<-glmer(response~(ClintonVoter*Negative)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model4)
summary(model5)
summary(model6)

#confidence intervals, using r code from Matt Stanley
confint(model2, parm="beta_",method="Wald")
confint(model3, parm="beta_",method="Wald")
confint(model5, parm="beta_",method="Wald")
confint(model6, parm="beta_",method="Wald")


########looking at potential covariates######

#Trump blanks Clinton
model1<-glmer(response~(TrumpVoter+Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst, family=binomial)
model2<-glmer(response~(TrumpVoter*Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst, family=binomial)

#Clinton blanks Trump
model3<-glmer(response~(TrumpVoter+Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst, family=binomial)
model4<-glmer(response~(TrumpVoter*Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst, family=binomial)

summary(model1)
summary(model2)
summary(model3)
summary(model4)

####removing objects from global environment at the end of each session

remove(model1, model2,model5, model6)
remove (ginasTable, means)

##trying out packages to make regression output into tables
install.packages("tibble")
install.packages("sjPlot")
library(sjPlot)

devtools::install_github("strengejacke/strengejacke")

#this gives CI for "odds ratio"
tab_model(model1, model2, show.est=TRUE, show.std=TRUE, file = "sjt_test2.doc")
?tab_model
?glmer  

#puts hyphen in CI, gives CI for "log odds"
sjt.glmer(model1, model2, exp.coef=FALSE, file = "sjt_test2.doc")
help("Deprecated")

#####use me to get perfectly formatted 95% CI's! (and more tables if needed)###
sjt.glmer(model2, exp.coef=FALSE,ci.hyphen = ", ", file = "sjt_test2.doc")
