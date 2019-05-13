#supplemental analyses- looking at models when including key demographics variables
#education, liberalism, religiosity (spelled incorrectly in dataset), gender (male=0,female=1)

install.packages("lme4")
install.packages("sjPlot")
library(lme4)
library(sjPlot)
install.packages("snakecase")
library(snakecase)


#in order to use Edu, must remove "other" category so that higher numbers mean more education
levels(test2$Edu)
table(test2$Edu)
test2$Edu2<-test2$Edu
test2<-test2[test2$Edu2!="other",]
test2$Edu2<-factor(test2$Edu2)
levels(test2$Edu2)


######subsetting#####

study2<-subset(test2, test2$Wave=="Post Election 11/9-11/14/16")
study1<-subset(test2, test2$Wave!="Post Election 11/9-11/14/16")

Tfirst1 <- subset(study1, order=="TC")
Cfirst1 <- subset(study1, order=="CT")
Tfirst2 <- subset(study2, order=="TC")
Cfirst2 <- subset(study2, order=="CT")

##confidence interval code from Matt Stanley
confint(modelname, parm="beta_",method="Wald")

#########info on variables for breaking down the interaction###########

#in R output, comparison is FIRST ITEM,  main is SECOND ITEM###
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

#####################################Using verb cat where 1=positive, 0=negative###############################

####Trump blanks Clinton#####

#study 1
model1<-glmer(response~(TrumpClinton+Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpClinton*Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
summary(model1)
summary(model2)

#study 2
model3<-glmer(response~(TrumpClinton+Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model4<-glmer(response~(TrumpClinton*Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model3)
summary(model4)


tab_model(model1, model2, transform=NULL, show.est=TRUE, show.se=TRUE, show.stat=TRUE, col.order= c( "est", "se", "stat", "p", "ci"), ci.hyphen = ", ", file = "demos_Tfirst1.doc")
tab_model(model3, model4, transform=NULL, show.est=TRUE, show.se=TRUE, show.stat=TRUE, col.order= c( "est", "se", "stat", "p", "ci"), ci.hyphen = ", ", file = "demos_Tfirst2.doc")


######Clinton blanks Trump#####

#study 1
model5<-glmer(response~(TrumpClinton+Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model6<-glmer(response~(TrumpClinton*Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
summary(model5)
summary(model6)

#study 2
model7<-glmer(response~(TrumpClinton+Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model8<-glmer(response~(TrumpClinton*Positive+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model7)
summary(model8)

tab_model(model5, model6, transform=NULL, show.est=TRUE, show.se=TRUE, show.stat=TRUE, col.order= c( "est", "se", "stat", "p", "ci"), ci.hyphen = ", ", file = "demos_Cfirst1.doc")
tab_model(model7, model8, transform=NULL, show.est=TRUE, show.se=TRUE, show.stat=TRUE, col.order= c( "est", "se", "stat", "p", "ci"), ci.hyphen = ", ", file = "demos_Cfirst2.doc")


###breaking down the interaction#####


#only include demographics that were significant in model 1
#demographics to include: Gender & Religiosity (Cfirst1), Gender (Tfirst1, Cfirst2, Tfirst2)
#always check to see if using study 1 or study 2 datasets

###Trump blanks Clinton#####

#In this order:
# 1. effect of voter among positive verbs
# 2. effect of voter among negative verbs & effect of verb among clinton voters
# 3. effect of verb among trump voters


model1<-glmer(response~(TrumpVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model3<-glmer(response~(ClintonVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)

model4<-glmer(response~(TrumpVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model5<-glmer(response~(TrumpVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model6<-glmer(response~(ClintonVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)

summary(model4)
summary(model5)
summary(model6)

confint(model4, parm="beta_",method="Wald")
confint(model5, parm="beta_",method="Wald")
confint(model6, parm="beta_",method="Wald")


####Clinton blanks Trump####
model1<-glmer(response~(TrumpVoter*Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model2<-glmer(response~(TrumpVoter*Positive+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model3<-glmer(response~(ClintonVoter*Positive++Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)


model4<-glmer(response~(TrumpVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model5<-glmer(response~(TrumpVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model6<-glmer(response~(ClintonVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)

summary(model4)
summary(model5)
summary(model6)

confint(model4, parm="beta_",method="Wald")
confint(model5, parm="beta_",method="Wald")
confint(model6, parm="beta_",method="Wald")


#####################################Using verb cat where 1=negative, 0=positive###############################

####Trump blanks Clinton#####

#study 1
model1<-glmer(response~(TrumpClinton+verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
model2<-glmer(response~(TrumpClinton*verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst1, family=binomial)
summary(model1)
summary(model2)

#study 2
model3<-glmer(response~(TrumpClinton+verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model4<-glmer(response~(TrumpClinton*verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model3)
summary(model4)



######Clinton blanks Trump#####

#study 1
model5<-glmer(response~(TrumpClinton+verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model6<-glmer(response~(TrumpClinton*verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
summary(model5)
summary(model6)

#study 2
model7<-glmer(response~(TrumpClinton+verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model8<-glmer(response~(TrumpClinton*verbcat+as.numeric(Edu2)+Gender2+Liberalism+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model7)
summary(model8)


###breaking down the interaction#####

remove(model3)

#only include demographics that were significant in model 1
#demographics to include: Gender & Religiosity (Cfirst1), Gender (Tfirst1, Cfirst2, Tfirst2)
#always check to see if using study 1 or study 2 datasets

###Trump blanks Clinton#####

model1<-glmer(response~(TrumpVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
##the above model is the SAME as model1 further above; variable names are just changed#
model2<-glmer(response~(TrumpVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
model3<-glmer(response~(ClintonVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Tfirst2, family=binomial)
summary(model1)
summary(model2)
summary(model3)


####Clinton blanks Trump####
model1<-glmer(response~(TrumpVoter*Negative+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model2<-glmer(response~(TrumpVoter*Positive+Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
model3<-glmer(response~(ClintonVoter*Negative++Gender2+Religiousity)+ (1|verb)+ (1|WorkerID), data=Cfirst1, family=binomial)
summary(model1)
summary(model2)
summary(model3)

model1<-glmer(response~(TrumpVoter*Negative+Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model2<-glmer(response~(TrumpVoter*Positive+Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
model3<-glmer(response~(ClintonVoter*Negative++Gender2)+ (1|verb)+ (1|WorkerID), data=Cfirst2, family=binomial)
summary(model1)
summary(model2)
summary(model3)
