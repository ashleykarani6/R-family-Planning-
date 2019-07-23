attach(newdata)
datasett<-data.frame(religion,age,city,livingchildren,w820,marstat,age5,anymethod,education,revised_unmet_need_cmw,unmet_need_cmw,unmet_cmw,wealth)
datasett
newdata1<-datasett[!is.na(datasett$revised_unmet_need_cmw)&datasett$unmet_cmw!="9",]
newdata2<-newdata1[newdata1$religion!="9"&newdata1$w820!="9",]
newdata3<-newdata2[newdata2$city!="4"&newdata2$city!="5",]
attach(newdata3)
unmetspacing<-newdata3[newdata3$unmet_need_cmw=="2",]
unmetspacing
unmetlimiting<-newdata3[newdata3$unmet_need_cmw=="3",]
unmetlimiting
totalunmetneed<-newdata3[newdata3$unmet_cmw=="1",]
totalunmetneed
nairobispacing<-unmetspacing[unmetspacing$city=="1",]
mombasaspacing<-unmetspacing[unmetspacing$city=="2",]
kisumuspacing<-unmetspacing[unmetspacing$city=="m3",]
nairobilimiting<-unmetlimiting[unmetlimiting$city=="1",]
mombasalimiting<-unmetlimiting[unmetlimiting$city=="2",]
kisumulimiting<-unmetlimiting[unmetlimiting$city=="3",]
nairobitotalunmet<-totalunmetneed[totalunmetneed$city=="1",]
mombasatotalunmet<-totalunmetneed[totalunmetneed$city=="2",]
kisumutotalunmet<-totalunmetneed[totalunmetneed$city=="3",]
totalnairobi<-newdata3[newdata3$city=="1",]
totalmombasa<-newdata3[newdata3$city=="2",]
totalkisumu<-newdata3[newdata3$city=="3",]
nrow(newdata3)
spacprop<-
  c((nrow(nairobispacing)/nrow(totalnairobi)),(nrow(mombasaspacing)/nrow(totalmombasa)),(nrow(kisumuspacing)/nrow(totalkisumu)))
spacprop
cities<-c("Nairobi","Mombasa","Kisumu")
spacingplot<-barplot(spacprop,names.arg=cities,xlab="City",ylab="Proportion",col="pink",main="Unmet need for spacing")
limitprop<-
  c((nrow(nairobilimiting)/nrow(totalnairobi)),(nrow(mombasalimiting)/nrow(totalmombasa)),(nrow(kisumulimiting)/nrow(totalkisumu)))
limitprop
limitingplot<-barplot(limitprop,names.arg=cities,xlab="City",ylab="Proportion",col="pink",main="Unmet need for limiting")
totalunmetprop<-
  c((nrow(nairobitotalunmet)/nrow(totalnairobi)),(nrow(mombasatotalunmet)/nrow(totalmombasa)),(nrow(kisumutotalunmet)/nrow(totalkisumu)))
totalunmetprop    
totalunmetplot<-barplot(totalunmetprop,names.arg=cities,xlab="City",ylab="Proportion",col="pink",main="Total unmet need")
attach(newdata3)
#model of unmet need for limiting against spacing
newdata6<-newdata3[newdata3$revised_unmet_need_cmw=="3" | newdata3$revised_unmet_need_cmw=="4",]
View(newdata6)

newdata6$revised_unmet_need_cmw<- as.character(newdata6$revised_unmet_need_cmw)
newdata6$revised_unmet_need_cmw[newdata6$revised_unmet_need_cmw== "3"] <- "0"
newdata6$revised_unmet_need_cmw[newdata6$revised_unmet_need_cmw== "4"] <- "1"
newdata6$revised_unmet_need_cmw<-as.factor(newdata6$revised_unmet_need_cmw)
View(newdata6)

revised_unmet_need_cmw3<-factor(newdata6$revised_unmet_need_cmw)
summary(revised_unmet_need_cmw3)

religion3<-factor(newdata6$religion)
summary(religion3)

city3<-factor(newdata6$city)
summary(city3)

livingchildren3<-newdata6$livingchildren
summary(livingchildren3)

employment3<-factor(newdata6$w820)
summary(employment3)


education3<-factor(newdata6$education)
summary(education3)

wealth3<-factor(newdata6$wealth)
summary(wealth3)

age3<-newdata6$age

agegroup3<-factor(newdata6$age5)
summary(agegroup3)
newmodel.spac<-glm(revised_unmet_need_cmw3 ~ religion3 + city3 + livingchildren3 + employment3 + age3 + education3 + wealth3, family="binomial")
summary(newmodel.spac)
predictoree<-round(newmodel.spac$fitted.values)
predictoree
newmodel.spac$fitted.values
finalclass<-table(predictoree,revised_unmet_need_cmw3)
class(finalclass)
prop.table(finalclass)
library(caret)
sensitivity(finalclass)
specificity(finalclass)
table(newdata6$revised_unmet_need_cmw3)
library(ROCR)
predi<-predict(newmodel.spac, type = "response")
predi
pred<-prediction(predi,revised_unmet_need_cmw3)
pred
#threshold/best value
persen<-performance(prediction.obj = pred, measure = "sens", x.measure = "cutoff")
plot(persen)
par(new=TRUE)                 
perspec<-performance(prediction.obj = pred, measure = "spec", x.measure = "cutoff")
plot(perspec)
#OR
#identifying best values
eval<-performance(pred,"acc")
plot(eval)
max<-which.max(slot(eval,"y.values")[[1]])
acc<-slot(eval,"y.values")[[1]][max]
cut<-slot(eval,"x.values")[[1]][max]
print(c(Accuracy=acc, Cutoff = cut))
#ROC CURVE
roc<-performance(pred,"tpr","fpr")
roc
plot(roc,
     colorize=T,
     lwd=3,
     main = "ROC CURVE",
     ylab = "sensitivity",
     xlab = "1-specificity")
abline(h=0.78,v=0.58)
#area under the curve
auc<-performance(pred, "auc")
auc<-unlist(slot(auc,"y.values"))
auc<-round(auc, 4)
legend(.6, .4, auc, title="AUC")

#model of unmet need for limiting against met need
newdata5<-newdata3[newdata3$revised_unmet_need_cmw=="1" | newdata3$revised_unmet_need_cmw=="3",]
View(newdata5)

newdata5$revised_unmet_need_cmw<- as.character(newdata5$revised_unmet_need_cmw)
newdata5$revised_unmet_need_cmw[newdata5$revised_unmet_need_cmw== "1"] <- "0"
newdata5$revised_unmet_need_cmw[newdata5$revised_unmet_need_cmw== "3"] <- "1"
newdata5$revised_unmet_need_cmw<-as.factor(newdata5$revised_unmet_need_cmw)
View(newdata5)

revised_unmet_need_cmw2<-factor(newdata5$revised_unmet_need_cmw)
summary(revised_unmet_need_cmw2)

religion2<-factor(newdata5$religion)
summary(religion2)

city2<-factor(newdata5$city)
summary(city2)

livingchildren2<-newdata5$livingchildren
summary(livingchildren2)

employment2<-factor(newdata5$w820)
summary(employment2)


education2<-factor(newdata5$education)
summary(education2)

wealth2<-factor(newdata5$wealth)
summary(wealth2)

age2<-newdata5$age
newmodel.limit<-glm(revised_unmet_need_cmw2 ~ religion2 + city2 + livingchildren2 + employment2 + age2 + education2 + wealth2, family="binomial")
summary(newmodel.limit)
predictoree2<-round(newmodel.limit$fitted.values)
predictoree2
newmodel.limit$fitted.values
finalclass2<-table(predictoree2,revised_unmet_need_cmw2)
class(finalclass2)
prop.table(finalclass2)
library(caret)
sensitivity(finalclass2)
specificity(finalclass2)
table(newdata5$revised_unmet_need_cmw2)
library(ROCR)
predi2<-predict(newmodel.limit, type = "response")
predi2
pred2<-prediction(predi2,revised_unmet_need_cmw2)
pred2
#threshold/best value
persen2<-performance(prediction.obj = pred2, measure = "sens", x.measure = "cutoff")
plot(persen2)
par(new=TRUE)                 
perspec2<-performance(prediction.obj = pred2, measure = "spec", x.measure = "cutoff")
plot(perspec2)
#OR
#identifying best values
eval2<-performance(pred2,"acc")
max2<-which.max(slot(eval,"y.values")[[1]])
acc2<-slot(eval2,"y.values")[[1]][max2]
cut2<-slot(eval2,"x.values")[[1]][max2]
print(c(Accuracy=acc2, Cutoff = cut2))
#ROC CURVE
roc2<-performance(pred2,"tpr","fpr")
roc2
plot(roc2,
     colorize=T,
     lwd=3,
     main = "ROC CURVE",
     ylab = "sensitivity",
     xlab = "1-specificity")
abline(h=0.7,v=0.31)
#area under the curve
auc2<-performance(pred2, "auc")
auc2<-unlist(slot(auc2,"y.values"))
auc2<-round(auc2, 4)
legend(.6, .4, auc2, title="AUC")


