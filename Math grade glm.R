
# ******************************************************************************* #
#                                                                                 #
# Student Alcohol Consumption 프로젝트                                            #
# URL : https://www.kaggle.com/uciml/student-alcohol-consumption                  #
#                                                                                 #
#                                                                                 #
#                                                                                 #
#                                                                                 #
# ******************************************************************************* #



########## Using Library ##########
options(digits=4, scipen=10)

library(rpart) ; library(rattle) ; library(ggplot2) ; library(party)
library(reshape2) ; library(gmodels)

########## Data Load ##########
DF <- read.table("F:\\R\\Study\\Student alcohol consumption and grade prediction\\data\\student-mat.txt", header=T, sep=",")


######### Data Processing #########
dim(DF)
str(DF)

DF$age_new <- ifelse(DF$age >= 19, "19세이상",ifelse(DF$age==15,"15세",ifelse(DF$age==16,"16세",ifelse(DF$age==17,"17세","18세"))))

######### EDA #########
names(DF)
table(DF$Walc) # 주말 알코올 소비
table(DF$Dalc) # 주중 알코올 소비

# CrossTable(DF$Dalc, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# View(data.frame(table(DF$Dalc, DF$Walc)))

DF$tmp_1 <- paste(DF$Dalc,DF$Walc,sep="")
DF <- transform(DF, alcohol = ifelse(tmp_1 %in% c("11","21","12"), "Low",
                                     ifelse(tmp_1 %in% c("22","32","42","13","23","33","43"),"Middle",
                                            ifelse(tmp_1 %in% c("14","24","15","25","35"),"High","Very High"))))
DF$alcohol <- factor(DF$alcohol, levels=c("Low","Middle","High","Very High"))
DF <- DF[,-35]


##### Grade summary #####
barplot(table(DF$G1))
barplot(table(DF$G2))
barplot(table(DF$G3))

plot(data.frame(G1=jitter(DF$G1),G2=jitter(DF$G2),G3=jitter(DF$G3)), cex=0.8, col="Light Sky Blue",
     main="G1, G2 and G3 Grade Scatter Plot")
DF <- transform(DF, G_12 = G1 + G2)
DF <- transform(DF, G123 = G1 + G2 + G3)
hist(DF$G_12, breaks=100)

hist(DF$G123, breaks=50, main="Distribution of Student's Grade", xlab="Grade", freq=FALSE,
     col="Light Sky Blue", border="white")
lines(density(DF$G123), col="red")


##### School 성적 분포 #####
ggplot(DF, aes(x=school, y=G123, col=school)) + geom_boxplot()
tapply(DF$G123, DF$school, summary)

##### sex, age, health ##### 
# 여성보다 남성의 성적이 조금더 높은 경향이 있음
# 남성의 경우 나이가 올라가면서 성적이 떨어지는 경향이 있음

# dcast(DF, sex ~ age, value.var="sex", length)
# prop.table(table(DF$sex, DF$age))

ggplot(DF, aes(x=sex, y=G123, col=sex))+geom_boxplot() +
  labs(x=NULL, y="Math Grade", title="Distribution Math Grade in Sex")
tapply(DF$G123, DF$sex, summary)

hist(DF[DF$sex =="F",]$G123, breaks=30, xlim=c(0,60), freq = FALSE, col=rgb(1,0,0,0.2), border="white",main="Distribution Math Grade in sex", xlab="Math Grade")
hist(DF[DF$sex =="M",]$G123, breaks=30, xlim=c(0,60), freq = FALSE, add=T, col=rgb(0,0,1,0.2),border="white")
lines(density(DF[DF$sex=="F",]$G123), col="hot pink", lwd=2)
lines(density(DF[DF$sex=="M",]$G123), col="Deep Sky Blue", lwd=2)
legend("topright", legend=c("Female","Male"), col=c(rgb(1,0,0,0.2), 
                                                      rgb(0,0,1,0.2)), pt.cex=2, pch=15 )
t.test(G123 ~ sex, data=DF, alternative = "two.sided", conf.level=0.95)



ggplot(DF, aes(x=sex, y=G123, fill=age_new)) + geom_boxplot(alpha=0.4) +
  labs(x="Sex", y="Math Grade", fill="Age", title="Distribution Math Grade in Sex & Age") +
  theme_bw() + scale_fill_brewer(palette = "Blues")

ggplot(DF, aes(x=as.factor(health), y=G123, col=as.factor(health))) + geom_boxplot()

ggplot(DF, aes(x=sex, y=G123, col=as.factor(health))) + geom_boxplot()

ggplot(DF, aes(x=age_new, y=G123, col=as.factor(health))) + geom_boxplot()

##### famsize, Pstatus, Medu, Fedu, Mjob, Fjob, guardian #####
# 부모님 교육 수준에 따라 성적에 차이가 있음
DF <- transform(DF, parents_edu = Medu + Fedu)
DF <- transform(DF, parents_edu_ct = ifelse(parents_edu <= 3 , "Low",ifelse(parents_edu <= 6,"Middle","High")))
DF$parents_edu_ct <- factor(DF$parents_edu_ct, levels=c("Low","Middle","High"))

ggplot(DF, aes(x=famsize, y=G123, col=famsize)) + geom_boxplot()
ggplot(DF, aes(x=Pstatus, y=G123, col=Pstatus)) + geom_boxplot()
ggplot(DF, aes(x=famsize, y=G123, col=Pstatus)) + geom_boxplot()
# ggplot(DF, aes(x=as.factor(Medu), y=G123, col=as.factor(Medu))) + geom_boxplot()
# ggplot(DF, aes(x=as.factor(Fedu), y=G123, col=as.factor(Fedu))) + geom_boxplot()
ggplot(DF, aes(x=as.factor(parents_edu), y=G123, fill=as.factor(parents_edu))) + geom_boxplot(alpha=.5,show.legend = F) +
  labs(x="Parents Education levels", y="Math Grade", title="Distribution Math Grade in Parents Education levels") + theme_bw() + scale_fill_brewer(palette = "Blues")

ggplot(DF, aes(x=parents_edu_ct, y=G123, col=parents_edu_ct)) + geom_boxplot()

ggplot(DF, aes(x=Mjob, y=G123, col=Mjob)) + geom_boxplot()
ggplot(DF, aes(x=Fjob, y=G123, col=Fjob)) + geom_boxplot()
ggplot(DF, aes(x=guardian, y=G123, col=guardian)) + geom_boxplot()


##### address, traveltime, freetime, goout #####
# 도시 학생들일 수록 성적이 높음
# 등하교 시간이 길 수록 성적이 낮아지는 경향이 있음
# freetime이 많을 수록 성적이 높은 경향이 있음
# goout 시간이 많을 수록 성적이 낮아지는 경향이 있음 
ggplot(DF, aes(x=address, y=G123, col=address)) + geom_boxplot()

# CrossTable(DF$address, DF$traveltime, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# ggplot(DF, aes(x=address, y=G123, col=as.factor(traveltime))) + geom_boxplot()

ggplot(DF, aes(x=as.factor(traveltime), y=G123, col=as.factor(traveltime))) + geom_boxplot()
ggplot(DF, aes(x=address, y=G123, fill=as.factor(traveltime))) + geom_boxplot(alpha=.5) +
  labs(x="Address", y="Math Grade", fill="Travel Time", title="Distribution Math Grade in Address & Travel Time") + scale_fill_brewer(palette = "Blues") + theme_bw()

ggplot(DF, aes(x=as.factor(freetime), y=G123, col=as.factor(freetime))) + geom_boxplot()
ggplot(DF, aes(x=as.factor(goout), y=G123, col=as.factor(goout))) + geom_boxplot()

DF$freetime_goout <- DF$freetime - DF$goout
ggplot(DF, aes(x=as.factor(freetime_goout), y=G123, fill=as.factor(freetime_goout))) + geom_boxplot(show.legend = F, alpha=.5) + 
  labs(x="FreeTime & Going Out", y="Math Grade", title="Distribution Math Grade in Free Time & Going Out") +
 theme_bw() + scale_fill_brewer(palette = "Blues")



# ggplot(DF, aes(x=as.factor(goout), y=G123, col=as.factor(freetime))) + geom_boxplot()

##### reason, studytime, failures, absences #####
# 공부시간이 길수록 성적이 높아지는 경향이 있음
# 공부시간과 FAIL간 음의 상관 관계
ggplot(DF, aes(x=reason, y=G123, col=reason)) + geom_boxplot()
ggplot(DF, aes(x=as.factor(studytime), y=G123, col=as.factor(studytime))) + geom_boxplot()

ggplot(DF, aes(x=as.factor(failures), y=G123, col=as.factor(failures))) + geom_boxplot()
CrossTable(DF$studytime, DF$failures, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)

plot(DF[,c("absences","G123")])
DF$absences_check <- ifelse(DF$absences == 0,"no","yes")
ggplot(DF, aes(x=absences_check, y=G123)) + geom_boxplot()

prop.table(table(DF$reason, DF$studytime),1)



##### schoolsup, famsup, paid, activities, nursery, higher, internet, romantic, famrel #####
# 성적에 영향을 미치는 변수는 없는 것 같음...
prop.table(table(DF$schoolsup, DF$studytime),1)
prop.table(table(DF$schoolsup, DF$failures),1)
prop.table(table(DF$schoolsup, DF$absences_check),1)

prop.table(table(DF$romantic, DF$absences_check),1)



prop.table(table(DF$schoolsup))
prop.table(table(DF$higher))
prop.table(table(DF$internet))
prop.table(table(DF$paid))


prop.table(table(DF$famsup))
prop.table(table(DF$activities))
prop.table(table(DF$nursery))
prop.table(table(DF$romantic))

prop.table(table(DF$famrel))

ggplot(DF, aes(x=schoolsup, y=G123, col=schoolsup)) + geom_boxplot()
ggplot(DF, aes(x=famsup, y=G123, col=famsup)) + geom_boxplot()
ggplot(DF, aes(x=famsup, y=G123, col=parents_edu_ct)) + geom_boxplot()


ggplot(DF, aes(x=nursery, y=G123, col=nursery)) +geom_boxplot()
ggplot(DF, aes(x=paid, y=G123, col=parents_edu_ct)) + geom_boxplot()

 
ggplot(DF, aes(x=activities, y=G123, col=activities)) + geom_boxplot()
ggplot(DF, aes(x=romantic, y=G123, col=romantic)) + geom_boxplot()
ggplot(DF, aes(x=activities, y=G123, col=romantic)) + geom_boxplot()

ggplot(DF, aes(x=romantic, y=G123, col=schoolsup)) + geom_boxplot()
ggplot(DF, aes(x=romantic, y=G123, col=absences_check)) + geom_boxplot()
ggplot(DF, aes(x=activities, y=G123, col=as.factor(studytime))) + geom_boxplot()


ggplot(DF, aes(x=as.factor(famrel), y=G123, col=as.factor(famrel))) + geom_boxplot()




DF <- transform(DF, schoolsup_num = ifelse(schoolsup == "no",0,1),
                    famsup_num = ifelse(famsup == "no",0,1),
                    nursery_num = ifelse(nursery == "no",0,1),
                    paid_num = ifelse(paid == "no",0,1))
DF <- transform(DF, support = schoolsup_num + famsup_num + nursery_num + paid_num)

ggplot(DF, aes(x=as.factor(support), y=G123, col=as.factor(support))) + geom_boxplot()



prop.table(table(DF$studytime, DF$romantic),1)
table(DF$romantic, DF$studytime)


##### Dalc, Walc, alcohol #####
DF$Dalc_Walc <- DF$Dalc + DF$Walc

prop.table(table(DF$Dalc))
prop.table(table(DF$Walc))

ggplot(DF, aes(x=as.factor(Dalc), y=G123, col=as.factor(Dalc))) + geom_boxplot()
ggplot(DF, aes(x=as.factor(Walc), y=G123, col=as.factor(Walc))) + geom_boxplot()
ggplot(DF, aes(x=alcohol, y=G123, col=alcohol)) + geom_boxplot()

ggplot(DF, aes(x=alcohol, y=G123, col=alcohol)) + geom_boxplot()
ggplot(DF, aes(x=G123)) + geom_histogram()+ facet_grid(as.factor(Dalc_Walc) ~ .)



ggplot(DF, aes(x=romantic, y=G123, col=absences_check)) + geom_boxplot()
ggplot(DF, aes(x=absences_check, y=G123))+geom_boxplot()

ggplot(DF, aes(x=as.factor(studytime), y=G123, fill=absences_check)) + geom_boxplot(alpha=.8) +
  labs(x="Study Time", y="Math Grade", fill="Absences Check", title="Distribution Math Grade in Study Time & Absence") +
  scale_fill_brewer(palette = "Blues") + theme_bw()


ggplot(DF, aes(x=as.factor(Dalc_Walc), y=G123, fill=romantic)) + geom_boxplot()


########## Data Modeling ##########


set.seed(99999)
number <- sample(1:nrow(DF), nrow(DF) * 0.7)

DF$parents_edu_ct <- factor(DF$parents_edu_ct, levels=c("Middle","High","Low"))
DF$address <- factor(DF$address, levels=c("U","R"))
DF$sex <- factor(DF$sex, levels=c("M","F"))
DF$absences_check <- factor(DF$absences_check, levels=c("yes","no"))


DF_train <- DF[number,]
DF_test <- DF[-number,]




fit_1 <- lm(G123 ~ sex+address+parents_edu + freetime_goout+Dalc_Walc + romantic + absences_check + studytime,
               data=DF_train)
summary(fit_1)
par(mfrow=c(2,2))
plot(fit_1)
step(fit_1)


# fit_2 <- glm(G123 ~ sex + address + goout + studytime + failures + parents_edu_ct ,
#              data=DF_train,
#              family=gaussian)
# summary(fit_2)
# plot(fit_2)


# fit_3 <- glm(G123 ~ sex + address + goout + studytime + failures ,
#              data=DF_train,
#              family=gaussian)
# summary(fit_3)
# plot(fit_3)

##### Verification #####
lm_mod_test <- function(y, fit.y){
  MAE <- mean(abs(y-fit.y))
  RMSE <- sqrt(mean((y-fit.y)^2))
  MAPE <- mean(abs((y-fit.y)/y)) * 100
  res <- list(MAE = MAE, RMSE = RMSE, MAPE = MAPE)
  return(res)
}


DF_test$fit_1 <- predict(fit_1, DF_test)
lm_mod_test(DF_test$G123, DF_test$fit_1)
plot(DF_test[,c("G123","fit_1")], cex=0.8, xlab="Math Grade", ylab="Predict Value", main="Regression Model Verification in Test Data", col="Blue", pch=19)
abline(lm(fit_1 ~ G123, data=DF_test), col="red")

cor(DF_test[,c("G123","fit_1")])

table(DF_test$G123)
table(DF_test$fit_1)


fit <- lm(G123 ~ sex+address+parents_edu + freetime_goout+Dalc_Walc + romantic + absences_check + studytime,
            data=DF)
summary(fit)
par(mfrow=c(2,2))
plot(fit_1)
step(fit_1)

DF$fit <- predict(fit, DF)
lm_mod_test(DF$G123, DF$fit)
plot(DF[,c("G123","fit")], xlim=c(0,60), ylim=c(0,60), cex=0.8)
cor(DF[,c("G123","fit")])

# DF_test$fit_3 <- predict(fit_3, DF_test)
# lm_mod_test(DF_test$G123, DF_test$fit_3)

