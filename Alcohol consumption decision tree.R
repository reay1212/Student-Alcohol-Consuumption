
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
options(digits=2, scipen=10)

library(RColorBrewer) ; library(gmodels) ; library(reshape2)
library(rpart) ; library(rpart.plot) ; library(rattle) ; library(e1071) ; library(caret)

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

##### 학교별 알코올 소비 #####
# 주중 소비는 큰차이 없으나, MS 학생들이 주말 알코올 소비가 높음 
# GP 학생들의 비중이 88%로 School 변수 제외

# prop.table(table(DF$school))
# prop.table(table(DF$school, DF$Dalc),1) ; prop.table(table(DF$school, DF$Walc),1)
# CrossTable(DF$school, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# CrossTable(DF$school, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)


##### 성별 & 나이 알코올 소비 ##### 
# 여학생들보다 남학생들의 알코올 소비가 높음 , 나이가 증가하면서 알코올 소비가 높음
CrossTable(DF$sex, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(DF$sex, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(DF$age_new, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
CrossTable(DF$age_new, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)

prop.table(table(DF$alcohol, DF$sex),1)
prop.table(table(DF$alcohol, DF$age_new),1)

data.frame(prop.table(table(DF$alcohol, DF$sex),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity", position="dodge") +
  labs(x="Alcohol COnsumption", y="percent", fill="Sex", title="Percentage Sex of Alcohol Consumption") + theme_bw() + scale_fill_brewer(palette = "Blues")


data.frame(prop.table(table(DF$alcohol, DF$age_new)),1) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity", position="dodge") +
  labs(x="Alcohol Consumption", y="Percent", fill="Age", title="Percentage Age of Alcohol Consumption") +
  scale_fill_brewer(palette = "Blues") + theme_bw()





##### 거주지 & 등교시간 알코올 소비 #####
# 시골지역 이고 거리가 등교시간이 길 수록 알코올 소비가 높은 경향이 있음
# CrossTable(DF$address, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# CrossTable(DF$address, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# prop.table(table(DF$address, DF$traveltime),1)
# CrossTable(DF$traveltime, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)
# CrossTable(DF$traveltime, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.t=FALSE, prop.chisq=FALSE, chisq=TRUE)

prop.table(table(DF$address, DF$alcohol),1)
prop.table(table(DF$alcohol, DF$traveltime),1)


data.frame(prop.table(table(DF$address, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") + scale_fill_brewer(palette = "Blues") + 
  geom_text(aes(label=paste(round(Freq*100,1), "%", sep="")), position = position_stack(vjust=0.5)) +
  labs(title="Percentage Address of Alcohol Consumption", x="Address",y="Percent", fill="Alcohol") +
  theme_bw()

##### famsize, Pstatus, Medu, Fedu, Mjob, Fjob, guardian, famsup, famrel  별 알코올 소비 ##### 
# 부모님의 학력이 높을 수록 교육적으로 지원 많이 해줌, 부모님의 학력이 높거나 낮을 수록 알코올 소비가 높은 경향이 있음
# 가족 수가 적을 수록 주중 알코올 소비가 높음
DF <- transform(DF, parents_edu = Medu + Fedu)
DF <- transform(DF, parents_edu_ct = ifelse(parents_edu <= 3 , "Low",ifelse(parents_edu <= 6,"Middle","High")))
DF$parents_edu_ct <- factor(DF$parents_edu_ct, levels=c("Low","Middle","High"))

# table(DF$parents_edu)
# table(DF$parents_edu_ct)
# 
# table(DF$Pstatus)
# CrossTable(DF$Pstatus, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# CrossTable(DF$Pstatus, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# 
# table(DF$famrel)
# CrossTable(DF$famrel, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# CrossTable(DF$famrel, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# 
# table(DF$guardian)
# CrossTable(DF$guardian, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# CrossTable(DF$guardian, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

CrossTable(DF$famsup, DF$parents_edu_ct, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

CrossTable(DF$parents_edu_ct, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
CrossTable(DF$parents_edu_ct, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

CrossTable(DF$famsize, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
CrossTable(DF$famsize, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)


prop.table(table(DF$famsup, DF$alcohol),1)
prop.table(table(DF$parents_edu_ct, DF$alcohol),1)
prop.table(table(DF$famsize, DF$alcohol),1)

data.frame(prop.table(table(DF$famsize, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
  geom_text(aes(label=paste(round(Freq*100,1), "%", sep="")), position = position_stack(vjust=0.5)) +
  scale_fill_brewer(palette = "Blues") + theme_bw() +
  labs(x="Family Size", y="Percent", title="Percentage Family Size of Alcohol Consumption", fill="Alcohol")


##### freetime, goout, romantic 별 알코올 소비 #####
# 주중에는 별다른 차이가 없으나 freetime이 높을 수록 주말 알코올 소비량이 높아지는 경향이 있음
# gooout 지수가 높을수록 주중 및 주말 알코올 소비량이 높아지는 경향이 있음 
table(DF$freetime, DF$goout)
table(DF$goout)
table(DF$romantic)

CrossTable(DF$freetime, DF$goout, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
View(dcast(DF, freetime + goout ~ Dalc, value.var="Dalc", length))

CrossTable(DF$freetime, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
CrossTable(DF$freetime, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

CrossTable(DF$goout, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
CrossTable(DF$goout, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

# CrossTable(DF$romantic, DF$Dalc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)
# CrossTable(DF$romantic, DF$Walc, prop.r=TRUE, prop.c=FALSE, prop.chisq=FALSE, prop.t=FALSE, chisq=TRUE)

prop.table(table(DF$freetime, DF$alcohol),1)
prop.table(table(DF$goout, DF$alcohol),1)

prop.table(table(DF$romantic, DF$alcohol),1)
prop.table(table(DF$romantic, DF$age_new),1)
DF$age_new_2 <- ifelse(DF$age_new %in% c("15세","16세"),"16<=","17>=")
DF$romant_age <- paste(DF$age_new_2, DF$romantic, sep=" ")
prop.table(table(DF$romantic, DF$age_new_2),1)
data.frame(prop.table(table(DF$romant_age, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity", position="dodge")


data.frame(prop.table(table(DF$goout, DF$alcohol),1)) %>%
  ggplot(aes(x=as.factor(Var1), y=Var2)) + 
  geom_point(aes(size=Freq),shape=21, fill="Light Sky Blue", alpha=0.5) +
  geom_text(aes(x=Var1, y=Var2, label=paste(round(Freq*100,1), "%", sep="")), size=4) +
  scale_size_area(max_size=40) + theme_bw() + 
  labs(x="Going out with friends", y="Alcohol", size="Percent", title="Percentage Going out of Alcohol Consumption") 

data.frame(prop.table(table(DF$freetime, DF$alcohol),1)) %>%
  ggplot(aes(x=as.factor(Var1), y=Var2)) + 
  geom_point(aes(size=Freq),shape=21, fill="Deep Sky Blue", alpha=0.5) +
  geom_text(aes(x=Var1, y=Var2, label=paste(round(Freq*100,1), "%", sep="")), size=4) +
  scale_size_area(max_size=40) + theme_bw() + 
  labs(x="Free time after school", y="Alcohol", size="Percent", title="Percentage Free time of Alcohol Consumption") 




##### reason, studytime, schoolup, paid, activities, nursery, higher, internet ###

##
# nursery, higher, activities 변수 사용 안함
# 학교를 선택한 목적에 따라 알코올 소비에 차이가 있음
# study 시간이 낮을 수록 알오올 소비량이 높음 
prop.table(table(DF$reason, DF$alcohol),1)
prop.table(table(DF$studytime, DF$alcohol),1)
prop.table(table(DF$schoolsup, DF$alcohol),1)
prop.table(table(DF$paid, DF$alcohol),1)
prop.table(table(DF$activities, DF$alcohol),1)
prop.table(table(DF$nursery, DF$alcohol),1)
prop.table(table(DF$higher, DF$alcohol),1)
prop.table(table(DF$internet, DF$alcohol),1)

DF <- transform(DF, schoolsup = ifelse(schoolsup == "no",0,1),
                    paid = ifelse(paid == "no",0,1))
DF <- transform(DF, extra_sup = schoolsup + paid)

DF <- transform(DF, reason_new = ifelse(reason %in% c("course","reputation"), "Purpose",
                                        ifelse(reason == "home", "region","other")))

prop.table(table(DF$extra_sup, DF$alcohol),1)
prop.table(table(DF$reason_new, DF$alcohol),1)



data.frame(prop.table(table(DF$studytime, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") + 
  geom_text(aes(label=paste(round(Freq*100,1), "%", sep="")), position=position_stack(vjust=0.5)) + 
  labs(x="Study Time", y="Percent", fill="Alcohol", title="Percentage Study Time of Alcohol Consumption") + scale_fill_brewer(palette = "Blues") + theme_bw()

data.frame(prop.table(table(DF$reason_new, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity")

##### health #####
# 건강이 좋을 수록 알코올 소비량이 높아지는 경향이 있음 
prop.table(table(DF$health, DF$alcohol),1)

data.frame(prop.table(table(DF$health, DF$alcohol),1)) %>%
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + geom_bar(stat="identity") +
  geom_text(aes(label=paste(round(Freq*100,1),"%",sep="")), position=position_stack(vjust=0.5)) +
  labs(x="Status of Health", y="Percent", fill="Alcohol", title="Percentage Status of Health of Alcohol Consumption") +
  scale_fill_brewer(palette = "Blues") + theme_bw()

######### Modeling ######### 
DF$age_new <- factor(DF$age_new, levels=c("15세","16세","17세","18세","19세이상"))
set.seed(99999)
number <- sample(1:nrow(DF), nrow(DF) * 0.7)

DF_train <- DF[number,]
DF_test <- DF[-number,]

##### #####
# fit_1 <- rpart(alcohol ~ sex + age_new + address + traveltime + parents_edu_ct + famsup + famsize + freetime + goout + reason + studytime + extra_sup + health,
#                data=DF_train, method="class")
# summary(fit_1)
# fancyRpartPlot(fit_1, tweak=1, space=-0.5, gap=-0.5)


# fit_2 <- rpart(alcohol ~ sex + age_new + address + parents_edu_ct + famsize + freetime + goout + reason  ,
#                data=DF_train,
#                control=rpart.control(minsplit=2,minbucket=1))
# summary(fit_2)
# printcp(fit_2)
# fancyRpartPlot(fit_2, space=-0.5, gap=-0.5)

##### 최종 모델 선정 #####
fit_3 <- rpart(alcohol ~ sex + age + address + famsize + freetime + goout + reason  ,
               data=DF_train,
               control = rpart.control(minsplit=2, maxdepth = 4))
fancyRpartPlot(fit_3, space=0, gap=0)
DF_train$fit <- predict(fit_3,DF_train, type="class")
##### 모델 Test #####

DF_test$fit_3 <-predict(fit_3, DF_test, type="class")
confusionMatrix(DF_test$fit_3, DF_test$alcohol)


fit_final <- rpart(alcohol ~ sex + age +address + famsize + freetime + goout + reason,
                   data=DF,
                   control = rpart.control(minsplit=2, maxdepth=4))
fancyRpartPlot(fit_final, space=0, gap=0, palettes = "Paired")
fancyRpartPlot(fit_final, palettes = "Paired")
plot(fit_final)

##### Bagging #####
test <- data.frame(
  pred1=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  pred2=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  pred3=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  pred4=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  pred5=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  pred6=predict(rpart(alcohol~sex+age+address+famsize+freetime+goout+reason,data=DF[sample(1:nrow(DF),replace=TRUE),],control=rpart.control(minsplit=2, maxdepth=4)),DF, type="class"),
  alcohol=DF$alcohol)


funcResultValue <- function(x){
  result <- NULL
  for(i in 1:nrow(x)){
    xtab <- table(t(x[i,]))
    rvalue <- names(sort(xtab, decreasing=T)[1])
    result <- c(result, rvalue)
  }
  return(result)
}

funcResultValue(test[,1:length(test)-1])
test$result <- funcResultValue(test[,1:length(test)-1])
str(test)
test$result <- factor(test$result, levels=c("Low","Middle","High","Very High"))

confusionMatrix(test$result, test$alcohol)

# 정확도 64%...





########## Profiling ##########
profiling <- DF_train[DF_train$fit %in% c("High","VeryHigh"),]
names(profiling)
table(profiling$address, profiling$famsize)
table(profiling$activities)
table(profiling$romantic, profiling$address)
table(profiling$famsize)
table(profiling$internet)
