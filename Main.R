rm(list = ls())

library(glmnet)
library(ROCR)
library(MASS)
library(ggplot2)
library(pheatmap)
library(randomForest)
library(dplyr)
library(caTools)
library(caret)
library(ROCR)
library(reshape)

#"Retrospective Study"
#Mention the transformation issues (i.e we removed the draw observations) we did in the data. Perhaps A Wins or doesn't win. MAybe include draws.
#COnsider LDA for all 3
#Assumptions:
#Multivariate normal distribution for entire set of variables
#Univariate normal distribution on response
#Linear relationships between scores on Y and scores on X for all variables
#Uniform error variances for response (Y) across all values of X

##main file for boxing casestudy
setwd("~/GitHub/6372BoxingProject")

#read in data - may need to remove stance na values
df <- read.csv("data.csv", na.strings = c("", "NA"))

#####################filter out bad & unnecessary data ########################################################
#eliminate draw outcomes
df <- filter(df, result != "draw")

#eliminate draw history & judge data since won't function as a predictor
df <- df[, -c(15, 16, 20:26)]

#filter out missing data
df <- df[complete.cases(df[, c(1:6, 9, 10)]), ]

#filter out weird age values
df <- subset(df, subset = (df$age_A >= 16 & df$age_A <= 60))
df <- subset(df, subset = (df$age_B >= 16 & df$age_B <= 60))

#filter out weird reach values
df <- subset(df, subset = (df$reach_A >= 100 & df$reach_A <= 300))
df <- subset(df, subset = (df$reach_B >= 100 & df$reach_B <= 300))

#assign as factors
df$result <- factor(df$result)

##############################potential modeling factors for interaction effects ########################

#create age delta
df$AdvAgeA <- df$age_A - df$age_B

#create height delta
df$AdvHeightA <- df$height_A - df$height_B

#create reach delta
df$AdvReachA <- df$reach_A - df$reach_B

#create weight delta
df$AdvWgtA <- df$weight_A - df$weight_B

#over 35 age binary ###35 is the age limit for amateur boxing, some have argued limits should exist for pro's###
df$Over35AgeA <- ifelse(df$age_A >= 35, 1, 0)
df$Over35AgeB <- ifelse(df$age_B >= 35, 1, 0)

#over 15 lbs weight delta?
df$Over15lbA <- ifelse(df$AdvWgtA >= 15, 1, 0)
df$Over15lbB <- ifelse(df$AdvWgtA <= -15, 1, 0)

#win % for boxers
df$WinPA <- df$won_A / (df$won_A + df$lost_A)
df$WinPB <- df$won_B / (df$won_B + df$lost_B)

#KO per fight
df$KoAPer <- df$kos_A / (df$won_A + df$lost_A)
df$KoBPer <- df$kos_B / (df$won_B + df$lost_B)

#add binaryresult value: 0 means that A won. 1 means that B won
df$binaryresult <- ifelse(df$result == "win_A", 0, 1) #doublecheck that binary result is correct #unique(df$binaryresult)


#Recategorize the stances that are NA.
df$stance_A <- as.character(df$stance_A)
df$stance_B <- as.character(df$stance_B)
df$stance_A[is.na(df$stance_A)] <- "Unknown"
df$stance_B[is.na(df$stance_B)] <- "Unknown"


#Check to make sure NA stances are recategorized #unique(df$stance_A) #unique(df$stance_B)

#Notcied that the stance is same for A and B in all observations to keep only A and re-label it
df$stance<-df$stance_A
df<-df[c(-7,-8)]
#unique(df$stance)

#Remove any reminaing NA observations
df <- na.omit(df)
#nrow(df) result is 7135 observations

############################EDA#################################################
#Need to add histograms, box Plots, corr and cov matrices,

#DD: Clearly our data is overepresenting the scenario when a wins vs when b wins.
hist(df$binaryresult)
  #Split df by category
  df_A_wins <- filter(df, binaryresult == 0)
  df_B_wins <- filter(df, binaryresult == 1)
  #nrow(df_A_wins) shows 6094 obs where a won
  #nrow(df_B_wins) shows 1041 obs where b won

set.seed(42)
  #Randomly sample approx 20% of useable observations per category and combine back together
#dftrain <- rbind(sample_n(df_A_wins, 1040), sample_n(df_B_wins, 1040)) 

#creates randomsample
df_A_wins<-sample_n(df_A_wins, 1000)
df_B_wins<-sample_n(df_B_wins, 1000)

#creates split @80%
SplitAWin<-sample.split(df_A_wins$binaryresult,SplitRatio = 0.8)
SplitBWin<-sample.split(df_B_wins$binaryresult,SplitRatio = 0.8)



#creates training and test dataset
trainingA<-subset(df_A_wins,SplitAWin==TRUE)
trainingB<-subset(df_B_wins,SplitBWin==TRUE)
testA<-subset(df_A_wins,SplitAWin==FALSE)
testB<-subset(df_B_wins,SplitBWin==FALSE)

#creates train test
dftrain<-rbind(trainingA,trainingB)
dftest<-rbind(testA,testB)

#write.csv(dftrain, file = "dftrain.csv")
hist(dftrain$binaryresult)
hist(dftest$binaryresult)


############################PCA#################################################

#Choose continuous variables as x axis
boxing.x <-
  subset(
    dftrain,
    select = c(
      AdvAgeA,     
      AdvHeightA,
      AdvReachA,
      AdvWgtA,
      WinPA,
      WinPB,
      KoAPer,
      KoBPer 
    )
  )

##Choose binary result as y axis
boxing.y<-subset(dftrain, select = c(binaryresult))
boxing.y <- as.factor(as.character(boxing.y))

#Scale x variables and run pcomp
pcresults <- prcomp(boxing.x, scale = TRUE)

#BiPlot shows that reach, weight and height aren't adding quite as much  value as others
biplot(pcresults, scale = 0)

#Put PC Scores into dataframe
pcscores <- as.data.frame(pcresults$x)

#Combine the pc scores with  variable with the df
pcscores$binaryresult<-boxing.y
pcscores<-data.frame(pcscores)

pceigen<-(pcresults$sdev)^2
pcprop<-pceigen/sum(pceigen)
pccumprop<-cumsum(pcprop)
plot(pcprop,type="l",main="Scree Plot",ylim=c(0,1),xlab="PC #",ylab="Proportion of Variation")
lines(pccumprop,lty=3)



#Combine everything into dftrain and ggplot
dftrain <- cbind(dftrain, pcresults$x) 

#Need to think about this.
ggplot(dftrain, aes(PC1, PC2, col = result, fill = result)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
ggplot(dftrain, aes(PC2, PC3, col = result, fill = result)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")
ggplot(dftrain, aes(PC1, PC3, col = result, fill = result)) +
  stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) +
  geom_point(shape = 21, col = "black")







####begin random forest work#####

#drop pca & unnecessary data
dftrain<-dftrain[,c(-15,-30:-37)]
dftrain$binaryresult<-as.factor(dftrain$binaryresult)
dftrain$stance<-as.factor(dftrain$stance)

dftest<-dftest[,-15]
dftest$binaryresult<-as.factor(dftest$binaryresult)
dftest$stance<-as.factor(dftest$stance)


str(dftrain)
summary(dftrain)


colSums(is.na(dftrain)|dftrain == '')

set.seed(42)
#runs random forest
rf.box <- randomForest(x = dftrain[,-27],y = dftrain[,27],mtry=4,ntree = 400,importance = T)
rf.box
#performs OOB estimation
bestmtry<-tuneRF(x = dftrain[,-27],y=dftrain[,27],stepFactor=1.5, ntree=400)
print(bestmtry)

#use testdata
y_pred <- predict(rf.box, newdata = dftest[-27])

#confusion matrix
cm <- table(dftest[, 27], y_pred)
cm
#RF Accuracy
(138+141)/(400)
#RF Sensitivity
138/(59+138)
#RF Specificity
141/(141+62)

#creates table for randome forest variables
rf.imp<-varImp(rf.box)
rf.imp$variable<-row.names(rf.imp)
rf.imp<-rf.imp[,-1]
colnames(rf.imp)<-c("Score","Variable")
rf.imp$Variable<-as.factor(rf.imp$Variable)


#creates plot of variable importance
ggplot(rf.imp, aes(y=Score, x=Variable)) +
  geom_bar( stat="identity", show.legend = F) + ggtitle("Random Forest variable importance") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#barplot()

###end

#Go get the ROC
rf.pred<-predict(rf.box,newdata=dftest[,-27],type="prob")
pred <- prediction(rf.pred[,2], dftest$binaryresult)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set RF boxing - mtry=4")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))



############################begin LDA#################################
#creates data for LDA removing correlated/created variables and class variable of stance
dfldatrain<-dftrain
dfldatrain<-dfldatrain[,c(-15:-26,-28)]
dfldatest<-dftest
dfldatest<-dfldatest[,c(-15:-26,-28)]

#fits lda
lda.fit<-lda(binaryresult ~ ., data=dfldatrain)
lda.fit

#predicts for lda
lda.pred <- predict(lda.fit, dfldatest)
names(lda.pred)

#makes confusion matrix
table(lda.pred$class, dfldatest$binaryresult)

#create lda roc
lpred <- prediction(lda.pred$posterior[,2], dfldatest$binaryresult)
roc.perf.lda = performance(lpred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(lpred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf.lda,main="AUC of Test set LDA boxing")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#LDA Accuracy
(114+137)/(400)
#LDA Sensitivity
141/(59+141)
#LDA Specificity
137/(137+59)








