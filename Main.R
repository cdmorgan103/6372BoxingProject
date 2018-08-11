rm(list = ls())

library(glmnet)
library(ROCR)
library(MASS)
library(ggplot2)
library(pheatmap)
library(randomForest)
library(dplyr)

#"Retrospective Study"
#Mention the transformation issues (i.e we removed the draw observations) we did in the data. Perhaps A Wins or doesn't win. MAybe include draws.
#COnsider LDA for all 3
#Add Cross Validation
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

  #Randomly sample approx 20% of useable observations per category and combine back together
dftrain <- rbind(sample_n(df_A_wins, 200), sample_n(df_B_wins, 200)) 
#write.csv(dftrain, file = "dftrain.csv")
hist(dftrain$binaryresult)

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







#################################Snippets from Dr Turner##############################################
##glmnet
#
#
# #Loadings for interpretation
# pc.result$rotation
#
# #Scree plot
# pc.eigen<-(pc.result$sdev)^2
# pc.prop<-pc.eigen/sum(pc.eigen)
# pc.cumprop<-cumsum(pc.prop)
# plot(1:9,pc.prop,type="l",main="Scree Plot",ylim=c(0,1),xlab="PC #",ylab="Proportion of Variation")
# lines(1:9,pc.cumprop,lty=3)
#
# #Use ggplot2 to plot the first few pc's
# ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
#   geom_point(aes(col=Censor), size=1)+
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   ggtitle("PCA plot of Gene Expression Data")
#
# ggplot(data = pc.scores, aes(x = PC1, y = PC3)) +
#   geom_point(aes(col=Censor), size=1)+
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   ggtitle("PCA plot of Gene Expression Data")
#
#
# ggplot(data = pc.scores, aes(x = PC2, y = PC3)) +
#   geom_point(aes(col=Censor), size=1)+
#   geom_hline(yintercept = 0, colour = "gray65") +
#   geom_vline(xintercept = 0, colour = "gray65") +
#   ggtitle("PCA plot of Gene Expression Data")
#
# #Lets look at a heatmap using hierarchical clustering to see if the
# #response naturually clusters out using the predictors
#
# #Transposting the predictor matrix and giving the response categories its
# #row names.
#
# library(RColorBrewer)
# x<-t(dat.train.x)
# colnames(x)<-dat.train.y
# pheatmap(x,annotation_col=data.frame(Cancer=dat.train.y),scale="row",legend=T,color=colorRampPalette(c("blue","white", "red"), space = "rgb")(100))
#
#
# #A good exercise would be to go in and play around with the
# #data but maybe add a few "fake" predictors that seperate out
# #the two response categories very well.  From there make the plots
# #again and see how they reflect that truth.
#
# #The heatmap could be done using the PC's as well.
#
#
#
#
#
#
# #glmnet requires a matrix
# dat.train.x <- as.matrix(dat.train.x)
# library(glmnet)
# cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
# plot(cvfit)
# coef(cvfit, s = "lambda.min")
#
# #Get training set predictions...We know they are biased but lets create ROC's.
# #These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
# fit.pred <- predict(cvfit, newx = dat.train.x, type = "response")
#
# #Compare the prediction to the real outcome
# head(fit.pred)
# head(dat.train.y)
#
# #Create ROC curves
# pred <- prediction(fit.pred[,1], dat.train.y)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
#
# #Plot ROC
# plot(roc.perf)
# abline(a=0, b= 1) #Ref line indicating poor performance
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Get Validation Set I
# dat.val1 <- dat[which(dat$Set == "Validation I"),]
# dat.val1.x <- dat.val1[,c(6:ncol(dat))]
# dat.val1.x <- as.matrix(dat.val1.x)
#
# dat.val1.y <- dat.val1$Censor
# dat.val1.y <- as.factor(as.character(dat.val1.y))
#
#
# #Run model from training set on valid set I
# fit.pred1 <- predict(cvfit, newx = dat.val1.x, type = "response")
#
# #ROC curves
# pred1 <- prediction(fit.pred1[,1], dat.val1.y)
# roc.perf1 = performance(pred1, measure = "tpr", x.measure = "fpr")
# auc.val1 <- performance(pred1, measure = "auc")
# auc.val1 <- auc.val1@y.values
# plot(roc.perf1)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.val1[[1]],3), sep = ""))
#
#
# #Rinse and Repeat for valid set II and III
# dat.val2 <- dat[which(dat$Set == "Validation II"),]
# dat.val2.x <- dat.val2[,c(6:ncol(dat))]
# dat.val2.x <- as.matrix(dat.val2.x)
#
# dat.val2.y <- dat.val2$Censor
# dat.val2.y <- as.factor(as.character(dat.val2.y))
#
# fit.pred2 <- predict(cvfit, newx = dat.val2.x, type = "response")
#
# pred2 <- prediction(fit.pred2[,1], dat.val2.y)
# roc.perf2 = performance(pred2, measure = "tpr", x.measure = "fpr")
# auc.val2 <- performance(pred2, measure = "auc")
# auc.val2 <- auc.val2@y.values
# plot(roc.perf2)
# abline(a=0, b= 1)
# text(x = .42, y = .6,paste("AUC = ", round(auc.val2[[1]],3), sep = ""))
#
# #Valid set III
# dat.val3 <- dat[which(dat$Set == "Validation III"),]
# dat.val3.x <- dat.val3[,c(6:ncol(dat))]
# dat.val3.x <- as.matrix(dat.val3.x)
#
# dat.val3.y <- dat.val3$Censor
# dat.val3.y <- as.factor(as.character(dat.val3.y))
#
#
# fit.pred3 <- predict(cvfit, newx = dat.val3.x, type = "response")
#
# pred3 <- prediction(fit.pred3[,1], dat.val3.y)
# roc.perf3 = performance(pred3, measure = "tpr", x.measure = "fpr")
# auc.val3 <- performance(pred3, measure = "auc")
# auc.val3 <- auc.val3@y.values
# plot(roc.perf3)
# abline(a=0, b= 1)
# text(x = .4, y = .6,paste("AUC = ", round(auc.val3[[1]],3), sep = ""))
#
#
# ## LDA
#
# #Training Set
# dat.train <- dat[which(dat$Set == "Training"),]
# dat.train.x <- dat.train[,6:ncol(dat)]
#
# dat.train.y <- dat.train$Censor
# dat.train.y <- as.factor(as.character(dat.train.y))
#
# fit.lda <- lda(dat.train.y ~ ., data = dat.train.x)
# pred.lda <- predict(fit.lda, newdata = dat.train.x)
#
# preds <- pred.lda$posterior
# preds <- as.data.frame(preds)
#
# pred <- prediction(preds[,2],dat.train.y)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Valid set I
# dat.val1 <- dat[which(dat$Set == "Validation I"),]
# dat.val1.x <- dat.val1[,c(6:ncol(dat))]
#
# dat.val1.y <- dat.val1$Censor
# dat.val1.y <- as.factor(as.character(dat.val1.y))
#
#
# pred.lda1 <- predict(fit.lda, newdata = dat.val1.x)
#
# preds1 <- pred.lda1$posterior
# preds1 <- as.data.frame(preds1)
#
# pred1 <- prediction(preds1[,2],dat.val1.y)
# roc.perf = performance(pred1, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred1, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Valid set II
# dat.val2 <- dat[which(dat$Set == "Validation II"),]
# dat.val2.x <- dat.val2[,c(5:ncol(dat))]
#
# dat.val2.y <- dat.val2$Censor
# dat.val2.y <- as.factor(as.character(dat.val2.y))
#
# pred.lda2 <- predict(fit.lda, newdata = dat.val2.x)
#
# preds2 <- pred.lda2$posterior
# preds2 <- as.data.frame(preds2)
#
# pred2 <- prediction(preds2[,2],dat.val2.y)
# roc.perf = performance(pred2, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred2, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Valid set III
# dat.val3 <- dat[which(dat$Set == "Validation III"),]
# dat.val3.x <- dat.val3[,c(5:ncol(dat))]
#
# dat.val3.y <- dat.val3$Censor
# dat.val3.y <- as.factor(as.character(dat.val3.y))
#
#
# pred.lda3 <- predict(fit.lda, newdata = dat.val3.x)
#
# preds3 <- pred.lda3$posterior
# preds3 <- as.data.frame(preds3)
#
# pred3 <- prediction(preds3[,2],dat.val3.y)
# roc.perf = performance(pred3, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred3, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
#
# #One more time with a Random Forest
# #I'm removing the fluff variables just to make coding easier.
# dat.train.rf <- dat[which(dat$Set == "Training"),-(1:4)]
# dat.train.rf$Censor<-factor(dat.train.rf$Censor)
#
# train.rf<-randomForest(Censor~.,data=dat.train.rf,mtry=4,ntree=500,importance=T)
# fit.pred<-predict(train.rf,newdata=dat.train.rf,type="prob")
#
#
#
# pred <- prediction(fit.pred[,2], dat.train.rf$Censor)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Predict Validation Set I
# dat.val1.rf <- dat.val1[,-(1:4)]
# #dat.val1.rf$Censor<-factor(dat.train.rf$Censor)
#
# pred.val1<-predict(train.rf,newdata=dat.val1.rf,type="prob")
#
#
#
# pred <- prediction(pred.val1[,2], dat.val1.rf$Censor)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Predict Validation Set 2
# dat.val2.rf <- dat.val2[,-(1:4)]
# #dat.val1.rf$Censor<-factor(dat.train.rf$Censor)
#
# pred.val2<-predict(train.rf,newdata=dat.val2.rf,type="prob")
#
#
#
# pred <- prediction(pred.val2[,2], dat.val2.rf$Censor)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
# #Predict Validation Set 3
# dat.val3.rf <- dat.val3[,-(1:4)]
# #dat.val1.rf$Censor<-factor(dat.train.rf$Censor)
#
# pred.val3<-predict(train.rf,newdata=dat.val3.rf,type="prob")
#
#
#
# pred <- prediction(pred.val3[,2], dat.val3.rf$Censor)
# roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
# auc.train <- performance(pred, measure = "auc")
# auc.train <- auc.train@y.values
# plot(roc.perf)
# abline(a=0, b= 1)
# text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))
#
#
#
#
#
#
#
#
#
# #Suppose we did not have all of these validation data sets.  We can assess how well our model building process works through Cross validation.
# #The idea is that we can get an idea of how well the approach is going to perform on new data not yet collected.
# #We will use AUC as the performance matrix.  Note we only consider the lasso logistic here, but in practice we could
# #run many different models to compare directly.
#
# nloops<-50   #number of CV loops
# ntrains<-dim(dat.train.x)[1]  #No. of samples in training data set
# cv.aucs<-c() #initializing a vector to store the auc results for each CV run
#
# for (i in 1:nloops){
#   index<-sample(1:ntrains,60)
#   cvtrain.x<-as.matrix(dat.train.x[index,])
#   cvtest.x<-as.matrix(dat.train.x[-index,])
#   cvtrain.y<-dat.train.y[index]
#   cvtest.y<-dat.train.y[-index]
#
#   cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class")
#   fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
#   pred <- prediction(fit.pred[,1], cvtest.y)
#   roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#   auc.train <- performance(pred, measure = "auc")
#   auc.train <- auc.train@y.values
#
#   cv.aucs[i]<-auc.train[[1]]
# }
#
# hist(cv.aucs)
# summary(cv.aucs)
#
#
#
#
# #Doing the same procedure for random allocation of response values.
# #Good practice when number of yes/no is not balanced.
#
#
# nloops<-50   #number of CV loops
# ntrains<-dim(dat.train.x)[1]  #No. of samples in training data set
# cv.aucs<-c()
# dat.train.yshuf<-dat.train.y[sample(1:length(dat.train.y))]
#
# for (i in 1:nloops){
#   index<-sample(1:ntrains,60)
#   cvtrain.x<-as.matrix(dat.train.x[index,])
#   cvtest.x<-as.matrix(dat.train.x[-index,])
#   cvtrain.y<-dat.train.yshuf[index]
#   cvtest.y<-dat.train.yshuf[-index]
#
#   cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class")
#   fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
#   pred <- prediction(fit.pred[,1], cvtest.y)
#   roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#   auc.train <- performance(pred, measure = "auc")
#   auc.train <- auc.train@y.values
#
#   cv.aucs[i]<-auc.train[[1]]
# }
#
# hist(cv.aucs)
# summary(cv.aucs)
