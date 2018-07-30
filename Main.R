
##main file for boxing casestudy
setwd("~/GitHub/6372BoxingProject")

#read in data - may need to remove stance na values
df<-read.csv("data.csv",na.strings = c("","NA"))

#####################filter out bad & unnecessary data ########################################################


#eliminates draw outcomes
df<-subset(df,subset = (df$result!="draw"))

#eliminate draw history & judge data since won't function as a predictor
df<-df[,-c(15,16,20:26)]

#filters out missing data
df<-df[complete.cases(df[,c(1:6,9,10)]),]

#filters out weird age values
df<-subset(df,subset = (df$age_A>=16 & df$age_A<=60))
df<-subset(df,subset = (df$age_B>=16 & df$age_B<=60))

#filters out weird reach values
df<-subset(df,subset = (df$reach_A>=100 & df$reach_A<=300))
df<-subset(df,subset = (df$reach_B>=100 & df$reach_B<=300))

#assign as factors
df$result<-factor(df$result)


##############################potential modeling factors for interaction effects ########################
#creates age delta
df$AdvAgeA<-df$age_A-df$age_B

#create height delta
df$AdvHeightA<-df$height_A-df$height_B

#creates reach delta
df$AdvReachA<-df$reach_A-df$reach_B

#creates weight delta
df$AdvWgtA<-df$weight_A-df$weight_B

#over 35 age binary
###35 is the age limit for amateur boxing, some have argued limits should exist for pro's###
df$Over35AgeA<-ifelse(df$age_A>=35,1,0)
df$Over35AgeB<-ifelse(df$age_B>=35,1,0)

#over 15 lbs weight delta?
df$Over15lbA<-ifelse(df$AdvWgtA>=15,1,0)
df$Over15lbB<-ifelse(df$AdvWgtA<=-15,1,0)

#win % for boxers
df$WinPA<-df$won_A/(df$won_A+df$lost_A)
df$WinPB<-df$won_B/(df$won_B+df$lost_B)

#KO per fight
df$KoAPer<-df$kos_A/(df$won_A+df$lost_A)
df$KoBPer<-df$kos_B/(df$won_B+df$lost_B)
