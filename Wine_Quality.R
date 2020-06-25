#Wine quality


wines<-read.csv("winequality-red.csv")

#For my ref
#fixed.acidity - acidity fixed with wine:does not evaporate readily
#volatile.acidity - too high leads to unpleasant taste
#citric.acid - adds freshness
#residual.sugar - rare to find wines with <1gm
#chlorides - amt of salt
#### sulphites prevents oxidization####
#free.sulphur.dioxide - not bonded with other molecules - harmless unless respiratory ailments
#total.sulphur.dioxide - bonded with other molecules - same as above
#density - density of water
#pH - acidic or basic - 0 acidic to 14 basic
#sulphates - wine additive - anti microbial
#alcohol - alcohol content
#quality - dependent variable


#unsupervised learning
wines_features<-wines
wines_features$quality<-NULL


#set same scale
wines_features<-scale(wines_features)
view(wines_features)

results<-kmeans(wines_features,6)
view(results)

table(wines$quality,results$cluster)
#####1      2      3      4      5      6
#3   1      2      0      0      7      0
#4   6      4      1      4     37      1
#5 226     96     17     45    281     16
#6  97    177      9    162    181     12
#7  11     78      1     81     23      5
#8   1      7      0     10      0      0

#the variables leading to quality donot align with the clusters? why?
#Maybe because there are rules for wine quality. its not based on a pattern


plot(wines[c("citric.acid","residual.sugar","pH")],
     col=results$cluster)

plot(wines[c("citric.acid","residual.sugar","pH")],
     col=wines$quality)

####################################
# Lets filter only good quality wine
wines_good<-wines[wines$quality>=6,]

wines_features<-wines_good
wines_features$quality<-NULL


#set same scale
wines_features<-scale(wines_features)
view(wines_features)

results<-kmeans(wines_features,3)
view(results)
results$size

table(wines_good$quality,results$cluster)
#     1   2   3
#6  399 213  26
#7   73 124   2
#8    7  11   0

#the variables leading to quality do not align with the clusters? why?
#Though I took good quality wine subset?
#Unknown component which determines quality of wine?


plot(wines_good[c("citric.acid","residual.sugar")],
     col=results$cluster)






# scatterplot to visualize correlation between independent variables when quality is high (quality>6)

quality_factors<-function(variable1,variable2){
  ggplot(wines,aes(x=variable1,y=variable2,color=quality))+
    geom_point(aes(color=as.factor(quality),size=quality))
}

quality_factors(wines$fixed.acidity,wines$volatile.acidity)+
  xlab("Fixed_Acidity")+
  ylab("Volatile_Acidity")

#There are 11 variables which contribute to quality.
#Above graph is not conclusive





#Regression model to arrive at a formula to produce best quality wine

#Assume quality>6 is good quality wine

wines$quality.num<-0
wines[wines$quality>=6,"quality.num"]<-rep(1,855)

#quality>=6 is good & quality<6 is bad

#visualizing quality in histograms
ggplot(data = wines,aes(x = quality)) +
  geom_histogram(binwidth = 0.05) +
  scale_x_log10()   # normal distribution


attach(wines)
y<-cbind(quality.num)  # dependent variables 
x<-cbind(fixed.acidity,   # independent variables
         volatile.acidity,
         citric.acid,
         residual.sugar,
         chlorides,
         free.sulfur.dioxide,
         total.sulfur.dioxide,
         density,
         pH,
         sulphates,
         alcohol)

summary(y)  # simple check to see how dependent variables are distributed

tables(y)

summary(lm(y~x))  # linear model - wrong here

logitmodel<-glm(y~x,family=binomial(link = "logit"))
summary(logitmodel)  #binomial - logistic


exp(logitmodel$coefficients)

