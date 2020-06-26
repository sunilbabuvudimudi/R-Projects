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
#function

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



#Ideally use training and testing subsets but this is experiment/WIP
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

summary(lm(y~x))  # linear model - wrong here

logitmodel<-glm(y~x,family=binomial(link = "logit"))
summary(logitmodel)  #binomial - logistic


#Call:
#  glm(formula = y ~ x, family = binomial(link = "logit"))
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.4025  -0.8387   0.3105   0.8300   2.3142  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            42.949948  79.473979   0.540  0.58890    
#xfixed.acidity          0.135980   0.098483   1.381  0.16736    
#xvolatile.acidity      -3.281694   0.488214  -6.722 1.79e-11 ***
#  xcitric.acid           -1.274347   0.562730  -2.265  0.02354 *  
#  xresidual.sugar         0.055326   0.053770   1.029  0.30351    
#xchlorides             -3.915713   1.569298  -2.495  0.01259 *  
#  xfree.sulfur.dioxide    0.022220   0.008236   2.698  0.00698 ** 
#  xtotal.sulfur.dioxide  -0.016394   0.002882  -5.688 1.29e-08 ***
#  xdensity              -50.932385  81.148745  -0.628  0.53024    
#xpH                    -0.380608   0.720203  -0.528  0.59717    
#xsulphates              2.795107   0.452184   6.181 6.36e-10 ***
#  xalcohol                0.866822   0.104190   8.320  < 2e-16 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 2209.0  on 1598  degrees of freedom
#Residual deviance: 1655.6  on 1587  degrees of freedom
#AIC: 1679.6
#
#Number of Fisher Scoring iterations: 4


###########################################################################################################
# For every unit change in quality.num ie., every time wine quality moves from bad to good quality,
#      is becasue of drop in volatile acidity, chlorides, total sulfur dioxide, density and pH   
#########################################AND###############################################################
# Presence of high intercept means there are other factors which cause variance in quality
###########################################################################################################


exp(logitmodel$coefficients)

