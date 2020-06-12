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

str(wines)
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
# good quality=1/bad quality=0


