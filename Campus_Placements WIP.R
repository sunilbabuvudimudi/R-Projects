

campus<-read.csv("Placement_Data_Full_Class.csv")

  
library(plyr)
 campus<- rename(campus, c("hsc_s" = "hsc_subject",
                   "degree_t" = "degree_subject",
                   "etest_p" = "employability_test",
                   "specialisation" = "specialization"))
 
 
 point_function<-function(data1,p,q,c){
   ggplot(data1)+
     geom_point(aes(x=p,y=q,color=c))
 }
 point_function(campus,campus$mba_p,campus$employability_test,campus$gender)+
   xlab("mba_percent")+
   ylab("employablity_percent")
 
 
 
 #str(campus)
 
 #Correlation equation
 cor(campus$mba_p,campus$degree_p) 

 mod <- lm(data=campus,employability_test~mba_p) 
 print(mod) 

 ggplot(campus)+geom_point(mapping = aes(x=degree_p,y=mba_p))+
  #geom_abline(slope = 0.4963,intercept = 41.1943)
  geom_abline()



table(campus$status)
#Not equal campus placed/not campus placed

Placed<-campus[campus$status=="Placed",]
Not_Placed<- campus[campus$status=="Not Placed",]


Placed_training_rows <- sample(1:nrow(Placed), 
                                   0.3*nrow(Placed)) 
NotPlaced_training_rows<-sample(1:nrow(Not_Placed), 
                                0.3*nrow(Placed)) 
#Resultant vectors of same length from both placed and not placed

trainingdata<-rbind(Placed[Placed_training_rows,],Not_Placed[NotPlaced_training_rows,])

testdata<-rbind(Placed[-Placed_training_rows,],Not_Placed[-NotPlaced_training_rows,])

table(trainingdata$status)
#training data with 44 placed and 44 not placed

str(trainingdata)

#logistic regression - placed or not placed --> binomial
logitMod <- glm(status ~ gender+degree_subject+workex+specialization+degree_p+mba_p+employability_test, 
                data=trainingdata, family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testdata))  # predicted scores

summary(logitMod)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            -16.89457    5.41715  -3.119  0.00182 ** 
#  genderM                  1.10950    0.70177   1.581  0.11388    
#degree_subjectOthers     0.16024    1.06306   0.151  0.88019    
#degree_subjectSci&Tech  -0.89930    0.70741  -1.271  0.20364    
#workexYes                1.06241    0.76320   1.392  0.16391    
#specializationMkt&HR    -0.37348    0.60087  -0.622  0.53423    
#degree_p                 0.23337    0.05890   3.962 7.43e-05 ***
#  mba_p                   -0.01546    0.06284  -0.246  0.80563    
#employability_test       0.02713    0.02338   1.160  0.24603    




library(tidyverse)
campus<-select(campus,-sl_no)

campus[!complete.cases(campus),] # there are NAs
campus[is.na(campus$status),] #NAs are becasue not placed will have no salary


barchart_function<-function(data1,x)
{
ggplot(data1)+
   geom_bar(aes(x),color='black',fill='red')+
   theme_classic()
}

barchart_function(campus,campus$specialization)
barchart_function(campus[campus$status=="Placed",],campus[campus$status=="Placed",]$specialization)


ggplot(campus, aes(mba_p, employability_test, color=factor(status)))+
   geom_point(alpha= 0.5, size=3.0)+
   xlab('mba percentage')+
   ylab('employability test percentage')+
   ggtitle('scatter plot visualizing masters percentage and employability test results')

ggplot(campus, aes(degree_p, employability_test, color=factor(status)))+
   geom_point(alpha= 0.5, size=3.0)+
   xlab('degree percentage')+
   ylab('employability test percentage')+
   ggtitle('scatter plot visualizing graduate percentage and employability test results')

# Masterspercentage is scattered. degree percentage is focussed(higher the % better the chances)
# Marks not important in MBA?



