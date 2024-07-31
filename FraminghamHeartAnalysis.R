#Programming in R Final Project

#multiple regression with RESTHEARTRTE as dependent and all other quant vars as independent variables

model<- lm(RESTHEARTRTE~ TOTCHOLESTEROL +
             AGE +
             SYSBP +
             DIABP +
             BMI +
             GLUCOSE,
           data = rest_heart_rate)
summary(model)

#from this model it looks like all these variables except BMI and DIABETES have a p-value which is significant

#some scatterplots to show the relation ship of the other variables to restheartrte


sc1 = ggplot(data=rest_heart_rate,aes(x=DIABP,y=RESTHEARTRTE))+
  geom_point()
sc2 = ggplot(data=rest_heart_rate,aes(x=AGE,y=RESTHEARTRTE))+
  geom_point()
sc3 = ggplot(data=rest_heart_rate,aes(x=SYSBP,y=RESTHEARTRTE))+
  geom_point()
sc4 = ggplot(data=rest_heart_rate,aes(x=BMI,y=RESTHEARTRTE))+
geom_point()
sc5 = ggplot(data=rest_heart_rate,aes(x=GLUCOSE,y=RESTHEARTRTE))+
  geom_point()
#these plost dont seem to show any strong  linear correlation


#we can also see from the model that the variable with the largest coefficient is Diabp
#so I will use diabp in a simple linear regression model to predict restheartrte

model2 <- lm(RESTHEARTRTE~DIABP,data=rest_heart_rate)
summary(model2)

#creating a scatterplot to show the relationship between DIABP and restheartrte visually
ggplot(data=rest_heart_rate,aes(x=DIABP,y=RESTHEARTRTE))+
         geom_point()



#one way anova testing the difference in restheartrte of current smokers vs non smokers
#making sure the standard deviations fit the equalness requirement of an anova test
library(dplyr)
group_by(rest_heart_rate, CURSMOKE) %>%
  summarise(
    count = n(),
    mean = mean(RESTHEARTRTE, na.rm = TRUE),
    sd = sd(RESTHEARTRTE, na.rm = TRUE)
  )
# these standard deviations are almost equal so it is okay to proceed with the anova test
#boxplots for visualizations
boxplot(rest_heart_rate$RESTHEARTRTE ~ rest_heart_rate$CURSMOKE,
        col='steelblue',
        main='Resting Heart Rate By Current Smoking Status',
        xlab='Current Smoker',
        ylab='Resting Heart Rate') 

# Compute the analysis of variance
onewayanovaresults <- aov(RESTHEARTRTE ~ CURSMOKE, data = rest_heart_rate)
# Summary of the analysis
summary(onewayanovaresults)


#two way anova on diabetes status and gender for resting heart rate
#making sure r recognizes diabetes as categorical var
rest_heart_rate$DIABETES<-as.factor(rest_heart_rate$DIABETES)
#visualizations 
boxplot(RESTHEARTRTE ~ DIABETES * GENDER, data=rest_heart_rate, frame = FALSE, 
        col = c("steelblue", "pink"), ylab="Resting Heart Rate")
# Two-way interaction plot
interaction.plot(x.factor = rest_heart_rate$DIABETES, trace.factor = rest_heart_rate$GENDER, 
                 response = rest_heart_rate$RESTHEARTRTE, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Diabetes status", ylab="Resting Heart Rate",
                 pch=c(1,19), col = c("pink", "steelblue"))
#from the interaction plot we can see the the lines are not perpindicular meaning that there is no interaction between these two variables

#two way anova
twowayanovaresults <- aov(RESTHEARTRTE ~ DIABETES + GENDER, data = rest_heart_rate)
summary(twowayanovaresults)
#post hoc tukey test to determine where difference is
twowayanovamod <- aov(RESTHEARTRTE ~ DIABETES * GENDER,
           data = rest_heart_rate)
summary(twowayanovamod)
plot(TukeyHSD(twowayanovamod, which = "DIABETES:GENDER"),)




#side by side bar chart to show counts of females with hypertension by smoking status

#first subset to include only females
female_heart<- subset(rest_heart_rate,GENDER =="Female")
#subset to only include hyperten and cursmoke
female_hyperten_cursmoke <- select(female_heart,5,8)

results<-as.data.frame(table(female_hyperten_cursmoke))


#bar plots
ggplot(data = results, aes(x = HYPERTEN, y = Freq, fill = CURSMOKE)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75)  +
  geom_text(aes(label = Freq), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n Hypertension Status", y = "Frequency\n", title = "Hypertension Status by Smoking Status for Female Participants") 


#boxplots of cholesterol level by diabetic status
boxplot(TOTCHOLESTEROL ~ DIABETES, data=female_heart, frame = FALSE, 
        col = c("steelblue"), ylab="Total Cholesterol")





#contingency tables to show which gender has higher hypertension frequencies
genderhyperten<-table(rest_heart_rate$HYPERTEN,rest_heart_rate$GENDER)


#contingency table to see whether smokers or non smoker have higher hypertension frequencies
smokehyperten<-table(rest_heart_rate$HYPERTEN,rest_heart_rate$CURSMOKE)


#contingency table to see whether diabetics or non diabetics have higher hypertension frequencies
diabeteshyperten<-table(rest_heart_rate$HYPERTEN,rest_heart_rate$DIABETES)


#contingency tablel to see which education level has higher hypertension frequencies
educhyperten<-table(rest_heart_rate$HYPERTEN,rest_heart_rate$EDUC)


















































