#Run a simple regression:
w = read.csv(file.choose()) #Using WVS again, naming the data variable "w". 

install.packages("plyr")
library(plyr)

#We are renaming "V160" below to "thirtyyrold". Because the WVS is super big, we are also making modifications to our "w" variable so that it just includes the variables we are interested in including.
w = rename(w, c("V160"="thirtyyrold")) #Question that gets at how comfortable respondents are working for a 30-year old; look it up here: http://www.worldvaluessurvey.us/WVSOnline.jsp
table(w$thirtyyrold) #See how many responses there are to each answer. 
w = rename(w, c("V164"="seventyyrold")) #Rename the following variables. 
w = rename(w, c("V242"="age"))
w = rename(w, c("V2"="country"))

#I want to see if one's age and country (U.S. vs. China) impact comfort in working for a 30-yr-old.
lm1 = lm(thirtyyrold ~ age + as.factor(country), data=w, subset = (country==156 | country==840)) #We subset U.S. and China. 
summary(lm1)

#The youngest respondents in the U.S. have an average willingness to work for thirty-yr-olds score of 7.78. For substantive interpretation, check to see what 7 is on WVS codebook. 
#Interpret: for each one-unit increase in age, there is a statistically significant average 0.003 point decrease in willingness to work for a thirty-yr-old, net of country. 
#Being from the U.S. is associated with a statistically significant average decrease in willingness to work for thirty-yr-old, net of age. 

#I also want to see if one's age and country (US vs. China) impact comfort with working for a 70-year old? 
lm2 = lm(seventyyrold ~ age + as.factor(country), data=w, subset = (country==156 | country==840))
summary(lm2)

#The youngest respondents in the U.S. have an average willingess to work for seventy-yr-olds score of 7.38. 
#For each one-unit increase in age, there is a statistically significant average 0.009 point increase in willingness to work for seventy-yr-old, net of country. 
#Being from the U.S. is associated with a statistically significant decrease in willingness to work for seventy-yr-old, net of age. 

#People from the U.S. seem just not to like to work for anyone!

w = rename(w, c("V10"="happy")) #Renaming a new variable, "V10", to "happy". 

#I want to control for overall level of happiness differences too, because I suspect that maybe happier people are just more satisfied working than people who are less happy. Or maybe, people in one country just tend to be happier than people in the other country.   
lm3 = lm(thirtyyrold ~ age + as.factor(country) + happy, data=w, subset = (country==156 | country==840))
summary(lm3)

#For each one-unit increase in age, there is an average 0.004 point decrease in willingness to work for a thirty-yr-old, net of other factors. 
#Being from the U.S. is associated with a statistically significant 1.435 point average decrease in willingness to work for a thirty-yr-old, net of other factors. 
#For each one-unit increase in happiness, there is a statistically significant average decrease in willingness to work for a thirty-yr-old, net of other factors. 

#I want to add an interaction term to the model that I think might moderate the original relationship between our dependent and independent variables. 
#We might think an interaction term is appropriate when we suspect that variables are very closely related in the context of our dependent variable. 
#For example, we may suspect that age and religiosity are closely related when trying to predict beliefs about abortion - in particular, we may suspect that as age and religiosity increase TOGETHER, respondents become less willing to support abortion. 

lm4 = lm(thirtyyrold ~ age*as.factor(country) + happy, data=w, subset = (country==156 | country==840)) #To interact two variables we add a star (*).
#In the above case, we are creating an interaction term between age and country. 
summary(lm4)

#We see that age, being from the U.S., and being happy are negatively associated with willingness to work for a thirty-yr-old. 
#The coefficient on the interaction term is not statistically significant, but if it were we'd say:
#For each one-unit increase in age for individuals from the U.S., there is an average 0.006 point increase in willingness to work for a thirty-yr-old.
#We are basically treating the "country" variable as a binary one - i.e., "going from 'being Chinese' to 'being from the U.S.'".  

anova(lm3, lm4) #Unclear if adding the interaction term improved my model. 

w$agesq = w$age^2 #We may want to include a quadratic in our equation to normalize our age variable if we suspect that very young and very old respondents' may be less valuable in the context of willingness to work for people of different ages. 
#We are squaring age and creating a new variable called "agesq". Our "age" variable still exists. 

lm5 = lm(thirtyyrold ~ age + agesq + as.factor(country) + happy, data=w, subset = (country==156 | country==840))
summary(lm5)

#For respondents from the U.S., there is a statistically significant average decrease of 1.44 points in willingness to work for a thirty-yr-old, net of other factors. 
#For each one-unit increase in happiness, there is a statistically significant average decrease of 0.22 point in willingness to work for a thirty-yr-old, net of other factors. 

#We can visualize this!
w2 <-subset(w, country==156 | country==840) #We are going to subset the data to include ONLY those respondents from the U.S. and China. 

install.packages("visreg")
library(visreg)
visreg(lm(thirtyyrold ~ age*as.factor(country) + happy, data = w2),
       xvar = "age", by = "country", overlay=T, partial = F, band = F, legend = F, 
       line = list(col = c("cyan3", "purple3"))) 
legend("bottomleft", c("china", "usa"), lwd = 2, col = c("cyan3", "purple3"), cex = 0.8)

#We can see that the slope for "thirtyyrold" differ for respondents from the U.S. and China. Seems like its steeper as respondents from China age than it is for respondents from the U.S. 
#So, as people from the China age, their tolerance for working for a thirty-yr-old declines more intensely than it does for people from the U.S., though people from the U.S. have less overall willingess to work for a thirty-yr-old than people from China. 
