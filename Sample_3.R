g = read.csv(file.choose()) #We use the GSS again, but this time name it "g". 

#We are going to run a simple bivariate regression - i.e., a regression with two variables, one dependent and one independent. 

g$realrinc1000s = (g$realrinc)/1000 #First, we will turn income into 1000s of dollars for ease of interpretation. Our new variable is called "realrinc100s". 

plot(as.factor(g$replaceu), g$realrinc1000s) #The other variable, "replaceu", asked:
#"How difficult or easy do you think it would be for your firm or organization to replace you if you left?" 
#with 1 being easily and 5 being very difficultly. 

#Command for running regression is below. We are labeling the regression results "lm1". We are also subsetting. The exclamation point (!) means NOT. 
#So we are saying: do not include persons with N/A for the variable "big5d2". 
lm1 = lm(replaceu ~ realrinc1000s , data=g,  subset = !is.na(big5d2) )
summary(lm1) #See regression summary. 

#Interpretation: for every 1000 dollars more, a person believes it is 0.008 points harder to find someone to replace them, on average. 
#Also, when realrinc1000s = 0, the average response to "replaceu" is 2.67. Check the substantive interpretation by looking at the actual meaning of 2 on arda. 

#We want to sdd an additional variable that might mediate or partly "explain" the initial association from that simple regression above.
#We know that there are usually many causes of a particular phenomena. We want to investigate the impact of a particular variable while "holding another variable constant". 

#For example, I might suspect that people who make more money are just more confident, so I looked at "big5d2".
#This variable prompts respondents to respond to the following: "To what extent do you agree or disagree with the following statements? 
#I see myself as someone who... d. Gets nervous easily".
#The higher the score, the more confident people are. 

plot(as.factor(g$big5d2), g$realrinc1000s) #See plot. Though there is potential evidence for relationships/associations between our variables, we cannot make any claims about statistical significance.

lm2 = lm(replaceu ~ realrinc1000s + big5d2, data=g) #Naming regression results "lm2" this time. If you name the regression results "lm1", the previous variable will be reset, and in order to access it again you'll have to run line 13 again. 
summary(lm2)

#Interpretation: for each category more confident a respondent is, there is a .13 point average increase in thinking tha they cannot be easily replaced, net of other factors. 
#Net of other factors means: holding other factors constant. 
#One way to think about this is that we are trying to isolate the impact of one variable, so we are keeping out the impact of another, potentially related variable.
#Another way to think about this is like a light swtich - only one light switch (dependent variable) can be turned on at a time. 
#So, we are saying that there is a 0.137 point average increase (from the statistically significant y-intercept, 2.221) in thinking that it'd be difficult for someone to replace a respondent, net of (or holding constant) the impact of nervousness.  
#We are also saying that net of income (realrinc1000s), there is a 0.137 average increase (from the y-intercept of 2.221) in thinking that it'd be difficult for someone to replace the respondent. 
#We interpret the y-intercept by saying, when income in 1000s (realrinc1000s) and nervousness (big5d2) BOTH equal 0, the average response to "replaceu" is 2.221. For substantive interpretation, see what 2 is on arda. 
#These are statistically significant, and indicate positive associations between both realrinc100s and replaceu, and big5d2 and replaceu, which means that:
#the variables increase and decrease together. 
#BUT: you will see that this when we compare lm2 to lm1, our second variable hardly changes the income variable, so it is not mediating the effect really.

#We can make our regressions look pretty when we want to export them with the stargazer package. 
#You can modify your regression's presentation, but a really simple way to present your regression is with the following:

install.packages("stargazer")
library(stargazer)
stargazer(lm1, lm2, type = "text")

#Another example of a multiple regression:

install.packages("plyr")
library(plyr)

d = read.csv(file.choose()) #We are going to use the World Value Survey this time around. Load that .csv file in using this command, or by changing the working directory. 

#Look here to find out what variables are within here: http://www.worldvaluessurvey.org/WVSOnline.jsp

#Here is a question:
#"Using this card, would you please indicate for each description whether that person is very much like you, like you, somewhat like you, not like you, or not at all like you? 
#It is important to this person to have a good time; to 'spoil' oneself (V73)".
#Higher scores meaning "less like me".

d = rename(d, c("V73"="spoil")) #We are renaming "V73" to "spoil" for ease of interpretation. 
d$rspoil = 7-d$spoil #We are reverse coding for ease of interpretation.
d$rspoil.lab <- ordered(d$rspoil, levels = c(1,2,3,4,5,6), labels = c("not at all like me", "2", "3", "4", "5", "very much like me")) #We are adding labels.
table(d$rspoil.lab) #We are looking at our new, labeled variable, "rspoil.lab". 

d = rename(d, c("V242"="age")) #Rename. 
d$married=ifelse(d$V57==1, 1,0) #Do a binary re-code. This is saying: if you responded "1" to V57, you will be assigned a "1". All others will be assigned a 0. 
d$female = ifelse(d$V240==2, 1, 0) #Again, binary re-code. This is saying: if you responded "2" to V240, you will be assigned a "1". All others are assigned a "0". 
d = rename(d, c("V239"="ses")) #Rename. 

#We are going to run a regression predicting if you want to spoil yourself as a function of age, sex, and marital status - i.e., how do age, sex, and marital status predict willingness to spoil oneself.
#We did this for Australia, and only if people also answered about "ses", which is a variable about socioeconomic status. 

lm1 = lm(as.numeric(rspoil.lab) ~ age + female + married, d, subset=V2==36 & !is.na(ses))  #We are sub-setting for Australia (36). 
summary(lm1)

#Interpretation of results: when age, female, and married ALL equal 0, the average Australian response to "rspoil.lab" is 4.45. 
#For each one-unit increase in age, there is an average 0.021 point decrease in willingness to spoil oneself ("rspoil.lab"), net of sex and marital status (or "net of other factors" or "holding constant other factors"). 
#For each one-unit increase in sex (i.e., being a female) is associated with a 0.137 point decrease in rspoil.lab, net of other factors. 
#For each one-unit increase in "married" (i.e., being married) is associated with a 0.215 decrease in rspoil.lab, holding constant other factors. 
#All of these are statistically significant, and each of the predictive/independent variables appear to be negatively associated with our dependent variable.


#Now, I will add another independent variable to the model. 
lm2 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses, d, subset=V2==36) #I am looking to see if age, sex, marital status, and the newly added variable, ses, are predictive of willingness to spoil oneself in Australia (#36). 
summary(lm2)

#Interpretation: for respondents in the youngest age group, who are male, who are no married, and who are in the lowest ses bracket, average willingness to spoil oneself is 4.238 for the average US respondent.
#For each one-unit increase in age for Australian respondents, there is an average 0.020 decrease in willingness to spoil oneself (from 4.328), net of other factors. 
#For female respondents in Australia, there is an average 0.0124 point decrease in willingness to spoil oneself, net of other factors. 
#For married respondents in Australia, there is an average 0.241 point decrease in willingness to spoil oneself, net of other factors. 
#For each one-unit increase in socioeconomic status for Australian respondents, there is a 0.033 point increase in willingness to spoil oneself, net of other factors. 
#All results except that coefficient on female (0.124) are statistically significant. The coefficient for female is approaching the threshold for statistical significance at 0.058, as indicated by the period (.). 

#We are going to run a a partial F-test, which compares the two models we made. F-tests help us see if the addition of our variable is supported/has any advantages for statistical significance.
#The f-test basically compares the model without the additional variable and the model with the additional variable to see if its a good idea to add. Sometimes we may add a predictive variable thinking that it is helpful, but in reality its not. 

#anova is partial F-test command. We compare lm1 and lm2. The only difference between the models, in this case, is the addition of the ses variable. 
anova(lm1, lm2)

#The statistically significant p-value on Model 2 indicates that the addition of "ses" is supported. 
#Also, the partial F value is just the square of the t-statistic on "ses". 

#We can run another partial F-test, with different models. In the below model, we include each category of "ses" in the model. 
lm3 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.factor(ses), d, subset=V2==36)
summary(lm3)

anova(lm1, lm3) #In this case, comparing lm1 and lm3. 

#As a whole, do the ses categories add to the predictive power of the model? Seems like no!

#I could also break the "ses" variable into high, medium, and low to see if that would change my results. 

#I am breaking "ses" into high, medium and low. Note: must include 4 breaks (-1, 4, 7, and 10) to create 3 categories, "poor", "middle", "rich".

d$ses.cat = cut(d$ses, breaks = c(-1, 4, 7, 10), label=c("poor","middle","rich")) 

lm4 = lm(as.numeric(rspoil.lab) ~ age + female + married + ses.cat, d, subset=V2==36) #Running regression to see if age, sex, marital status, and our new categorical variable, "ses.cat", can predict willingness to spoil onself for Australian respondents. 
summary(lm4)

#Interpret: the youngest, male, unmarried, poor Australian respondents have an average "willingness to spoil themselves" score of 4.323. 
#;increased age is associated with a statistically significant average decrease in willingness to spoil self for Australian respondents, net of other factors; 
#being female is associated with a non statistically significant average decrease in willingness to spoil self for Australian respondents, net of other factors;
#being married is associated with a statistically significant average decrease in willingness to spoil self for Australian respondents, net of other factors. 
#For the categorical variable that we created, each "category" is compared to the bottom-most one - i.e., they are essentially binary variables. SO:
#We interpret the coefficient on ses.catmiddle by saying: compared to Australian respondents who are poor, being in the middle bracket for "ses" is associated with a non statistically significant average increase in willingness to spoil self, net of other factors. 
#Similarly, we interpret the coefficient on ses.catrich by saying: compared to Australian respondents who are poor, being in the rich bracket for "ses" is associated with a statistically significant average increase in willingness to spoil self, net of other factors. 

anova(lm1, lm4) #We run a partial F-test here.
#We find that adding the ses.cat variable does not necessarily add predictive power to our model, but its approaching the threshold for statistical significance.  

#These categories could also be included as as numeric ones, though this is not necessarily helpful here based on what our variables look like. 

lm5 = lm(as.numeric(rspoil.lab) ~ age + female + married + as.numeric(ses.cat), d, subset=V2==36)
summary(lm5)

#Interpret: each one-unit increase in age and being married is associated with an statistically significant average decrease in willingness to spoil self for Australian respondents, net of other factors.
#Also, each one-unit increase in ses.cat is associated with a statistically significant average increase in willingness to spoil self for Australian respondents, net of other factors. 
#One reason why we might want to avoid the numeric approach is that it simplifies our interpretation, so we may miss something.
#If you remember the results from lm4, only "ses.catrich" has a statistically significant coefficient, which is something we miss in lm5. 

