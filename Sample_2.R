#Start by setting a working directory for all your files and programs (see previous script for tips). Or run line 4:
#If you are using a PC, you have to replace each / with two //.

d = read.csv(file.choose()) #We are still working with the gss file, but instead of naming it "gss", we are naming it "d". 

#Many times we want to re-code a continuous variable (i.e., one that can take on many variables between a given range. For example, height or weight).
#The simplest way is to make a dummy or binary variable, which takes one of two responses - either 0 or 1, for example. 

d$hi.attend = ifelse((d$attend>4), 1, 0) #

#Above is a binary re-code. We are saying: for those who respond to the following,"How often do you attend religious services?" 
#as 4x or more, you are assigned as 1. For all other response types, you are assigned as a 0. 

table(d$hi.attend, d$attend) #Here we are checking the re-coding. 0 should appear for all categories less than 4, and 1 should appear for everything greater than or equal to 4.

#Sometimes we want to do more. For example, break a variable into categories. 
#We can create a number of categories for religious attendance: weak, moderate, and strong. Note: these are categories that I am using my judgement to make. 
#I do so by checking what the variable responses look like, and categorizing as I think is appropriate. 

d$attend.cat = cut(d$attend, breaks = c(-1, 2, 5, 8), label=c("weak","moderate","strong"), ordered=TRUE) #Here we are creating three categories, even though there are three numbers (-1, 2, 5, and 8)

table(d$attend.cat, d$attend) #Check the re-code.  


#We can go a step further and re-code based on several conditions. 

d$bothftw = ifelse((d$wrkstat==1 & d$sex==1), 1, 0) #For example, here we grouping all those who work full-time (i.e., those who respond to the question associated
#with "wrkstat" as 1), and male (i.e., those who responded to the question associated with "sex" as 1).
#Note: we are making a new variable that represents these two conditions called "bothftw". 

table(d$bothftw, d$wrkstat, d$sex) #Then, we can see this in a table.

#There are other ways to apply multiple labels.
#For example, 1 if you've once married, 0 if you aren't once married:

d$oncemarried[d$marital==1 ] <- 0
d$oncemarried[d$marital==2 ] <- 1
d$oncemarried[d$marital==3 ] <- 1
d$oncemarried[d$marital==4 ] <- 1
d$oncemarried[d$marital==5 ] <- 0

table(d$oncemarried, d$marital) #Here we check against the original coding scheme. 


#What do we have if we have many N/As?
#The answers to "How scientific are each of the following fields? 
#If you have not heard of a particular field, just say you haven't heard of it. 
#F. Economics. Is economics very scientific, pretty scientific, not too scientific, or not scientific at all?" 
#and 5="Haven't heard of it", so we want to make that a missing answer. 

d$econ.new = d$econsci
d$econ.new[d$econsci==5] <- NA

table(d$econsci, d$econ.new) #See re-code!


#Another example of re-coding another variable and attach value labels.

d$hi.attend.lab <- ordered(d$hi.attend, levels = c(0,1), labels = c("low", "high")) #We are using the hi.attend variable that we created above. 

table(d$hi.attend.lab, d$hi.attend) #See re-code.

#Sometimes we will want to reverse code a variable and then add labels and make it ordered:

d$rhappy = 4-d$happy #To reverse code a variable, do this: (highest category + 1) - orginal_variable. 
#We may want to re-code if we are preparing to do regressions. Typically, interpreting regression results is easier if the variables are increasing with every additional option. 

d$rhappy.fact = as.factor(d$rhappy) #This makes th numeric variable into a factor. 

d$lab.rhappy <- ordered(d$rhappy, levels = c(1,2,3), labels = c("unhappy", "so-so", "happy")) 
#In the above we are transforming the factor variable into an ORDERED factor, with value labels, "unhappy", "so-so", and "happy". 

table(d$lab.rhappy, d$rhappy) #Check!

mean(d$happy, na.rm=T) #The original variable, "happy", was numeric, so we can get the mean. 

mean(as.numeric(d$lab.rhappy), na.rm=T) #The new variable, "lab.rhappy", is an ordered factor.
#We need to tell R to treat it like a number, hence, the as.numeric. 

#We can cross-tabulate with our newly created variables!

d$hi.attend = ifelse((d$attend>4), 1, 0) ## as before ##

d$hi.thorough = ifelse((d$big5c1<2), 1, 0) #This is a question about "To what extent do you agree or disagree with the following statements? 
#I see myself as someone who... c. Does a thorough job". 
#People who say they strongly agree are coded 1, otherwise 0.

#install.packages("gmodels") 
#We need to install the gmodels package. Remember, to remove the markdown specification, remove the # from before the code on the above line. Try!

library(gmodels) #Bring the package into R. 

CrossTable(d$hi.attend, d$hi.thorough, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS")  
#the results indicate that being more thorough is not associated with going to religious services more, 
#which is what some psychological theories would have predicted. 
#This does not tell us statistical signficiance. Just gives us an idea of potential relationships. 



#To test statistical significance, we want to run a regression. We are starting with a linear regression with 1 independent variable, and 1 dependent variable. 
#Independent variable is the suspected cause, dependent variable is the suspected effect. We want to see if there is a statistically significant relationship between the suspected cause and suspected effect. 
#We will want to make all the recodes necessary to make the model as easy to interpret as possible. This might mean reverse coding a variable, or creating new categories altogether. 

install.packages("psych") #You may already have this package. That is OK, you can re-run. 

library(psych)

describe(d$hrs1) #Looking to see info about respondents' work hours.

describe(d$sphrs1) #Looking to see info about respondents' spouses' work hours.

#I want to see how one's work hours are related to one's spouses' hours. I think they may be positively related. 
#This means that as one variable's value increases or decreases, the other variable's value does the same.
#If variables are negatively related, it means that one variable's increase is associated with the other variable's decrease, and vice versa. 

lm1 = lm(hrs1 ~ sphrs1, data=d) #This is how we run a linear regression model. We are creating a varible called "lm1", which represents the value of the regression. 
#We want to see if spouse hrs (sphrs1) are predictive of an individual's own working hrs (hrs1). The order of the variables is important here. 
#If we reverse the order, we are going to see the results for if one's own hrs predict spouse's hrs, which is not what we are interested in. 

summary(lm1) #See results!
#We can see that for individuals whose spouses work 0 hours, which is the lowest possible response for this particular variable, the average person works 40.6 hrs per week.
#We can see that the p-value (Pr(>|t|)), is very, very small (Note: this is an exponential value). 
#We can also see that the t-value is relatively large (28.021), and the standard error is 1.44. This combination of values indicates statistical significance. The more stars, the more statistical significance.
#Values are typically considered statistically significance if the p-value is below 0.05. 
#Statistical significance indicates that these values did not likely appear by chance. 
#We can see also that for each one-unit increase in "sphrs1" - i.e., for each increase in spouse hours - there is an average 0.046 point increase in an individuals own working hrs (40.56 + 0.046)
#Note: the relationship between the variables appears to be positive - i.e., they increase/decrease together. 
#HOWEVER, the value for sphrs1 is not statistically significant. The p-value is above 0.05. This does not necessarily mean that there is no relationship between the two, 
#BUT it does mean that we cannot conclude from this analysis that there is a statistically significant relationship between the two variables.
#When we run regressions we are seeking to "disprove the null hypothesis."
#The "null hypothesis", in any regression analysis, is that claim that there is absolutely no relationship between our dependent and independent variable(s). 
#In this case, our null hypothesis is that spouse working hours and not at all associated with an individuals own working hours.
#We are looking to DISPROVE the null, which is something we are able to do when we find statistical significance. 

#We can plot two variables, either as a scatter plot or boxplot, and add in trend/regression lines.
#The trend/regression line, in this case, is 0.04, even though its not statistically significant. 

plot(d$sphrs1, d$hrs1, main="Scatterplot Example", 
     xlab="Spouse Hours", ylab="Hours", pch=19) #Scatter X and Y.

abline(lm(hrs1 ~ sphrs1, data=d), col="blue") #Add in a regression line. 

#OR:

plot(jitter(d$sphrs1), jitter(d$hrs1), main="Scatterplot Example", 
     xlab="Spouse Hours", ylab="Hours", pch=19) #Same as above.

#Or, for a boxplot):

plot(d$sex, d$hrs1, main="Scatterplot Example", 
     xlab="Sex", ylab="Hours", pch=19) #This creates a scatter.

plot(as.factor(d$sex), d$hrs1, main="Scatterplot Example", 
     xlab="Sex", ylab="Hours", pch=19) #This creates a box plot.

mean(d[d$sex == 1, 'hrs1'], na.rm=T)
describe(d[d$sex == 1, 'hrs1']) #Respondent's work hours.

#Other useful graphing codes:

hist(d$hrs1) #Draws a histogram.

dense <- density(d$hrs1, na.rm=T) #Returns the density data. 
plot(dense) #Plots the results as a kernel density plot.

#install.packages("ggplot2")
library(ggplot2)
ggplot(d, aes(x=d$sphrs1, y=d$hrs1)) + #Another scatter plot.
  geom_point(shape=1)      +    #Use hollow circles.
  geom_smooth(method=lm)   #Add linear regression line.

