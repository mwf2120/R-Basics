#First, load data into R. In this case, data needs to be in .csv file format. If your data is in excel, re-save as .csv. 
#Remember where your data is (i.e., on your Desktop), or save the .csv file to a particular location (i.e., within a folder)
#This sample will work for the General Social Survey data that is in this repo. 

#Note: anything with a # sign is "marked down" - i.e., its not active, and will not run unless you remove the #. 
#Markdown can be used to explain what you are doing in the code, to write notes to yourself, etc. 

#You can "run" each code, line by line, by clicking anywhere on a particular line, and then clicking "Run" (above).

gss = read.csv(file.choose()) #This is one very straightforward way to load data into R. Look for pop-up window and choose your GSS file. 

#Alternatively, you will direct R to your data by "setting a working directory", which is like directing R, step-by-step, to the location of your data file.. 
#For example, if my data were located on my desktop:

#setwd("/Users/michaelaflum/Desktop")

#and then read in your csv with this command:

#gss = read.csv("gss.csv") 

#The "gss" is basically a variable. We are telling R: read in my csv file from the path that I specified, and call the csv file "gss". 
#You can name the variable anything you'd like! Once you've read it in, it should appear in your "Global Environment" on the top right under "Data". 

#If my csv file were located within a folder called "Practice" on my Desktop, the setwd command would differ only by the following:

#setwd("/Users/michaelaflum/Desktop/Practice")
#Note: everything is case sensitive! 

names(gss) #Once your data is loaded, you should check to see what variables are there? "vpsu" and "vstrat" etc.

#Below, in the "Console", you will see the name of each of some of the columns in the csv file. 
#You can check by clicking on "gss" under "Data". The csv should pop-up in an additional tab.

#Or, you can run the below command and look at the data as a spreadsheet.
View(gss)

#There are other ways to check out your data. For example, with the table, summary, or describe commands.
#To do so, you type one of the following, and in parenthesis "call" your data followed by a $ sign and a particular variable name. For example:

summary(gss$jbintfam) #Returns the minimum, median, mean, max, etc. of a particular variable.

#We can also tabulate a given variable - i.e., see how many responses fall into each possible response type for a given variable.
#In the case of jbintfam, there are five possible options. 

#If I want to, for example, investigate something like work-life conflict, we can do the following:

table(gss$jbintfam) #This tabulates responses for: "How often do you feel that the demands of your job interfere with your family life?" 

#If I look up JBINTFAM at www.thearda.com above, I see the following:
#1 = Always, and 5 = Never

#We can also get proportions for this variable:
prop.table(table(gss$jbintfam)) 
#Reminder, to transform these into % we subtract 1 and multiply by 100. 

options(digits=2) #This shortens proportions them. Update values by running the following again:
prop.table(table(gss$jbintfam))

#Only 5% are "Always" feeling interfered with, while 20% are "Never" feeling interfered with.

options(digits=7) ##If I want to get back to my original proportions. Default is 7.
prop.table(table(gss$jbintfam)) #Update values by running again.

install.packages("psych") #Install this package. Packages each have different capacities, but they help with analysis. 
library(psych) #Reminder: once a package is installed, it must be brought into your R library with this command. 

#Let's analyze another variable:
describe(gss$wrkstat) #Summarizes "Last week were you working full-time, part-time, going to school, keeping house, or what?" as if it were a number. 
#This gives us a lot of information, some helpful and some not.
#The describe function is better for numeric variables like "IF WORKING, FULL OR PART TIME: How many hours did you work last week, at all jobs?". The mean is 42.08.

table(gss$wrkstat) #This is better. Provides a table of categories: we see that 2322 people work full-time, for instance. 

#We can also compare the means, standard deviations etc. for variables. To do so, we need a package, plyr.

install.packages("plyr")

library(plyr)

#Reminder: just because something shows up in red does not necessarily mean its wrong. See console for example!

ddply(gss, "jbintfam", summarise, Mean = mean(hrs1, na.rm = T), SD = sd(hrs1, na.rm = T))

#ddply's results show that "Always" work 49.95 hours, while "Nevers" work 38.34 hours, with everyone else monotonically related.

install.packages("doBy")  #This package is going to do the same thing as using plyr above.
library(doBy)

summaryBy(hrs1~jbintfam, data=gss, FUN=c(mean, sd), na.rm=T)

boxplot(hrs1~jbintfam, data=gss) #This graphically displays the relationship in boxplot form. Should appear in bottom right under "Plots". If not, click on "Plots".

install.packages("ggplot2") #This program will make a fancy boxplot instead. 
library(ggplot2)

p = ggplot(gss, aes(factor(jbintfam), hrs1))

p + geom_boxplot() + geom_jitter()

p + geom_boxplot(aes(fill = factor(jbintfam))) #This includes NAs, which are the "missings" in the data file.

#We can cross-tabulate two categorical or ordinal variables. 

install.packages("gmodels") #Install this package. 
library(gmodels)

CrossTable(gss$jbintfam, gss$wrkstat, prop.r=F, prop.c=T, prop.t=F, prop.chisq=F, format="SPSS") 
#The above generates how frequently different types of workers feel interference between family and work.

#Results: full-timers experience 18% always conflicted, vs. part-timers experience 31% always conflicted.

#We can tell, from the cross-tabulation, that there may be a relationship/relationships between the variable responses,
#BUT, we cannot tell if this is statistically significant from a cross-tabulation. 

#No need to save your session, but save your R Script!
