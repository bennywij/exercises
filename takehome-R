# takehome exercise
# benny wijatno
# Aug 9, 2013


###### Load the data
takehome <- read.csv("~/Downloads/takehome.csv")
takehome_clean <- read.csv("~/Documents/takehome_cleaned.csv")
takehome_clean_treatment_gospel <- read.csv("~/Documents/takehome_cleaned_again.csv")
takehome_clean_landing_gospel <- read.csv("~/Documents/takehome_cleaned_again2.csv")

str(takehome)

###### Look at the data
takehome[1:100,]
summary(takehome)
min(ts) #1356998400 Tuesday 1st January 2013 12:00:00 AM UTC
max(ts) #1357084799 Tuesday 1st January 2013 11:59:59 PM UTC

sapply(takehome, sd)
#user_id           ts           ab landing_page    converted 
#2.893190e+09 2.494427e+04 4.993811e-01 5.000013e-01 3.014614e-01 

xtabs(~ab + landing_page, data = takehome)
xtabs(~ab + landing_page, data = takehome_clean)
# landing_page
# ab          new_page old_page
# control          0    90815
# treatment    95574     4759

conting_table<-xtabs(~converted + landing_page, data = takehome)
conting_table_clean<-xtabs(~converted + landing_page, data = takehome_clean)

conting_table_clean_treatment_gospel<-xtabs(~converted + ab, data = takehome_clean_treatment_gospel)
conting_table_clean_landing_gospel<-xtabs(~converted + landing_page, data = takehome_clean_landing_gospel)
#   landing_page
# converted new_page old_page
# 0    85883    85940
# 1     9691     9634
takehome[190000:191148,]


#check to make sure ab and landing_page are consistent
#this is most easily done in sql
# control  : 90815   treatment:100333    100333+90815
# old_page:  95574   new_page:95574      95574*2
# not true, some treatment is getting the old page
# must consider doing this minus tainted data

#do a box and whisker plot
attach(takehome)
library(lattice)
bwplot(converted ~ landing_page)
#doesn't tell you anything because conversion variable is binary -- keep this in case we want to measure cont var

#do naive comparison
install.packages("plyr")
library(plyr)
dlply(takehome, "landing_page", summary)

#conversion for treatment 0.1017  
#conversion for control 0.1004
#conversion for new_page 0.1014
#conversion for old_page 0.1008
# ((.1017/.1004)-1)*100 # 1.294821 % lift (treatment vs control)
# ((.1014/.1008)-1)*100 # 0.5952381 % lift (new page vs old page)

#what's up with the misalignment between ab and landing_page?
#did test end early? maybe stopped by automated system?

# looked at tail
takehome[190000:191148,]
# and test looked like it continued to run?
# need to look at records where there's a mismatch

takehome[takehome$landing_page=="old_page" & takehome$ab=="treatment",]
#many rows -- this seems like a data problem. Use page as authoritative.
# it looks like in treatment, there are some users that are still getting the old page.
# despite this, old page leads to bigger effect! unintuitive.
takehome[takehome$landing_page=="new_page" & takehome$ab=="control",]
#no rows

takehome[takehome$landing_page=="old_page" & takehome$ab=="control",]
takehome[takehome$landing_page=="new_page" & takehome$ab=="treatment",]
takehome[takehome$user_id==1611441281 & takehome$ab=="treatment",]

install.packages("car")
library(car)
scatterplot(as.numeric(ab) ~ as.numeric(landing_page))

###### do some statistical testing

#conversion for treatment 0.1017  
#conversion for control 0.1004
#conversion for new_page 0.1014
#conversion for old_page 0.1008
# ((.1017/.1004)-1)*100 # 1.294821 % lift (treatment vs control)
# ((.1014/.1008)-1)*100 # 0.5952381 % lift (new page vs old page)

mean(takehome[takehome$landing_page=="new_page",]$converted) #0.1013979
sd(takehome[takehome$landing_page=="new_page",]$converted) #0.3018564
str(takehome[takehome$landing_page=="new_page",]$converted) #95574


mean(takehome[takehome$landing_page=="old_page",]$converted) #0.1008015
sd(takehome[takehome$landing_page=="old_page",]$converted) #0.3010672
str(takehome[takehome$landing_page=="old_page",]$converted) #0.3010672

# this is technically the wrong test for conversion, but good to see anyway
t.test(takehome[takehome$landing_page=="new_page",]$converted
       ,takehome[takehome$landing_page=="old_page",]$converted, var.equal=TRUE, paired=FALSE)

t.test(takehome[takehome$ab=="treatment",]$converted
       ,takehome[takehome$ab=="control",]$converted, var.equal=TRUE, paired=FALSE)

str(conting_table)

# this is the correct test for a binary outcome
chisq.test(conting_table)
chisq.test(conting_table_clean)
chisq.test(conting_table_clean_treatment_gospel)
chisq.test(conting_table_clean_landing_gospel)

# logistic regression converted against new_page
#mydata$rank <- factor(mydata$rank)
#mylogit <- glm(admit ~ gre + gpa + rank, data = mydata, family = "binomial")
naivemodel <- lm(converted ~ landing_page, data = takehome)
summary(naivemodel)
model <- glm(converted ~ landing_page, data = takehome, family = "binomial" )
summary(model)
model2 <- glm(converted ~ ab, data = takehome, family = "binomial" )
summary(model2)

str(takehome)


###### plot some nice graphs

install.packages("Hmisc")
library(Hmisc)
install.packages("ggplot2")
library(ggplot2)

df <- data.frame(binconf(c(9691,9634), c(95574,95574), alpha=0.05, method="wilson") *100 )
df$landing_page <- c("new_page", "old_page")
df
qplot(ymin=Lower, ymax=Upper, x=landing_page, data=df, 
      geom="errorbar") + labs(y="Conversion Rate")  +  geom_point(aes(y=PointEst))

df <- data.frame(binconf(c(9691,9119), c(90811,90811), alpha=0.05, method="wilson") *100 )
df$landing_page <- c("new_page", "old_page")
df
qplot(ymin=Lower, ymax=Upper, x=landing_page, data=df, 
      geom="errorbar") + labs(y="Conversion Rate")  +  geom_point(aes(y=PointEst))

df <- data.frame(binconf(c(10205,9120), c(95574,90814), alpha=0.05, method="wilson") *100 )
df$ab <- c("a - treatment", "b - control")
df
qplot(ymin=Lower, ymax=Upper, x=ab, data=df, 
      geom="errorbar") + labs(y="Conversion Rate")  +  geom_point(aes(y=PointEst))

df <- data.frame(binconf(c(9691,9634), c(90811,95573), alpha=0.05, method="wilson") *100 )
df$landing_page <- c("new_page", "old_page")
df
qplot(ymin=Lower, ymax=Upper, x=landing_page, data=df, 
      geom="errorbar") + labs(y="Conversion Rate")  +  geom_point(aes(y=PointEst))


# run test longer, but for how long? estimate size needed for confidence interval to not include zero
power.prop.test(p1=0.10, p2=0.101, power=0.8, alternative="one.sided"
                , sig.level=0.05)
power.prop.test(p1=0.10, p2=0.102, power=0.8, alternative="one.sided"
                , sig.level=0.05)
power.prop.test(p1=0.10, p2=0.105, power=0.8, alternative="one.sided"
                , sig.level=0.05)

###### References
#http://www.evanmiller.org/ab-testing/chi-squared.html
# http://www.evanmiller.org/ab-testing/t-test.html
# http://pragmati.st/2013/02/15/calculating-sample-sizes-for-ab-tests/
