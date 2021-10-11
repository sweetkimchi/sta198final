library("dplyr")
library("tidyr")
library("lubridate")
library("tidyverse")
library("janitor")
library("oddsratio")
library("car")


# load the raw data #
getwd()
COVID_raw <- read.csv("COVID_raw_12.8.csv")

# outlier variables to NA
COVID_clean <- COVID_raw
COVID_clean$localsiphours[COVID_clean$localsiphours == 528] <-NA 
COVID_data<-COVID_clean

###------------------------------CREATE NEW VARIABLES-------------------------------------------------###

attach(COVID_data)

##factor sex and create labels
(sexf<- factor(sex))
(sexf <-factor(sex, labels= c("Male", "Female")))

## create age categories and turn to categorical variable
agegroup = cut(COVID_data$age, breaks= c(18,34,49,150)) 
levels(agegroup) = c("18-34", "35-49", ">=50")

## create household income (hhiincome) categories and turn to categorical variable
# 1,2,3,4,5 (<50,000k) 
# 6,7,8,9,10 (50k to <100K)  
# 11 (100k to 150k) 
# 12 (>150k) 
(hhincomef <- factor(hhincome))
(hhincomef <-factor(hhincome, labels =c ("<50k", "<50k", "<50k", "<50k", "<50k","50-<100k","50-<100k","50-<100k", "50-<100k", "50-<100k", "100-150k", ">150k")))


## create new children binary variable (code as yes/no)
#0=no children
#1-6 = yes children
(childrenf<- factor(hhchildren))
(childrenf <-factor(hhchildren, labels= c("No", "Yes", "Yes","Yes","Yes","Yes", "Yes")))

## create new education binary variable
#1,3,4,5,7 = not a college graduate
#6 = college or more
(educf<-factor(educ))
(educf<-factor(educ, labels = c("Notcollegegraduate", "Notcollegegraduate", "Notcollegegraduate", "College","Notcollegegraduate")))

## create new depression score binary variable
# none or mild = 0
# moderate or severe = 1
COVID_data <- COVID_data %>%
  mutate(depression_dichot= case_when(phq_sum <=9 ~ 0,
                                      phq_sum >=10 ~ 1))

## create new employment binary variable
# employed or student (1,2,6)
# unemployed or other (3,4,5,7,8)
(employf<-factor(employ1))
(employf<-factor(employ1, labels = c("employed", "employed", "unemployed", "unemployed","unemployed", "employed","unemployed","unemployed")))

COVID_data <- COVID_data %>%
  mutate(essentialnew= case_when(employ1 ==1 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==1 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==1 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==2 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==2 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==2 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==3 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==3 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==3 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==4 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==4 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==4 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==5 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==5 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==5 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==6 & essntlsrvcs== 1 ~ 1,
                                 employ1 ==6 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==6 & essntlsrvcs== 7 ~ 2,
                                 
                                 
                                 employ1 ==7 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==7 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==7 & essntlsrvcs== 7 ~ 2,
                                 
                                 employ1 ==8 & essntlsrvcs== 1 ~ 2,
                                 employ1 ==8 & essntlsrvcs== 2 ~ 2,
                                 employ1 ==8 & essntlsrvcs== 7 ~ 2,))


(essentialnewf<-factor(COVID_data$essentialnew))
(essentialnewf<-factor(COVID_data$essentialnew, labels = c("essential", "nonessential")))

## create new race/ethnicity binary variable
#nonwhite=1
#nonhispanicwhite=2
COVID_data <- COVID_data %>%
  mutate(racenew= case_when(ethnicity ==1 & race== 0 ~ 1,
                            ethnicity ==1 & race== 1 ~ 1,
                            ethnicity ==1 & race== 2 ~ 1,
                            ethnicity ==1 & race== 3 ~ 1,
                            ethnicity ==1 & race== 4 ~ 1,
                            ethnicity ==1 & race== 5 ~ 1,
                            ethnicity ==1 & race== 6 ~ 1,
                            ethnicity ==2 & race== 0 ~ 1,
                            ethnicity ==2 & race== 1 ~ 1,
                            ethnicity ==2 & race== 2 ~ 1,
                            ethnicity ==2 & race== 3 ~ 1,
                            ethnicity ==2 & race== 4 ~ 2,
                            ethnicity ==2 & race== 5 ~ 1,
                            ethnicity ==2 & race== 6 ~ 1,))


(racenewf<-factor(COVID_data$racenew))
(racenewf<-factor(COVID_data$racenew, labels = c("nonwhite", "nonhispanicwhite")))                         
levels(racenewf)


## create new depression severity binary variable
(depressionnewf<-factor(COVID_data$depression_dichot))
(depressionnewf<- factor(COVID_data$depression_dichot, labels = c("noneormild", "moderateorsevere")))
levels(depressionnewf)


## create new variable for localsiphours (>= 23 hours at home)
#compliant = 1
#notcompliant = 0

COVID_data<-COVID_data %>%
  mutate(localsiphours_1 = case_when(localsiphours >=23 ~1,
                                     localsiphours <=22 ~ 0))


## create a new variable that is a sum of leavehomeact variables and spending time at home#
COVID_data <- COVID_data %>%
  mutate(leavehomeacttotnew = leavehomeact___1+leavehomeact___2+leavehomeact___3+leavehomeact___4+leavehomeact___5+leavehomeact___6+localsiphours_1)


## create a new variable that is a sum of leavehome variables#
COVID_data <- COVID_data %>%
  mutate(leavehometotnew = leavehomereason___1+leavehomereason___3+leavehomereason___4+leavehomereason___5+leavehomereason___6)

COVID_data <- data.frame(COVID_data, sexf, agegroup, hhincomef, educf, racenewf, childrenf, depressionnewf)


###------------------------------ANALYSIS-------------------------------------------------###
# filter out missing zip, covid sick outcome if still sick, and limited functioning 
COVID_data_sample <- COVID_data %>%
  filter(localsip==1) %>%
  filter(!is.na(zip)) %>%
  filter(is.na(covidsickoutcome) | covidsickoutcome !=2) %>%
  filter(is.na(dis_alone) | dis_alone !=1) 

### filter out not essential workers
COVID_data_notessential <- COVID_data_sample %>%
  filter(is.na(essntlsrvcs) | essntlsrvcs !=1)

## descriptive analyses for each characteristic
summary(COVID_data_notessential$age, na.rm=TRUE)
sd(COVID_data_notessential$age, na.rm=TRUE)

tabyl(COVID_data_notessential$state, sort = true)

tabyl(COVID_data_notessential$Classification, sort = true)

tabyl(COVID_data_notessential$sexf, sort = true)

tabyl(COVID_data_notessential$racenewf, sort = true)

tabyl(COVID_data_notessential$agegroup, sort = true)

tabyl(COVID_data_notessential$educf, sort = true)

tabyl(COVID_data_notessential$hhincomef, sort = true)

tabyl(COVID_data_notessential$childrenf, sort = true)

tabyl(COVID_data_notessential$depression_dichot, sort = true)

tabyl(COVID_data_notessential$comorbid, sort = true)

summary(COVID_data_notessential$leavehometotnew)
sd(COVID_data_notessential$leavehometotnew)

summary(COVID_data_notessential$leavehomeacttotnew, na.rm=TRUE)
sd(COVID_data_notessential$leavehomeacttotnew, na.rm=TRUE)


### frequencies for reasons for leaving home ###
tabyl(COVID_data_notessential$leavehomereason___1, sort = true)

tabyl(COVID_data_notessential$leavehomereason___3, sort = true)

tabyl(COVID_data_notessential$leavehomereason___4, sort = true)

tabyl(COVID_data_notessential$leavehomereason___5, sort = true)

tabyl(COVID_data_notessential$leavehomereason___6, sort = true)


### frequencies for public health practices###
tabyl(COVID_data_notessential$leavehomeact___1, sort = true)

tabyl(COVID_data_notessential$leavehomeact___2, sort = true)

tabyl(COVID_data_notessential$leavehomeact___3, sort = true)

tabyl(COVID_data_notessential$leavehomeact___4, sort = true)

tabyl(COVID_data_notessential$leavehomeact___5, sort = true)

tabyl(COVID_data_notessential$leavehomeact___6, sort = true)

tabyl(COVID_data_notessential$localsiphours_1, sort = true)


####### logisitic regression #######

## check reference and levels for each categorical variable
contrasts(Classification)
contrasts(agegroup)
contrasts(hhincomef)
contrasts(childrenf)
contrasts(depressionnewf)
contrasts(educf)

## create new comorbid binary variable 
comorbidf <- factor(COVID_data_notessential$comorbid)
levels(comorbidf) = c("1 or more", "None")
contrasts(comorbidf)

## change reference for comorbid and depression severity 
comorbidf_relevel <- relevel(comorbidf, ref = "None")
COVID_data_notessential$depression_dichotf <- factor(COVID_data_notessential$depression_dichot)
levels(COVID_data_notessential$depression_dichotf) = c("Mildnone", "Modsevere")

## change reference categories for cateogrical variables
Classification1 <-relevel(COVID_data_notessential$Classification, ref="Urban")
levels(Classification1)

edu1 <-relevel(COVID_data_notessential$educf, ref="College")
levels(edu1)
class(leavehomereason___1)

### Adjusted logistic regression For Leaving Home and association with each characteristic ###
# work
fullmod_work <- glm(leavehomereason___1 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + childrenf + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_work)
##multicollinearity##
vif(fullmod_work) 
or_glm(data=COVID_data_notessential,model=fullmod_work)


# grocery shopping
fullmod_grocery <- glm(leavehomereason___3 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + childrenf + depression_dichotf + comorbidf_relevel, 
                       data = COVID_data_notessential,
                       family = "binomial")
summary(fullmod_grocery)
##multicollinearity##
vif(fullmod_grocery) 
or_glm(data=COVID_data_notessential,model=fullmod_grocery)



# other essential shopping
fullmod_othershop <- glm(leavehomereason___4 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + childrenf + depression_dichotf + comorbidf_relevel, 
                         data = COVID_data_notessential,
                         family = "binomial")
summary(fullmod_othershop)
##multicollinearity##
vif(fullmod_othershop)
or_glm(data=COVID_data_notessential,model=fullmod_othershop)


# exercise
fullmod_exercise <- glm(leavehomereason___5 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + childrenf + depression_dichotf + comorbidf_relevel, 
                        data = COVID_data_notessential,
                        family = "binomial")
summary(fullmod_exercise)
##multicollinearity##
vif(fullmod_exercise)
or_glm(data=COVID_data_notessential,model=fullmod_exercise)



# walking dog
fullmod_dog <- glm(leavehomereason___6 ~ Classification1 + sexf + agegroup + edu1 + hhincomef + childrenf + depression_dichotf + comorbidf_relevel, 
                   data = COVID_data_notessential,
                   family = "binomial")
summary(fullmod_dog)
##multicollinearity##
vif(fullmod_dog) 
or_glm(data=COVID_data_notessential,model=fullmod_dog)


##Adjusted logistic regression model Protective Health Behaviors and association with each characteristic ##
# spending at least 23 hours inside home
fullmod_leavehome <- glm(localsiphours_1 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                         data = COVID_data_notessential,
                         family = "binomial")
summary(fullmod_leavehome)
#multicollinearity
vif(fullmod_leavehome) 
or_glm(data=COVID_data_notessential,model=fullmod_leavehome)


# physical/social distancing
fullmod_socialdist <- glm(leavehomeact___1 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                          data = COVID_data_notessential,
                          family = "binomial")
summary(fullmod_socialdist)
##multicollinearity##
vif(fullmod_socialdist)
or_glm(data=COVID_data_notessential,model=fullmod_socialdist)


# protective mask
fullmod_mask <- glm(leavehomeact___2 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_mask)
##multicollinearity##
vif(fullmod_mask)
or_glm(data=COVID_data_notessential,model=fullmod_mask)


# wearing gloves
fullmod_gloves <- glm(leavehomeact___3 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                      data = COVID_data_notessential,
                      family = "binomial")
summary(fullmod_gloves)
##multicollinearity##
vif(fullmod_gloves)
or_glm(data=COVID_data_notessential,model=fullmod_gloves)


# using hand sanitizer
fullmod_santizer <- glm(leavehomeact___4 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                        data = COVID_data_notessential,
                        family = "binomial")
summary(fullmod_santizer)
##multicollinearity##
vif(fullmod_santizer) 
or_glm(data=COVID_data_notessential,model=fullmod_santizer)


# using disinfectant wipes
fullmod_wipes<- glm(leavehomeact___5 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_wipes)
##multicollinearity##
vif(fullmod_wipes) 
or_glm(data=COVID_data_notessential,model=fullmod_wipes)


# washing hands frequently
fullmod_hands<- glm(leavehomeact___6 ~ Classification1 + sexf + agegroup + hhincomef + edu1 + childrenf + depression_dichotf + comorbidf_relevel, 
                    data = COVID_data_notessential,
                    family = "binomial")
summary(fullmod_hands)
##multicollinearity##
vif(fullmod_hands) 
or_glm(data=COVID_data_notessential,model=fullmod_hands)