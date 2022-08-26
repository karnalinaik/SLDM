library(dplyr)
data=read.csv(file.choose())
head(data)
data

ncol(data)
nrow(data)

summary(data)

data_male = data[data$Gender == 'Male', ]
data_female = data[data$Gender == 'Female', ]
data_male
data_female

data_1 = data_male$Empowerment.Ads
data_2 = data_female$Empowerment.Ads
hist(data_1)
hist(data_2)


#t TEST

#Null hypothesis : Means are equal

#t statistics for empowerment 
res <- t.test(data_1, data_2, var.equal = TRUE)
res

#Interpretation : Since p value is less than 0.05 we reject null hypothesis 
#and state that - The current empowerment advertising mean are significantly different
#Men and women have significantly different perspective in considering an ad as empowering

data_1a = data_male$Sterotypes.Ads
data_2a = data_female$Sterotypes.Ads
hist(data_1a)
hist(data_2a)

#t statistics for stereotype

res_1 <- t.test(data_1a, data_2a, var.equal = TRUE)
res_1
#Interpretation : Since p value is greater than 0.05 we do not reject null hypothesis 
#and state that - There is no significant difference in current stereotype advertising
#Men and women does not have  significantly different perspective in considering an ad as stereotype





#CHI SQUARE

# Education and Reinforcing
#MAle
#Null Hypothesis  : Two variables are independent

chi_male_ed_re = table( data_male$Education,data_male$Reinforcing)
chi_male_ed_re
chisq.test(chi_male_ed_re, simulate.p.value = TRUE)

#Interpretation : Since the p value is greater than 0.05, we do not reject null hypothesis

#Female
#Null Hypothesis  : Two variables are independent


chi_female_ed_re = table(data_female$Education,data_female$Reinforcing)
chi_female_ed_re
chisq.test(chi_female_ed_re, simulate.p.value = TRUE)

#Interpretation : Since the p value is greater than 0.05, we do not reject null hypothesis


#Education and Transform

#Male
chi_male_ed_tr = table(data_male$Education, data_male$Transform)
chi_male_ed_tr
chisq.test(chi_male_ed_tr, simulate.p.value = TRUE)


#Female
#Null Hypothesis  : Two variables are independent

chi_female_ed_tr = table(data_female$Education,data_female$Transform)
chi_female_ed_tr
chisq.test(chi_female_ed_tr, simulate.p.value = TRUE)

#Interpretation : Since the p value is greater than 0.05, we do not reject null hypothesis


#Reinforcing and Transform

#MAle
chi_male_re_tr = table(data_male$Reinforcing,data_male$Transform)
chi_male_re_tr
chisq.test(chi_male_re_tr, simulate.p.value = TRUE)


#Female
#Null Hypothesis  : Two variables are independent

chi_female_re_tr = table(data_female$Reinforcing,data_female$Transform)
chi_female_re_tr
chisq.test(chi_female_re_tr, simulate.p.value = TRUE)

#Interpretation : Since the p value is greater than 0.05, we do not reject null hypothesis




