#analyse the outcome of the experiment
library(dplyr)
library(plotly)

#set paths for data files
in_patient_observation_log<-paste(var_DIR_HOME, "apps/app03/data/patient_observations_log.txt", sep="")

#reads the data files into dataframes
patient_observation_log<-read.csv(in_patient_observation_log, header=TRUE, sep = "|",na.strings = "NA", nrows = -100)

var_measurement_name="Exercise Capacity"

observation_set1_full<- patient_observation_log %>%
  filter(measurement_name==var_measurement_name) %>%
  select(patient_id, gender, age, random, obsSeq, measurement_unit, actual_value)

#first reading as baseline and max reading as after treatment
Var_before_reading=as.numeric(min(observation_set1_full$obsSeq))
Var_after_reading=as.numeric(max(observation_set1_full$obsSeq))

observation_set1<- subset(observation_set1_full) %>%
  filter(obsSeq %in% c(Var_before_reading,Var_after_reading))
  
observation_set1_agg<- observation_set1 %>%
  group_by(patient_id) %>%
  summarise(
    "random"=unique(random),
    "gender" = unique(gender),
    "before_value" = max(if_else(obsSeq==Var_before_reading,actual_value,0)),
    "after_value" = max(if_else(obsSeq==Var_after_reading,actual_value,0))  
  )

observation_set1_agg$diff<-observation_set1_agg$after_value - observation_set1_agg$before_value

##########################################################
#Begin analysis
##########################################################

#test if the means are distributed normally
#treatment group

obs_set1_agg_treatment<-observation_set1_agg %>%
  filter(random=="Treatment")


###################################################
#Paired T Test to compare the means of before and after
#Paired T Test (dependent)
#the distribution or before and after values
boxplot(obs_set1_agg_treatment$before_value, obs_set1_agg_treatment$after_value)


plot_ly() %>%
  add_trace(y=~before_value, type="box", data = obs_set1_agg_treatment, name="Before",
            boxpoints = "all", jitter = 0.3,pointpos = -1.8) %>%
  add_trace(y=~after_value, type="box", name="after",
            boxpoints = "all", jitter = 0.3,pointpos = -1.8)


#Visualize distribution of the difference
plot_ly(data = obs_set1_agg_treatment, x=~diff, type = "histogram")
plot_ly(data = obs_set1_agg_treatment, x=~diff, type = "scatter")
  
qqplot<-qqnorm(obs_set1_agg_treatment$diff)
qqplot<-qqline(obs_set1_agg_treatment$diff)
qqplot
#T test to check if the before and after means are different
var_conf_level=0.95
#Paired T-test (dependent samples)
res_pttest<-t.test(obs_set1_agg_treatment$after_value, obs_set1_agg_treatment$before_value, 
       mu=0, alternative = "two.sided", paired = TRUE, 
       conf.level = var_conf_level)
res_pttest

###################################
#unpaired T Test to compare the means of male and female
#unpaired T Test (independent)

obs_set1_agg_treatment %>%
  group_by(gender) %>%
  summarise(
    count=n(),
    mean=mean(diff, na.rm = TRUE),
    sd=sd(diff, na.rm = TRUE)
  )

#normality test
#From the output, the two p-values are greater than the significance level 0.05 implying that the distribution of the data are not significantly different from the normal distribution. 
#In other words, we can assume the normality.
with(obs_set1_agg_treatment, shapiro.test(diff[gender == "M"]))
with(obs_set1_agg_treatment, shapiro.test(diff[gender == "F"]))

#the distribution of the difference
qqnorm(obs_set1_agg_treatment$diff[obs_set1_agg_treatment$gender=="F"])
qqline(obs_set1_agg_treatment$diff[obs_set1_agg_treatment$gender=="F"])

qqnorm(obs_set1_agg_treatment$diff[obs_set1_agg_treatment$gender=="M"])
qqline(obs_set1_agg_treatment$diff[obs_set1_agg_treatment$gender=="M"])

#boxplot(obs_set1_agg_treatment$diff ~ obs_set1_agg_treatment$gender)
plot_ly() %>%
  add_trace(y=~diff, color=~gender, type="box", data=obs_set1_agg_treatment,
            boxpoints = "all", jitter = 0.3,pointpos = -1.8)

#F-Test - Do the two populations have the same variances?
#The p-value of F-test is p = 0.1713596. 
#It’s greater than the significance level alpha = 0.05. 
#In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test which assume equality of the two variances.
res_ftest <- var.test(diff ~ gender, data = obs_set1_agg_treatment)
res_ftest

#Is there any significant difference between women and men difference?
# Compute t-test
#res <- t.test(women_weight, men_weight, var.equal = TRUE)
res_ttest <- t.test(diff ~ gender, data = obs_set1_agg_treatment, var.equal = TRUE,
                    alternate="two.sided", conf=var_conf_level, paired=FALSE)
res_ttest

##############################
#unpaired T Test to compare the means of control and treatment
#unpaired T Test (independent)

observation_set1_agg %>%
  group_by(random) %>%
  summarise(
    count=n(),
    mean=mean(diff, na.rm = TRUE),
    sd=sd(diff, na.rm = TRUE)
  )

#normality test
#From the output, the two p-values are greater than the significance level 0.05 implying that the distribution of the data are not significantly different from the normal distribution. 
#In other words, we can assume the normality.
with(observation_set1_agg, shapiro.test(diff[random == "Control"]))
with(observation_set1_agg, shapiro.test(diff[random == "Treatment"]))

#Visualize the distribution of the difference in QQ plot
qqnorm(observation_set1_agg$diff[observation_set1_agg$random=="Control"])
qqline(observation_set1_agg$diff[observation_set1_agg$random=="Control"])

qqnorm(observation_set1_agg$diff[observation_set1_agg$random=="Treatment"])
qqline(observation_set1_agg$diff[observation_set1_agg$random=="Treatment"])

#boxplot(obs_set1_agg_treatment$diff ~ obs_set1_agg_treatment$gender)
plot_ly() %>%
  add_trace(y=~diff, color=~random, type="box", data=observation_set1_agg,
            boxpoints = "all", jitter = 0.3,pointpos = -1.8)

#F-Test - Do the two populations have the same variances?
#The p-value of F-test is p = 0.1713596. 
#It’s greater than the significance level alpha = 0.05. 
#In conclusion, there is no significant difference between the variances of the two sets of data. 
#Therefore, we can use the classic t-test which assumes equality of the two variances.
res_ftest <- var.test(diff, random, data = observation_set1_agg())
res_ftest

#Is there any significant difference between women and men difference?
# Compute t-test
#The p-value of the test is 0.01327, which is less than the significance level alpha = 0.05. 
#We can conclude that men’s average weight is significantly different from women’s average weight with a p-value = 0.01327.
#res <- t.test(women_weight, men_weight, var.equal = TRUE)
res_ttest <- t.test(diff ~ random, data = observation_set1_agg, var.equal = TRUE,
                    alternate="two.sided", conf=var_conf_level, paired=FALSE)
res_ttest
res_ttest$p.value
################################################
#One-way ANOVA: Test if the means of the treatment and control groups are equal
#For 2 samples, the results should match with Unpaired T-Test
model_anova<-lm(diff ~ random, data = observation_set1_agg)
anova(model_anova)
#Post-hoc test - Which of the different groups have different means
TukeyHSD(aov(model_anova))
#######################################################
#plot to visualize the effect of treatment as compared to control
plot_ly(data=observation_set1) %>%
  add_trace(x=~obsSeq, y=~actual_value, 
            type="scatter",mode="lines", 
            color=~random)

######################################################
#Visualize all observations
plot_ly(data=observation_set1_full) %>%
  add_trace(x=~patient_id, y=~obsSeq, z=~actual_value,
            type="scatter3d",mode="lines", 
            color=~random)

plot_ly(data=observation_set1_full) %>%
  add_trace(x=~obsSeq, y=~patient_id, z=~actual_value,
            type="heatmap")
#####################################################
