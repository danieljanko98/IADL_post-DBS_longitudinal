#--------LOADING PACKAGES---------

#Different packages
library(readxl) #loading excel files
library(magrittr) #
library(dplyr)
library(ggplot2) #plotting 

#--------DATA PREPROCESSING--------

# load the whole data set

data <- read_excel("long_data.xlsx")

#changing values to numeric

data <- data %>%
  mutate_at(vars(psych.pdaq:psych.bdi_21), as.numeric)

#removing NAs
data[, 9:100] <- replace(data[,9:100], is.na(data[, 9:100]),0)

#Create subscores (cognitive and motor) for FAQ measure. If faq_uvod is 1, then use the scores from faq_vykon, otherwise
#use scores from faq_nikdy. Sum up all the items that make up the cognitive score.

#faq_cog

data$faq_cog <- ifelse(data$faq_uvod_2 == 1, as.numeric(data$faq_vykon_2), ifelse(data$faq_uvod_2 == 2, as.numeric(data$faq_nikdy_2), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_3 == 1, as.numeric(data$faq_vykon_3), ifelse(data$faq_uvod_3 == 2, as.numeric(data$faq_nikdy_3), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_7 == 1, as.numeric(data$faq_vykon_7), ifelse(data$faq_uvod_7 == 2, as.numeric(data$faq_nikdy_7), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_8 == 1, as.numeric(data$faq_vykon_8), ifelse(data$faq_uvod_8 == 2, as.numeric(data$faq_nikdy_8), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_9 == 1, as.numeric(data$faq_vykon_9), ifelse(data$faq_uvod_9 == 2, as.numeric(data$faq_nikdy_9), 0))


#faq_motor

data$faq_motor <- ifelse(data$faq_uvod_1 == 1, as.numeric(data$faq_vykon_1), ifelse(data$faq_uvod_1 == 2, as.numeric(data$faq_nikdy_1), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_4 == 1, as.numeric(data$faq_vykon_4), ifelse(data$faq_uvod_4 == 2, as.numeric(data$faq_nikdy_4), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_5 == 1, as.numeric(data$faq_vykon_5), ifelse(data$faq_uvod_5 == 2, as.numeric(data$faq_nikdy_5), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_6 == 1, as.numeric(data$faq_vykon_6), ifelse(data$faq_uvod_6 == 2, as.numeric(data$faq_nikdy_6), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_10 == 1, as.numeric(data$faq_vykon_10), ifelse(data$faq_uvod_10 == 2, as.numeric(data$faq_nikdy_10), 0))

#faq_total 

data$faq_total <- data$faq_motor + data$faq_cog

#bdi_total

data <- data %>% 
  mutate(bdi_total = psych.bdi_1+psych.bdi_2+psych.bdi_3+psych.bdi_4+psych.bdi_5+psych.bdi_6+
                                      psych.bdi_7+psych.bdi_8+psych.bdi_9+psych.bdi_10+psych.bdi_11+psych.bdi_12+
                                      psych.bdi_13+psych.bdi_14+psych.bdi_15+psych.bdi_16+psych.bdi_17+psych.bdi_18+
                                      psych.bdi_19+psych.bdi_20+psych.bdi_21) 

#drsii_total

data <- data %>% 
  mutate(drsii = psych.drs_att + psych.drs_ip + psych_drs.cons + psych.drs_conc + psych.drs_mem)


#save the new document as csv

write.csv(data, file = "long_data_new.csv", row.names = TRUE)

#--------DATA ANALYSIS------------
#Load the data with only the columns that I need -
#id, faq_total, faq_cog, faq_motor, drsii_total, bdi_total, ledd, and demographics 

data_new <- read.csv("long_data_new.csv", sep = ",") %>%
  select(id, ass, ledd, bdi_total, drsii, faq_cog, faq_motor, 
         age_surgery, sex, faq_total, date.psych, date.birth, date.surg)

#--------PLOTTING RAW DATA-----------

# plotting FAQ total

faq_total_plot <- ggplot(data_new, aes(x = ass, y = faq_total)) + 
  geom_boxplot()

#plotting FAQ cog

faq_cog_plot <- ggplot(data_new, aes(x = ass, y = faq_cog)) +
  geom_boxplot()

#plotting FAQ motor

faq_motor_plot <- ggplot(data_new, aes(x = ass, y = faq_motor)) +
  geom_boxplot()

# Calculating different time values


#age at surgery 
unique_id <- distinct(data_new, id, .keep_all = TRUE)
unique_id <- unique_id %>%
  # Convert date of birth to date object and handle invalid dates
  mutate(date.birth = as.Date(date.birth, format = "%Y-%m-%d", na.rm = TRUE)) %>%
  # Calculate age at event and handle missing or invalid dob values
  mutate(age_surgery = ifelse(is.na(date.birth), NA, 
                              as.numeric(difftime(as.Date(date.surg), date.birth, units = "days")) / 365.25))
age_at_surgery <- unique_id$age_surgery
age_at_surgery <- na.omit(age_at_surgery)
mean_age_at_surgery <- round(mean(age_at_surgery),1)
sd_age_at_surgery <- round(sd(age_at_surgery),1)

# how long before surgery they were tested
pre <- subset(data_new, ass == "pre")
  #check for repeating vales
duplicated(pre$id)

# y1

y1 <- subset(data_new, ass == "r1")
#check for repeating vales
duplicated(pre$id)

y1 <- y1 %>%
  # Convert date of birth to date object and handle invalid dates
  mutate(date.birth = as.Date(date.birth, format = "%Y-%m-%d", na.rm = TRUE)) %>%
  # Calculate age at event and handle missing or invalid dob values
  mutate(age = ifelse(is.na(date.birth), NA, 
                              as.numeric(difftime(as.Date(date.psych), date.birth, units = "days")) / 365.25))
# y3

y3 <- subset(data_new, ass == "r3")
#check for repeating vales
duplicated(pre$id)

# y5

y5 <- subset(data_new, ass == "r5")
#check for repeating vales
duplicated(pre$id)

# y7

y7 <- subset(data_new, ass == "r7")
#check for repeating vales
duplicated(pre$id)

# y9

y9 <- subset(data_new, ass == "r9")
#check for repeating vales
duplicated(pre$id)

#creating new data set for regression analysis

#data for FAQ at 5 years follow-up
y5 <- subset(data_new, ass == "r5")
faq_y5 <- as.numeric(y5$psych.faq)
faq_y5 <- na.omit(faq_y5)
print(faq_y5)

#data for cognition 1 year after surgery
y1 <- subset(datanew, ass == "r1")
cognition_y1 <- as.numeric(y1$drsii)
cognition_y1 <- na.omit(cognition_y1)

#combined data
regression_data <- data.frame(faq_y5, cognition_y1)


#Analysis

#loading packages
library(lme4)

#mixed effects regression for total faq score as a function of time, levodopa equivalent daily dose, and pdaq.
#we also want to model patient-specific intercepts and patient specific slopes for time. 
mer <- lmer(psych.faq ~ ass + ledd + psych.pdaq ( 1 + ass | id), data = data)
summary(mer)

#mixed effects regression model with repeated measures to compare faq_cog and faq_motor. 
#we also want to see their relationship to the total faq score. 

model <- lmer(cbind(faq_cog, faq_motor) ~ psych.fag + ass (1 | id), data = data)
summary(model)

#linear regression model
#we want to see if cognitive scores 1 year after surgery are correlated with FAQ scores 5 years after surgery

regression <- lm(faq_y5 ~ cognition_y1, data = regression_data)
summary(regression)

#plotting the data

plot(regression_data$cognition_y1, regression_data$faq_y5, xlab = "Cognition 1 year after surgery", ylab = "FAQ scores 5 years after surgery")
abline(regression, col = red)



