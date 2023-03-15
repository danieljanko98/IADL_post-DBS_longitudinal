#--------LOADING PACKAGES---------

#Different packages
library(readxl) #loading excel files
library(magrittr) #
library(dplyr)
library(ggplot2) #plotting
library(lme4)

#--------DATA PREPROCESSING--------

# load the whole data set

data <- read.csv("data_031323.csv", sep = ";")

#changing values to numeric

data <- data %>%
  mutate_at(vars(ledd:psych.pdaq_15), as.numeric)

#removing NAs
data[, 13:162] <- replace(data[,13:162], is.na(data[, 13:162]),0)

data <- data[1:411, ]

#Create subscores (cognitive and motor) for FAQ measure. If faq_uvod is 1, then use the scores from faq_vykon, otherwise
#use scores from faq_nikdy. Sum up all the items that make up the cognitive score.

#faq_cog

data$faq_cog <- ifelse(data$faq_uvod_2 == 1, as.numeric(data$faq_vykon_2), ifelse(data$faq_uvod_2 == 2, as.numeric(data$faq_nikdy_2), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_3 == 1, as.numeric(data$faq_vykon_3), ifelse(data$faq_uvod_3 == 2, as.numeric(data$faq_nikdy_3), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_7 == 1, as.numeric(data$faq_vykon_7), ifelse(data$faq_uvod_7 == 2, as.numeric(data$faq_nikdy_7), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_8 == 1, as.numeric(data$faq_vykon_8), ifelse(data$faq_uvod_8 == 2, as.numeric(data$faq_nikdy_8), 0))

data$faq_cog <- data$faq_cog + ifelse(data$faq_uvod_9 == 1, as.numeric(data$faq_vykon_9), ifelse(data$faq_uvod_9 == 2, as.numeric(data$faq_nikdy_9), 0))

data$faq_cog

#faq_motor

data$faq_motor <- ifelse(data$faq_uvod_1 == 1, as.numeric(data$faq_vykon_1), ifelse(data$faq_uvod_1 == 2, as.numeric(data$faq_nikdy_1), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_4 == 1, as.numeric(data$faq_vykon_4), ifelse(data$faq_uvod_4 == 2, as.numeric(data$faq_nikdy_4), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_5 == 1, as.numeric(data$faq_vykon_5), ifelse(data$faq_uvod_5 == 2, as.numeric(data$faq_nikdy_5), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_6 == 1, as.numeric(data$faq_vykon_6), ifelse(data$faq_uvod_6 == 2, as.numeric(data$faq_nikdy_6), 0))

data$faq_motor <- data$faq_motor + ifelse(data$faq_uvod_10 == 1, as.numeric(data$faq_vykon_10), ifelse(data$faq_uvod_10 == 2, as.numeric(data$faq_nikdy_10), 0))

data$faq_motor
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
        faq_total, date.psych, date.birth, date.surg)

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

unique_id <- distinct(data_new, id, .keep_all = TRUE) #creating a subset  with only 1 ID
# unique_id[unique_id == ""] <- NA #changing missing values to NA
# unique_id <- na.omit(unique_id) #removing NA
unique_id$date.birth <- as.Date(unique_id$date.birth, format = "%Y-%m-%d") #changing dates to dates format
unique_id$date.surg <- as.Date(unique_id$date.surg, format = "%Y-%m-%d")
unique_id$age_surgery <- round(as.numeric((unique_id$date.surg - unique_id$date.birth)/365.25),1) #calculating age in years 
mean_age_at_surgery <- round(mean(unique_id$age_surgery),1) #mean age
sd_age_at_surgery <- round(sd(unique_id$age_surgery),1) #sd of age

# how long before surgery they were tested

pre <- subset(data_new, ass == "pre")
rownames(pre) <- NULL #change numbering of rows to reflect the new subset
rownames(pre) <- seq(nrow(pre)) #change numbering of rows to reflect the new subset
table(pre$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(pre$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- pre[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
pre <- pre[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

pre[pre == ""] <- NA #changing missing values to NA
pre <- na.omit(pre) #removing NA
pre$date.psych <- as.Date(pre$date.psych, format = "%Y-%m-%d") #changing dates to dates format
pre$date.surg <- as.Date(pre$date.surg, format = "%Y-%m-%d")
pre$age_assessment <- round(as.numeric((pre$date.surg - pre$date.psych)/30),2) #calculating months before surgery
mean_months_pre_surg <- round(mean(pre$age_assessment),1) #mean age
sd_months_pre_surg <- round(sd(pre$age_assessment),1) #sd of age

hist(pre$age_assessment, breaks = 15) #plot the data for visual inspection 

# y1

y1 <- subset(data_new, ass == "r1")
rownames(y1) <- NULL #change numbering of rows to reflect the new subset
rownames(y1) <- seq(nrow(y1)) #change numbering of rows to reflect the new subset
table(y1$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y1$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y1[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y1 <- y1[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y1[y1 == ""] <- NA #changing missing values to NA
y1 <- na.omit(y1) #removing NA
y1$date.psych <- as.Date(y1$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y1$date.surg <- as.Date(y1$date.surg, format = "%Y-%m-%d")
y1$age_assessment <- round(as.numeric((y1$date.psych - y1$date.surg)/30),2) #calculating months after surgery
mean_months_y1_surg <- round(mean(y1$age_assessment),1) #mean age
sd_months_y1_surg <- round(sd(y1$age_assessment),1) #sd of age

hist(y1$age_assessment, breaks = 15) #plot the data for visual inspection 

# y3

y3 <- subset(data_new, ass == "r3")
rownames(y3) <- NULL #change numbering of rows to reflect the new subset
rownames(y3) <- seq(nrow(y3)) #change numbering of rows to reflect the new subset
table(y3$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y3$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y3[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y3 <- y3[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y3[y3 == ""] <- NA #changing missing values to NA
y3 <- na.omit(y3) #removing NA
y3$date.psych <- as.Date(y3$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y3$date.surg <- as.Date(y3$date.surg, format = "%Y-%m-%d")
y3$age_assessment <- round(as.numeric((y3$date.psych - y3$date.surg)/365.25),2) #calculating months after surgery
mean_months_y3_surg <- round(mean(y3$age_assessment),1) #mean age
sd_months_y3_surg <- round(sd(y3$age_assessment),1) #sd of age

hist(y3$age_assessment, breaks = 30) #plot the data for visual inspection 

# y5

y5 <- subset(data_new, ass == "r5")
rownames(y5) <- NULL #change numbering of rows to reflect the new subset
rownames(y5) <- seq(nrow(y5)) #change numbering of rows to reflect the new subset
table(y5$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y5$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y5[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y5 <- y5[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y5[y5 == ""] <- NA #changing missing values to NA
y5 <- na.omit(y5) #removing NA
y5$date.psych <- as.Date(y5$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y5$date.surg <- as.Date(y5$date.surg, format = "%Y-%m-%d")
y5$age_assessment <- round(as.numeric((y5$date.psych - y5$date.surg)/365.25),2) #calculating years after surgery
mean_months_y5_surg <- round(mean(y5$age_assessment),1) #mean age
sd_months_y5_surg <- round(sd(y5$age_assessment),1) #sd of age

hist(y5$age_assessment, breaks = 15) #plot the data for visual inspection

# y7

y7 <- subset(data_new, ass == "r7")
rownames(y7) <- NULL #change numbering of rows to reflect the new subset
rownames(y7) <- seq(nrow(y7)) #change numbering of rows to reflect the new subset
table(y7$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y7$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y7[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y7 <- y7[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y7[y7 == ""] <- NA #changing missing values to NA
y7 <- na.omit(y7) #removing NA
y7$date.psych <- as.Date(y7$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y7$date.surg <- as.Date(y7$date.surg, format = "%Y-%m-%d")
y7$age_assessment <- round(as.numeric((y7$date.psych - y7$date.surg)/365.25),2) #calculating years after surgery
mean_months_y7_surg <- round(mean(y7$age_assessment),1) #mean age
sd_months_y7_surg <- round(sd(y7$age_assessment),1) #sd of age

hist(y7$age_assessment, breaks = 15) #plot the data for visual inspection

# y9

y9 <- subset(data_new, ass == "r9")
rownames(y9) <- NULL #change numbering of rows to reflect the new subset
rownames(y9) <- seq(nrow(y9)) #change numbering of rows to reflect the new subset
table(y9$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y9$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y9[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y9 <- y9[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y9[y9 == ""] <- NA #changing missing values to NA
y9 <- na.omit(y9) #removing NA
y9$date.psych <- as.Date(y9$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y9$date.surg <- as.Date(y9$date.surg, format = "%Y-%m-%d")
y9$age_assessment <- round(as.numeric((y9$date.psych - y9$date.surg)/365.25),2) #calculating years after surgery
mean_months_y9_surg <- round(mean(y9$age_assessment),1) #mean age
sd_months_y9_surg <- round(sd(y9$age_assessment),1) #sd of age

hist(y9$age_assessment, breaks = 15) #plot the data for visual inspection

#y11

y11 <- subset(data_new, ass == "r11")
rownames(y11) <- NULL #change numbering of rows to reflect the new subset
rownames(y11) <- seq(nrow(y11)) #change numbering of rows to reflect the new subset
table(y11$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y11$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y11[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y11 <- y11[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y11[y11 == ""] <- NA #changing missing values to NA
y11 <- na.omit(y11) #removing NA
y11$date.psych <- as.Date(y11$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y11$date.surg <- as.Date(y11$date.surg, format = "%Y-%m-%d")
y11$age_assessment <- round(as.numeric((y11$date.psych - y11$date.surg)/365.25),2) #calculating years after surgery
mean_months_y11_surg <- round(mean(y11$age_assessment),1) #mean age
sd_months_y11_surg <- round(sd(y11$age_assessment),1) #sd of age

hist(y11$age_assessment, breaks = 15) #plot the data for visual inspection

#y13

y13 <- subset(data_new, ass == "r13")
rownames(y13) <- NULL #change numbering of rows to reflect the new subset
rownames(y13) <- seq(nrow(y13)) #change numbering of rows to reflect the new subset
table(y13$id) #check for repeating values, inspect it visually and then continue

#skip if no need for data removal
row_numbers <- which(y13$id == "IPN...") #find where are the specific duplicated values (row numbers)
comparison <- y13[c(row_numbers),] #print the duplicated rows i a new subset for better visual comparison 
y13 <- y13[-41, ] #decide which rows to delete
rm(comparison) #deleting the comparison subset
rm(row_numbers) #deleting the row_numbers subset

y13[y13 == ""] <- NA #changing missing values to NA
y13 <- na.omit(y13) #removing NA
y13$date.psych <- as.Date(y13$date.psych, format = "%Y-%m-%d") #changing dates to dates format
y13$date.surg <- as.Date(y13$date.surg, format = "%Y-%m-%d")
y13$age_assessment <- round(as.numeric((y13$date.psych - y13$date.surg)/365.25),2) #calculating years after surgery
mean_months_y13_surg <- round(mean(y13$age_assessment),1) #mean age
sd_months_y13_surg <- round(sd(y13$age_assessment),1) #sd of age

hist(y13$age_assessment, breaks = 15) #plot the data for visual inspection

#---------DESCRIPTIVE STATISTICS-----------

#number of subjects 
N_all <- length(unique(data_new$id)) #number of subjects across all assessments
N_pre <- length(unique(pre$id)) #N at pre-surg
N_y1 <- length(unique(y1$id)) #N at 1 year post-surg
N_y3 <- length(unique(y3$id)) #N at 3 years post-surg
N_y5 <- length(unique(y5$id)) #N at 5 years post-surg
N_y9 <- length(unique(y9$id)) #N at 9 years post-surg
N_y11 <- length(unique(y11$id)) #N at 11 years post-surg
N_y13 <- length(unique(y13$id)) #N at 13 years post-surg

#number of assessments for each subject 

ass_count <- data_new %>% #grouping the data by ID and count
  group_by(id) %>%.       #the number of unique assessments
  summarize(n_ass = n_distinct(ass))

hist(ass_count$n_ass, breaks = 20) #plot the data just for fun
n_of_individual_ass <- table(ass_count$n_ass) #see teh individual counts 

# number of participant that have specific series of assessments

#number of subj with pre and post
n_with_2 <- sum(ass_count$n_ass == 2 &  
              all(c("pre", "r1") %in% 
              data_new$ass[data_new$id %in% ass_count$id]))

#number of subj with pre, post, and y3
n_with_3 <- sum(ass_count$n_ass == 3 &
                  all(c("pre", "r1", "r3") %in% 
                        data_new$ass[data_new$id %in% ass_count$id]))

#number of subj with pre, post, y3, and y5
n_with_4 <- sum(ass_count$n_ass == 4 &  
                  all(c("pre", "r1", "r3", "r5") %in% 
                        data_new$ass[data_new$id %in% ass_count$id]))

#number of subj with pre, post, y3, y5, y7
n_with_5 <- sum(ass_count$n_ass == 5 &  
                  all(c("pre", "r1", "r3", "r5", "r7") %in% 
                        data_new$ass[data_new$id %in% ass_count$id]))

#--------ANALYSIS---------









