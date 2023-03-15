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
