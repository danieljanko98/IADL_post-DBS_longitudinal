#--------LOADING PACKAGES---------

#Different packages
library(readxl) #loading excel files
library(magrittr) #
library(dplyr)
library(ggplot2) #plotting
library(lme4)
library(ggpp)
library(ggpmisc)
library(performance)
#packages for the flow chart
library(rsvg)
library(DiagrammeR)
library(DiagrammeRsvg)

#--------DATA PREPROCESSING--------

# load the whole data set

data <- read.csv("data_040723.csv", sep = ";")

#changing values to numeric

data <- data %>%
  mutate_at(vars(ledd:psych.pdaq_15), as.numeric)

#removing NAs
data[, 13:185] <- replace(data[,13:185], is.na(data[, 13:185]),0)

#removing extra rows
#data <- data[1:286, ]

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

#Load the data with only the columns that I need -
#id, faq_total, faq_cog, faq_motor, drsii_total, bdi_total, ledd, and demographics 

data_new <- read.csv("long_data_new.csv", sep = ",") %>%
  select(id, ass, ledd, bdi_total, drsii, faq_cog, faq_motor, 
        faq_total, date.psych, date.birth, date.surg)

#---------SECOND-LEVEL PREPROCESSING-----------


#creating a data frame with individual subjects and their number of assessments 

ass_count <- data_new %>% #grouping the data by ID and count
  group_by(id) %>%       #the number of unique assessments
  summarize(n_ass = n_distinct(ass))

# hist(ass_count$n_ass, breaks = 20) #plot the data just for fun
# n_of_individual_ass <- table(ass_count$n_ass) #see the individual counts 

# creating time values for analysis (time before and after surgery as continuous
#variable to be used in the analysis)

data_new$date.psych <- as.Date(data_new$date.psych, format = "%Y-%m-%d")
data_new$date.surg <- as.Date(data_new$date.surg, format = "%Y-%m-%d")
data_new$time <- round(as.numeric(difftime(data_new$date.psych, data_new$date.surg, units = "days")) / 365.25, 2)

#creating a group of subjects that only have one assessment

id_1 <- ass_count[ass_count$n_ass == 1, "id"]

#additional data cleaning (preparing data for hte flow chart + excluding subjects)

data_new$why_excluded <- NA
data_new[data_new$id %in% unique(data_new$id), "why_excluded"] <- "Included" 
data_new[data_new$id %in% id_1$id, "why_excluded"] <- "Only one assessment"
write.csv(distinct(data_new, id, .keep_all = TRUE), file = "data_exclusions.csv", row.names = TRUE) 
data_exclusion <- read.csv("data_exclusions.csv") %>%
  select(id, why_excluded)
data_exclusion$why_excluded[data_exclusion$id %in% c("IPN062", "IPN080", "IPN084", "IPN091", "IPN101", "IPN128", 
                                                     "IPN135", "IPN136", "IPN148", "IPN185", "IPN261", "IPN363")] <- 
  c("GPi-DBS", "GPi-DBS", "VIM-DBS", "GPi-DBS", "GPi-DBS", "VIM-DBS", 
    "VIM-DBS", "VIM-DBS", "VIM-DBS", "VIM-DBS", "Duodopa", "GPi-DBS")
table(data_exclusion$why_excluded) %>% print()

#creating a group of subjects that have bad DBS location or duodapa

id_dbs <- data_exclusion[data_exclusion$why_excluded %in% c("GPi-DBS", "VIM-DBS", "Duodopa"),]

#removing subjects with only one score

data_new <-  data_new[!(data_new$id %in% id_1$id), ]

#removing subjects with bad DBS location

data_new <- data_new[!(data_new$id %in% id_dbs$id),]

#number of subjects after all removals
Final_N <- length(unique(data_new$id))

#---------DESCRIPTIVE STATISTICS-----------

#age at surgery 

unique_id <- distinct(data_new, id, .keep_all = TRUE) #creating a subset  with only 1 ID
unique_id$date.birth <- as.Date(unique_id$date.birth, format = "%Y-%m-%d") #changing dates to dates format
unique_id$date.surg <- as.Date(unique_id$date.surg, format = "%Y-%m-%d")
unique_id$age_surgery <- round(as.numeric(difftime(unique_id$date.surg, unique_id$date.birth, units = "days")) / 365.25, 2)
unique_id <- na.omit(unique_id) #removing NAs
mean_age_at_surgery <- round(mean(unique_id$age_surgery),1) #mean age
sd_age_at_surgery <- round(sd(unique_id$age_surgery),1) #sd of age



#flow chart 

f1 <- " digraph {
  
  /// define nodes which will include numbers reflecting the inclusion/exclusion process
  node [ fontname = Calibri, fontsize = 24, shape = box, style = rounded , width = 5 , margin= 0.5 , penwidth = 2 ];
  
  /// create a box for all patients, i.e., sum(t) = 146 patients
  all_pats [ label =<
  <b>146 consecutive<br/>PD patients </b><br/><br/>Local database 2000-2023<br/>General University Hospital<br/>in Prague
  >];
  
  /// create a box for all STN-DBS patients, i.e., sum(t[c(1,4,5,6,7,8,9,11)])) = 134 patients
  all_stn [ label =<
  <b>134 patients </b><br/><br/>implanted with STN-DBS
  >];
  
  /// create a box for all included patients, i.e., t[4] = 84 patients
  all_incl [ label =<
  <b>93 patients </b><br/><br/>followed-up longitudinally
  >];
  
   /// create nodes for exluded patients, specifying only these characteristics that will differ from the nodes above
  node [ fixedsize = T, width = 6, height = 2, margin= 0.5 ];
  
  /// create a box for non-STN-DBS patients, i.e., t[c(3,13,2,10,12)] with sum(t[c(3,13,2,10,12)]) = 27 patients
  excl_nostn [ label =<
  <b>12 patients excluded due to</b><br align = 'left'/><br align = 'left'/>
  5 GPi-DBS<br align = 'left'/>
  6 VIM-DBS<br align = 'left'/>
  1 duodopa<br align = 'left'/>
  ? rejected<br align = 'left'/>
  ? suspended<br align = 'left'/>
  >];
  
  /// create a box for STN-DBS excluded patients, i.e., t[c(1,7,9, 8, 6,11, 5)], sum(t[c(1,7,9,8,6,11,5)]) = 47 patients
  excl_stn [ label =<
  <b>41 patients excluded due to</b><br align = 'left'/><br align = 'left'/>
  41 only one assessment<br align = 'left'/>
  ? unilateral STN-DBS<br align = 'left'/>
  ? not speaking Czech<br align = 'left'/>
  >];
  
   /// create dummy nodes for horizontally forking out of the vertical 'inclusion flow' to excluded sides
  node [ shape = rectangle, width = 0, height = 0, label = '', fill = black ];
  
  /// create directed edges in the inclusion (from dummy/fork vertically to inclusion boxes)
  /// and the exlusion (from forks horizontally to exclusion boxes) parts of the flowchart
  /// first make the arrows bigger
  edge [ arrowsize = 2.5, penwidth = 2.5 ]
  
  /// specifiy paths
  fork1 -> all_stn; fork2 -> all_incl;
  
  /// for the horizontal paths use 'rank = same' to ensure their nodes are level
  { rank = same ; fork1 -> excl_nostn }
  { rank = same ; fork2 -> excl_stn }
  
  /// create non-directed edges from the inclusion boxes to dummy/fork boxes (vertically)
  edge [ dir = none, penwidth = 2.5 ]
  all_pats -> fork1; all_stn -> fork2;
  /// seperate dummy/fork nodes from exclusion boxes by some reasonable distance (in inches)
  nodesep = 1
  }"

grViz(f1) %>% export_svg %>% charToRaw %>% rsvg_png("Fig 1 inclusion-exclusion flowchart.png")

#--------ANALYSIS---------

#plotting
ggplot(data_new, aes(x = time, y = faq_total, group = id)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  facet_wrap(~id, scales = "free") +
  labs(x = "Assessment", y = "FAQ Total")

#linear mixed-effects model with time as a predictor 

#log of faq_total
data_new$log_faq <- log((data_new$faq_total + 1))

#log of faq_cog

data_new$log_faq_cog <- log((data_new$faq_cog + 1)) 

#log of faq_motor

data_new$log_faq_mot <- log((data_new$faq_motor + 1))


#removing subjects with only one score
data_new <-  data_new[!(data_new$id %in% id_1$id), ]

 

#fitting the model 1

#lme_model <- lmer(faq_total ~ ass + (1 | id), data = data_new)

#fitting model 2

#lme_model_2 <- lmer(faq_total ~ time + (1 | id), data = data_new)

#fitting model 3 (uncorrelated random intercepts + slopes)

#lme_model_3 <- lmer(faq_total ~ time + (1 + time | id), data = data_new[!(data_new$id %in% id_1$id), ])

#fitting model 4 with log

lme_model_4 <- lmer(log_faq ~ time + (1 + time | id), data = data_new)
summary(lme_model_4)
check_model(lme_model_4)
plot(check_distribution(lme_model_4))

#model for FAQ_cog

lme_model_4_cog <- lmer(log_faq_cog ~ time + (1 + time | id), data = data_new)
summary(lme_model_4_cog)
plot(check_distribution(lme_model_4_cog))

#model for FAQ_motor

lme_model_4_mot <- lmer(log_faq_mot ~ time + (1 + time | id), data = data_new)
summary(lme_model_4_mot)
plot(check_distribution(lme_model_4_mot))

