#Descriptive Statistics
#rows 308 - ... in the "Complete_Analysis_iADL_Long" script
#run after "Time_Values_iADL_Long" because it is using some objects created by that script

#packages
library(dplyr)


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
