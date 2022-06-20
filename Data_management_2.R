# Librairies --------------------------------------------------------------

library(tidyverse)

# Importing data ----------------------------------------------------

data <- read.csv2("/home/baptiste.criniere/Documents/PB_PD_PM/Data/no_mutation_redcap.csv", sep =",")
names(data) <- c("ID", "Repeat_Instrument", "Repeat_Instance", "DATE_DE_NAISSANCE",
                 "Age_at_onset", "Date_examen", "UPDRS_On_ou_Off", "UPDRS")



# Age at onset ------------------------------------------------
data$DATE_DE_NAISSANCE <- as.Date(data$DATE_DE_NAISSANCE, format = "%d/%m/%Y")
data$Year_naissance <- format(data$DATE_DE_NAISSANCE, format = "%Y")
data$Date_examen <- as.Date(data$Date_examen, format = "%d/%m/%Y")
data$Year_exam <- format(data$Date_examen, format = "%Y")

# I decide to delete the third UPDRS mesurement
data <- data[-c(3393,5406, 5569, 8911, 847, 2353),]

data1 <- data %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(Age_at_onset = max(Age_at_onset, na.rm = TRUE)) %>% 
  dplyr::mutate(Age_at_onset = ifelse(Age_at_onset == "-Inf", NA, Age_at_onset))
data2 <- data %>% 
  dplyr::select(ID, Year_naissance) %>% 
  dplyr::group_by(ID) %>%
  dplyr::summarize(Year_naissance = max(Year_naissance, na.rm = TRUE))

data2 <- dplyr::left_join(data1,data2, by = "ID") %>% 
  dplyr::mutate(Year_of_diagnosis = as.numeric(Year_naissance) + Age_at_onset)




# UPDRS  on & off -------------------------------------------------------

data3 <- data %>% 
  dplyr::filter(UPDRS_On_ou_Off %in% "On") %>% 
  dplyr::rename(UPDRS_on = UPDRS) %>% 
  dplyr::select(ID, UPDRS_on, Year_exam)


data4 <- data %>% 
  dplyr::filter(UPDRS_On_ou_Off %in% "Off") %>% 
  dplyr::rename(UPDRS_off = UPDRS) %>% 
  dplyr::select(ID, UPDRS_off)
  
data5 <- dplyr::left_join(data3, data4, by = "ID")

data6 <- dplyr::left_join(data2, data5, by = "ID") %>% 
  dplyr::mutate(Disease_duration = as.numeric(Year_exam) - as.numeric(Year_of_diagnosis)) %>% 
  dplyr::select(-Year_naissance, -Year_exam, -Year_of_diagnosis)
  

# which(duplicated(data6$ID))
# FPD-NAN-GOU-688-20
write.csv2(data6, "/home/baptiste.criniere/Documents/PB_PD_PM/Data/no_mutation_redcap2.csv")



# Analyse -----------------------------------------------------------------


rm(list = ls())
data <- read.csv2("/home/baptiste.criniere/Documents/PB_PD_PM/Data/no_mutation_redcap2.csv", sep =";")
data <- data[,-1]
data <- data %>% 
  dplyr::mutate(UPDRS_on = as.numeric(as.character(UPDRS_on))) %>% 
  dplyr::mutate(UPDRS_off = as.numeric(as.character(UPDRS_off)))
mod0 <- lm(UPDRS_on ~  Disease_duration, data)
summary(mod0)


# FPD-BOR-LAL-469-5
databis <- data %>% 
  dplyr::mutate(age_binaire = ifelse(Age_at_onset >55, "Yes", "Non"))
mod0 <- lm(UPDRS_on ~  Disease_duration*age_binaire, databis)
summary(mod0)

databis %>% ggplot(aes(x = Disease_duration, y = UPDRS_on, group = age_binaire, color = age_binaire))+
  geom_smooth(method = lm, formula = y ~ x, se=F)+
  geom_count(alpha= 0.8)+
  xlab("Disease duration")+
  theme_bw()+
  xlim(0, 40)+
  ylab("UPDRS on and converted MDS")

 
sjPlot::plot_model(mod0, type = "diag")








