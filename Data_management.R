# Librairies --------------------------------------------------------------

library(openxlsx)
library(tidyverse)

# Importing data ----------------------------------------------------

data1 <- openxlsx::read.xlsx("/home/baptiste.criniere/Documents/PB_PD_PM/Data/NGC_PRKN_new2.xlsx")


# Age at onset ------------------------------------------------

unique(data1$ID, data1$age_debut)

data2 <- data1 %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(Age_at_onset = max(age_debut, na.rm = TRUE)) %>% 
  dplyr::mutate(Age_at_onset = ifelse(Age_at_onset == "-Inf", NA, Age_at_onset))

# UPDRS -------------------------------------------------------

# k <- c()
# for (i in 1:nrow(data1)){
#   if (data1$redcap_repeat_instrument[i] %in% "updrsiii_examen_moteur"){
#     a <- data1$redcap_repeat_instance[i]
#     k <- c(k, a)
#   }
# }

data3 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "updrsiii_examen_moteur") %>%
  dplyr::filter(updrs_on_off == 1 & redcap_repeat_instance != 3) %>% 
  dplyr::select(ID, redcap_repeat_instance, score_total_updrs_3, date_updrs_3) %>%
  dplyr::mutate(ddn = openxlsx::convertToDate(date_updrs_3)) %>%
  tidyr::spread(redcap_repeat_instance, score_total_updrs_3) %>% 
  dplyr::rename(UPDRS_1 = "1", UPDRS_2 = "2") %>% 
  dplyr::mutate(updrs_1_on = ifelse(!is.na(UPDRS_1), 1, NA),
                updrs_2_on = ifelse(!is.na(UPDRS_2), 1, NA),
                date_updrs_1 = ifelse(!is.na(UPDRS_1), date_updrs_3, NA),
                date_updrs_2 = ifelse(!is.na(UPDRS_2), date_updrs_3, NA)
                ) %>% 
  dplyr::select(-date_updrs_3)


data4 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "updrsiii_examen_moteur") %>%
  dplyr::filter(updrs_on_off == 2) %>% 
  dplyr::select(ID, redcap_repeat_instance, score_total_updrs_3, date_updrs_3) %>%
  tidyr::spread(redcap_repeat_instance, score_total_updrs_3) %>% 
  dplyr::rename(UPDRS_1 = "1", UPDRS_2 = "2") %>%
  dplyr::mutate(updrs_1_on = ifelse(!is.na(UPDRS_1), 2, NA),
                updrs_2_on = ifelse(!is.na(UPDRS_2), 2, NA),
                date_updrs_1 = ifelse(!is.na(UPDRS_1), date_updrs_3, NA),
                date_updrs_2 = ifelse(!is.na(UPDRS_2), date_updrs_3, NA)
                ) %>% 
  dplyr::select(-date_updrs_3)


data5 <- dplyr::left_join(data3, data4, by="ID") %>% 
  dplyr::mutate(UPDRS_1 = ifelse(is.na(UPDRS_1.y), UPDRS_1.x, UPDRS_1.y),
         UPDRS_2 = ifelse(is.na(UPDRS_2.y), UPDRS_2.x, UPDRS_2.y),
         ) %>% 
  dplyr::mutate(UPDRS_1 = ifelse(is.na(UPDRS_1), UPDRS_1.y, UPDRS_1),
         UPDRS_2 = ifelse(is.na(UPDRS_2), UPDRS_2.y, UPDRS_2.y),
  ) %>% 
  dplyr::mutate(updrs_1_on = ifelse(is.na(updrs_1_on.y), updrs_1_on.x, updrs_1_on.y),
         updrs_2_on = ifelse(is.na(updrs_2_on.y), updrs_2_on.x, updrs_2_on.y),
  ) %>% 
  dplyr::mutate(updrs_1_on = ifelse(is.na(updrs_1_on), updrs_1_on.y, updrs_1_on),
         updrs_2_on = ifelse(is.na(updrs_2_on), updrs_2_on.y, updrs_2_on),
  ) %>% 
  dplyr::mutate(date_updrs_1 = ifelse(is.na(date_updrs_1.y), date_updrs_1.x, date_updrs_1.y),
                date_updrs_2 = ifelse(is.na(date_updrs_2.y), date_updrs_2.x, date_updrs_2.y),
  ) %>% 
  dplyr::mutate(date_updrs_1 = ifelse(is.na(date_updrs_1), date_updrs_1.y, date_updrs_1),
                date_updrs_2 = ifelse(is.na(date_updrs_2), date_updrs_2.y, date_updrs_2),
  ) %>% 
  dplyr::select(ID, UPDRS_1, updrs_1_on, date_updrs_1, UPDRS_2, updrs_2_on, date_updrs_2) %>% 
  dplyr::mutate(date_updrs_1 = openxlsx::convertToDate(date_updrs_1),
                date_updrs_2 = openxlsx::convertToDate(date_updrs_2)) %>% 
  dplyr::mutate(Year_UPDRS_1 = format(date_updrs_1, format = "%Y") %>% 
                  as.numeric(),
                Year_UPDRS_2 = format(date_updrs_2, format = "%Y") %>% 
                  as.numeric(),
                )


rm(data3, data4)
data3 <- data5
rm(data5)
# Disease duration --------------------------------------------------------

data4 <- data1 %>% 
  dplyr::select(ID, ddn.y) %>% 
  dplyr::mutate(ddn = openxlsx::convertToDate(ddn.y)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::distinct() %>% 
  dplyr::select(ID, ddn) %>% 
  dplyr::filter(!is.na(ddn)) %>% 
  dplyr::mutate(Year_of_birth = format(ddn, format = "%Y") %>% 
                  as.numeric())

data4 <- dplyr::left_join(data2, data4, by = "ID") %>% 
  dplyr::mutate(Year_of_diagnosis = Year_of_birth + Age_at_onset) %>% 
  dplyr::select(-ddn)


data5 <- dplyr::left_join(data4, data3, by = "ID") %>% 
  dplyr::mutate(Disease_duration_1 = Year_UPDRS_1 - Year_of_diagnosis) %>% 
  dplyr::mutate(Disease_duration_2 = Year_UPDRS_2 - Year_of_diagnosis) %>% 
  dplyr::select(ID, Age_at_onset,
                UPDRS_1, updrs_1_on,
                UPDRS_2, updrs_2_on,
                Disease_duration_1, Disease_duration_2)


data <- data5
rm(data3, data4, data2, data5)
data <- data %>% 
  dplyr::mutate(Disease_duration = Disease_duration_1) %>% 
  dplyr::select(-Disease_duration_1, -Disease_duration_2)

# Les mutations -----------------------------------------------------------
 
data2 <- data1 %>% 
  dplyr::select(ID, gender, Zygosity, 
                mut1.type, mut2.type) %>% 
  dplyr::distinct()
  
data <-  dplyr::left_join(data, data2, by = "ID") %>% 
  dplyr::mutate(UPDRS_on = ifelse(updrs_1_on == 1 &!is.na(updrs_1_on)& !is.na(UPDRS_1),
                                  UPDRS_1, NA)) %>% 
  dplyr::mutate(UPDRS_on = ifelse(updrs_2_on == 1 & !is.na(updrs_2_on)&!is.na(UPDRS_2),
                                  UPDRS_2, UPDRS_on)) %>% 
  dplyr::mutate(UPDRS_off = ifelse(updrs_1_on == 2 &!is.na(updrs_1_on)& !is.na(UPDRS_1),
                                UPDRS_1, NA)) %>% 
  dplyr::mutate(UPDRS_off = ifelse(updrs_2_on == 2 &!is.na(updrs_2_on)& !is.na(UPDRS_2),
                                   UPDRS_2, UPDRS_off))


data <- data %>% 
  dplyr::select(-UPDRS_1, -UPDRS_2, -updrs_1_on, -updrs_2_on) 


# MMSE --------------------------------------------------------------------

k <- c()
for (i in 1:nrow(data1)){
  if (data1$redcap_repeat_instrument[i] %in% "mmse"){
    a <- data1$redcap_repeat_instance[i]
    k <- c(k, a)
  }
}

data3 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "mmse") %>%
  dplyr::filter(redcap_repeat_instance == 1) %>% 
  dplyr::select(ID, mmse_score_total) %>% 
  dplyr::rename(MMSE_1 = mmse_score_total)

data4 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "mmse") %>%
  dplyr::filter(redcap_repeat_instance == 2) %>% 
  dplyr::select(ID, mmse_score_total) %>% 
  dplyr::rename(MMSE_2 = mmse_score_total)


data5 <- dplyr::left_join(data3, data4, by = "ID")

data <- dplyr::left_join(data, data5, by = "ID")

# MOCA --------------------------------------------------------------------

k <- c()
for (i in 1:nrow(data1)){
  if (data1$redcap_repeat_instrument[i] %in% "moca"){
    a <- data1$redcap_repeat_instance[i]
    k <- c(k, a)
  }
}

data3 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "moca") %>%
  dplyr::select(ID, total_moca) %>% 
  dplyr::rename(MOCA = total_moca)


data <- dplyr::left_join(data, data3)



# stades_de_hoehn_et_yahr -------------------------------------------------

k <- c()
for (i in 1:nrow(data1)){
  if (data1$redcap_repeat_instrument[i] %in% "stades_de_hoehn_et_yahr"){
    a <- data1$redcap_repeat_instance[i]
    k <- c(k, a)
  }
}

data3 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "stades_de_hoehn_et_yahr") %>%
  dplyr::filter(stades_hoehn_yahr_on_off == 1 & redcap_repeat_instance != 3) %>% 
  dplyr::select(ID, redcap_repeat_instance, stades_hoehn_yahr_on_off, stades_hoehn_yahr) %>% 
  dplyr::select(-redcap_repeat_instance, -stades_hoehn_yahr_on_off) %>% 
  dplyr::rename(Hoehn_yahr_on = stades_hoehn_yahr) %>% 
  dplyr::mutate(Hoehn_yahr_on = as.numeric(Hoehn_yahr_on))


data4 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "stades_de_hoehn_et_yahr") %>%
  dplyr::filter(stades_hoehn_yahr_on_off == 2 & redcap_repeat_instance != 3) %>% 
  dplyr::select(ID, redcap_repeat_instance, stades_hoehn_yahr_on_off, stades_hoehn_yahr) %>% 
  dplyr::select(-redcap_repeat_instance, -stades_hoehn_yahr_on_off) %>% 
  dplyr::rename(Hoehn_yahr_off = stades_hoehn_yahr) %>% 
  dplyr::mutate(Hoehn_yahr_off = as.numeric(Hoehn_yahr_off))



data5 <- dplyr::left_join(data3, data4, by="ID")
data <- dplyr::left_join(data, data5, by="ID")
rm(data3, data4, data5)

# UPDRS 3 & stades_de_hoehn_et_yahr -----------------------------------------------------------------

data <- data %>% 
  dplyr::rename(Disease_duration_1 = Disease_duration)



data3 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "updrsiii_examen_moteur") %>%
  dplyr::filter(redcap_repeat_instance == 3) %>% 
  dplyr::select(ID, redcap_repeat_instance, score_total_updrs_3, date_updrs_3, updrs_on_off) %>% 
  dplyr::mutate(ddn = openxlsx::convertToDate(date_updrs_3)) %>%
  dplyr::rename(UPDRS_ON_2 = score_total_updrs_3) %>% 
  dplyr::select(ID, UPDRS_ON_2, ddn)


data4 <- data1 %>%
  dplyr::filter(redcap_repeat_instrument %in% "stades_de_hoehn_et_yahr") %>%
  dplyr::filter(redcap_repeat_instance == 3) %>% 
  dplyr::select(ID, stades_hoehn_yahr_on_off, stades_hoehn_yahr) %>% 
  dplyr::rename(Hoehn_yahr_on_2 = stades_hoehn_yahr) %>% 
  dplyr::select(ID, Hoehn_yahr_on_2)



# Disease duration 2 ------------------------------------------------------


data2 <- data1 %>% 
  dplyr::group_by(ID) %>% 
  dplyr::summarize(Age_at_onset = max(age_debut, na.rm = TRUE)) %>% 
  dplyr::mutate(Age_at_onset = ifelse(Age_at_onset == "-Inf", NA, Age_at_onset))


data5 <- data1 %>% 
  dplyr::select(ID, ddn.y) %>% 
  dplyr::mutate(ddn = openxlsx::convertToDate(ddn.y)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::distinct() %>% 
  dplyr::select(ID, ddn) %>% 
  dplyr::filter(!is.na(ddn)) %>% 
  dplyr::mutate(Year_of_birth = format(ddn, format = "%Y") %>% 
                  as.numeric())

data5 <- dplyr::left_join(data2, data5, by = "ID") %>% 
  dplyr::mutate(Year_of_diagnosis = Year_of_birth + Age_at_onset) %>% 
  dplyr::select(-ddn)


data5 <- dplyr::left_join(data5, data3, by = "ID") 

data5 <- data5 %>% 
  dplyr::mutate(Year_UPDRS_3 = format(ddn, format = "%Y")) %>% 
  dplyr::mutate(Year_UPDRS_3 = as.numeric(Year_UPDRS_3)) %>% 
  dplyr::mutate(Disease_duration_2 = Year_UPDRS_3 - Year_of_diagnosis) 


data5 <- data5 %>% 
  dplyr::filter(!is.na(UPDRS_ON_2)) %>% 
  dplyr::select(ID, UPDRS_ON_2, Disease_duration_2)


data <- dplyr::left_join(data, data5, by = "ID")
data <- dplyr::left_join(data, data4, by = "ID")



# Export de la base -------------------------------------------------------
ledd <- openxlsx::read.xlsx("/home/baptiste.criniere/Documents/PB_PD_PM/Data/NGC_LEDD.xlsx")



data <- data %>% 
  dplyr::rename(UPDRS_on_2 = UPDRS_ON_2) %>% 
  dplyr::select(ID, gender, Zygosity, mut1.type, mut2.type, Age_at_onset, Disease_duration_1,
                UPDRS_on, UPDRS_off,Hoehn_yahr_on, Hoehn_yahr_off, MMSE_1, MOCA,
                Disease_duration_2, UPDRS_on_2, Hoehn_yahr_on_2, MMSE_2)

data <- dplyr::left_join(data, ledd)
write.xlsx(data, "/home/baptiste.criniere/Documents/PB_PD_PM/Data/Clean_Redcap_3.xlsx")


















