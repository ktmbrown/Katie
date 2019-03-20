library(dplyr)
library(ggplot2)
library(readxl)

directory <- as.data.frame(read_excel("data/Data.xlsx", sheet = "Directory"))
dropout <- as.data.frame(read_excel("data/Data.xlsx", sheet = "Dropout"))

raw_data <- merge(dropout,directory,by='LEAID')

# Choose only positive Enrollment Base greater than 0
clean_data <- raw_data %>% filter(EBS912 > 0)

# Removing Missing, NA, and data that did not meet standards
clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -1,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -2,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -9,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -3,3,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -4,ifelse(EBS912 < 3,0,EBS912-3),TOTD912))

# Convert Type09 to Factor
clean_data <- clean_data %>% mutate(TYPE09 = as.factor(TYPE09))
clean_data <- clean_data %>% mutate(FIPST = as.factor(FIPST))

# Assigning NA to values less than 0

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 < 0,NA,SECGUI09))

clean_data <- clean_data %>% mutate(STUSUP09 = ifelse(STUSUP09 < 0,NA,STUSUP09))

clean_data <- clean_data %>% mutate(SPECED09 = ifelse(SPECED09 < 0,NA,SPECED09))

clean_data <- clean_data %>% mutate(SECTCH09 = ifelse(SECTCH09 < 0,NA,SECTCH09))

clean_data <- clean_data %>% mutate(SCHADM09 = ifelse(SCHADM09 < 0,NA,SCHADM09))

clean_data <- na.omit(clean_data)

# New variables

clean_data <- clean_data %>% mutate(TOTSS = SECGUI09 + STUSUP09)

clean_data <- clean_data %>% mutate(SSLOAD = ifelse(TOTSS == 0,0,EBS912/TOTSS))

clean_data <- clean_data %>% mutate(ADMLOAD = ifelse(SCHADM09 == 0,0,EBS912/SCHADM09))

clean_data <- clean_data %>% mutate(SPECEDRATE = SPECED09 / EBS912)

clean_data <- clean_data %>% mutate(SECTCHLOAD = ifelse(SECTCH09 == 0,0,EBS912/SECTCH09))

clean_data <- clean_data %>% mutate(DRP912 = TOTD912 / EBS912)

#----------- National Models -----------#

nat_model <- glm(clean_data$TOTD912 ~ clean_data$SPECED09 + clean_data$SECGUI09 + 
                   clean_data$STUSUP09 + clean_data$SSLOAD  +
                   clean_data$SCHADM09 + clean_data$SECTCH09, 
             family = quasipoisson, data = clean_data)

#----------- Thurmont Data ------------# 

thurmIdx <- grep('Thurmont',directory$NAME09,ignore.case = T)
thurmLEAID <- directory[thurmIdx,'LEAID']
thurm <- clean_data %>% filter(LEAID == thurmLEAID)


# NJ DATA #

nj <- clean_data %>% filter(FIPST == thurm$FIPST)

# NJ model #

nj_model <- glm(nj$TOTD912 ~ nj$SPECED09 + nj$STUSUP09 +
                      nj$SECGUI09 + nj$SECTCH09 +
                      nj$SCHADM09 + nj$TYPE09, family = quasipoisson, data = nj)




#One plot with all the data
p <- ggplot(nj_w_drops,aes(x=STID09, y=DRP912,fill = THURM)) + 
      geom_bar(stat="identity") 
p

nj_w_drops <- nj_w_drops %>% filter(PK1209 > 0)

#One plot with all the data
p <- ggplot(nj_w_drops,aes(x=STID09, y=SPECED09/EBS912,fill = THURM)) + 
      geom_bar(stat="identity") 
p

