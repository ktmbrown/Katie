library(dplyr)
library(ggplot2)
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

clean_data <- na.omit(clean_data)

# Looking at counselors and student support staff

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -9,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -9,NA,STUSUP09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -1,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -2,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -1,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -2,NA,SECGUI09))

clean_data <- na.omit(clean_data)

clean_data <- clean_data %>% mutate(TOTSS = SECGUI09 + STUSUP09)

clean_data <- clean_data %>% mutate(GCLOAD = ifelse(TOTSS == 0,0,EBS912/TOTSS))

clean_data <- clean_data %>% mutate(DRP912 = TOTD912 / EBS912)

summary(clean_data)

plot(clean_data$GCLOAD, clean_data$DRP912EX)


#One plot with all the data
p <- ggplot(nj_w_drops,aes(x=STID09, y=DRP912,fill = THURM)) + 
      geom_bar(stat="identity") 
p

nj_w_drops <- nj_w_drops %>% filter(PK1209 > 0)

#One plot with all the data
p <- ggplot(nj_w_drops,aes(x=STID09, y=SPECED09/EBS912,fill = THURM)) + 
      geom_bar(stat="identity") 
p

