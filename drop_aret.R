library(readxl)
library(dplyr)

# read in data from excel file
directory <- as.data.frame(read_excel('data/Data.xlsx',sheet = 'Directory'))
dropout <- as.data.frame(read_excel('data/Data.xlsx',sheet = 'Dropout'))

# merge data into one dataframe
drop_data <- merge(directory, dropout, by = "LEAID")

# Removing Missing, NA, and data that did not meet standards
clean_data <- drop_data %>% mutate(TOTD912 = ifelse(TOTD912 == -1,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -2,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -9,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -3,3,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -4,max(0,EBS912-3),TOTD912))

# Removing Missing, NA, and data that did not meet standards
clean_data <- clean_data %>% mutate(EBS912 = ifelse(EBS912 == -1,NA,EBS912))

clean_data <- clean_data %>% mutate(EBS912 = ifelse(EBS912 == -2,NA,EBS912))

clean_data <- clean_data %>% mutate(EBS912 = ifelse(EBS912 == -9,NA,EBS912))

# Looking at counselors and student support staff

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -9,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -9,NA,STUSUP09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -1,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -2,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -1,NA,SECGUI09))

clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(STUSUP09 == -2,NA,SECGUI09))

clean_data <- na.omit(clean_data)

clean_data <- clean_data %>% mutate(TOTSS = SECGUI09 + STUSUP09)

clean_data <- clean_data %>% mutate(GCLOAD = ifelse(SECGUI09 == 0, 0, EBS912/TOTSS))

clean_data <- clean_data %>% mutate(DRATE = ifelse(EBS912 == 0, 0, TOTD912/EBS912))

# fix where dropouts are greater than enrollment
clean_data <- clean_data %>% filter(DRATE < 1)

                                    