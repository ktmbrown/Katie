library(dplyr)
directory <- read.csv('data/direct.csv', header=T,
                        colClasses=c('numeric','factor','factor',rep('character',21), 
                                     rep("numeric",2), rep("character",5), rep("numeric",27)))

dropout <- read.table('data/dr091a.txt', header=T,fill=T,sep='\t', colClasses=
                            c('character','factor','numeric',rep('numeric',7)))

drop_data <- merge(directory,dropout,by="LEAID")

clean_data <- drop_data %>% mutate(TOTD912 = ifelse(TOTD912 == -1 | TOTD912 == -2 |
                                                            TOTD912 == -9,NA,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -3,3,TOTD912))

clean_data <- clean_data %>% mutate(TOTD912 = ifelse(TOTD912 == -4,max(0,EBS912-3),TOTD912))


clean_data <- clean_data %>% mutate(SECGUI09 = ifelse(SECGUI09 == -1 | SECGUI09 == -2 |
                                                           SECGUI09 == -9,NA,SECGUI09))

clean_data <- na.omit(clean_data)
