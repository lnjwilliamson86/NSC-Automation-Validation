library(tidyverse) #open tidyverse package

library(lubridate) #open lubridate package to allow to work with date and time data


#read data from data/example_raw_test_data.prn this file contains process data collected through the NSC test.

######## Need to fix this bit########
raw_process<-read_tsv("data/example_raw_test_data.prn",col_names=TRUE,skip=4)%>% #read data from file data/example_raw_test_data.prn, skipping the first 4 rows as they contain metadata, assigns dataframe to raw_process
  mutate(elapsed = seconds(row_number()-1))%>%
  View()#adds column elapsed by numbering the rows, because we know that an observation has been taken every second for the duration of the test

####summarise used mode to group by phase 6 to determine max, min and the time at which these occur and the time that it takes to get back to 1097 This is a nice to have for the purposes of this course. 

#create a plot for the process data
process_data <- ggplot (data=raw_process, 
                            mapping=aes(x= elapsed,
                                        y= Sample_Temp)) +
  geom_point()+
  geom_hline(aes(yintercept=1097),colour= "red")+
  geom_hline(aes(yintercept=1103),colour= "red")+
  ylim(600,1200)+
  labs(title= "Example Process Plot", x="Time Elapsed (Seconds)", y="Sample Temperature oC",caption = "Temperature control limits indicated by red lines")


ggsave("figures/process_data.png",plot=process_data)

