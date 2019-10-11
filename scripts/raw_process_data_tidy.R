library(tidyverse) #open tidyverse package

library(lubridate) #open lubridate package to allow to work with date and time data

#read data from data/example_raw_test_data.prn this file contains process data collected through the NSC test.

raw_process<-read_tsv("data/example_raw_test_data.prn",col_names=TRUE,skip=4)%>% #read data from file data/example_raw_test_data.prn, skipping the first 4 rows as they contain metadata, assigns dataframe to raw_process
  view()#views data frame

#add test_duration
#add upperlimit 1103
#add lowerlimit 1097

#summarise used mode to group by phase 6 to determine max, min and the time at which these occur and the time that it takes to get back to 1097
