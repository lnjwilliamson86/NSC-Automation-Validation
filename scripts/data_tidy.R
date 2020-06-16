-



#t test for difference between means for CSR & CRI
#1. Filter all_data to give data for each sample
SR0204_only <- all_data %>%
  filter(Sample=="SR0204")

SR0170_only <- all_data %>%
  filter(Sample=="SR0170")

SR0190_only <- all_data %>%
  filter(Sample=="SR0190")

#2. T test for CRI Means
SR0204_CRI_t <- t.test(CRI~Mode, data = SR0204_only)

SR0170_CRI_t <- t.test(CRI~Mode, data = SR0170_only)

SR0190_CRI_t <- t.test(CRI~Mode, data = SR0190_only)

all_CRI_t <- t.test(CRI~Mode, data =  all_data)

#3. T test for CSR Means
SR0204_CSR_t <- t.test(CSR~Mode, data = SR0204_only)

SR0170_CSR_t <- t.test(CSR~Mode, data = SR0170_only)

SR0190_CSR_t <- t.test(CSR~Mode, data = SR0190_only)

all_CSR_t <- t.test(CSR~Mode, data =  all_data)

# 4. Print to console
SR0204_CRI_t 

SR0170_CRI_t 

SR0190_CRI_t

all_CRI_t

SR0204_CSR_t 

SR0170_CSR_t

SR0190_CSR_t 

all_CSR_t








#THIS SECTION WAS NOT REQUIRED AS I WAS ABLE TO CALCULATE THE RECOVERY TIME WITH THE DATA IN DTTM FORMART. removing date information from the process data columns.Each test has assosicated process data of interst for this project is the time that it takes
#the sample temperature to return to the defined limit. The code/strategy for tidying has come from the following websites.  
#https://stackoverflow.com/questions/42996544/how-to-remove-the-date-from-a-column-containing-both-date-and-time-using-r
#http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/

#all_data_time_only<-all_data%>% # takes the dataframe all_data and assigns to validation data frame.
  #separate(time_to_1100oC,into= c("date","time_to_1100oC"),sep=" ")%>% #separates col time_to_1100oC into 2 cols one called date and the other called time_to_1100oC
  #separate(Runtime_switch_CO2, into= c("date_2","Runtime_switch_CO2"),sep=" ")%>% #separates col Runtime_switch_CO2 into 2 cols one called date and the other called Runtime_switch_CO2
  #separate(runtime_max_temp_switch_CO2, into= c("date_3","runtime_max_temp_switch_CO2"),sep=" ")%>% #separates col runtime_max_temp_switch_CO2 into 2 cols one called date and the other called runtime_max_temp_switch_CO2
  #separate(runtime_min_temp_switch_CO2, into= c("date_4","runtime_min_temp_switch_CO2"),sep=" ")%>% #separates col runtime_min_temp_switch_CO2 into 2 cols one called date and the other called runtime_min_temp_switch_CO2
  #separate(time_return_range, into= c("date_5","time_return_range"),sep=" ")%>% #separates col time_return_range into 2 cols one called date and the other called time_return_range
  #separate(total_runtime, into= c("date_6", "total_runtime"),sep= " ")%>% #separates col total_runtime into 2 cols one called date and the other called total_runtime
  #select(-c(date,date_2,date_3,date_4,date_5,date_6))%>% # select function removes the added date-date_6 cols from the data frame. 
  #view()#views the data frame














