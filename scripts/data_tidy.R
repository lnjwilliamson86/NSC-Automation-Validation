library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

#library(lubridate) #open lubridate package to allow to work with date and time data

library(tsibble)

library(anytime)

#Read data from data/comissioning_data.xlsx. This file contains four sheets each of which contain the data from a 
#single sample type. SR0204 is a high reactivty control sample, SR0190 is a low reactivity control sample SR0170 is a medium reactivity control sample and 
#Other are test samples that have been included in project but may not be used for assessing the effect of process, this has been commented out as the othertab isn't working properly. 
#automation on test results.The results for each sample include test results as well as process data. 

SR0204_data<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0204",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","date","numeric","date","numeric","date","numeric","date","date","date","text")) #reads data in sheet SR0204 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

SR0190_data<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0190",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","date","numeric","date","numeric","date","numeric","date","date","date","text")) #reads data in sheet SR0190 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

SR0170_data<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0170",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","date","numeric","date","numeric","date","numeric","date","date","date","text")) #reads data in sheet SR0170 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

#other_data<- read_excel("data/comissioning_data.xlsx",
                       #sheet="Other",
                        #col_names=TRUE,
                        #col_types=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","date","numeric","date","numeric","date","numeric","date","date","date","text")) #reads data in sheet Other Samples of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

#create a single data frame and add the following columns of test results test_number,final_mass,mass_loss,CRI and CSR. 

all_data<- bind_rows(SR0204_data,SR0190_data,SR0170_data)%>% # binds four data frames SR0204_data, SR0190_data, SR0170_data into a single data frame and assigns the variable all_data
  mutate(test_number= row_number(), #Add a column that gives each line a unique identifier
         final_mass= `Weight+10mm` + `Weight-10mm`, # calculates a new column called final_mass by adding `Weight+10mm` and `Weight-10mm`columns.
         mass_loss=Weight_Out - final_mass, # calculates a new column called mass_loss by subracting Weight_out from final_mass
         CRI= (((Weight_In-Weight_Out)/Weight_In)*100), #calculates a new column called CRI using formula ((Weight_In - Weight_Out)/Weight_In) *100
         CSR= ((`Weight+10mm`/Weight_Out)*100), #calculates a new column called CSR by using formula (`Weight+10mm`/Weight_Out)*100
         PID_response= (time_return_range - Runtime_switch_CO2))%>%
   #view()#views the dataframe

#Filter all data to create a dataframe that contains just the 30 replicates that are the commissioning valaidation data and assigns to the validation_data table
validation_data <- filter(all_data,Validation == "Y")%>%
  #view()#views the data frame

#Save all_data drate frame as CSV in results folder
write_csv(all_data,"results/all_data_output.csv",na = "NA", append = FALSE, col_names = TRUE )

#Save validation_data data frame as CSV in results folder
write_csv(validation_data,"results/validation_data_output.csv",na = "NA", append = FALSE, col_names = TRUE )

#Determine mean CSR for each sample and assigns to mean_CSR
summary_CSR<-all_data%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR))

#Determine mean CRI for each sample and assigns to mean_CRI
summary_CRI<-all_data%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI))

#Determine mean CSR for autoamted runs for each sample and assigns to mean_auto_CSR
summary_auto_CSR<-all_data%>%
  filter(Mode == "Auto")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR))

#Determine mean CRI for automated runs for each sample and assigns to mean_auto_CRI
summary_auto_CRI<-all_data%>%
  filter(Mode == "Auto")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI))

#Determine mean CSR for manual runs for each sample and assigns to mean_man_CSR
summary_man_CSR<-all_data%>%
  filter(Mode == "Manual")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(), sd= sd(CSR))

#Determine mean CRI for manual runs for each sample and assigns to mean_man_CRI
summary_man_CRI<-all_data%>%
  filter(Mode == "Manual")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(), sd= sd(CRI))

#Using nested full joins CSR summary tables to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the select function to select and rename columns, also allows columns to be reordered based on the order they are listed in the select function.
CSR_Summary<- full_join(summary_auto_CSR, summary_CSR, by= "Sample") %>%
  full_join(., summary_man_CSR, by="Sample")%>%
    select(Sample, Mean_All= mean.y ,SD_All = sd.y,Mean_Auto = mean.x, SD_Auto =sd.x,  Mean_Man= mean, SD_Man = sd)

#Using nested full joins CRI summary tables to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the select function to select and rename columns, also allows columns to be reordered based on the order they are listed in the select function.
CRI_Summary<- full_join(summary_auto_CRI, summary_CRI, by= "Sample") %>%
  full_join(., summary_man_CRI, by="Sample")%>%
  select(Sample, Mean_All= mean.y ,SD_All = sd.y,Mean_Auto = mean.x, SD_Auto =sd.x,  Mean_Man= mean, SD_Man = sd)

#Save CSR_Summary data frame as CSV in results folder
write_csv(CSR_Summary,"results/CSR_Summary_output.csv",na = "NA", append = FALSE, col_names = TRUE )

#Save CRI_Summary data frame as CSV in results folder
write_csv(CSR_Summary,"results/CRI_Summary_output.csv",na = "NA", append = FALSE, col_names = TRUE )







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














