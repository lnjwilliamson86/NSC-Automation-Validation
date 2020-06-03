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




##Determine Summary Statistics for each sample and join into tables for export. 
#Determine mean CSR for each sample and assigns to mean_CSR
summary_CSR<-all_data%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine mean CRI for each sample and assigns to mean_CRI
summary_CRI<-all_data%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))

#Determine mean CSR for autoamted runs for each sample and assigns to mean_auto_CSR
summary_auto_CSR<-all_data%>%
  filter(Mode == "Auto")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine mean CRI for automated runs for each sample and assigns to mean_auto_CRI
summary_auto_CRI<-all_data%>%
  filter(Mode == "Auto")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))

#Determine mean CSR for manual runs for each sample and assigns to mean_man_CSR
summary_man_CSR<-all_data%>%
  filter(Mode == "Manual")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CSR), n=n(), sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine mean CRI for manual runs for each sample and assigns to mean_man_CRI
summary_man_CRI<-all_data%>%
  filter(Mode == "Manual")%>%
  group_by(Sample)%>%
  summarise(mean= mean(CRI), n=n(), sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))




#Using nested full joins CSR summary tables to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the select function to select and rename columns, also allows columns to be reordered based on the order they are listed in the select function.
CSR_Summary<- full_join(summary_auto_CSR, summary_CSR, by= "Sample") %>%
  full_join(., summary_man_CSR, by="Sample")%>%
    select(Sample, Mean_All= mean.y ,Median_All= median.y, SD_All = sd.y, var_All = var.y, IQR_All = IQR.y, n_All= n.y, 
           Mean_Auto = mean.x, Median_Auto= median.x, SD_Auto =sd.x, var_Auto = var.x, IQR_Auto = IQR.x, n_Auto= n.x, 
           Mean_Man= mean, Median_Man= median, SD_Man = sd, var_Man = var, IQR_Man = IQR, n_Man=n)

#Using nested full joins CRI summary tables to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the select function to select and rename columns, also allows columns to be reordered based on the order they are listed in the select function.
CRI_Summary<- full_join(summary_auto_CRI, summary_CRI, by= "Sample") %>%
  full_join(., summary_man_CRI, by="Sample")%>%
  select(Sample, Mean_All= mean.y ,Median_All= median.y, SD_All = sd.y, var_All = var.y, IQR_All = IQR.y, n_All= n.y, 
         Mean_Auto = mean.x, Median_Auto= median.x, SD_Auto =sd.x, var_Auto = var.x, IQR_Auto = IQR.x, n_Auto= n.x, 
         Mean_Man= mean, Median_Man= median, SD_Man = sd, var_Man = var, IQR_Man = IQR, n_Man=n)

#Save CSR_Summary data frame as CSV in results folder
write_csv(CSR_Summary,"results/CSR_Summary_output_by_sample.csv",na = "NA", append = FALSE, col_names = TRUE )

#Save CRI_Summary data frame as CSV in results folder
write_csv(CRI_Summary,"results/CRI_Summary_output_by_sample.csv",na = "NA", append = FALSE, col_names = TRUE )



##repeat of above to provide summary table for all samples 
#Determine summary statistics for CSR of all samples  and assigns to summary_CSR_all
summary_CSR_all<-all_data%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine summary CRI for all sample and assigns to summary_CRI_all
summary_CRI_all<-all_data%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))

#Determine Summary CSR for autoamted runs for each sample and assigns to summary_auto_CSR_all
summary_auto_CSR_all<-all_data%>%
  filter(Mode == "Auto")%>%
  summarise(mean= mean(CSR), n=n(),sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine Summary CRI for automated runs for each sample and assigns to summary_auto_CRI_all
summary_auto_CRI_all<-all_data%>%
  filter(Mode == "Auto")%>%
  summarise(mean= mean(CRI), n=n(),sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))

#Determine Summary CSR for manual runs for each sample and assigns to summary_man_CSR_all
summary_man_CSR_all<-all_data%>%
  filter(Mode == "Manual")%>%
  summarise(mean= mean(CSR), n=n(), sd= sd(CSR),var= var(CSR), median =median(CSR), IQR= IQR(CSR))

#Determine summary CRI for manual runs for each sample and assigns to summary_man_CRI_all
summary_man_CRI_all<-all_data%>%
  filter(Mode == "Manual")%>%
  summarise(mean= mean(CRI), n=n(), sd= sd(CRI),var= var(CRI), median =median(CRI), IQR= IQR(CRI))


#Using nested full joins CSR summary tables for all samples  to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the add_coloumn fucntion to albel the rows
CSR_Summary_all<- full_join(summary_auto_CSR_all, summary_CSR_all,by= NULL) %>%
  full_join(., summary_man_CSR_all, by= NULL)%>%
  add_column(.,mode=c("auto","all","man"),.before =1)

#Using nested full joins CRI summary tables for all samples to a single data frame https://stackoverflow.com/questions/32066402/how-to-perform-multiple-left-joins-using-dplyr-in-r
#also uses the select function to select and rename columns, also allows columns to be reordered based on the order they are listed in the select function.
CRI_Summary_all<- full_join(summary_auto_CRI_all, summary_CRI_all, by= NULL) %>%
  full_join(., summary_man_CRI_all, by=NULL)%>%
  add_column(.,mode=c("auto","all","man"),.before =1)

  
#Save CSR_Summary data frame as CSV in results folder
write_csv(CSR_Summary_all,"results/CSR_Summary_output.csv",na = "NA", append = FALSE, col_names = TRUE )

#Save CRI_Summary data frame as CSV in results folder
write_csv(CRI_Summary_all,"results/CRI_Summary_output.csv",na = "NA", append = FALSE, col_names = TRUE )




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














