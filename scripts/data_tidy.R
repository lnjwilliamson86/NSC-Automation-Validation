library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

#library(lubridate) #open lubridate package to allow to work with date and time data

library(tsibble)

library(anytime)

#Read data from data/comissioning_data.xlsx. This file contains four sheets each of which contain the data from a 
#single sample type. SR0204 is a high reactivty control sample, SR0190 is a low reactivity control sample SR0170 is a medium reactivity control sample and 
#Other are test samples that have been included in project but may not be used for assessing the effect of process
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

other_data<- read_excel("data/comissioning_data.xlsx",
                        sheet="Other",
                        col_names=TRUE,
                        col_types=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","date","numeric","date","numeric","date","numeric","date","date","date","text")) #reads data in sheet Other Samples of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

#create a single data frame and add the following columns of test results test_number,final_mass,mass_loss,CRI and CSR. 

all_data<- bind_rows(SR0204_data,SR0190_data,SR0170_data,other_data)%>% # binds four data frames SR0204_data, SR0190_data, SR0170 and other_data into a single data frame and assigns the variable all_data
  mutate(test_number= row_number(), #Add a column that gives each line a unique identifier
         final_mass= `Weight+10mm` + `Weight-10mm`, # calculates a new column called final_mass by adding `Weight+10mm` and `Weight-10mm`columns.
         mass_loss=Weight_Out - final_mass, # calculates a new column called mass_loss by subracting Weight_out from final_mass
         CRI= (((Weight_In-Weight_Out)/Weight_In)*100), #calculates a new column called CRI using formula ((Weight_In - Weight_Out)/Weight_In) *100
         CSR= ((`Weight+10mm`/Weight_Out)*100))%>% #calculates a new column called CSR by using formula (`Weight+10mm`/Weight_Out)*100
  view()#views the dataframe

#removing date information from the process data columns.Each test has assosicated process data of interst for this project is the time that it takes
#the sample temperature to return to the defined limit. The code/strategy for tidying has come from the following websites.  
#https://stackoverflow.com/questions/42996544/how-to-remove-the-date-from-a-column-containing-both-date-and-time-using-r
#http://www.datasciencemadesimple.com/drop-variables-columns-r-using-dplyr/

all_data_time_only<-all_data%>% # takes the dataframe all_data and assigns to validation data frame.
  separate(time_to_1100oC,into= c("date","time_to_1100oC"),sep=" ")%>% #separates col time_to_1100oC into 2 cols one called date and the other called time_to_1100oC
  separate(Runtime_switch_CO2, into= c("date_2","Runtime_switch_CO2"),sep=" ")%>% #separates col Runtime_switch_CO2 into 2 cols one called date and the other called Runtime_switch_CO2
  separate(runtime_max_temp_switch_CO2, into= c("date_3","runtime_max_temp_switch_CO2"),sep=" ")%>% #separates col runtime_max_temp_switch_CO2 into 2 cols one called date and the other called runtime_max_temp_switch_CO2
  separate(runtime_min_temp_switch_CO2, into= c("date_4","runtime_min_temp_switch_CO2"),sep=" ")%>% #separates col runtime_min_temp_switch_CO2 into 2 cols one called date and the other called runtime_min_temp_switch_CO2
  separate(time_return_range, into= c("date_5","time_return_range"),sep=" ")%>% #separates col time_return_range into 2 cols one called date and the other called time_return_range
  separate(total_runtime, into= c("date_6", "total_runtime"),sep= " ")%>% #separates col total_runtime into 2 cols one called date and the other called total_runtime
  select(-c(date,date_2,date_3,date_4,date_5,date_6))%>% # select function removes the added date-date_6 cols from the data frame. 
  as_hms(time_to_1100oC)
  view()#views the data frame

#Filter all data to create a dataframe that contains just the 30 replicates that are the commissioning valaidation data and assigns to the validation_data table
validation_data <- filter(all_data_time_only,Validation == "Y")%>%
 view()





#add a col to the dataframe using runtime_switch_Co2 and time_return_range to get PID_response_time using mutate. 
#validation %>%
  #mutate(PID_response= time_return_range - Runtime_switch_CO2 )%>%
  #view()



validation%>%
  int_length(start=time_return_range,end=Runtime_switch_CO2)%>%
  view()


int<-interval(start=hms(time_return_range),end=hms(Runtime_switch_CO2))
  int_length(int)%>%
  view()
  
int <- interval(start=ymd_hms("2019-01-01"),end=ymd_hms("2019-02-01"))
  
#https://rpubs.com/joelrudinas03/422499
#https://bookdown.org/mikemahoney218/LectureBook/working-with-dates-and-times.html

int_length(int)%>%
    view()
  
