library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

library(lubridate) #open lubridate package to allow to work with date and time data

#Read data from data/comissioning_data.xlsx. This file contains three sheets each of which contain the data from a 
#single sample type. SR0204 is a high reactivty control sample, SR0190 is a low reactivity control sample and 
#Other are test samples that have been included in project but may not be used for assessing the effect of process
#automation on test results.   

SR0204_data<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0204",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) #reads data in sheet SR0204 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

SR0190_data<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0190",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) #reads data in sheet SR0190 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

other_data<- read_excel("data/comissioning_data.xlsx",
                        sheet=3,
                        col_names=TRUE,
                        col_types=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) #reads data in sheet Other Samples of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.

#create a single data frame and add the following columns test_number,final_mass,mass_loss,CRI and CSR. 

all_data<- bind_rows(SR0204_data,SR0190_data,other_data)%>% # binds three data frames SR0204_data, SR0190_data and other_data into a single data frame and assigns the variable all_data
  mutate(test_number= c(1:81), #Add a column that gives each line a unique identifier
         final_mass= `Weight+10mm` + `Weight-10mm`, # calculates a new column called final_mass by adding `Weight+10mm` and `Weight-10mm`columns.
         mass_loss=Weight_Out - final_mass, # calculates a new column called mass_loss by subracting Weight_out from final_mass
         CRI= (((Weight_In-Weight_Out)/Weight_In)*100), #calculates a new column called CRI using formula ((Weight_In - Weight_Out)/Weight_In) *100
         CSR= ((`Weight+10mm`/Weight_Out)*100)) #calculates a new column called CSR by using formula (`Weight+10mm`/Weight_Out)*100
  
  
SR0204_data_test<- read_excel("data/comissioning_data.xlsx",
                         sheet="SR0204",
                         col_names=TRUE,
                         col_type=c("text","text","text","date","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")) #reads data in sheet SR0204 of the file comissioning_data.xlsx, column types have been specified to facilitate binding rows.
   
 
  
  
  

  
