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

#create a single data frame and add an identifier column,. 

all_data<- bind_rows(SR0204_data,SR0190_data,other_data)%>% # binds three data frames SR0204_data, SR0190_data and other_data into a single data frame and assigns the variable all_data
  mutate(test_number= c(1:81))%>% #Add a column that gives each line a unique identifier
  #add a column for final mass
  #add a column for mass loss
  #add a column for CRI
  #add a column for CSR
  

  
