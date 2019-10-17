library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

library(lubridate) #open lubridate package to allow to work with date and time data

# create an XY plot for sample SR0204 CRI vs replicate, colour by mode, also want to add mean CSR
tidy_SR0204 <- validation%>% #NOTE THAT THIS WILL CHANGE WHEN THE LAST COL IS ADDED AND WILL BE CALLED tidy_NSC_data
  group_by(Sample)%>%
  filter(Sample=="SR0204")

mean_SRO204_CRI <- tidy_SR0204 %>%
  summarise(mean(CRI))



SR0204_CRI_plot <-
  
  ggplot(data=tidy_SR0204, # maps the sample data  to aesthetics 
                         mapping=aes(x= Replicate_Number,
                                     y= CRI,
                                     colour=Mode)) +
  geom_point(shape=2,size=2) + # applies the aesthetics to the geometry 
  geom_hline(yintercept= 45.33)+
  labs(title= "SR0204 CRI", x="Replicate")

ggplot(data=tidy_SR0204, # maps the sample data  to aesthetics 
       mapping=aes(x= Replicate_Number,
                   y= CRI,
                   colour=Mode)) +
  geom_point(shape=2,size=2) + # applies the aesthetics to the geometry 
  geom_hline(aes(yintercept= CRI,mean_SR0204_CRI)) +
  labs(title= "SR0204 CRI", x="Replicate")


