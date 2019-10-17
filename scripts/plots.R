library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

library(lubridate) #open lubridate package to allow to work with date and time data

library(cowplot) #open cowplot package to create multipanel plots

# create an XY plot for sample SR0204 CRI vs replicate and CSRvs replicate, colour by mode, and a reference line that shows the mean of the dataset
#1. create a dataframe that just contains the data from sample SR0204 
tidy_SR0204 <- validation%>% #NOTE THAT THIS WILL CHANGE WHEN THE LAST COL IS ADDED AND WILL BE CALLED using tidy_NSC_data and assign to tidy_SR0204 data frame
  group_by(Sample)%>% #group by Sample
  filter(Sample=="SR0204") #filter for sample SR0204

#2. determine mean CRI & CSR for sample SR0204 ## Note that this is not needed to add the geom_hline in the XY Plots below. 
mean_CRI <- tidy_SR0204 %>%
  summarise(mean(CRI))

mean_CSR<-tidy_SR0204%>%
  summarise(mean(CSR))

#3. create the CRI vs replicate plot and assign to SR0204_CRI_plot
SR0204_CRI_plot <-ggplot(data=tidy_SR0204, 
                         mapping=aes(x= Replicate_Number,
                                     y= CRI,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  geom_hline(aes(yintercept=mean(CRI)), colour = "red") +
  labs(title= "High Reactivity (SR0204) CRI", x="Replicate")

#4. create the CSR vs replicate plot and assign to SR0201_CSR_plot
SR0204_CSR_plot <-ggplot(data=tidy_SR0204, 
                         mapping=aes(x= Replicate_Number,
                                     y= CSR,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  geom_hline(aes(yintercept=mean(CSR)), colour = "red") +
  labs(title= "High Reactivity (SR0204) CSR", x="Replicate")

#5. join the CRI and CSR plots and assign to high_reactivity_results_plot 
high_reactivity_results_plot<-plot_grid(SR0204_CRI_plot, SR0204_CSR_plot)

#6. Save all three figures to the figures folder in png format 
ggsave("figures/SR0204_CRI_plot.png",plot=SR0204_CRI_plot)
ggsave("figures/SR0204_CSR_plot.png",plot=SR0204_CSR_plot)
ggsave("figures/SR0204_results_plot.png",plot = high_reactivity_results_plot)






