library(tidyverse) #open tidyverse package

library(readxl) #open readxl package to allow excel files to be read in 

#library(lubridate) #open lubridate package to allow to work with date and time data

library(cowplot) #open cowplot package to create multipanel plots

# create an XY plot for sample SR0204 CRI vs replicate and CSRvs replicate, colour by mode, and a reference line that shows the mean of the dataset
#1. create a dataframe that just contains the data from sample SR0204 
tidy_SR0204 <- all_data_time_only%>% #NOTE THAT THIS WILL CHANGE WHEN THE LAST COL IS ADDED AND WILL BE CALLED using tidy_NSC_data and assign to tidy_SR0204 data frame
  group_by(Sample)%>% #group by Sample
  filter(Sample=="SR0204") #filter for sample SR0204

#2. determine mean CRI & CSR for sample SR0204 ## Note that this is not needed to add the geom_hline in the XY Plots below. 
mean_CRI_SR0204 <- tidy_SR0204 %>%
  summarise(mean(CRI))

mean_CSR_SR0204<-tidy_SR0204%>%
  summarise(mean(CSR))

#3. create the CRI vs replicate plot and assign to SR0204_CRI_plot
SR0204_CRI_plot <-ggplot(data=tidy_SR0204, 
                         mapping=aes(x= Replicate_Number,
                                     y= CRI,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CRI)), colour = "#EC519D") +
  labs(title= "High Reactivity (SR0204) CRI", x="Replicate")

#4. create the CSR vs replicate plot and assign to SR0201_CSR_plot
SR0204_CSR_plot <-ggplot(data=tidy_SR0204, 
                         mapping=aes(x= Replicate_Number,
                                     y= CSR,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2)+
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CSR)), colour = "#EC519D") +
  labs(title= "High Reactivity (SR0204) CSR", x="Replicate")

#5. join the CRI and CSR plots and assign to high_reactivity_results_plot 
high_reactivity_results_plot<-plot_grid(SR0204_CRI_plot, SR0204_CSR_plot)

#6. Save all three figures to the figures folder in png format 
ggsave("figures/SR0204_CRI_plot.png",plot=SR0204_CRI_plot)
ggsave("figures/SR0204_CSR_plot.png",plot=SR0204_CSR_plot)
ggsave("figures/SR0204_results_plot.png",plot = high_reactivity_results_plot)

# create an XY plot for sample SR0190 CRI vs replicate and CSRvs replicate, colour by mode, and a reference line that shows the mean of the dataset
#1. create a dataframe that just contains the data from sample SR0190 
tidy_SR0190 <- all_data_time_only%>% #NOTE THAT THIS WILL CHANGE WHEN THE LAST COL IS ADDED AND WILL BE CALLED using tidy_NSC_data and assign to tidy_SR0204 data frame
  group_by(Sample)%>% #group by Sample
  filter(Sample=="SR0190") #filter for sample SR0190

#2. determine mean CRI & CSR for sample SR0190 ## Note that this is not needed to add the geom_hline in the XY Plots below. 
mean_CRI_SR0190 <- tidy_SR0190 %>%
  summarise(mean(CRI))

mean_CSR-SR190<-tidy_SR0190%>%
  summarise(mean(CSR))

#3. create the CRI vs replicate plot and assign to SR0190_CRI_plot
SR0190_CRI_plot <-ggplot(data=tidy_SR0190, 
                         mapping=aes(x= Replicate_Number,
                                     y= CRI,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CRI)), colour = "#EC519D") +
  labs(title= "Low Reactivity (SR0190) CRI", x="Replicate")

#4. create the CSR vs replicate plot and assign to SR0190_CSR_plot
SR0190_CSR_plot <-ggplot(data=tidy_SR0190, 
                         mapping=aes(x= Replicate_Number,
                                     y= CSR,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CSR)), colour = "#EC519D") +
  labs(title= "Low Reactivity (SR0190) CSR", x="Replicate")

#5. join the CRI and CSR plots and assign to low_reactivity_results_plot 
low_reactivity_results_plot<-plot_grid(SR0190_CRI_plot, SR0190_CSR_plot)

#6. Save all three figures to the figures folder in png format 
ggsave("figures/SR0190_CRI_plot.png",plot=SR0190_CRI_plot)
ggsave("figures/SR0190_CSR_plot.png",plot=SR0190_CSR_plot)
ggsave("figures/SR0190_results_plot.png",plot = low_reactivity_results_plot)

# create an XY plot for sample SR0170 CRI vs replicate and CSRvs replicate, colour by mode, and a reference line that shows the mean of the dataset
#1. create a dataframe that just contains the data from sample SR0170 
tidy_SR0170 <- all_data_time_only%>% #NOTE THAT THIS WILL CHANGE WHEN THE LAST COL IS ADDED AND WILL BE CALLED using tidy_NSC_data and assign to tidy_SR0204 data frame
  group_by(Sample)%>% #group by Sample
  filter(Sample=="SR0170") #filter for sample SR0170

#2. determine mean CRI & CSR for sample SR0190 ## Note that this is not needed to add the geom_hline in the XY Plots below. 
mean_CRI_SR0170 <- tidy_SR0170 %>%
  summarise(mean(CRI))

mean_CSR-SR170<-tidy_SR0170%>%
  summarise(mean(CSR))

#3. create the CRI vs replicate plot and assign to SR0170_CRI_plot
SR0170_CRI_plot <-ggplot(data=tidy_SR0170, 
                         mapping=aes(x= Replicate_Number,
                                     y= CRI,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CRI)), colour = "#EC519D") +
  labs(title= "Medium Reactivity (SR0170) CRI", x="Replicate")

#4. create the CSR vs replicate plot and assign to SR0170_CSR_plot
SR0170_CSR_plot <-ggplot(data=tidy_SR0170, 
                         mapping=aes(x= Replicate_Number,
                                     y= CSR,
                                     colour=Mode)) +
  geom_point(shape="triangle",size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  geom_hline(aes(yintercept=mean(CSR)), colour = "#EC519D") +
  labs(title= "Medium Reactivity (SR0170) CSR", x="Replicate")

#5. join the CRI and CSR plots and assign to medium_reactivity_results_plot 
medium_reactivity_results_plot<-plot_grid(SR0170_CRI_plot, SR0170_CSR_plot)

#6. Save all three figures to the figures folder in png format 
ggsave("figures/SR0170_CRI_plot.png",plot=SR0170_CRI_plot)
ggsave("figures/SR0170_CSR_plot.png",plot=SR0170_CSR_plot)
ggsave("figures/SR0170_results_plot.png",plot = medium_reactivity_results_plot)

#create a CSR vs CRI plot for the 30 replicates that are the validation tests. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_validation_only <- ggplot (data=validation_data, 
                        mapping=aes(x= CRI,
                                    y= CSR,
                                    colour=Sample)) +
  geom_point(shape="triangle",size=2) + 
  geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
  labs(title= "CRI vs CSR for Automated Temperature Control Comissioning")
  
ggsave("figures/CRI_CSR_plot_validation_only.png",plot=CRI_CSR_plot_validation_only)

#create a CSR vs CRI plot for all data shape by method. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_all <- ggplot (data=all_data_time_only, 
                        mapping=aes(x= CRI,
                                    y= CSR,
                                    colour=Mode)) +
  geom_point(aes(shape=Sample),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  theme_bw()
  #geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
  #labs(title= "CRI vs CSR for Control Samples")

ggsave("figures/CRI_CSR_plot_all.png",plot=CRI_CSR_plot_all)

#create a CSR vs CRI plot for all data shape by mode colour by operator. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_all_operator <- ggplot (data=all_data_time_only, 
                            mapping=aes(x= CRI,
                                        y= CSR,
                                        colour=Operator)) +
  geom_point(aes(shape=Mode),size=2) + 
  geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
  labs(title= "CRI vs CSR for Control Samples")

ggsave("figures/CRI_CSR_plot_all_operator.png",plot=CRI_CSR_plot_all_operator)




#create a CSR vs CRI plot for SR0204 data only. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_SR0204 <- ggplot (data=tidy_SR0204, 
                            mapping=aes(x= CRI,
                                        y= CSR,
                                        colour=Mode)) +
  geom_point(size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  theme_bw()
#geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
#labs(title= "CRI vs CSR for Control Samples")

ggsave("figures/CRI_CSR_plot_SR0204.png",plot=CRI_CSR_plot_SR0204)



#create a CSR vs CRI plot for SR0170 data only. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_SR0170 <- ggplot (data=tidy_SR0170, 
                               mapping=aes(x= CRI,
                                           y= CSR,
                                           colour=Mode)) +
  geom_point(size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  theme_bw()
#geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
#labs(title= "CRI vs CSR for Control Samples")

ggsave("figures/CRI_CSR_plot_SR0170.png",plot=CRI_CSR_plot_SR0170)

#create a CSR vs CRI plot for SR0190 data only. The geom_abline function plots the relationship from https://doi.org/10.1080/03019233.2018.1486795 CSR=100.34-1.34*CRI
CRI_CSR_plot_SR0190 <- ggplot (data=tidy_SR0190, 
                               mapping=aes(x= CRI,
                                           y= CSR,
                                           colour=Mode)) +
  geom_point(size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9"))+
  theme_bw()
#geom_abline(aes(intercept=100.34,slope=-1.34), colour = "#EC519D") +
#labs(title= "CRI vs CSR for Control Samples")

ggsave("figures/CRI_CSR_plot_SR0190.png",plot=CRI_CSR_plot_SR0190)



# Create a plot to show time to return to range vs CRI/CSR a shape by mode colour by sample. note that CRI plot has legend removed so that it can be joinde with the CSR plot
PID_CRI_plot <- ggplot (data=all_data, 
                                     mapping=aes(x= PID_response_sec,
                                                 y= CRI,
                                                 colour=Sample)) +
  geom_point(aes(shape=Mode),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9","#75767A"))+
  theme_bw()+
  theme(legend.position = "none")+
  labs(x="Time to return to range (secs)")
ggsave("figures/PID_CRI_plot.png",plot=PID_CRI_plot)
 

PID_CSR_plot <-ggplot (data=all_data, 
        mapping=aes(x= PID_response_sec,
                    y= CSR,
                    colour=Sample)) +
  geom_point(aes(shape=Mode),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9","#75767A"))+
  theme_bw()+
  labs(x="Time to return to range (secs)")
ggsave("figures/PID_CSR_plot.png",plot=PID_CSR_plot)

#join PID response plots together 
PID_plot<-plot_grid(PID_CRI_plot, PID_CSR_plot, rel_widths = c(0.75,1))
ggsave("figures/PID_plot.png",plot=PID_plot)  

# create plots looking at furnace recovery time and CSR/CRI. 
Furnace_response_plot <-ggplot (data=filter(all_data,Mode=="Auto"), 
        mapping=aes(x= PID_response_sec,
                    y= Furnace,
                    colour=Sample)) +
  geom_point(aes(shape=Mode),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9","#75767A"))+
  scale_y_continuous(breaks= c(1,2,3))+
  theme_bw()+
  labs(x="Time to return to range (secs)")
ggsave("figures/Furnace_response_plot.png",plot=Furnace_response_plot)

Furnace_CRI_plot <-ggplot (data=filter(all_data,Mode=="Auto"), 
                                mapping=aes(x= CRI,
                                            y= Furnace,
                                            colour=Sample)) +
  geom_point(aes(shape=Mode),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9","#75767A"))+
  scale_y_continuous(breaks= c(1,2,3))+
  theme_bw()
ggsave("figures/Furnace_CRI_plot.png",plot=Furnace_CRI_plot)

Furnace_CSR_plot <-ggplot (data=filter(all_data,Mode=="Auto"), 
                           mapping=aes(x= CSR,
                                       y= Furnace,
                                       colour=Sample)) +
  geom_point(aes(shape=Mode),size=2) + 
  scale_colour_manual(values= c("#001932", "#00B1D9","#75767A"))+
  scale_y_continuous(breaks= c(1,2,3))+
  theme_bw()
ggsave("figures/Furnace_CSR_plot.png",plot=Furnace_CSR_plot)

#box and wiskerplots for CRI and CSR for SR0204
SR0204_CRI_BWplot <- ggplot(data= filter(all_data,Sample=="SR0204"),
                          mapping=aes(x=Mode,
                                      y=CRI,
                                      fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0204_CRI_BWplot.png",plot=SR0204_CRI_BWplot)

SR0204_CSR_BWplot <- ggplot(data= filter(all_data,Sample=="SR0204"),
                            mapping=aes(x=Mode,
                                        y=CSR,
                                        fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0204_CSR_BWplot.png",plot=SR0204_CSR_BWplot)

#box and wiskerplots for CRI and CSR for SR0190
SR0190_CRI_BWplot <- ggplot(data= filter(all_data,Sample=="SR0190"),
                            mapping=aes(x=Mode,
                                        y=CRI,
                                        fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0190_CRI_BWplot.png",plot=SR0190_CRI_BWplot)

SR0190_CSR_BWplot <- ggplot(data= filter(all_data,Sample=="SR0190"),
                            mapping=aes(x=Mode,
                                        y=CSR,
                                        fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0190_CSR_BWplot.png",plot=SR0190_CSR_BWplot)

#box and wiskerplots for CRI and CSR for SR0170
SR0170_CRI_BWplot <- ggplot(data= filter(all_data,Sample=="SR0170"),
                            mapping=aes(x=Mode,
                                        y=CRI,
                                        fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0170_CRI_BWplot.png",plot=SR0170_CRI_BWplot)

SR0170_CSR_BWplot <- ggplot(data= filter(all_data,Sample=="SR0170"),
                            mapping=aes(x=Mode,
                                        y=CSR,
                                        fill=Mode))+
  geom_boxplot()+
  scale_fill_manual(values= c("#00B1D9","#75767A"))+
  theme_bw()
ggsave("figures/SR0170_CSR_BWplot.png",plot=SR0170_CSR_BWplot)

#create grid plots for CRI and CSR
CSR_BWplot<- plot_grid(SR0204_CSR_BWplot,SR0170_CSR_BWplot,SR0190_CSR_BWplot)
ggsave("figures/CSR_BWplot.png",plot=CSR_BWplot)

CRI_BWplot<- plot_grid(SR0204_CRI_BWplot,SR0170_CRI_BWplot,SR0190_CRI_BWplot)
ggsave("figures/CRI_BWplot.png",plot=CRI_BWplot)


