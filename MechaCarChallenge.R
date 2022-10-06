library(tidyverse)
#Deliverable 1
#Import mpg data
MechaCar_mpg<- read.csv(file= "Resources/MechaCar_mpg.csv")
#Mpg Linear Regression
mpg_model <-lm(mpg~ vehicle_length + vehicle_weight + spoiler_angle +
     ground_clearance + AWD, data = MechaCar_mpg)
summary(mpg_model)

#Deliverable 2
#Import suspension coil data
MechaCar_sus_coil<- read.csv(file= "Resources/Suspension_Coil.csv")
#Create a summary of mean, median, variance, and std. dev. of PSI column
total_summary <- MechaCar_sus_coil%>%summarize(PSI_Mean=mean(PSI), 
                                               PSI_Median=median(PSI),
                                               PSI_Variance= var(PSI),
                                               PSI_Standard_Deviation= sd(PSI),
                                               .groups='keep')
#Create a summary by lot 
lot_summary<- MechaCar_sus_coil%>%group_by(Manufacturing_Lot)%>%
  summarize(PSI_Mean=mean(PSI), 
            PSI_Median=median(PSI),
            PSI_Variance= var(PSI),
            PSI_Standard_Deviation= sd(PSI),
            .groups='keep')

#Deliverable 3
#Perform a t-test to determine if the PSI across all lots is different from
#the population mean of 1,500 PSI
PSI_all_t_test<-t.test(MechaCar_sus_coil$PSI, mu=1500)
#Perform t-test to determine if the PSI for Lot 1 is different from pop.mean
PSI_lot1_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot1')$PSI, mu=1500)
#Perform t-test to determine if the PSI for Lot 2 is different from pop.mean
PSI_lot2_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot2')$PSI, mu=1500)
#Perform t-test to determine if the PSI for Lot 3 is different from pop.mean
PSI_lot3_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot3')$PSI, mu=1500)
