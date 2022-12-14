# MechaCar_Statitstical_Analysis_With_R
# Overview 
## Purpose
At a hypothetical automobile company the newest car, MechaCar, is in production, but recent production troubles are blocking the manufacturing team's progress. Production data on MechaCar is available for [miles per gallon test results of 50 prototypes](Resources/MechaCar_mpg.csv) and [weight capacities of multiple suspension coils from multiple production lots](Resources/Suspension_Coil.csv). The goal of this project was to review production data for insights that may help the manufacturing team using R, a programing language used for statistical modeling and hypothesis testing. An analysis roadmap for this project is provided below.
## Analysis Roadmap
First, the production data was imported and read into R dataframes. Then, R built-in **stats** was used to gather statistical calculations and perform hypothesis testing. The the **tidyverse** package containing the **dplry** library was also imported and used in the second and third parts of the analysis to group data for statistical testing. The analysis was broken down into following four main parts:

1. **Linear Regression to Predict MPG**
  A multiple linear regression analysis was performed to identify which variables in the dataset predict the miles per gallon(mpg) variable of MechaCar   prototypes.
2. **Summary Statistics on Suspension Coils**
  Summary statistics were collected on the pounds per square inch (PSI) of the suspension coils from the manufacturing lots.
3. **T-Tests on Suspension Coils**
  T-tests were performed to determine if the manufacturing lots are statistically different from the mean population.
4. **Study Design: MechaCar vs Competition**
  A statistical study was designed quantifying vehicle performance of the MechaCar vehicles against vehicles from other manufacturers.

Results and a summary of results for the first three parts of the analysis are found under the corresponding subheading in the "Analysis" section. The code referenced in the "Analysis" section can be found in the [R script file in this respository](MechaCarChallenge.R).

# Analysis 
## Linear Regression to Predict MPG
### Results
The **lm() function** from the built-in R **stats** library can be used to perfrom linear regression on data to predict a continuous dependent variable based on an independent variable. In this part of the analysis, the data used was from a file containing [miles per gallon test results of 50 prototypes](Resources/MechaCar_mpg.csv). This data was loaded into an R dataframe named **"MechaCar_mpg"**. Then, multiple linear regression was performed with five independent variables to identify which of these variables could predict the dependent variable **mpg** (miles per gallon). The five independent variables used were **vehicle_length**, **vehicle_weight**, **spoiler_angle**, **ground_clearance**, and **AWD** (drive train). Therefore, to generate a linear regression model the following statement was used in R:
```
lm(formula = mpg~ vehicle_length + cehicle_weight + spoiler_angle + ground_clearance + AWD, data=MechaCar_mpg)
```
The output of this statement was the coefficients for each variable in the equation:

![mpg_coefficients](https://user-images.githubusercontent.com/104794100/194467377-d86dd774-b314-4884-af7f-d4e6ee723d5e.png)

These coefficients were used to create the linear regression model for the dataset: 

***mpg= 6.27(vehicle_length) + 0.00126(vehicle_weight) + 0.0688(spoiler_angle) + 3.55(ground_clearance) - 3.41(AWD) - 104***

A result summary was then generated by using the **summary() function** and passing the model as an argument. The output returned was as follows:

![Screen Shot 2022-10-07 at 12 23 57 AM](https://user-images.githubusercontent.com/104794100/194469469-fad4fbcc-fd70-41fe-866d-1b20156ca6dc.png)

### Summary of Results
The null hypothesis of a linear regression model proposes that the slope of the linear model is zero meaning dependent values are determined by random chance. The p-value generated from this model was 5.35e-11. Assuming a significance level 0.05 this p-value was far lesser than the significance level, so the null hypothesis was rejected.

The result output included a **Pr(|t|)** for each varaible. This value represents the probability that each coeffcient contributes a random amount of variance to the linear model. According to the results, **vehicle_weight** and **ground_clearance** have a significant impact on **mpg**, so they are statistically unlikely to provide random amounts of variance to the linear model. 

This model produced an r-squared value of 0.7149 meaning that about 71.5% of variability in mpg can be explained by the model.
An r-squared value of 0.7 or greater shows a generally high level of correlation between variables. Based on this model's r-squared value, I believe th model could do an above-average job when it comes to predicting miles per gallon MechaCar prototypes effectively. The **Intercept** was also shown to be statistically significant. Log transfoming the **vehicle_weight** and **ground_clearance** variables in the model caused a slight decrease in the r-squared, so  I believe there are other varaibles that can be included to bring th r-squared value closer to 1 and improve the model's predictive power.

## Summary Statistics on Suspension Coils
### Results
The **summarize() function** from the **dplyr** library can be used to summarize a group of data and generate a summary dataframe. In this part of the analysis, the data used was from a file containing [weight capacities of multiple suspension coils from multiple production lots](Resources/Suspension_Coil.csv). This data was loaded into an R dataframe named **"MechaCar_sus_coil"**. The goal for this section of analysis was to determine if the manufacturing process was consistent across production lots. The PSI of the suspensison coils for vehicles from each lot was recorded and the mean, median, variance, and standard deviation of the **PSI** column were calculated via the **mean()**, **median()**, **var()**, and **sd() function** then added to a data frame via the **summarize() function**. Then, with the additional use of the **group_by() function**, the same summary statistics were calculated for each of the three manufacturing lots.

The following statement created a summary dataframe named **"total_summary"** for the **"PSI"** column summary statistics for all lots:
```
total_summary <- MechaCar_sus_coil%>%summarize(PSI_Mean=mean(PSI), 
                                               PSI_Median=median(PSI),
                                               PSI_Variance= var(PSI),
                                               PSI_Standard_Deviation= sd(PSI),
                                               .groups='keep')
```
The R dataframe generated is shown below:

![PSI_summary](https://user-images.githubusercontent.com/104794100/194483748-fa6b01b7-4eac-4024-8308-0ade996195bf.png)

The following statement created a summary dataframe named **"lot_summary"** for the **"PSI"** column summary statistics per manufacturing lot by using the **group_by() function**:
```
lot_summary<- MechaCar_sus_coil%>%group_by(Manufacturing_Lot)%>%
  summarize(PSI_Mean=mean(PSI), 
            PSI_Median=median(PSI),
            PSI_Variance= var(PSI),
            PSI_Standard_Deviation= sd(PSI),
            .groups='keep')
```
The R dataframe generated is shown below:

![lot_summary](https://user-images.githubusercontent.com/104794100/194484111-699a7848-b01c-438a-ba52-ec16f7acdbe6.png)

### Summary of Results
The design specification for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 PSI. According to the PSI variance calculated in the **total_summary** dataframe (62 PSI) the manufacturing data met the design specifiction. However, when the PSI variance was calculated per manufacturing lot, Lots 1 and 2 met the design specification (0.98 PSI and 7.5 PSI, respectively), but Lot 3 did not meet the design specification because the calculated PSI variance from the manufacturing data was 170 PSI. 

## T-Tests on Suspension Coils
### Results 
The **t.test() function** from the **stats** library can be used to perform a one or two sample t-test and determine whether there is a statistical difference between the mean of a sample dataset and the potential population data or between two sample means. In this part of the analysis, the data used was from a file containing [weight capacities of multiple suspension coils from multiple production lots](Resources/Suspension_Coil.csv). This data was loaded into an R dataframe named **"MechaCar_sus_coil"**. The goal for this section of analysis was to perfrom four one-sample t-tests where the population mean was 1,500 PSI and determine if the PSI was statistically different. The first t-test compared the PSI across all manufacturing lots against the population mean. The additional three t-tests involved the usage of the **subset() argument** to compare the PSI for each manufacturing lot against the population mean. 

The following statement performed a t-test comparing the PSI across all manufacturing lots against the population mean:
```
PSI_all_t_test<-t.test(MechaCar_sus_coil$PSI, mu=1500)
```
The t-test results for a comparison across all lots:

![All](https://user-images.githubusercontent.com/104794100/194494439-d760c732-90a5-45f3-a87c-13cf9bffbfa2.png)

The following statement performed a t-test comparing the PSI from Manufacturing Lot 1 against the population mean:
```
PSI_lot1_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot1')$PSI, mu=1500)
```
The t-test results for a comparison with Manufacturing Lot 1:

![Lot1](https://user-images.githubusercontent.com/104794100/194494187-369743e6-aae6-4b30-bef9-dacd03725db9.png)

The following statement performed a t-test comparing the PSI from Manufacturing Lot 2 against the population mean:
```
PSI_lot2_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot2')$PSI, mu=1500)
```
The t-test results for a comparison with Manufacturing Lot 2:

![Lot2](https://user-images.githubusercontent.com/104794100/194493427-a0a8bef9-909e-4514-b19d-e1a5a50547f1.png)

The following statement performed a t-test comparing the PSI from Manufacturing Lot 3 against the population mean:
```
PSI_lot3_t_test<-t.test(subset(MechaCar_sus_coil, Manufacturing_Lot== 'Lot3')$PSI, mu=1500)
```
The t-test results for a comparison with Manufacturing Lot 3:

![Lot3](https://user-images.githubusercontent.com/104794100/194493997-bdd4163a-6aeb-4d29-9440-87568ebeb7cc.png)

### Summary of Results
The null hypothesis of a one-sample t-test proposes that there is no statistical difference between the observed sample mean and its presumed population mean. Assuming a significance level of 0.05, anything above would mean there is not sufficient evidence to reject the null hypothesis

The t-test performed comparing the PSI across all manufacturing lots against the population mean produced a p-value of 0.06, therefore, there was failure to reject the null hypothesis was no statistical difference between the sample mean and the population mean of 1,500 PSI.

The t-test performed comparing the PSI from Manufacturing Lot 1 against the population mean produced a p-value of 1, therefore, there was failure to reject the null hypothesis was no statistical difference between the sample mean and the population mean of 1,500 PSI.

The t-test performed comparing the PSI from Manufacturing Lot 2 against the population mean produced a p-value of 0.6, therefore, there was failure to reject the null hypothesis was no statistical difference between the sample mean and the population mean of 1,500 PSI.

The t-test performed comparing the PSI from Manufacturing Lot 3 against the population mean produced a p-value of 0.04, therefore, the null hypothesis was rejected and there was a statistical difference between the sample mean and the population mean of 1,500 PSI.

## Study Design: MechaCar vs Competition
I suggest a statistical study be performed on the safety rating of the MechaCar to qunatify how it perfroms against the competition. For this type of analysis, safety rating data will need to be generated on all 50 prototypes of the MechaCar. Additionally, satefy rating data of a similar sample size would need to be gathered on a similar car from the competitors. Similarity could be based on body features of the two cars. I believe a paired t-test would be the best statistical test to use to show this comparison and potentially convince a customer to invest in the MechaCar over the competition. Since sample data will be taken from two different populations there would need to be evidence of statistical difference between thr true mean difference of the paired samples. This would be best quantified via a paired t-test. The null hypothesis in this analysis would be that the difference between paired observations is equal to zero meaning there is no difference in saftey rating between the two car types.
