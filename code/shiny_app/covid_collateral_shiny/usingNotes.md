# Using this app
In the app you can display the weekly percentage of eligible people that contact primary care for 16 different health conditions between 2017 and 2020. 

You can choose whether to plot the overall time series or a breakdown of data by four different categories: age, region, sex and ethnicity. 

You can also see results from our interrupted time series analysis. 

**Important note**: if there were 5 or fewer contacts in a particular group in one week then we censored the data and assumed 5 contacts. This can make some time series appear volatile as they bounce between 0 and 5 contacts between weeks, however this was an essential step to protect anonymity in our data. 

## Primary Care Contacts
### Selecting the health conditions 
There is a tickbox for the 16 health conditions we studied. These are grouped according to biological systems: 

- **Endocrine, nutritional and metabolic diseases**: Diabetic Emergencies.
- **Mental, Behavioral and Neurodevelopmental disorders**: Acute Alcohol-Related Events, Anxiety, Depression, Eating Disorders, Obsessive Compulsive Disorder (OCD), Self-Harm, Serious Mental Illness.
- **Diseases of the circulatory system**: Cerebrovascular Accident, Heart Failure, Myocardial Infarction, Transient Ischaemic Attacks, Unstable Angina, Venous Thromboembolism.
- **Diseases of the respiratory system**: Asthma Exacerbations, Chronic Obstructive Pulmonary Disease (COPD) Exacerbations.

### Choose a category breakdown 
- Age 
- Region 
- Sex 
- Ethnicity 

### Choose the time period
The series runs from January 2017 to July 2020. You can select any dates within this range to display. The default is to show 2020 data only. 

### Display an indicator of when lockdown was announced? 
This is a simple tick box that puts a dotted line on the graphs marking 23 March 2020, the date of lockdown announcement in the UK. 

Why is this an option? In our study we found that primary contacts for most of these conditions were lower following the implementation of restrictions targeted at reducing COVID-19 transmission.

## Interrupted Time Series Analysis (ITS)
There are two main choices that determine the estimates of the effect of COVID-19 restrictions on primary care contact rates: (1) when to end to pre-restrictions period, (2) when to start the with-restrictions period. In our paper we chose to end the pre-restriction period on *1^{st} March 2020* because people had already changed their behaviour before lockdown was announced. We then excluded *3 weeks* of data as people adjusted to the imminent UK-wide lockdown and the new restrictions to control COVID-19. 

In this app you can vary both of these parameters to see what effect this has on our estimate of the size of the _reduction_ in primary care contacts and the subsequent _recovery_ in contact rates since the lockdown announcement. 
