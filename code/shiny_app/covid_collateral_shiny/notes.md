# About the app 
This app accompanies a paper entitled _COVID-19 collateral: Indirect acute effects of the pandemic on physical and mental health in the UK_.

The paper can be accessed here: [link](http://www.twitter.com)

You can use this app to examine the data used in this analysis from 2017 to 2020. We analysed primary care records from approximately 10 million people across England and Northern Ireland. We wanted to analyse whether people contacted primary care less because of the indirect effects of the COVID-19 pandemic.

_Last updated 22/10/2020_

## Authors
Kathryn E Mansfield, PhD\*, Rohini Mathur, PhD\*, John Tazare, MSc\*, Alasdair D Henderson, PhD\*, Amy Mulick, MSc\*, Helena Carreira, PhD, Anthony A Matthews, PhD, Patrick Bidulka, MSc, Alicia Gayle, MSc, Harriet Forbes, PhD, Sarah Cook, PhD, Angel YS Wong, PhD, Helen Strongman, PhD, Kevin Wing, PhD, Charlotte Warren-Gash, PhD, Sharon L Cadogan, PhD, Liam Smeeth, PhD, Joseph Hayes, PhD, Jennifer K Quint, PhD, Martin McKee, PhD, Sinéad M Langan, PhD

\* Contributed equally

## Acknowledgments
This study is based in part on data from the Clinical Practice Research Datalink obtained under licence from the UK Medicines and Healthcare products Regulatory Agency. The data is provided by patients and collected by the NHS as part of their care and support. The interpretation and conclusions contained in this study are those of the authors alone. The study was approved by the Independent Scientific Advisory Committee (Protocol number: 20_089R2).

## Code
All analysis code and data are available at [GitHub](https://github.com/johntaz/COVID-Collateral)

## Contact
Corresponding author: Kathryn Mansfield [email](mailto:kathryn.mansfield@lshtm.ac.uk)
About the app: Alasdair Henderson, [email](mailto:alasdair.henderson1@lshtm.ac.uk)

## Data source
**Important note**: if there were 5 or fewer contacts in a particular group in one week then we censored the data and assumed 5 contacts. This can make some time series appear volatile as they bounce between 0 and 5 contacts between weeks, however this was an essential step to protect anonymity in our data. 

The Clinical Practice Research Datalink (CPRD) Aurum includes de-identified routinely collected primary care health record data from participating general practices covering 13% of the English population and Northern Ireland from 2019. The data are broadly representative of the UK population with respect to age, sex, ethnicity, and geographic region.^[1]

^[1] Wolf A, Dedman D, Campbell J, et al. Data resource profile: Clinical Practice Research Datalink (CPRD) Aurum. International Journal of Epidemiology 2019; : 1–8.

## Strengths, limitations, assumptions & detail on the populations
### Strengths & limitations
We performed a rapid assessment of changes in primary care contacts following the introduction of UK-population-wide restrictions up to July 2020 in a large representative sample of the UK population. Historical data allowed us to compare observed patterns in 2020 to trends in the previous 3-years. We estimated relative and absolute changes in contact patterns, with a focus on easy to interpret measures. 

Our study describes and quantifies the reduction in primary care contacts across a wide range of health conditions likely to be affected by COVID-19 to generate hypotheses. Further research is needed to understand the specific drivers behind these changes. It is important that we understand what happened to individuals who did not consult their GP. Specifically, were they treated in secondary care or did they self-manage, and how much of our findings can be explained by genuine changes in disease incidence?

Without hospital and mortality data, we are unable to investigate whether, for example, any reduction in GP contacts resulted in corresponding increases in hospital attendances or deaths. We focused on studying any record of study conditions, hence, our results reflect all primary care contacts, including diagnoses recorded by general practice staff from hospital discharge letters.

### Assumptions 
The table below lays out details of the study populations and how a "contact" was defined for each condition. We used dynamic denominator populations that were updated weekly and were specific to each condition. For example, we assumed that anyone aged over 10 years old could contact primary care for mental health conditions. However we only counted diabetic emergencies amongst those who have been diagnosed with diabetes before.

**Condition** | **Denominator population** | **Condition definition**
------------- | ------------------- | ------------------------------
**Diabetes**  |  | 
Diabetic emergencies | All individuals (aged ≥11 years) with prevalent diagnoses of diabetes mellitus at the start of each week of followup. Individuals contributed to the study population from the latest of the start of followup in the overall population and the date of their first record indicating a diagnosis of diabetes. | Any record of diabetesrelated hyperglycaemia, hypoglycaemia, ketoacidosis, or diabetic coma. Multiple records occurring within **seven days** of each other were considered as representing the same event.
**Alcohol** | | 
Acute alcohol-related event | All adults (aged ≥18 years) | Any record for acute physical or psychological alcoholrelated event, including acute alcoholic pancreatitis, Multiple records occurring within **14 days** of each other were considered as representing the same event. 
**Mental health** | |
Anxiety | All individuals (aged ≥11 years) from the overall study population. | Any record of symptoms or diagnoses of: social phobia, agoraphobia, panic, generalized anxiety disorder, and mixed anxiety and depression. Multiple records occurring within **seven days** of each other were considered as representing the same event.
 |  | 
Depression | All children (aged ≥11 years) and adults (aged ≥18) from the overall study population. | Any record of major depressive disorder, dysthymia, mixed anxiety and depression, and adjustment disorders with depressed mood. We also included codes for depressive symptoms. Multiple records occurring within **seven days** of each other were considered as representing the same event.
 |  | 
Selfharm | All children (aged ≥11 years) and adults (aged ≥18 years) from the overall study population. | Selfharm defined as records that indicated explicit or undertermined intention to selfharm, nonsuicidal or suicidal selfharm (including overdoses with drugs commonly implicated in suicide, e.g. paracetamol). Multiple records occurring within **seven days** of each other were considered as representing the same event.
 |  | 
Serious mental illness | All children (aged ≥11 years) and adults (aged ≥18 years) from the overall study population. | Severe mental illness included diagnoses of schizophrenia and other psychotic disorders, and bipolar disorders. Multiple records occurring within **seven days** of each other were considered as representing the same event.
 |  | 
Eating disorders | All children (aged ≥11 years) and adults (aged ≥18 years) from the overall study population. | Eating disorders included anorexia nervosa, bulimia nervosa, and other specified feeding and eating disorders. Multiple records occurring within **seven days** of each other were considered as representing the same event.
 |  | 
Obsessive compulsive disorder | All children (aged 517 years) and adults (aged ≥18 years) from the overall study population. | Obsessive compulsive disorder was defined by codes for body dysmorphic disorders, hypochondriasis, hoarding disorder, and body focused repetitive behaviour disorders. Multiple records occurring within **seven days** of each other were considered as representing the same event.
**Cardiovascular** | |
Myocardial infarction | All adults (aged ≥31 years) | Any record for myocardial infarction allowing for a **1year** window between successive records. Multiple records occurring within one year of each other were considered as representing the same event.
 |  | 
Unstable angina | All adults (aged ≥31years) | Any record for unstable angina, allowing for a **6month** window between successive records. Multiple records occurring within six months of each other were considered as representing the same event.
 |  | 
Transient ischaemic attacks | All adults (aged ≥31 years) | Any record for transient ischaemic, allowing for a **6month** window between successive records. Multiple records occurring within six months of each other were considered as representing the same events. 
 |  | 
Cerebrovascular accident | All adults (aged ≥31 years) | Any record for cerebrovascular accidents, allowing for a **1year** window between successive records. Multiple records occurring within one year of each other were considered as representing the same event.
 |  | 
Cardiac failure | All adults (aged ≥31 years) | Given the complexity with capturing acute events for a chronic condition, we only counted an individual&#39;s **first ever diagnosis** with cardiac failure.
 |  | 
Venous thromboembolism (pulmonary embolism and deep venous thrombosis) | All adults (aged ≥31 years) | Any record for venous thromboembolism, allowing for a **1year** window between successive records. Multiple records occurring within one year of each other were considered as representing the same event.
| **Respiratory** | |
Asthma exacerbations | All individuals (aged ≥11 years) with a current asthma diagnosis (i.e. asthma code in the last two or three years if aged ≤18 years or 18+ years, respectively). Individuals joined the study population from the start of followup in the overall population if there was a current asthma diagnosis (i.e. within last 23 years) at this time or from the date of their first record indicating an asthma diagnosis within overall followup. Participants remained in the study until there was no current asthma diagnosis or the end of overall followup. They were able to reenter the study if there was a later diagnostic code for asthma before the end of overall followup. Following an existing definition, individuals 41 years and over with asthma were considered as likely to have COPD (and therefore not included in the asthma study population denominator) if they had a subsequent COPD diagnosis recorded within the two years following the current asthma record.^[1] | Asthma exacerbations were defined as records for morbidity codes for asthma exacerbations and status asthmaticus, and a primary care prescription for an oral corticoseroid.^[2] Multiple records occurring within **14 days** of each other were considered as representing the same event.
 |  | 
COPD exacerbations | Adults (aged ≥41 years) with an established diagnosis of COPD and evidence of a smoking history.^[3] Individuals joined the study population from the latest of the start of followup in the overall population and the date of their first record indicating diagnosis of COPD. | Exacerbations of COPD were defined using morbidity codes in individuals with existing COPD for COPD exacerbations, lower respiratory tract infections, breathlessness or sputum production, and a new prescription for an oral corticosteroid or antibiotic.^[4] Multiple records occurring within **14 days** of each other were considered as representing the same event.
<img width=100/> | <img width=400/> | <img width=800/>
	
^[1] Bloom CI, Nissen F, Douglas IJ, Smeeth L, Cullinan P, Quint JK. Exacerbation risk and characterisation of the UK's asthma population from infants to old age. Thorax 2018; 73: 313–20.

^[2] Bloom CI, Palmer T, Feary J, Quint JK, Cullinan P. Exacerbation patterns in adults with Asthma in England A population-based study. Am J Respir Crit Care Med 2019; 199: 446–53.

^[3] Quint JK, Müllerova H, DiSantostefano RL, et al. Validation of chronic obstructive pulmonary disease recording in the Clinical Practice Research Datalink (CPRD-GOLD). BMJ Open 2014; 4: 1–8.

^[4] Rothnie KJ, Müllerová H, Hurst JR, et al. Validation of the recording of acute exacerbations of COPD in UK primary care electronic healthcare records. PLoS One 2016; 11: 1–14.

## Declaration of interests
MM is a member of Independent SAGE.

## Funding
SML is funded by a Wellcome Trust Senior Clinical Fellowship (205039/Z/16/Z). MM is Research Director of the European Observatory on Health Systems and Policies. AYSW is funded by a BHF Immediate Postdoctoral Basic Science Research Fellowship (EPNCZQ52). JFH is supported by the Wellcome Trust (211085/Z/18/Z), the University College London Hospitals NIHR Biomedical Research Centre and the NIHR North Thames Applied Research Collaboration. CWG is funded by a Wellcome Intermediate Clinical Fellowship (201440/Z/16/Z).