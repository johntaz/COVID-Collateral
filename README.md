# COVID-Collateral
Study investigating the indirect effects of COVID-19.

While a reduction in hospital activity during the COVID-19 pandemic has been well documented, there has been only limited small scale research on impacts on primary care. We explored the effects of the COVID-19 pandemic and its control on general practice consultations for adverse acute physical and mental health outcomes in England to inform public health planning and policy. 

Full paper at: [Link](https://doi.org/10.1016/S2589-7500(21)00017-0)

Shiny app at: [Link](https://a-henderson91.shinyapps.io/covid_collateral_shiny/)

## Authors
Kathryn E Mansfield, PhD\*, Rohini Mathur, PhD\*, John Tazare, MSc\*, Alasdair D Henderson, PhD\*, Amy Mulick, MSc\*, Helena Carreira, PhD, Anthony A Matthews, PhD, Patrick Bidulka, MSc, Alicia Gayle, MSc, Harriet Forbes, PhD, Sarah Cook, PhD, Angel YS Wong, PhD, Helen Strongman, PhD, Kevin Wing, PhD, Charlotte Warren-Gash, PhD, Sharon L Cadogan, PhD, Liam Smeeth, PhD, Joseph Hayes, PhD, Jennifer K Quint, PhD, Martin McKee, PhD, Sinéad M Langan, PhD

\* First authors

### Table of contents
- [Project folder structure](#project-folder-structure)
  + [Code](#code)
  + [Codelists](#codelists)
  + [Data](#data)
  + [Doc](#doc)
  + [Graphfiles](#graphfiles)
  
### Project folder structure

##### Code
- _Run all R code from `COVID-Collateral.Rproj`_
- `data_prep` takes raw CPRD data and aggregates into weekly number of outcomes and weekly denominators by strata. Note - data are held on separate secure server. 
- `its` Interrupted time series analysis code for Figure 3 and Table 3 and sensitivity analysis for diabetes consultations
- `Plot_code` Functions to plot weekly percentage of consultations by outcome (Figures 1 and 2, S2-S4)
- `Shiny_app` All code and data for the accompanying Shiny app 

##### Codelists
- Storage for finalised codelists used in the study for all conditions 

##### Data
- Summary analysis datasets for all conditions. Note data censored if weekly outcomes < 5. 

##### Doc
- Storage for any relevant documentation 
- [Getting Started](/doc/gettingStarted.md)
- Approved Independent Scientific Advisory Committee (ISAC) application (Word document)

##### Graphfiles
- Outputs from analysis code
