# Scaling of Mortality in 742 Metropolitan Areas of the Americas
Repository for the article _Scaling of Mortality in 742 Metropolitan Areas of the Americas_.

[![DOI](https://zenodo.org/badge/409335730.svg)](https://zenodo.org/badge/latestdoi/409335730)

* _Data_Management.R_: Contains data management for both mortality and population. 
* _Analysis.R_: Contains all analysis for the paper. 
* _MS38_HELPER_FUNCTIONS.R_: Contains a few helper functions.

The file _public_use_data.csv_: Contains a public-use dataset. Note that the following changes have been applied due to restrictions in data sharing agreements

* All death counts below 10 have been suppressed
* City names have been censored

This file contains the following columns:

* Country: BR (Brazil), US (USA), MX (Mexico), or LA (Argentina, Chile, Colombia, Costa Rica, El Salvador, Guatemala, Panama, and Peru)
* city_id: unique (dummy) city identifier. 
* type: type of cause of death (all-cause mortality, large groupings, small groupings)
* category: category of causes of death (all-cause mortality or CMNN/cancer/NCDs/injuries)
* name: specific cause of death
* deaths: number of deaths for 2012-2016 [except SV: 2010-2014] (corrected for undercounting in LA cities, with ill-defined causes redistributed. If <10, suppressed) 
* pop: total population for 2012-2016 [except SV: 2010-2014] 
* prop0: proportion of the population aged 0-14 for 2012-2016 [except SV: 2010-2014] 
* prop15: proportion of the population aged 15-39 for 2012-2016 [except SV: 2010-2014] 
* prop40: proportion of the population aged 40-64 for 2012-2016 [except SV: 2010-2014] 
* prop65: proportion of the population aged >=65 for 2012-2016 [except SV: 2010-2014] 
