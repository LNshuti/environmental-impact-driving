# Estimate the Market Size for Electic and Plug-In Hybrid Vehicles In Africa
The goal of this repository is to use open data repositories to answer the following questions: What is the estimated market size of electric and hybrid vehicles in Africa? What is the cost effectiveness of investing in electric vehicles as a public good to clean-up the air of all major African cities? What are environmental externalities generated by the extraction of natural resources such as **Cobalt and Lithium** to supply the appetite for batteries for electric vehicles?


**Definition of terms:**
------------------------
Trade balance = imports - exports. 

A country is said to have a trade *imbalance* if the country imports more than it exports.

- BEV = Battery Electric Vehicle
- MPG = Miles per gallon


#### Data Sources
We use the World Bank's World Development Indicator APIs to pull trade data from the World Bank database. Additionally, we rely on **International Trade Data** from the Harvard data-verse. This source is richer and has more detailed information related to the flow of goods and services between countries.  

Using the **wb_search()** function, we search for all indicators that are related to trade. Display some of them and their corresponding descriptions. Of the trade metrics found, exclude those without a valid description. For consistency, we limit ourselves to indicators that are measured in **US dollars.**  

##### Import required libraries
```
library(TSstudio)
library(plotly)
library(forecast)
library(finnts)
library(tidyverse)
```

#### Case Study: Rwanda

![](plots/rwanda_centrality.png)

According to a report by Bajpai and Bower from the International Growth Center, there were approximately 221,000 registered vehicles in Rwanda in 2020. The same study also states that the growth in vehicle ownership stood at 12% year-over-year. An older summary report by the Rwanda Bureau of Statistics reported the following trend in vehicle ownership: 

|Year    |Registred Vehicles   |% Growth| 
:---------|:-------------------|:-------|
|2011    |105545               |-       |
|2012    |125159               |18.6%   |
|2013    |136824               |9.3%    |
|2014    |149012               |8.9%    |
|2015    |166893               |12%     |
|2016    |183703               |10.1%   |

![](plots/auto_ownership_trend.png)

Rwanda can improve her trade balance by using fewer vehicles that consume petrol. We use empirical evidence to show that it is more cost effective to replace, albeit in a phased out manner that doesn't harm the national budget, the vehicle fleets belonging to all major government institutions.

Rwanda's key imports over time
![](plots/rwanda_imports.png)


# Application of Linear Optimization 
Determine optimal location for electric vehicle charging stations in Rwanda. 

![](plots/rwanda_geography.png)

We use this tutorial as a reference:

https://cran.r-project.org/web/packages/prioritizr/vignettes/prioritizr.html


#### References: 

Jitendra, B and Jonathan B (2020). A road map for e-mobility transition in Rwanda (Policy brief 200018).
  International Growth Centre.
  https://www.theigc.org/wp-content/uploads/2020/05/Bajpai-and-Bower-2020-policy-brief.pdf 
  
Jesse Piburn (2020). wbstats: Programmatic Access to the World Bank API. Oak Ridge
  National Laboratory. Oak Ridge, Tennessee. URL
  https://doi.org/10.11578/dc.20171025.1827
  
The Growth Lab at Harvard University. International Trade Data (SITC, Rev. 2).
  2019-05-31. 2019. V5. Harvard Dataverse. URL. https://doi.org/10.7910/DVN/H8SFD2. 
  doi/10.7910/DVN/H8SFD2

Brummitt, Charles (2018). Replication Data for: Machine-learned patterns suggest that
  diversification drives economic development. UNF:6:5S4h+uFVmGPqZ17LpYHC4Q==.
  doi = 10.7910/DVN/B0ASZU.https://doi.org/10.7910/DVN/B0ASZU
