## Seagrant: Understanding climate impacts on the Maine coastal fish and invertrbrate community through synthesis of the Maine-New Hampshire Inshore Trawl Survey

### Project Description

#### From the proposal:

 > The overarching goal of this research is to synthesize data collected through the Maine-New Hampshire Inshore Trawl Survey to understand how climate change, fishing, and other environmental drivers are impacting key fish and invertebrate communities in coastal Maine waters.


### Objective 1

* Analyze changes in biodiversity in space and time and evaluate associations with environmental factors and fishing
* To be rerun with updated Maine DMR data

#### Analysis
* biodiversity metrics/ maps
* GAMMs

#### Data
* Will double check that all necessary data is available in data folder

#### Scripts
* See Ashley's code folder for scripts
* pulled relevant scripts from original Seagrant directory


### Objective 2

* Identify species groups and assess changes in habitat suitability if functional groups and communities in space and time

#### Analysis
* removed shrimp and other unimportant invertebrates from top 50 species analysis
* feeding guilds from [NOAA IEA definitions](https://noaa-edab.github.io/tech-doc/aggroups.html)
* stratified mean for NMDS convergence and simplified plotting using [NOAA IEA procedure](https://noaa-edab.github.io/tech-doc/inshoresurvdat.html#data-analysis-29)
* Bray-Curtis dissimilarity matrix for NMDS and anosim/adonis

#### Data
* raw data from [Maine DMR portal](https://mainedmr.shinyapps.io/MaineDMR_Trawl_Survey_Portal/) (MaineDMR_Trawl_Survey_Catch_Data_2021-05-14.csv)
* old data (full-me-dmr-expcatch.csv) 
* species with feeding guild designation (species_groups.csv)
* 4 community matrices for top 50 species and functional groups (biomass and abundance) - output from ME_trawl_NMDS_species_7.21.R and ME_trawl_NMDS_groups_7.21.R saved for future use in Rmarkdowns or NMDS so you don't have to run beginning cleaning code

#### Scripts
* set up with here function, so directory paths should work on any computer
* Scripts located in code subfolder of objective 2 folder
* Scripts that end in 7.21 are updated scripts from July 2021, the older versions are saved and have more versions of plots but are pretty messy 
* basic plotting that is not for NMDS in ME_trawl_plots.R
* old script (ME_trawl_NMDS.R) went with the old data and trying out different NMDS plots
* Pretty self explanatory anosim_adonis_analysis_7.21.R 
* NEFSC_NMDS_groups.R putting science center data into functional groups- not needed anymore
* All Rmarkdowns in subfolder 

### Objective 3

* Analyze joint distribution of key predator-prey species within the community, with a particular focus on lobster and cod
* contact Andrew Allyn (aallyn@gmri.org) for questions or code

### Objective 4

* Evaluate how ecosystem changes align with shifts in diversity and composition of fishery landings over time in ports along Maine's coastline

#### Analysis
* Biodiversity metrics for landings data (by county and grouped into year)
* calculated with catch weight not abundance
* richness, shannon-weiner diversity, simpson's evenness, average taxonomic distinctness
* compare to new trawl biodiversity metrics calculated as fall and spring combined and with weight for most relevant comparison to landings metrics
* Functional groups defined for landings and plots based on NOAA IEA feeding guilds
* want to look at trawl region definitions for landings comparison next

#### Data
* Landings data from [DMR portal](https://mainedmr.shinyapps.io/Landings_Portal/) 
* In data folder MaineDMR_Landings_Time_Series_Data_2021-07-09.csv
* County and species specific landings 2008-2020
*  Rob Watts (rob.watts@maine.gov) supplied older data (2006-2007)

#### Scripts
* fishery_landings.Rmd for landings biodiversity and functional group plots
* biodiversity.Rmd for new trawl biodiversity metrics calculated for comparison

### Contact

Jerelle Jesse (jjesse@gmri.org)
