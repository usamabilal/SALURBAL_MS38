rm(list=ls())
library(readxl)
library(vroom)
library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
source("MS38_HELPER_FUNCTIONS.R")
t3<-read_excel("Other_data/tier3_newproposal_v2.xlsx")
t3<-t3 %>% select(cause_code, destination_code, name) %>% 
  rename(icd10_t3_red=cause_code,final_code=destination_code)

# creating 4 US files: mortality, total pop, population by age, and ill-defined diseases (R codes)
files<-list.files("../US_DATA/Mortality", full.names = T)
files<-files[grepl(paste(2012:2016, collapse="|"), files)]
#file<-files[[1]]
mortality_us<-map(files,function(file){
  temp<-vroom(file) %>% 
    mutate(icd10_t3_red=as.numeric(icd10_t3_red))
  temp2<-temp %>% filter(icd10_t3%in%c(1610, 1620)) %>% select(death_year, res_cbsa)
  temp2<-temp2 %>% group_by(death_year, res_cbsa) %>% 
    summarise(deaths=n()) %>% 
    rename(year=death_year, cbsa=res_cbsa) %>% filter(!is.na(cbsa)&!is.na(year))
  temp2<-temp2 %>% group_by(year, cbsa) %>% 
    summarise(ill_defined_diseases=sum(deaths))
  temp<-temp %>% select(death_year, res_cbsa, icd10_t3_red) %>% left_join(t3)
  temp<-temp %>% group_by(death_year, res_cbsa, final_code) %>% 
    summarise(deaths=n()) %>% 
    rename(year=death_year, cbsa=res_cbsa) %>% filter(!is.na(cbsa)&!is.na(year))
  # remove deaths outside of UAs
  temp<-temp %>% group_by(year, cbsa, final_code) %>% 
    summarise(deaths=sum(deaths))
  # make sure all cells are in there (if NA=0)
  template<-expand.grid(cbsa=unique(temp$cbsa),
                        final_code=unique(temp$final_code),
                        year=unique(temp$year))
  temp<-full_join(temp, template) %>% 
    mutate(deaths=replace_na(deaths, 0))
  template<-expand.grid(cbsa=unique(temp2$cbsa),
                        year=unique(temp2$year))
  temp2<-full_join(temp2, template) %>% 
    mutate(ill_defined_diseases=replace_na(ill_defined_diseases, 0))
  list(temp, temp2)
})
us_illdefined<-map_dfr(mortality_us, function(xx) xx[[2]])
mortality_us<-map_dfr(mortality_us, function(xx) xx[[1]])
# get pop for 2010 for restriction 
pop_us_2010<-vroom("../US_DATA/Population/pop_msa.csv") %>% rename(pop=pop_msa) %>% 
  filter(year==2010) %>% 
  select(cbsa, pop)
# list of CBSAs restrict US CBSAs by 100k (to make it comparable with LAC cities)
include_us<-pop_us_2010 %>% filter(pop>=100000) %>% pull(cbsa)
# get pop
pop_us<-vroom("../US_DATA/Population/pop_msa.csv") %>% rename(pop=pop_msa) %>% 
  filter(year%in%(2012:2016))
# age distro 
popage_us<-vroom("../US_DATA/Population/pop_msa_age_sex.csv") %>% 
  rename(pop=pop_msa, age=age_5yr_group) %>% 
  mutate(age=ifelse(age<15, 0,
                    ifelse(age<40, 15,
                           ifelse(age<65, 40,
                                  65)))) %>% 
  group_by(cbsa, year, age) %>% summarise(pop=sum(pop)) %>% 
  filter(year%in%(2012:2016)) 

#Latin American data
load("../SALURBAL_DATA/l1s.RData")
# creating 4 LAC files: mortality, total pop, population by age, and illdefined
mortality_dir<-"../SALURBAL_DATA/Mortality Data/DTHRED/Tier 3/"
mortality_files<-list.files(mortality_dir, pattern="L1", full.names = T)
#file<-mortality_files[[6]]
mortality_lac<-map_dfr(mortality_files, function(file){
  print(file)
  mortality<-vroom(file)
  country<-unique(mortality$ISO2)
  mortality<-mortality %>% filter(YEAR>=2010, YEAR<=2016) %>% 
    mutate(year=YEAR)%>% 
    group_by(SALID1, year, DTHCOD_FINAL3) %>% 
    summarise(deaths=sum(DTHRED3DEATHS))
  # fill in any potential gaps
  template<-expand.grid(SALID1=unique(mortality$SALID1),
                        DTHCOD_FINAL3=unique(mortality$DTHCOD_FINAL3),
                        year=unique(mortality$year))
  mortality<-full_join(mortality, template) %>% 
    mutate(deaths=replace_na(deaths, 0),
           iso2=country)
  mortality
})
t3<-t3 %>% 
  rename(DTHCOD_FINAL3=icd10_t3_red)
mortality_lac<-mortality_lac %>% left_join(t3)
mortality_lac<-mortality_lac %>% group_by(iso2, SALID1, year, final_code) %>% 
  summarise(deaths=sum(deaths))
# get ill-defined data [different file]
mortality_dir<-"../SALURBAL_DATA/Mortality Data/DIN"
mortality_files<-list.files(mortality_dir, full.names = T)
lac_illdefined<-map_dfr(mortality_files, function(file){
  print(file)
  mortality<-vroom(file)
  mortality<-mortality %>% filter(YEAR>=2010, YEAR<=2016, DINCOD_GHE3%in%c(1610, 1620)) %>% 
    mutate(year=YEAR)%>% 
    group_by(SALID1, year) %>% 
    summarise(ill_defined_diseases=n())
  # fill in any potential gaps
  template<-expand.grid(SALID1=unique(mortality$SALID1),
                        year=unique(mortality$year))
  mortality<-full_join(mortality, template) %>% 
    mutate(ill_defined_diseases=replace_na(ill_defined_diseases, 0))
  mortality
}) %>% left_join(l1s %>% select(SALID1, iso2))

pop_dir<-"../SALURBAL_DATA/Population Data/L1AD/Age Category/"
pop_files<-list.files(pop_dir, pattern="L1", full.names = T)
pop_lac<-map_dfr(pop_files, function(file){
  population<-vroom(file)
  country<-unique(population$iso2)
  # filter to 2010:2016
  population<-population %>% filter(YEAR>=2010, YEAR<=2016)
  population$year<-population$YEAR
  population<-population %>% group_by(SALID1, year) %>% 
    summarise(pop=sum(PRJL1ADPOP)) %>% 
    ungroup()
  # fill in any potential gaps
  template<-expand.grid(SALID1=unique(population$SALID1),
                        year=unique(population$year))
  population<-full_join(population, template) %>% 
    mutate(pop=replace_na(pop, 0))
  population
})


# get popage for LAC
pop_dir<-"../SALURBAL_DATA/Population Data/L1AD/Age Category/"
pop_files<-list.files(pop_dir, pattern="L1", full.names = T)
popage_lac<-map_dfr(pop_files, function(file){
  population<-vroom(file)
  # filter to 2010:2016
  population<-population %>% rename(year=YEAR, age=PRJAGE5C) %>% 
    filter(year%in%(2010:2016)) %>% 
    mutate(age=ifelse(age<15, 0,
                      ifelse(age<40, 15,
                             ifelse(age<65, 40,
                                    65)))) %>% 
    group_by(SALID1, year, age) %>% 
    summarise(pop=sum(PRJL1ADPOP)) %>% 
    ungroup()
  # fill in any potential gaps
  template<-expand.grid(SALID1=unique(population$SALID1),
                        age=unique(population$age),
                        year=unique(population$year))
  population<-full_join(population, template) %>% 
    mutate(pop=replace_na(pop, 0))
  population
})



# need 3 final files
## crosswalk with codes and names
## mortality and population in LAC and the US
### by year
### collapsed
## age structure of population in LAC and the US
### by year
### collapsed

# one crosswalk with causes and names
# also categorize
## category2= CMNN, NCD, Injuries
## category3= CMNN, NCD, Injuries
## category4= CMNN, Cancer, NCD, Injuries
## category5= CMNN, Cancer, NCD, Non-Violent, Violent injuries

cause_names<-t3 %>% rename(cause_code=DTHCOD_FINAL3) %>% 
  filter(!duplicated(final_code)) %>% 
  select(final_code, name) %>% 
  mutate(category5=case_when(
      final_code<590 ~ "CMNN",
      final_code%in%(600:780) ~ "Cancer",
      final_code<1480 ~ "NCDs",
      final_code==1580 ~ "Homicides",
      final_code==1570 ~ "Suicides",
      T ~ "Non-violent injuries"),
    category4=case_when(
      category5%in%c("Homicides","Suicides", "Non-violent injuries") ~ "Injuries",
      T ~ category5),
    category3=case_when(
      category4%in%c("Cancer", "NCDs") ~ "NCDs",
      T ~ category4),
    category2=case_when(
      category3!="Injuries" ~ "Diseases",
      T ~ category3)) %>% 
  mutate(category2=factor(category2, levels=c("Diseases", "Injuries")),
         category3=factor(category3, levels=c("CMNN","NCDs", "Injuries")),
         category4=factor(category4, levels=c("CMNN","Cancer", "NCDs", "Injuries")),
         category5=factor(category5, levels=c("CMNN","Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")))
table(cause_names$category5, useNA = "always")
cause_names %>% arrange(final_code) %>% group_by(category5) %>% 
  summarise(all=paste(name, collapse=";")) %>% 
  pull(all)
table(cause_names$name, cause_names$category5)
table(cause_names$category5, cause_names$category4)
table(cause_names$category4, cause_names$category3)
table(cause_names$category3, cause_names$category2)

## one file with deaths, props by age, and total pop, by year
# merge all LAC
include_lac<-l1s %>% filter(iso2!="NI") %>% pull(SALID1)
years<-data.frame(iso2=c("AR", "BR", "CL", "CO", "CR", "MX", "PA", "PE", "SV", "GT"))
years<-years %>% mutate(y1=ifelse(iso2=="SV", 2010, 2012))
years<-years %>% mutate(y2=y1+4)
# restrict years
mortality_lac<-mortality_lac %>% 
  full_join(years) %>% 
  filter(year>=y1, year<=y2) %>% 
  select(-y1, -y2)
# correct counts
correction<-vroom("../SALURBAL_DATA/Mortality Data/UCNT/UCNT_COMPLETE_L1AD_20200824.csv")
correction<-correction %>% 
  group_by(SALID1) %>% 
  summarise(ucnt=hmean(UCNT))
mortality_lac<-full_join(mortality_lac, correction) %>% 
  mutate(deaths=deaths/ucnt) %>% 
  select(-ucnt)
lac_illdefined<-full_join(lac_illdefined, correction) %>% 
  mutate(ill_defined_diseases=ill_defined_diseases/ucnt) %>% 
  select(-ucnt)
# restrict pop to the appropriate years
pop_lac<-pop_lac %>% 
  left_join(l1s) %>% 
  full_join(years) %>% 
  filter(year>=y1, year<=y2) %>% 
  select(-y1, -y2)

# now collapse population
pop_lac_collapsed<-pop_lac %>% 
  group_by(iso2, SALID1) %>% 
  summarise(pop=mean(pop))
pop_us_collapsed<-pop_us %>% 
  group_by(cbsa) %>% 
  summarise(pop=mean(pop))
# put together
pop_both_collapse<-bind_rows(
  pop_lac_collapsed %>% select(SALID1, pop) %>% rename(city=SALID1),
  pop_us_collapsed %>% select(cbsa, pop) %>% rename(city=cbsa)) %>% 
  ungroup() %>% select(-iso2)
pop_both_year<-bind_rows(
  pop_lac %>% select(SALID1, year, pop) %>% rename(city=SALID1),
  pop_us%>% select(cbsa, year, pop) %>% rename(city=cbsa))

# get city names 
city_names_usa<-pop_us %>% ungroup() %>% filter(!duplicated(cbsa)) %>% select(cbsa, cbsa_name) %>% 
  rename(city=cbsa, city_name=cbsa_name)
city_names_lac<-pop_lac %>% ungroup() %>% filter(!duplicated(SALID1)) %>% select(SALID1, city_link) %>% 
  rename(city=SALID1, city_name=city_link)
city_names<-bind_rows(city_names_usa, city_names_lac)


mortality_us<-mortality_us %>% filter(cbsa%in%include_us) %>% 
  rename(YEAR=year, 
         city=cbsa) %>% 
  mutate(iso2="US",
         country_name="United States") %>% 
  select(iso2, country_name, city, YEAR, final_code, deaths)
mortality_lac<-mortality_lac %>% 
  left_join(l1s %>% select(SALID1, country_name)) %>% 
  rename(YEAR=year, 
         city=SALID1) %>% 
  select(iso2, country_name, city, YEAR, final_code, deaths)
mortality_both_year<-
  bind_rows(mortality_us %>% ungroup(), 
            mortality_lac %>% ungroup()) %>% 
  mutate(us=as.numeric(iso2=="US"))
mortality_both_collapse<-mortality_both_year %>% 
  group_by(iso2, country_name, city, us, final_code) %>% 
  summarise(deaths=mean(deaths))

  
# ill-defined
ill_defined_collapse<-bind_rows(us_illdefined %>% 
                         filter(cbsa%in%include_us) %>% 
                         mutate(iso2="US", us=1, city=cbsa) %>% 
                         group_by(city, iso2, us) %>% 
                         summarise(ill_defined_diseases=mean(ill_defined_diseases)),
                       lac_illdefined %>% mutate(us=0, city=SALID1) %>% 
                         group_by(city, iso2, us) %>% 
                         summarise(ill_defined_diseases=mean(ill_defined_diseases))) %>% 
  ungroup() %>% 
  select(city, ill_defined_diseases)

# population structure for adjustment
popage_lac<-popage_lac %>% 
  left_join(l1s) %>% 
  full_join(years) %>% 
  filter(year>=y1, year<=y2) %>% 
  select(-y1, -y2)
popage_lac_year<-popage_lac %>% 
  full_join(pop_lac %>% rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(SALID1, year, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1:2)), addprop)
popage_us_year<-popage_us %>% 
  full_join(pop_us %>% rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(cbsa, year, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1:2)), addprop)
popage_lac_collapsed<-popage_lac %>% 
            group_by(iso2, SALID1, age) %>% 
            summarise(pop=mean(pop)) %>% 
            full_join(pop_lac %>% 
                        group_by(iso2, SALID1) %>% 
                        summarise(pop=mean(pop)) %>% 
                        rename(total=pop)) %>% 
            mutate(prop=pop/total) %>% 
            select(SALID1, prop, age) %>%  spread(age, prop) %>% 
            rename_at(vars(-(1:2)), addprop)
popage_us_collapsed<-popage_us %>% 
            group_by(cbsa, age) %>% 
            summarise(pop=mean(pop)) %>% 
            full_join(pop_us %>% 
                        group_by(cbsa) %>% 
                        summarise(pop=mean(pop)) %>% 
                        rename(total=pop)) %>% 
            mutate(prop=pop/total) %>% 
            select(cbsa, prop, age) %>%  spread(age, prop) %>% 
            rename_at(vars(-(1)), addprop) 
popage_collapse<-bind_rows(popage_us_collapsed %>% ungroup() %>% rename(city=cbsa), 
                           popage_lac_collapsed %>% ungroup() %>% rename(city=SALID1) %>% select(-iso2))
popage_year<-bind_rows(popage_us_year %>% ungroup() %>% rename(city=cbsa), 
                           popage_lac_year %>% ungroup() %>% rename(city=SALID1))
# center pop distribution to world standard pop
# 0-14=0.2614
# 15-39=0.3937
# 40-64=0.2626
# 65+=0.0823
popage_collapse<-popage_collapse %>% 
  mutate(prop0=prop0-0.2614,
         prop15=prop15-0.3937,
         prop40=prop40-0.2626,
         prop65=prop65-0.0823)
popage_year<-popage_year %>% 
  mutate(prop0=prop0-0.2614,
         prop15=prop15-0.3937,
         prop40=prop40-0.2626,
         prop65=prop65-0.0823)

# ensure all are restricted to the appropriate cities
included<-c(include_us, include_lac)
mortality_both_collapse<-mortality_both_collapse %>% 
  filter(city%in%included)
mortality_both_year<-mortality_both_year %>% 
  filter(city%in%included)
ill_defined_collapse<-ill_defined_collapse %>% 
  filter(city%in%included)
pop_both_collapse<-pop_both_collapse %>% 
  filter(city%in%included)
pop_both_year<-pop_both_year %>% 
  filter(city%in%included)
popage_collapse<-popage_collapse %>% 
  filter(city%in%included)
popage_year<-popage_year %>% 
  filter(city%in%included)
city_names<-city_names %>% 
  filter(city%in%included)
# FILES:
## Crosswalks:  cause_names and city_names
## Mortality: mortality_both_collapse & mortality_both_year
## Mortality // ill-defined: ill_defined_collapse
## Population denominators: pop_both_year & pop_both_collapse
## Population structure: popage_collapse & popage_year
summary(mortality_both_collapse);head(mortality_both_collapse)
summary(pop_both_collapse);head(pop_both_collapse)
summary(popage_collapse);head(popage_collapse)
head(cause_names);head(city_names)
table(mortality_both_collapse$iso2, useNA = "always")
table(mortality_both_year$iso2, mortality_both_year$YEAR, useNA = "always")
table(mortality_both_collapse$final_code, useNA = "always")

save(cause_names, city_names,
     mortality_both_collapse, mortality_both_year,
     ill_defined_collapse,pop_both_year, pop_both_collapse,
     popage_collapse, popage_year,
     file="analytic files/ms38data.rdata")

# US DATA FOR COMMUTING ZONES
rm(list=ls())
library(readxl)
library(vroom)
library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
source("MS38_HELPER_FUNCTIONS.R")
t3<-read_excel("Other_data/tier3_newproposal_v2.xlsx")
t3<-t3 %>% select(cause_code, destination_code, name) %>% 
  rename(icd10_t3_red=cause_code,final_code=destination_code)
cz<-vroom("Other_data/counties10-zqvz0r.csv") %>% 
  mutate(res_fips=as.numeric(FIPS),
         cz=as.numeric(OUT10)) %>% 
  select(res_fips, cz)
# creating 4 US files: mortality, total pop, population by age, and ill-defined diseases (R codes)
files<-list.files("../US_DATA/Mortality", full.names = T)
files<-files[grepl(paste(2012:2016, collapse="|"), files)]
#file<-files[[1]]
mortality_us<-map_dfr(files,function(file){
  temp<-vroom(file) %>% 
    mutate(icd10_t3_red=as.numeric(icd10_t3_red),
           res_fips=as.numeric(res_fips)) %>% 
    left_join(cz)
  temp<-temp %>% select(death_year, cz, icd10_t3_red) %>% left_join(t3)
  temp<-temp %>% group_by(death_year, cz, final_code) %>% 
    summarise(deaths=n()) %>% 
    rename(year=death_year) %>% filter(!is.na(cz)&!is.na(year))
  # remove deaths outside of UAs
  temp<-temp %>% group_by(year, cz, final_code) %>% 
    summarise(deaths=sum(deaths))
  # make sure all cells are in there (if NA=0)
  template<-expand.grid(cz=unique(temp$cz),
                        final_code=unique(temp$final_code),
                        year=unique(temp$year))
  temp<-full_join(temp, template) %>% 
    mutate(deaths=replace_na(deaths, 0))
  temp
})
# get pop for 2010 for restriction 
pop_us_2010<-vroom("../US_DATA/Population/pop_county.csv") %>% 
  mutate(res_fips=as.numeric(fips)) %>% 
  left_join(cz) %>% filter(!is.na(cz)) %>% 
  group_by(year, cz) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  filter(year==2010)
# restrict US CZs by 100k (to make it comparable with LAC cities)
include_us<-pop_us_2010 %>% filter(pop>=100000) %>% pull(cz)
# get pop
pop_us<-vroom("../US_DATA/Population/pop_county.csv") %>% 
  mutate(res_fips=as.numeric(fips)) %>% 
  left_join(cz) %>% filter(!is.na(cz)) %>% 
  group_by(year, cz) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  filter(year%in%(2012:2016))
# age distro 
popage_us<-vroom("../US_DATA/Population/pop_county_age_sex.csv") %>% 
  mutate(res_fips=as.numeric(fips),
         age=as.numeric(age_5yr_group)) %>% 
  left_join(cz) %>% filter(!is.na(cz)) %>% 
  group_by(year, age, cz) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  mutate(age=ifelse(age<15, 0,
                    ifelse(age<40, 15,
                           ifelse(age<65, 40,
                                  65)))) %>% 
  group_by(cz, year, age) %>% summarise(pop=sum(pop)) %>% 
  filter(year%in%(2012:2016)) 

cause_names<-t3 %>% rename(cause_code=DTHCOD_FINAL3) %>% 
  filter(!duplicated(final_code)) %>% 
  select(final_code, name) %>% 
  mutate(category5=case_when(
    final_code<590 ~ "CMNN",
    final_code%in%(600:780) ~ "Cancer",
    final_code<1480 ~ "NCDs",
    final_code==1580 ~ "Homicides",
    final_code==1570 ~ "Suicides",
    T ~ "Non-violent injuries"),
    category4=case_when(
      category5%in%c("Homicides","Suicides", "Non-violent injuries") ~ "Injuries",
      T ~ category5),
    category3=case_when(
      category4%in%c("Cancer", "NCDs") ~ "NCDs",
      T ~ category4),
    category2=case_when(
      category3!="Injuries" ~ "Diseases",
      T ~ category3)) %>% 
  mutate(category2=factor(category2, levels=c("Diseases", "Injuries")),
         category3=factor(category3, levels=c("CMNN","NCDs", "Injuries")),
         category4=factor(category4, levels=c("CMNN","Cancer", "NCDs", "Injuries")),
         category5=factor(category5, levels=c("CMNN","Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")))
## one file with deaths, props by age, and total pop, collapsed
pop_us_collapsed<-pop_us %>% 
  group_by(cz) %>% 
  summarise(pop=mean(pop)) %>% 
  rename(city=cz)
mortality_us_collapsed<-mortality_us %>% filter(cz%in%include_us) %>% 
  rename(YEAR=year, 
         city=cz) %>% 
  mutate(iso2="US",
         country_name="United States") %>% 
  select(iso2, country_name, city, YEAR, final_code, deaths) %>% 
  group_by(iso2, country_name, city, final_code) %>% 
  summarise(deaths=mean(deaths))
# population structure for adjustment
popage_us_year<-popage_us %>% 
  full_join(pop_us %>% rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(cz, year, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1:2)), addprop)
popage_us_collapsed<-popage_us %>% 
  group_by(cz, age) %>% 
  summarise(pop=mean(pop)) %>% 
  full_join(pop_us %>% 
              group_by(cz) %>% 
              summarise(pop=mean(pop)) %>% 
              rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(cz, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1)), addprop) %>% 
  rename(city=cz)
# center pop distribution to world standard pop
# 0-14=0.2614
# 15-39=0.3937
# 40-64=0.2626
# 65+=0.0823
popage_us_collapsed<-popage_us_collapsed %>% 
  mutate(prop0=prop0-0.2614,
         prop15=prop15-0.3937,
         prop40=prop40-0.2626,
         prop65=prop65-0.0823)
summary(mortality_us_collapsed);head(mortality_us_collapsed)
summary(pop_us_collapsed);head(pop_us_collapsed)
summary(popage_us_collapsed);head(popage_us_collapsed)
table(mortality_us_collapsed$final_code, useNA = "always")

mortality_us_cz<-mortality_us_collapsed%>% filter(city%in%include_us)
pop_us_cz<-pop_us_collapsed%>% filter(city%in%include_us)
popage_us_cz<-popage_us_collapsed%>% filter(city%in%include_us)
save(mortality_us_cz,
     pop_us_cz,popage_us_cz,
     file="analytic files/ms38data_cz.rdata")

# UA definition
rm(list=ls())
library(readxl)
library(vroom)
library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
source("MS38_HELPER_FUNCTIONS.R")
t3<-read_excel("Other_data/tier3_newproposal_v2.xlsx")
t3<-t3 %>% select(cause_code, destination_code, name) %>% 
  rename(icd10_t3_red=cause_code,final_code=destination_code)
ua_shp<-read_sf("Other_data/Shps/cb_2013_us_ua10_500k/cb_2013_us_ua10_500k.shp") %>% 
  mutate(ua=as.numeric(GEOID10))
ua_shp$ua_area<-st_area(ua_shp)
county_shp<-read_sf("Other_data/Shps/cb_2013_us_county_500k/cb_2013_us_county_500k.shp") %>% 
  mutate(res_fips=as.numeric(paste0(STATEFP, COUNTYFP)))
county_shp$county_area<-st_area(county_shp)
ua<-st_intersection(county_shp, ua_shp)
ua$intersection_area<-st_area(ua)
ua<-ua %>% 
  # calculate % of county in the overlap area to select the county with the max overlap
  mutate(overlap=intersection_area/county_area) %>% 
  arrange(ua, desc(overlap)) %>% 
  filter(!duplicated(res_fips)) %>% 
  as_tibble() %>% 
  select(ua, res_fips)
# creating 4 US files: mortality, total pop, population by age, and ill-defined diseases (R codes)
files<-list.files("../US_DATA/Mortality", full.names = T)
files<-files[grepl(paste(2012:2016, collapse="|"), files)]
#file<-files[[1]]
mortality_us<-map_dfr(files,function(file){
  temp<-vroom(file) %>% 
    mutate(icd10_t3_red=as.numeric(icd10_t3_red),
           res_fips=as.numeric(res_fips)) %>% 
    left_join(ua)
  temp<-temp %>% select(death_year, ua, icd10_t3_red) %>% left_join(t3)
  temp<-temp %>% group_by(death_year, ua, final_code) %>% 
    summarise(deaths=n()) %>% 
    rename(year=death_year) %>% filter(!is.na(ua)&!is.na(year))
  # remove deaths outside of UAs
  temp<-temp %>% group_by(year, ua, final_code) %>% 
    summarise(deaths=sum(deaths))
  # make sure all cells are in there (if NA=0)
  template<-expand.grid(ua=unique(temp$ua),
                        final_code=unique(temp$final_code),
                        year=unique(temp$year))
  temp<-full_join(temp, template) %>% 
    mutate(deaths=replace_na(deaths, 0))
  temp
})
# get pop for 2010 for restriction 
pop_us_2010<-vroom("../US_DATA/Population/pop_county.csv") %>% 
  mutate(res_fips=as.numeric(fips)) %>% 
  left_join(ua) %>% filter(!is.na(ua)) %>% 
  group_by(year, ua) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  filter(year==2010)
# restrict US UAs by 100k (to make it comparable with LAC cities)
include_us<-pop_us_2010 %>% filter(pop>=100000) %>% pull(ua)
# get pop
pop_us<-vroom("../US_DATA/Population/pop_county.csv") %>% 
  mutate(res_fips=as.numeric(fips)) %>% 
  left_join(ua) %>% filter(!is.na(ua)) %>% 
  group_by(year, ua) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  filter(year%in%(2012:2016))
# age distro 
popage_us<-vroom("../US_DATA/Population/pop_county_age_sex.csv") %>% 
  mutate(res_fips=as.numeric(fips),
         age=as.numeric(age_5yr_group)) %>% 
  left_join(ua) %>% filter(!is.na(ua)) %>% 
  group_by(year, age, ua) %>% 
  summarise(pop=sum(pop_county, na.rm=T)) %>% 
  mutate(age=ifelse(age<15, 0,
                    ifelse(age<40, 15,
                           ifelse(age<65, 40,
                                  65)))) %>% 
  group_by(ua, year, age) %>% summarise(pop=sum(pop)) %>% 
  filter(year%in%(2012:2016)) 

cause_names<-t3 %>% rename(cause_code=DTHCOD_FINAL3) %>% 
  filter(!duplicated(final_code)) %>% 
  select(final_code, name) %>% 
  mutate(category5=case_when(
    final_code<590 ~ "CMNN",
    final_code%in%(600:780) ~ "Cancer",
    final_code<1480 ~ "NCDs",
    final_code==1580 ~ "Homicides",
    final_code==1570 ~ "Suicides",
    T ~ "Non-violent injuries"),
    category4=case_when(
      category5%in%c("Homicides","Suicides", "Non-violent injuries") ~ "Injuries",
      T ~ category5),
    category3=case_when(
      category4%in%c("Cancer", "NCDs") ~ "NCDs",
      T ~ category4),
    category2=case_when(
      category3!="Injuries" ~ "Diseases",
      T ~ category3)) %>% 
  mutate(category2=factor(category2, levels=c("Diseases", "Injuries")),
         category3=factor(category3, levels=c("CMNN","NCDs", "Injuries")),
         category4=factor(category4, levels=c("CMNN","Cancer", "NCDs", "Injuries")),
         category5=factor(category5, levels=c("CMNN","Cancer", "NCDs", "Non-violent injuries","Suicides", "Homicides")))
## one file with deaths, props by age, and total pop, collapsed
pop_us_collapsed<-pop_us %>% 
  group_by(ua) %>% 
  summarise(pop=mean(pop)) %>% 
  rename(city=ua)
mortality_us_collapsed<-mortality_us %>% filter(ua%in%include_us) %>% 
  rename(YEAR=year, 
         city=ua) %>% 
  mutate(iso2="US",
         country_name="United States") %>% 
  select(iso2, country_name, city, YEAR, final_code, deaths) %>% 
  group_by(iso2, country_name, city, final_code) %>% 
  summarise(deaths=mean(deaths))
# population structure for adjustment
popage_us_year<-popage_us %>% 
  full_join(pop_us %>% rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(ua, year, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1:2)), addprop)
popage_us_collapsed<-popage_us %>% 
  group_by(ua, age) %>% 
  summarise(pop=mean(pop)) %>% 
  full_join(pop_us %>% 
              group_by(ua) %>% 
              summarise(pop=mean(pop)) %>% 
              rename(total=pop)) %>% 
  mutate(prop=pop/total) %>% 
  select(ua, prop, age) %>%  spread(age, prop) %>% 
  rename_at(vars(-(1)), addprop) %>% 
  rename(city=ua)
# center pop distribution to world standard pop
# 0-14=0.2614
# 15-39=0.3937
# 40-64=0.2626
# 65+=0.0823
popage_us_collapsed<-popage_us_collapsed %>% 
  mutate(prop0=prop0-0.2614,
         prop15=prop15-0.3937,
         prop40=prop40-0.2626,
         prop65=prop65-0.0823)
summary(mortality_us_collapsed);head(mortality_us_collapsed)
summary(pop_us_collapsed);head(pop_us_collapsed)
summary(popage_us_collapsed);head(popage_us_collapsed)
table(mortality_us_collapsed$final_code, useNA = "always")

mortality_us_ua<-mortality_us_collapsed %>% filter(city%in%include_us)
pop_us_ua<-pop_us_collapsed%>% filter(city%in%include_us)
popage_us_ua<-popage_us_collapsed%>% filter(city%in%include_us)
save(mortality_us_ua,
     pop_us_ua,popage_us_ua,
     file="analytic files/ms38data_ua.rdata")



## COMPUTE % MISSING
rm(list=ls())
library(readxl)
library(vroom)
library(tidyverse)
library(broom)
library(grid)
library(gridExtra)
source("MS38_HELPER_FUNCTIONS.R")
# US
pop_us_2010<-vroom("../US_DATA/Population/pop_msa.csv") %>% rename(pop=pop_msa) %>% 
  filter(year==2010) %>% 
  select(cbsa, pop)
# list of CBSAs restrict US CBSAs by 100k (to make it comparable with LAC cities)
include_us<-pop_us_2010 %>% filter(pop>=100000) %>% pull(cbsa)
files<-list.files("../US_DATA/Mortality", full.names = T)
files<-files[grepl(paste(2012:2016, collapse="|"), files)]
#file<-files[[1]]
mortality_us<-map_dfr(files,function(file){
  temp<-vroom(file) %>% 
    filter(res_cbsa%in%include_us) %>% 
    rename(year=death_year) %>% 
    mutate(missing_age=as.numeric(is.na(age)),
           missing_sex=as.numeric(is.na(male)),
           garbage=icd10_t3%in%c(1610, 1620)) %>% 
    group_by(year) %>% 
    summarise(missing_age=sum(missing_age),
              missing_sex=sum(missing_sex),
              garbage=sum(garbage),
              total=n())
  temp
})
#Latin American data
mortality_dir<-"../SALURBAL_DATA/Mortality Data/DIN/"
mortality_files<-list.files(mortality_dir,full.names = T)
#file<-mortality_files[[10]]
mortality_lac<-map_dfr(mortality_files, function(file){
  print(file)
  temp<-vroom(file)
  y1<-ifelse(grepl("SV", file), 2010, 2012)
  y2<-ifelse(grepl("SV", file), 2014, 2016)
  temp<-temp %>% filter(YEAR>=y1, 
                                  YEAR<=y2) %>% 
    mutate(year=YEAR,
           garbage=DINCOD_GHE3%in%c(1610, 1620))%>% 
    group_by(year) %>% 
    summarise(missing_age=sum(DINIMAGE),
              missing_sex=sum(DINIMMALE),
              garbage=sum(garbage),
              total=n())
  temp
})

bind_rows(mortality_lac %>% 
  mutate(region="LAC") %>% 
  group_by(region) %>% 
  summarise(missing_age=sum(missing_age),
            missing_sex=sum(missing_sex),
            garbage=sum(garbage),
            total=sum(total)) %>% 
  mutate(missing_age=missing_age/total*100,
         missing_sex=missing_sex/total*100,
         garbage=garbage/total*100),
  mortality_us %>% 
  mutate(region="US") %>% 
  group_by(region) %>% 
  summarise(missing_age=sum(missing_age),
            missing_sex=sum(missing_sex),
            garbage=sum(garbage),
            total=sum(total)) %>% 
  mutate(missing_age=missing_age/total*100,
         missing_sex=missing_sex/total*100,
         garbage=garbage/total*100))
