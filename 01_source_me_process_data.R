# BEGINNING OF CODE SHARED WITH INDUSTRY TOOL--------
tictoc::tic()
# libraries------------
library("lubridate")
library("tidyverse")
library("here")
library("readxl")
# Functions------------------
source(here::here("R","functions.R"))
# "constants"... that change every year------------
year1 <- as.numeric(year(today())) 
year2 <- year1 + 5
year3 <- year1 + 10
# Read in the dataframes------------------------
wages_raw <- read_excel(here("raw_data", list.files(here("raw_data"), pattern = "Wages", ignore.case = TRUE)))

# jobs data
jo_raw <- read_csv(here("raw_data", 
                        list.files(here("raw_data"), pattern = "JO", ignore.case = TRUE)),
                  locale = readr::locale(encoding = "latin1"),
                  skip=3,
                  col_select = -1)%>%
  pivot_longer(cols=-c(NOC, Description, Industry, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  clean_tbbl() %>%
  filter(noc != "#t")

# industry characteristics
ind_char_raw <- read_excel(here("raw_data", "lmo64_characteristics.xlsx")) %>%
  clean_tbbl()%>%
  rename(industry_code=lmo_ind_code,
         industry=lmo_detailed_industry)%>%
  group_by(across(c(-naics)))%>%
  nest()%>%
  mutate(naics_definition=map_chr(data, nest_to_string))%>%
  select(-data)

# employment data

employment_raw <- read_csv(here("raw_data", 
                        list.files(here("raw_data"), pattern = "Emp")),
                   locale = readr::locale(encoding = "latin1"),
                   skip=3,
                   col_select = -1)%>%
  pivot_longer(cols=-c(NOC, Description, Industry, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  clean_tbbl() 

# demand/supply data

ds_raw <- read_csv(here("raw_data", 
                                list.files(here("raw_data"), pattern = "DS")),
                           locale = readr::locale(encoding = "latin1"),
                           skip=3,
                           col_select = -1)%>%
  pivot_longer(cols=-c(NOC, Description, Industry, Variable, `Geographic Area`),
               names_to = "date",
               values_to = "value")%>%
  clean_tbbl()

# NOC Mappings
noc_mappings_raw <- read_csv(here("raw_data", "NOC Mappings.csv")) %>%
  clean_tbbl()

# occupation characteristics file
education_occupation_raw <- read_excel(here("raw_data",  
                                            list.files(here("raw_data"), pattern = "Occupation")),
                                       skip=3) %>%
  clean_tbbl()%>%
  rename(noc = noc_2016)

# high opportunity occupations
hoo <- read_excel(here("raw_data",  list.files(here("raw_data"), pattern = "HOO"))) %>%
  clean_tbbl() %>%
  mutate(high_opportunity_occupation = factor(high_opportunity_occupation, labels = c("non-hoo", "hoo"))) %>%
  rename(occupation_group = high_opportunity_occupation) %>%
  filter(noc != "#t")

#PROCESSING---------------

long <- bind_rows(jo_raw, employment_raw)
columns_to_keep <- colnames(long)

# jo_employment INPUT TO Clean_JO.csv AND jobs_and_industry----------------

jo_employment <- long%>%
  filter(
    industry != "all_industries", # Note that if label "all industries" changes in the excel file, will need to change (**)
    description != "total",
    noc != "#t"
  )%>%
  left_join(ind_char_raw, by = "industry")%>%
  select(all_of(columns_to_keep), industry_code, aggregate_industry)%>%
  distinct()%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  select(
    industry,
    date,
    noc,
    description,
    employment,
    geographic_area,
    job_openings,
    expansion_demand,
    replacement_demand,
    deaths,
    retirements,
    industry_code, 
    aggregate_industry
  ) %>%
  filter(!is.na(geographic_area))


# noc_geo INPUT TO occ_group---------------
noc_geo <- jo_raw %>%
  select(noc, geographic_area) %>%
  distinct()

# occ_group INPUT TO group_and_wages--------------
occ_group <- education_occupation_raw %>%
  select(
    noc,
    starts_with("occ_group") & !contains("hoo_bc")
  ) %>%
  filter(noc != "#t") %>%
  mutate(all_occupations = "all_occupations") %>%
  pivot_longer(cols = -noc, names_to = "name", values_to = "occupation_group") %>%
  select(noc, occupation_group) %>%
  full_join(noc_geo, multiple = "all") %>%
  bind_rows(hoo) %>%
  distinct() %>%
  na.omit() %>%
  clean_tbbl()

# END OF CODE SHARED WITH INDUSTRY TOOL--------

#jo_all_industries INPUT TO ds_and_jo------------

jo_all_industries <-jo_raw %>% 
  filter(description != "total",
         noc != "#t",
         industry == "all_industries")

# ds_and_jo INPUT TO Supply_cleaned.csv -----------
ds_and_jo <- ds_raw%>%
  bind_rows(jo_all_industries)%>%
  group_by(noc, description, industry, variable, geographic_area, date)%>%
  summarize(value=mean(value, na.rm=TRUE))%>% #deal with some duplicate records
  pivot_wider(names_from = variable, values_from = value)%>%
  rename(young_people_starting_work = new_entrants,
         immigrants = `net_international_in-migration`,
         migrants_from_other_provinces = `net_interregional_in-migration`)%>%
  mutate(additional_supply_requirement = job_openings - immigrants - migrants_from_other_provinces - young_people_starting_work,
         labour_force_exits = -1 * (deaths + retirements),
         net_change_in_labour_force = young_people_starting_work + immigrants + migrants_from_other_provinces + additional_supply_requirement + labour_force_exits
  )%>%
  select(Date=date, 
         `Geographic Area`=geographic_area, 
         NOC=noc, 
         Description=description, 
         Industry=industry, 
         `Job Openings`=job_openings, 
         `Young people starting work`=young_people_starting_work, 
         Immigrants=immigrants, 
         `Migrants from other provinces`=migrants_from_other_provinces,
         `Additional supply requirement`=additional_supply_requirement,
         `Labour force exits`=labour_force_exits,
         `Net change in labour force`=net_change_in_labour_force)


# ind_char2 INPUT TO jobs_and_industry-----------
ind_char2 <- ind_char_raw%>% 
  mutate(ind_group_tech_intensive_industries = as.character(ind_group_tech_intensive_industries),
         sector = if_else(ind_group_tech_intensive_industries == "tech_intensive", 
                          "technology", 
                          as.character(sector)),
         sector = factor(sector)
         )%>%
  ungroup()%>%
  select(-ind_group_trades_intensive_industries, 
         -ind_group_tech_intensive_industries, 
         -ita_sector_advisory_group)

#jobs_and_industry INPUT TO jobs_industry_noc--------------

jobs_and_industry <-
  full_join(jo_employment,
    ind_char2,
    by = c("industry_code", "industry", "aggregate_industry"))%>%
  filter(industry != "all_industries")%>%
  clean_tbbl()

#break down NOC mappings by hierarchy
# noc_broad_occ INPUT TO noccupation------------
noc_broad_occ <- noc_mappings_raw%>%
  filter(hierarchical_structure == "broad_occupational_category")%>%
  mutate(code = paste0("#", code))%>%
  select(noc1 = code, noc1_description = class_title)

#' Do major groups next... note that for some Classes the Code is a range e.g. 01-05
#' what we want to split these ranges up e.g. Codes 1,2,3,4,5
#' and we do so by first splitting the range into a start and a finish value,
#' nesting the data by the description, creating a variable NOC2 containing the sequence,
#' and then un-nest NOC2.

# noc_major_group INPUT TO noccupation-------------
noc_major_group <- noc_mappings_raw%>%
  filter(hierarchical_structure == "major_group")%>%
  separate(code, into = c("start", "finish"), sep = "-", fill = "right")%>% 
  mutate(start=as.numeric(start),
         finish=as.numeric(finish))%>%
  select(start, finish, noc2_description = class_title)%>%
  group_by(noc2_description)%>%
  nest()%>%
  mutate(noc2 = map(data, fill_range))%>%
  select(-data)%>%
  unnest(noc2)%>% 
  mutate(noc2 = str_pad(noc2, width = 2, pad = "0"),
         noc2 = paste0("#", noc2))

# noc_minor_group INPUT TO noccupation-------------
noc_minor_group <- noc_mappings_raw%>%
  filter(hierarchical_structure == "minor_group")%>%
  select(noc3 = code, noc3_description = class_title)%>% 
  mutate(noc3 = str_pad(noc3, width = 3, pad = "0"),
         noc3 = paste0("#", noc3))

# noc_unit INPUT TO noccupation---------------
noc_unit <- noc_mappings_raw %>%
  filter(hierarchical_structure == "unit_group")%>%
  select(noc = code, noc4_description = class_title)%>% 
  mutate(noc = str_pad(noc, width = 4, pad = "0"),
         noc = paste0("#", noc))
 
# education_occupation INPUT TO jobs_employment-----------
education_occupation <- education_occupation_raw %>%
  select(noc, 
         noc1, 
         noc2, 
         noc3, 
         education_typical_background= contains("typical_education"))%>%
  filter(noc!="#t")

# jobs_employment INPUT TO occupation AND by_aggregated_industry AND by_individual_industry AND j_openings AND emp---------------

# jobs_employment <- jobs_and_industry%>%
#   full_join(education_occupation, by = c("noc"))

jobs_employment <- jo_employment%>%
  filter(industry != "all_industries")%>%
  full_join(education_occupation, by = c("noc"))

# j_openings INPUT TO group_wages_characteristics------------------
j_openings <- jobs_employment%>%
  select(noc, 
         date, 
         region = geographic_area, 
         job_openings)%>%
  filter(date != year1 - 1,
         date != year1)%>%
  group_by(noc, region)%>%
  summarize(job_openings=sum(job_openings))

# emp INPUT TO group_wages_characteristics--------------------
emp <- jobs_employment%>%
  select(noc, 
         date, 
         region = geographic_area,
         employment)%>%
  filter(date == year1)%>%
  group_by(noc, region)%>%
  summarize(employment=sum(employment))

# occupation INPUT TO noccupation------------
occupation <- jobs_employment%>%
  group_by(geographic_area,
           noc,
           noc1,
           noc2,
           noc3,
           description,
           education_typical_background)%>%
  nest()%>%
  mutate(aggregated = map(data, common_aggregates))%>%
  select(-data)%>%
  unnest(aggregated)

#' Take the NOC levels (noc_broad_occ, noc_major_group, noc_minor_group and noc_unit)
#'  and join them to our occupation data frame ==> this gives us the descriptions for 
#'  each NOC level (ie NOC1 and NOC1 description, NOC2 and NOC2 description etc)

# noccupation INPUT TO Occupations_regional.csv AND noc_mappings2------------
noccupation <- occupation%>%
  full_join(noc_broad_occ, by = "noc1") # merge with broad occupations
noccupation <- noccupation%>%
  full_join(noc_major_group, by = "noc2") # merge with major groups
noccupation <- noccupation%>%
  full_join(noc_minor_group, by = "noc3") # merge with minor groups
noccupation <- noccupation%>%
  full_join(noc_unit, by = "noc") # merge with unit groups

#noc_mappings2 INPUT TO jobs_industry_noc--------------
noc_mappings2 <-
  unique(noccupation[, c(
    "noc",
    "noc1",
    "noc2",
    "noc3",
    "noc1_description",
    "noc2_description",
    "noc3_description",
    "noc4_description"
  )])

#jobs_industry_noc INPUT TO Jobs_and_Industry.csv AND mapping---------------
jobs_industry_noc <- jobs_and_industry%>%
  full_join(noc_mappings2, by = "noc")%>%
  mutate(sector=str_replace(sector, "agrifood_sector", "agrifoods_sector"))

# mapping INPUT TO by_individual_industry-----------
mapping <- unique(jobs_industry_noc[, c("industry", "aggregate_industry")]) # Take the all unique pairings of Industry and Aggregate Industry from the Jobs and Industry data frame
colnames(mapping) <- c("level_value", "aggregate_industry") # rename these columns to match what we have for the previous data frame

# by_sector (not referenced below????)------------
# by_sector <- aggregate_jobs_employment_by(Sector)

# by_aggregated_industry INPUT TO individual_industry_agg_industry------------
by_aggregated_industry <- aggregate_jobs_employment_by(aggregate_industry)%>%
  mutate(aggregate_industry = level_value)

# by_individual_industry INPUT TO individual_industry_agg_industry------------
by_individual_industry <- aggregate_jobs_employment_by(industry)%>%
  full_join(mapping, by="level_value") # merge to mapping, now we have a column with the Aggregate industries matching to the LMO 61 industries

# individual_industry_agg_industry INPUT TO Employment_Growth_Rates.csv-------------
# Tableau wants . in 2 of the column names WTF???
individual_industry_agg_industry <- bind_rows(by_individual_industry, by_aggregated_industry)%>%
  unique()%>%
  select(Level.Value=level_value,
         Geographic.Area=geographic_area,
         Variable=variable,
         Level=level,
         Value=value,
         `Aggregate Industry`=aggregate_industry
  )%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))

# wages_cleaned INPUT TO group_and_wages--------------------
wages_cleaned <- wages_raw%>%
  select(noc= `NOC 2016`,
         low_wage = contains("Low"),
         median_wage = contains("Median"),
         high_wage = contains("High"))%>%
  remove_missing()%>%
  clean_tbbl()

#group_and_wages INPUT TO group_wages_characteristics-------------------------
group_and_wages <- wages_cleaned%>%
  full_join(occ_group, by=c("noc"="noc"), multiple = "all")%>%
  na.omit()

# occ_characteristics INPUT TO group_wages_characteristics----------------------------
occ_characteristics <- education_occupation_raw%>%
  select(
    noc,
    occupation_title = description,
    typical_education = contains("typical"),
    interest1,
    interest2,
    interest3,
    skill1,
    skill2,
    skill3,
    interests,
    top_3_skills_and_competencies = skills_top_3)%>%
  filter(noc != "#t")

# group_wages_characteristics INPUT TO occ_characteristics_wage.csv--------------------
group_wages_characteristics <- full_join(group_and_wages, 
                                         occ_characteristics, 
                                         by = "noc")%>%
  rename(region = geographic_area)%>%
  full_join(j_openings, by = c("noc", "region"))%>%
  full_join(emp, by = c("noc", "region"))%>%
  filter(!is.na(occupation_group))%>%
  select(NOC=noc,
         Region=region,
         `Occupation Group`=occupation_group,
         `Low Wage`=low_wage,
         `Median Wage`=median_wage,
         `High Wage`=high_wage,
         `Occupation Title`=occupation_title,
         `Typical Education`=typical_education,
         `Alternative Education`=typical_education, #alternative education not provided
         Interest1=interest1,
         Interest2=interest2,
         Interest3=interest3,
         Skill1=skill1,
         Skill2=skill2,
         Skill3=skill3,
         Interests=interests,
         `Top 3 Skills and Competencies`=top_3_skills_and_competencies,
         `Job Openings`=job_openings,
         `Employment year1`=employment)%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))

#this is a hack... figure 2_2 is a sheet from LMO 2022 Edition Charts and Tables.xlsx
sources <- read_csv(here("raw_data","figure2_2.csv"))%>%
  pivot_longer(cols=-name, names_to = "Date", values_to = "value")%>%
  filter(name!="Total supply change")%>%
  pivot_wider(names_from = name, values_from = value)%>%
  rename(`Decline in Unemployment`=`Decline in unemployment`)%>%
  mutate(Date=as.numeric(Date))%>%
  openxlsx::write.xlsx(here("processed_data","Sources of new workers.xlsx"))


# Write_to_File------------------
clean_and_save(jo_employment, "Clean_JO.csv")

ds_and_jo%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))%>%
  write_csv(here("processed_data","Supply_cleaned.csv"))

noccupation%>%
  select(NOC=noc,
         NOC3=noc3,
         NOC2=noc2,
         NOC1=noc1,
         `Geographic Area`=geographic_area,
         `Description`=description,
         `Education:.Typical.Background`=education_typical_background,
         `Employment year1`,
         `Expansion year1-year3`,
         `Replacement year1-year3`,
         `Job Openings year1-year3`,
         `NOC1 Description`=noc1_description,
         `NOC2 Description`=noc2_description,
         `NOC3 Description`=noc3_description,
         `NOC4 Description`=noc4_description)%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))%>%
write_csv(here("processed_data","Occupations_regional.csv"))


jobs_industry_noc%>%
  select(NOC = noc,
         `Industry Code` = industry_code,
         Industry = industry,
         Date = date,
         Description=description,
         Employment=employment,
         `Geographic Area`=geographic_area,
         `Job Openings`=job_openings,
         `Expansion Demand`=expansion_demand,
         `Replacement Demand`=replacement_demand,
         Deaths=deaths,
         Retirements=retirements,
         `NAICS Definition`=naics_definition,
         `Aggregate Industry`=aggregate_industry,
         `Sector Code`=sector_code,
         Sector=sector,
         NOC1=noc1,
         NOC2=noc2,
         NOC3=noc3,
         `NOC1 Description`=noc1_description,
         `NOC2 Description`=noc2_description,
         `NOC3 Description`=noc3_description,
         `NOC4 Description`=noc4_description)%>%
  rapply(as.character, classes = "factor", how = "replace")%>%
  tibble()%>%
  mutate(across(where(is.character), make_title))%>%
  write_csv(here("processed_data", "Jobs_and_Industry.csv"))

write_csv(individual_industry_agg_industry, here("processed_data", "Employment_Growth_Rates.csv"))
write_csv(group_wages_characteristics, here("processed_data","occ_characteristics_wage.csv"))

# document objects for 02_knit_me.Rmd----------

df_names = ls()[sapply(ls(), function(x) any(is.data.frame(get(x))))]
dfs <- tibble(df_names)%>%
  mutate(info = map(df_names, col_names_type_example))
saveRDS(dfs, "dataframes.RDS")
tictoc::toc()



