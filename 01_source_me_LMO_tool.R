#to-do before running code----------------
#' ensure that the current versions of the following files are available in subdirectory "LMO Master Databases"
#' "JO single variables.csv"
#' "Industry characteristics.xlsx"
#' "Emp single variables.csv"
#' "DS single variables.csv"
#' "Occupation characteristics.xlsx"
#' "HOO list.xlsx"
#' 
#' and the current version of these files are available in subdirectory "Tableau Tool Inputs"
#' "NOC Mappings.csv"
#' "IndustryProfiles_Descriptions.xlsx"
#' "2020Preliminary wages.xlsx" MUST BE ONLY FILE IN "Tableau Tool Inputs" CONTAINING STRING "wages"

# Functions------------
# this function take a tbbl with two columns, start and finish and returns either start OR a sequence between start and finish.
tictoc::tic()
fill_range <- function(tbbl){
  if(is.na(tbbl$finish)){
    tbbl$start
  }else{
    seq(tbbl$start, tbbl$finish)
  }
}

#calculates aggregates for Occupation dataframe.
common_aggregates <- function(tbbl){
  first_year <- tbbl%>%
    filter(date == year1)
  all_other_years <- tbbl%>%
    filter(date %in% c(as.numeric(year1 + 1):year3)) 
  first_year_employment <- sum(first_year$employment)
  expansion <- sum(all_other_years$expansion_demand)
  replacement <- sum(all_other_years$replacement_demand)
  job_openings <- sum(all_other_years$job_openings)
  
  tibble(`Employment year1` = first_year_employment,
         `Expansion year1-year3` = expansion,
         `Replacement year1-year3` = replacement,
         `Job Openings year1-year3` = job_openings)
}

# calculates common aggregates, then adds cagrs and annual replacement rate.
all_aggregates <- function(tbbl){
  ca <- common_aggregates(tbbl)
  first_year <- tbbl%>%
    filter(date == year1)
  first_year_employment <- sum(first_year$employment)
  second_year <- tbbl%>%
    filter(date == year2)
  second_year_employment <- sum(second_year$employment)
  third_year <- tbbl%>%
    filter(date == year3)
  third_year_employment <- sum(third_year$employment)
  
  cagr1 <- 100 * ((second_year_employment/first_year_employment)^(1 / 5) - 1)
  cagr2 <- 100 * ((third_year_employment/second_year_employment)^(1 / 5) - 1)
  cagr3 <- 100 * ((third_year_employment/first_year_employment)^(1 / 10) - 1)
  
  ave_replace <- tbbl%>%
    group_by(date)%>%
    summarize(employment = sum(employment),
              replacement_demand = sum(replacement_demand))%>%
    mutate(replacement_rate = replacement_demand / employment)%>%
    summarize(mean(replacement_rate)*100)%>%
    pull()
  
  bind_cols(ca, 
            `Employment Growth year1-year2` = cagr1, 
            `Employment Growth year2-year3` = cagr2, 
            `Employment Growth year1-year3` = cagr3,
            `Annual Replacement Rate` = ave_replace
  )
}

# aggregates dataframe jobs_employment by geographic_area and some other var 
aggregate_jobs_employment_by <- function(var){
  jobs_employment%>%
    group_by(geographic_area, {{ var  }})%>%
    nest()%>%
    mutate(aggregated = map(data, all_aggregates))%>%
    select(-data)%>%
    unnest(aggregated)%>%
    pivot_longer(cols = -c(geographic_area, {{  var  }}))%>%
    rename(variable = name,
           level_value = {{  var  }})%>%
    mutate(level = rlang::englue("{{  var  }}"))
}

# converts a camel_case string to a title.
make_title <- function(strng){
  strng <- str_to_title(str_replace_all(strng,"_"," "))
}

#' takes a tibble and converts all factors to strings, then converts those (camel_case) strings to titles. 
#' finally converts camel_case columns to titles. 
clean_and_save <- function(tbbl, file_name){
  tbbl <- tbbl%>%
    rapply(as.character, classes = "factor", how = "replace")%>%
    tibble()%>%
    mutate(across(where(is.character), make_title))
  colnames(tbbl) <- make_title(colnames(tbbl))
  write_csv(tbbl, here("Tableau Tool Inputs", file_name))
}

get_example <- function(vec){
  example <- sample(vec, 1)
  ifelse(is.factor(vec), as.character(example), example)
}

# creates summary of a dataframe (columns and column types)
col_names_type_example <- function(df){
  cname <- colnames(get(df))
  ctype <- sapply(get(df), class)
  cexample <- sapply(get(df), get_example)
  tbbl <- tibble(column = cname, type = ctype, example = cexample)
}

# Load code common to both LMO and Industry tools--------
source("previously_duplicated_code.R")

#jo_all_industries INPUT TO ds_merged------------
jo_all_industries <-jo_raw %>% 
  filter(description != "total",
         noc != "#t",
         industry == "all_industries")%>%
  pivot_wider(names_from = variable, values_from = value)


# ds_merged INPUT TO Supply_cleaned.csv -----------
ds_merged <- ds_raw%>%
  pivot_wider(names_from = variable, values_from = value)%>%
  full_join(
    jo_all_industries,
    by = c("date", 
           "geographic_area", 
           "noc", 
           "description", 
           "industry", 
           "deaths", 
           "expansion_demand", 
           "job_openings", 
           "retirements"))%>%
  rename(young_people_starting_work = new_entrants,
         immigrants = `net_international_in-migration`,
         migrants_from_other_provinces = `net_interregional_in-migration`)%>%
  mutate(additional_supply_requirement = job_openings - immigrants - migrants_from_other_provinces - young_people_starting_work,
         labour_force_exits = -1 * (deaths + retirements),
         net_change_in_labour_force = young_people_starting_work + immigrants + migrants_from_other_provinces + additional_supply_requirement + labour_force_exits
         )%>%
  select(date, 
         geographic_area, 
         noc, 
         description, 
         industry, 
         job_openings, 
         young_people_starting_work, 
         immigrants, 
         migrants_from_other_provinces,
         additional_supply_requirement,
         labour_force_exits,
         net_change_in_labour_force)

# ind_char2 INPUT TO jobs_and_industry-----------
ind_char2 <- ind_char_raw%>% 
  filter(sector != "total")%>%
  mutate(sector = if_else(ind_group_tech_intensive_industries == "tech_intensive", 
                          "technology", 
                          sector)
         )%>%
  select(-ind_group_trades_intensive_industries, 
         -ind_group_tech_intensive_industries, 
         -ita_sector_advisory_group)

#jobs_and_industry INPUT TO jobs_employment AND mapping AND jobs_industry_noc--------------
jobs_and_industry <-
  full_join(jo_employment,
    ind_char2,
    by = c("industry_code", "industry", "aggregate_industry"))%>%
  select(-agg_industry_code)%>%
  filter(industry != "all_industries")%>%
  clean_tbbl()

#break down NOC mappings by hierarchy
# noc_broad_occ INPUT TO occupation2------------
noc_broad_occ <- noc_mappings_raw%>%
  filter(hierarchical_structure == "broad_occupational_category")%>%
  mutate(code = paste0("#", code))%>%
  select(noc1 = code, noc1_description = class_title)

#' Do major groups next... note that for some Classes the Code is a range e.g. 01-05
#' what we want to split these ranges up e.g. Codes 1,2,3,4,5
#' and we do so by first splitting the range into a start and a finish value,
#' nesting the data by the description, creating a variable NOC2 containing the sequence,
#' and then un-nest NOC2.

# noc_major_group INPUT TO occupation2-------------
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

# noc_minor_group INPUT TO occupation2-------------
noc_minor_group <- noc_mappings_raw%>%
  filter(hierarchical_structure == "minor_group")%>%
  select(noc3 = code, noc3_description = class_title)%>% 
  mutate(noc3 = str_pad(noc3, width = 3, pad = "0"),
         noc3 = paste0("#", noc3))

# noc_unit INPUT TO occupation2---------------
noc_unit <- noc_mappings_raw %>%
  filter(hierarchical_structure == "unit_group")%>%
  select(noc = code, noc4_description = class_title)%>% 
  mutate(noc = str_pad(noc, width = 4, pad = "0"),
         noc = paste0("#", noc))
 
# education_occupation INPUT TO jobs_employment AND occ_characteristics-----------
education_occupation <- education_occupation_raw %>%
  select(noc, noc1, noc2, noc3, education_typical_background)%>%
  filter(noc!="#t")

# jobs_employment INPUT TO occupation AND by_aggregated_industry AND j_openings AND emp---------------
jobs_employment <- jobs_and_industry%>%
  full_join(education_occupation, by = c("noc"))

# occupation INPUT TO occupation2------------
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
# occupation2 INPUT TO Occupations_regional.csv AND noc_mappings2------------
occupation2 <- occupation%>%
  full_join(noc_broad_occ, by = "noc1") # merge with broad occupations
occupation2 <- occupation2%>%
  full_join(noc_major_group, by = "noc2") # merge with major groups
occupation2 <- occupation2%>%
  full_join(noc_minor_group, by = "noc3") # merge with minor groups
occupation2 <- occupation2%>%
  full_join(noc_unit, by = "noc") # merge with unit groups

#noc_mappings2 INPUT TO jobs_industry_noc--------------
noc_mappings2 <-
  unique(occupation2[, c(
    "noc",
    "noc1",
    "noc2",
    "noc3",
    "noc1_description",
    "noc2_description",
    "noc3_description",
    "noc4_description"
  )])

#jobs_industry_noc INPUT TO Jobs_and_Industry.csv---------------
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
individual_industry_agg_industry <- bind_rows(by_individual_industry, by_aggregated_industry)%>%
  unique()

# wages_cleaned INPUT TO group_and_wages--------------------
wages_cleaned <- wages_raw%>%
  select(noc= NOC,
         region = "Economic Region",
         low_wage = contains("Low Wage"),
         median_wage = contains("Median Wage"),
         high_wage = contains("High Wage"))%>%
  remove_missing()%>%
  filter(region != "National")%>%
  mutate(region= factor(
    region,
    levels = levels(as.factor(region)),
    labels = c(
      "British Columbia",
      "Cariboo",
      "Kootenay",
      "Mainland South West",
      "North Coast & Nechako",
      "North Coast & Nechako",
      "North East",
      "Thompson Okanagan",
      "Vancouver Island Coast"
    )
  ),
  region=as.character(region)
  )%>%
  clean_tbbl()

#group_and_wages INPUT TO group_wages_characteristics-------------------------
group_and_wages <- wages_cleaned%>%
  full_join(occ_group, by=c("region"="geographic_area","noc"="noc"), multiple = "all")%>%
  na.omit()

# occ_characteristics INPUT TO group_wages_characteristics----------------------------
occ_characteristics <- education_occupation_raw%>%
  select(
    noc,
    occupation_title = description,
    typical_education = education_typical_background,
    alternative_education = education_alternative_background,
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
                                         by = "noc")

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

# group_wages_characteristics INPUT TO occ_characteristics_wage.csv---------------
group_wages_characteristics <-
  full_join(group_wages_characteristics, j_openings, by = c("noc", "region"))
group_wages_characteristics <-
  full_join(group_wages_characteristics, emp, by = c("noc", "region"))%>%
  filter(!is.na(occupation_group))

# Write_to_File------------------
clean_and_save(jo_employment, "Clean_JO.csv")
clean_and_save(ds_merged, "Supply_cleaned.csv")
clean_and_save(occupation2, "Occupations_regional.csv")
clean_and_save(jobs_industry_noc, "Jobs_and_Industry.csv")
clean_and_save(individual_industry_agg_industry, "Employment_Growth_Rates.csv")
clean_and_save(group_wages_characteristics, "occ_characteristics_wage.csv")

# document objects for 02_knit_me.Rmd----------

df_names = ls()[sapply(ls(), function(x) any(is.data.frame(get(x))))]
dfs <- tibble(df_names)%>%
  mutate(info = map(df_names, col_names_type_example))
saveRDS(dfs, "dataframes.RDS")
tictoc::toc()



