tictoc::tic()
#1.1 PRE-REQS----------------
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
#' "2020Preliminary wages.xlsx": LIKELY THIS FILE NAME WILL CHANGE AND THEREFORE NEED TO CHANGE LINE 5 OF FILE "previously_duplicated_code.R"
# Functions------------
#this function take a tbbl with two columns, start and finish and returns start OR a sequence between start and finish.
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
    filter(Date == year1)
  all_other_years <- tbbl%>%
    filter(Date %in% c(as.numeric(year1 + 1):year3)) 
  first_year_employment <- sum(first_year$Employment)
  expansion <- sum(all_other_years$`Expansion Demand`)
  replacement <- sum(all_other_years$`Replacement Demand`)
  job_openings <- sum(all_other_years$`Job Openings`)
  
  tibble(`Employment year1` = first_year_employment,
         `Expansion year1-year3` = expansion,
         `Replacement year1-year3` = replacement,
         `Job Openings year1-year3` = job_openings)
}

all_aggregates <- function(tbbl){
  ca <- common_aggregates(tbbl)
  first_year <- tbbl%>%
    filter(Date == year1)
  first_year_employment <- sum(first_year$Employment)
  second_year <- tbbl%>%
    filter(Date == year2)
  second_year_employment <- sum(second_year$Employment)
  third_year <- tbbl%>%
    filter(Date == year3)
  third_year_employment <- sum(third_year$Employment)
  
  CAGR1 <- 100 * ((second_year_employment/first_year_employment)^(1 / 5) - 1)
  CAGR2 <- 100 * ((third_year_employment/second_year_employment)^(1 / 5) - 1)
  CAGR3 <- 100 * ((third_year_employment/first_year_employment)^(1 / 10) - 1)
  
  ave_replace <- tbbl%>%
    group_by(Date)%>%
    summarize(employment = sum(Employment),
              replacement_demand = sum(`Replacement Demand`))%>%
    mutate(replacement_rate = replacement_demand / employment)%>%
    summarize(mean(replacement_rate)*100)%>%
    pull()
  
  bind_cols(ca, 
            `Employment Growth year1-year2` = CAGR1, 
            `Employment Growth year2-year3` = CAGR2, 
            `Employment Growth year1-year3` = CAGR3,
            `Annual Replacement Rate` = ave_replace
  )
}

aggregate_jobs_employment_by <- function(var){
  jobs_employment%>%
    group_by(`Geographic Area`, {{ var  }})%>%
    nest()%>%
    mutate(aggregated = map(data, all_aggregates))%>%
    select(-data)%>%
    unnest(aggregated)%>%
    pivot_longer(cols = -c(`Geographic Area`, {{  var  }}))%>%
    rename(`Geographic.Area` = `Geographic Area`,
           Variable = name,
           `Level.Value` = {{  var  }},
           Value = value)%>%
    mutate(Level = rlang::englue("{{  var  }}"))
}


# 1.2 a lot of code duplication between the Rmd versions of LMO tool and industry tool. Load the common code.
source("previously_duplicated_code.R")

# 1.4_Prep_Supply_Demand-------
# The final output is a file with columns Date, Geographic Area, NOC, Description, Industry, Job Openings, Young People Starting Work, Immigrants, Migrants of other provinces, additional supply requirement, labour force exits, net change in labour force

DS <- DS_raw # make a copy of the raw data file

DS <- DS%>%
  mutate(NOC = as.factor(trimws(NOC)),
         Description = str_split_fixed(Description, "\t", n = 2)[, 1],#gets rid of tab delimiters \t
         Industry = as.factor(tolower(trimws(Industry))),
         Variable = as.factor(trimws(Variable)),
         `Geographic Area` = as.factor(`Geographic Area`))%>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  select(order(colnames(.))) %>%
  arrange(Date, NOC, Description, Industry, `Geographic Area`)

# Take that duplicate of the Job Openings data frame that we made, and filter so that for each occupation, we have all industries

JO_duplicate <- JO_duplicate %>%
  filter(Description != "Total",
        NOC != "#T",
        Industry == "All industries")%>%
  mutate(NOC =as.factor(NOC),
         Industry = as.factor(tolower(trimws(Industry))),
         Description = as.factor(trimws(str_split_fixed(Description, "\t", n = 2)[, 1])))%>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  select(order(colnames(.))) %>%
  arrange(Date, NOC, Description, Industry, `Geographic Area`)       

# Merge demand/supply with job openings
# Columns are Date, NOC, Description, Industry, Geographic Area, Deaths, Expansion Demand, Replacement Demand, Job Openings, Retirements, Net International In-Migration, Net Interregional In-Migration, Net Other Mobility, New Entrants
DS_merged <-
  merge(
    JO_duplicate,
    DS,
    by = c("Date", "Geographic Area", "NOC", "Description", "Industry", "Deaths", "Expansion Demand", "Job Openings", "Retirements"),
    all = TRUE)%>%
  rename(`Young people starting work` = `New Entrants`,
         Immigrants = `Net International In-Migration`,
         `Migrants from other provinces` = `Net Interregional In-Migration`)%>%
  mutate(`Additional supply requirement` =`Job Openings` - Immigrants - `Migrants from other provinces` -`Young people starting work`,
         `Labour force exits` = -1 * (Deaths + Retirements),
         `Net change in labour force` = `Young people starting work` + Immigrants + `Migrants from other provinces` + `Additional supply requirement` + `Labour force exits`
         )%>%
  select(`Date`, 
         `Geographic Area`, 
         `NOC`, 
         `Description`, 
         `Industry`, 
         `Job Openings`, 
         `Young people starting work`, 
         `Immigrants`, 
         `Migrants from other provinces`, 
         `Additional supply requirement`, 
         `Labour force exits`, 
         `Net change in labour force`)%>%
  mutate(Industry = str_to_title(as.character(Industry)))
 
# 1.5_Prep_Job_Openings_With_Industry_Characteristics-----

# 1. Jobs_and_Industry
# Columns: "NOC", "Industry Code", "Industry", "Date", "Description", "Geographic Area", "Job Openings", "Expansion Demand", "Replacement Demand", "Deaths", "Retirements", "Employment", "NAICS Definition", "Aggregate Industry", "Sector Code", "Sector", "NOC1", "NOC2", "NOC3", "NOC1 Description", "NOC2 Description", "NOC3 Description", "NOC4 Description"
# 2. Occupation2
# Columns: "NOC", "NOC3", "NOC2", "NOC1", "Geographic Area", "Description", "Education:.Typical.Background", "Employment year1", "Expansion year1-year3", "Replacement year1-year3", "Job Openings year1-year3", "NOC1 Description", "NOC2 Description", "NOC3 Description","NOC4 Description"
# Read in the cleaned job openings file we created
# Columns include Industry, Date, NOC, Description, Industry Code, Geographic Area, Job Openings, Expansion Demand, Replacement Demand, Deaths, Retirement, Employment, Aggregate Industry
job_openings <- JO_Employment

job_openings <- job_openings%>%
  mutate(Industry = as.factor(trimws(tolower(Industry))),
         `Aggregate Industry` = tolower(`Aggregate Industry`))

ind_char <- ind_char_raw

ind_char <- ind_char%>%
  mutate(Industry = as.factor(trimws(tolower(Industry))))

ind_char$Industry <-
  as.factor(trimws(tolower(ind_char$Industry))) # trim white space, factor, to lower case for industry column
# Rename columns (FRAGILE!)
#colnames(ind_char) <- str_to_title(str_replace_all(colnames(ind_char),"\\."," "))
colnames(ind_char) <-
  c(
    "Industry Code",
    "Industry",
    "NAICS Definition",
    "Agg Industry Code",
    "Aggregate Industry",
    "Sector Code",
    "Sector",
    "Ind Group: Trades Intensive Industries",
    "Ind group: Tech Intensive Industries",
    "ITA Sector Advisory Group"
  )

# Create a copy of the industry characteristics file with only the first seven columns, Industry Code, Industry, NAICS definitions, agg industry code, aggregate industry, sector code, sector
ind_char2 <- filter(ind_char, Sector != "Total")%>%
  mutate(Sector = if_else(`Ind group: Tech Intensive Industries`== "Tech intensive", 
                          "Technology", 
                          Sector)
         )%>%
  select(-`Ind Group: Trades Intensive Industries`, 
         -`Ind group: Tech Intensive Industries`, 
         -`ITA Sector Advisory Group`)

# Merge these two data frames
# Columns are industry code, industry, date, noc, description, geographic area, jobs openings, expansion demand, replacement demand, deaths, retirements, employment, aggregate industry.x (code for aggregate industry from df 1),NAICS defintion, Aggregate Industry.y (code for aggregate industry from df2), Sector code (aggregate industry), Sector
Jobs_and_industry <-
  merge(job_openings,
    ind_char2,
    by = c("Industry Code", "Industry"),
    all = TRUE
  )%>%
  select(-`Aggregate Industry.x`, -`Agg Industry Code`)%>%
  rename(`Aggregate Industry`=`Aggregate Industry.y`)%>%
  mutate(`Industry Code` = as.factor(`Industry Code`),
         Industry = as.factor(Industry),
         Date = as.factor(Date),
         Description = as.factor(Description),
         `NAICS Definition` = as.factor(`NAICS Definition`),
         `Aggregate Industry` = as.factor(`Aggregate Industry`),
         `Sector Code` = as.factor(`Sector Code`), 
         Sector = as.factor(Sector), 
         `Geographic Area` = as.factor(`Geographic Area`))%>%
  filter(Industry != "all industries")

#break down NOC mappings by hierarchy---------------

# factor hierarchical structure column
# Broad occupational category, major group, minor group, unit group
noc_mappings <- noc_mappings_raw
noc_mappings$`Hierarchical structure` <-
  as.factor(noc_mappings$`Hierarchical structure`)

# Do broad occupational categories first

noc_broad_occ <- noc_mappings%>%
  filter(`Hierarchical structure` == "Broad occupational category")%>%
  mutate(Code = paste0("#", Code))%>%
  select(NOC1 = Code, `NOC1 Description` = `Class title`)

#' Do major groups next... note that for some Classes the Code is a range e.g. 01-05
#' what we want to split these ranges up e.g. Codes 1,2,3,4,5
#' and we do so by first splitting the range into a start and a finish value,
#' nesting the data by the description, creating a variable NOC2 containing the sequence,
#' and then unnesting NOC2.

noc_major_group <- noc_mappings%>%
  filter(`Hierarchical structure` == "Major group")%>%
  separate(Code, into = c("start", "finish"), sep = "-", fill = "right")%>% 
  mutate(start=as.numeric(start),
         finish=as.numeric(finish))%>%
  select(start, finish, `NOC2 Description` = `Class title`)%>%
  group_by(`NOC2 Description`)%>%
  nest()%>%
  mutate(NOC2 = map(data, fill_range))%>%
  select(-data)%>%
  unnest(NOC2)%>% 
  mutate(NOC2 = str_pad(NOC2, width = 2, pad = "0"),
         NOC2 = paste0("#", NOC2))

# Do minor groups next

noc_minor_group <- noc_mappings%>%
  filter(`Hierarchical structure` == "Minor group")%>%
  select(NOC3=Code, `NOC3 Description`=`Class title`)%>% 
  mutate(NOC3 = str_pad(NOC3, width = 3, pad = "0"),
         NOC3 = paste0("#", NOC3))

# Finally do the unit group

noc_unit <- noc_mappings %>%
  filter(`Hierarchical structure` == "Unit group")%>%
  select(NOC=Code, `NOC4 Description`=`Class title`)%>% 
  mutate(NOC = str_pad(NOC, width = 4, pad = "0"),
         NOC = paste0("#", NOC))
 
#Jobs and employment-----------

jobs_employment <-
  Jobs_and_industry # make a copy of Jobs_and_industry data frame, and name it jobs_employment

jobs_employment <- jobs_employment%>%
  filter(Industry != "all industries")%>%
  mutate(`Industry Code` = as.factor(`Industry Code`),
        Industry = as.factor(tolower(Industry)),
        NOC = as.factor(NOC),
        `Geographic Area` = as.factor(`Geographic Area`),
        `Aggregate Industry` = as.factor(`Aggregate Industry`))

education_occupation <- education_occupation_raw

education_occupation <- education_occupation %>%
  select(starts_with("NOC"), `Education:.Typical.Background`, -contains("&"))%>%
  mutate(NOC = as.factor(NOC),
         NOC1 = as.factor(NOC1),
        NOC2 = as.factor(NOC2),
        NOC3 = as.factor(NOC3))

jobs_employment <-
  merge(jobs_employment, education_occupation, by = c("NOC"))%>%
  mutate(Industry = as.factor(tolower(Industry)))

#Occupation dataframe does not have full set of aggregates
Occupation <- jobs_employment%>%
  group_by(`Geographic Area`,
           NOC,
           NOC1,
           NOC2,
           NOC3,
           Description,
           `Education:.Typical.Background`)%>%
  nest()%>%
  mutate(aggregated = map(data, common_aggregates))%>%
  select(-data)%>%
  unnest(aggregated)


# Take the previously defined NOC levels (noc_broad_occ, noc_major_group, noc_minor_group and noc_unit) and bind them to our occupation data frame ==> this gives us the descriptions for each NOC level (ie NOC1 and NOC1 description, NOC2 and NOC2 description etc)
Occupation2 <-
  merge(Occupation, noc_broad_occ, by = "NOC1", all = TRUE) # merge with broad occupations
Occupation2 <-
  merge(Occupation2, noc_major_group, by = "NOC2", all = TRUE) # merge with major groups
Occupation2 <-
  merge(Occupation2, noc_minor_group, by = "NOC3", all = TRUE) # merge with minor groups
Occupation2 <-
  merge(Occupation2, noc_unit, by = "NOC", all = TRUE) # merge with unit groups
Occupation2 <-
  Occupation2[!is.na(Occupation2$`Employment year1`), ] # remove rows with NAs

#Occupation2 is not further modified below----------------

# From this dataframe we can get the NOC mappings (so the NOC digits & the NOC descriptions)
NOC_mappings2 <-
  unique(Occupation2[, c(
    "NOC",
    "NOC1",
    "NOC2",
    "NOC3",
    "NOC1 Description",
    "NOC2 Description",
    "NOC3 Description",
    "NOC4 Description"
  )])
Jobs_and_Industry <-
  merge(Jobs_and_industry, NOC_mappings2, by = "NOC") # merge these mappings with the Jobs_and_industry data frame
Jobs_and_Industry$Industry <-
  str_to_title(as.character(Jobs_and_Industry$Industry)) # convert to title case for the industry
Jobs_and_Industry$`Aggregate Industry` <- str_to_title(trimws(Jobs_and_Industry$`Aggregate Industry`))
Jobs_and_Industry$Sector <- str_to_title(trimws(as.character(Jobs_and_Industry$Sector)))
Jobs_and_Industry[which(Jobs_and_Industry$Sector == "Agrifood Sector"), "Sector"] <- "Agrifoods Sector"

# 1.6 Aggregate dataframe jobs_employment by region and sector (not referenced below????)------------

# by_sector <- aggregate_jobs_employment_by(Sector)

# 1.7 Aggregate dataframe jobs_employment by region and aggregate industry------------

by_aggregated_industry <- aggregate_jobs_employment_by(`Aggregate Industry`)

# 1.8 Aggregate dataframe jobs_employment by region and industry------------

by_individual_industry <- aggregate_jobs_employment_by(`Industry`)

# To this data frame , we are going to map the aggregate industry categories, we can get these from the Jobs_and_Industry data frame
mapping <- unique(Jobs_and_industry[, c("Industry", "Aggregate Industry")]) # Take the all unique pairings of Industry and Aggregate Industry from the Jobs and Industry data frame
colnames(mapping) <- c("Level.Value", "Aggregate Industry") # rename these columns to match what we have for the previous data frame
mapping$Level.Value <- tolower(mapping$Level.Value) # convert to lower case to match up what we had before

by_individual_industry$Level.Value <- tolower(by_individual_industry$Level.Value) # convert to lower case to ensure the industries will match up
by_individual_industry <- merge(by_individual_industry, mapping) # merge to mapping, now we have a column with the Aggregate industries matching to the LMO 61 industries

# Duplicate the titles of the aggregate industries to the aggregate industry column
by_aggregated_industry$`Aggregate Industry` <- by_aggregated_industry$Level.Value

# Bind the aggregated industry data frame to the LMO 61 industry data frame
individual_industry_agg_industry <- rbind(by_individual_industry, by_aggregated_industry)
individual_industry_agg_industry$Level.Value <-
  str_to_title(individual_industry_agg_industry$Level.Value) # convert industries to title case
individual_industry_agg_industry <- # ensure there are no duplicates
  unique(individual_industry_agg_industry)

# 1.9_Wage_Data--------------------
# We need a data frame with low wages, median wages, and high wages for the 500 occupations

wages_cleaned <- wages_raw%>%
  select(NOC,
        Region = "Economic.Region",
        `Low Wage` = "Low.Wage.($).2020.(1st.decile)",
        `Median Wage` = "Median.Wage.($).2020",
        `High Wage` = "High.Wage.($).2020.(9th.decile)")%>%
  remove_missing()%>%
  filter(Region != "National")%>%
  mutate(Region= factor(
    Region,
    levels = levels(as.factor(Region)),
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
  ))


# We also want to do this by occupation group (ie HOO group)
colnames(group) <- c("NOC", "Occupation Group", "Region") # Rename columns to NOC, Region and Occupation Group

group <- group%>%
  mutate(NOC = as.factor(NOC),
         `Occupation Group` = as.factor(group$`Occupation Group`))

group_and_wages <- merge(group, wages_cleaned, all = TRUE) # merge the group data with the wages data

occ_characteristics <- occ_char_raw%>%
  select(
    NOC,
    `Occupation Title` = "Description",
    `Typical Education` = "Education:.Typical.Background",
    `Alternative Education` = "Education:.Alternative.Background",
    `Interest1`,
    `Interest2`,
    `Interest3`,
    `Skill1`,
    `Skill2`,
    `Skill3`,
    `Interests`,
    `Top 3 Skills and Competencies` = "Skills:.Top.3")%>%
  filter(NOC != "#T")%>%
  mutate(NOC=as.factor(NOC))

# Merge this cleaned data frame with the groups and wages data frame
group_wages_characteristics <- merge(group_and_wages, occ_characteristics)

# To this we need to add the job openings data for year1-2031

jo <- jobs_employment%>%
  select("NOC", "Date", "Geographic Area", "Job Openings")%>%
  filter(Date != year1 - 1,
         Date != year1)

jo <- aggregate(`Job Openings` ~ NOC + `Geographic Area`, jo, sum) # aggregate job openings by NOC and geographic area
colnames(jo) <- c("NOC", "Region", "Job Openings") # rename the columns

# We need to add Employment year1

emp <- jobs_employment%>%
  select("NOC", "Date", "Geographic Area", "Employment")%>%
  filter(Date == year1)

emp <- aggregate(Employment ~ NOC + `Geographic Area`, emp, sum) # aggregate employment by NOC and geographic area
colnames(emp) <- c("NOC", "Region", "Employment year1") # rename the columns

# we need to merge these to our data frame, match up by NOC and Region
group_wages_characteristics <-
  merge(group_wages_characteristics, jo, by = c("NOC", "Region"))
group_wages_characteristics <-
  merge(group_wages_characteristics, emp, by = c("NOC", "Region"))
group_wages_characteristics <- group_wages_characteristics[!is.na(group_wages_characteristics$`Occupation Group`), ]

# 1.10_Write_to_File---------------------------------------------------------------------------------------------------------

fwrite(JO_Employment, here("Tableau Tool Inputs", "Clean_JO.csv"))
fwrite(DS_merged, here("Tableau Tool Inputs","Supply_cleaned.csv"))
fwrite(Occupation2, here("Tableau Tool Inputs", "Occupations_regional.csv"))
fwrite(Jobs_and_Industry, here("Tableau Tool Inputs", "Jobs_and_Industry.csv"))
fwrite(individual_industry_agg_industry, here("Tableau Tool Inputs", "Employment_Growth_Rates.csv"))
fwrite(group_wages_characteristics, here("Tableau Tool Inputs", "occ_characteristics_wage.csv"))
tictoc::toc()