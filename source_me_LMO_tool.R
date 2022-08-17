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
 
#OLD CODE BELOW-------------

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
         
#' we need to have typical education background, 
#' NOC (hierarchy), 
#' occupation title, 
#' employment, 
#' expansion year1-year3, 
#' replacement year1-year3, 
#' job openings year1-year3 (wages too?) (**)

# Create two null data frames for storage
by_occupation <- NULL
Occupation <- NULL

#' For each geography and at each occupation (4 digit) level, 
#' we need to create a variable for employment year1, 
#' expansion year1-year3, 
#' replacement year1-year3 
#' and job openings year1-year3

for (i in 1:nlevels(jobs_employment$`Geographic Area`)) {
  # loop through each level of geography

  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # create a temporary data frame, filter to only include the geography of interest
  temp$NOC <-
    as.factor(temp$NOC) # ensure the various NOC levels are of type `factor`
  temp$NOC3 <- as.factor(temp$NOC3) # factor
  temp$NOC2 <- as.factor(temp$NOC2) # factor
  temp$NOC1 <- as.factor(temp$NOC1) # factor
  temp$Description <-
    as.factor(temp$Description) # factor the description column
  temp$`Education:.Typical.Background` <-
    as.factor(temp$`Education:.Typical.Background`) # factor the education column
  temp$`Geographic Area` <-
    as.factor(temp$`Geographic Area`) # factor the geography column

  temp <-
    filter(temp, Date %in% seq(year1, year3)) # we don't need the data that is less than the minimum year, filter this out

  # Get employment year1 data for each geography

  employment_year1 <-
    filter(temp, Date == year1) # filter to only include minimum year in data
  employment_year1 <-
    aggregate(
      Employment ~ `Geographic Area` + NOC + NOC1 + NOC2 + NOC3 + Description + `Education:.Typical.Background`,
      employment_year1,
      sum
    ) # aggregate by the NOC level & education level

  # Get expansion, replacement and job openings data for year1-year3 for the specified geography, aggregated at the NOC & education level
  df_year1_year3 <-
    filter(temp, Date != year1) # remove year1 (data included in sum is year1+1-year3)
  expansion_year1_year3 <-
    aggregate(
      `Expansion Demand` ~ `Geographic Area` + NOC + NOC1 + NOC2 + NOC3 + Description + `Education:.Typical.Background`,
      df_year1_year3,
      sum
    ) # aggregate expansion demand
  replacement_year1_year3 <-
    aggregate(
      `Replacement Demand` ~ `Geographic Area` + NOC + NOC1 + NOC2 + NOC3 + Description + `Education:.Typical.Background`,
      df_year1_year3,
      sum
    ) # aggregate replacement demand
  job_openings_year1_year3 <-
    aggregate(
      `Job Openings` ~ `Geographic Area` + NOC + NOC1 + NOC2 + NOC3 + Description + `Education:.Typical.Background`,
      df_year1_year3,
      sum
    ) # aggregate job openings

  # Rename the Columns to the appropriate headings
  colnames(employment_year1)[ncol(employment_year1)] <-
    "Employment year1"
  colnames(expansion_year1_year3)[ncol(expansion_year1_year3)] <-
    "Expansion year1-year3"
  colnames(replacement_year1_year3)[ncol(replacement_year1_year3)] <-
    "Replacement year1-year3"
  colnames(job_openings_year1_year3)[ncol(job_openings_year1_year3)] <-
    "Job Openings year1-year3"

  # Merge these data frames together, so we have all the data saved into the data frame "Occupation"
  by_occupation <-
    merge(
      employment_year1,
      expansion_year1_year3,
      by = c(
        "Geographic Area",
        "NOC",
        "NOC1",
        "NOC2",
        "NOC3",
        "Description",
        "Education:.Typical.Background"
      )
    )
  by_occupation <- merge(by_occupation, replacement_year1_year3)
  by_occupation <- merge(by_occupation, job_openings_year1_year3)
  Occupation <- rbind(by_occupation, Occupation)
}


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

# From this file we can get a data frame with the NOC mappings (so the NOC digits & the NOC descriptions)
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

# 1.6_Prep_Growth_Rates_Sectors------------------
# We need to calculate growth rates for the individual industries, the aggregate industries and at the sector level because these will be too difficult to aggregate directly in Tableau. This section loops over the sectors.
# Create two empty data frames to act as storage as we loop through
by_sector_industry <- NULL # placeholder for first loop
by_sector <- NULL # placeholder for second loop

# Handle by sector first, for each geography and at each sector level we are going to calculate employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) 2026-2031, Employment Growth (CAGR) year1-2031, Expansion year1-2031, Replacement year1-2031, Total Job Openings year1-2031

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or sector) and Value (numeric value for the variable)

for (i in 1:nlevels(jobs_employment$`Geographic Area`)) {
  # loop through each geographic area
  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # filter a temporary data frame from "job_employment" that only includes the geographic area of interest
  temp$`Sector` <-
    as.factor(temp$`Sector`) # ensure that "sector code" is a factor

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-2026, Employment Growth (CAGR) 2026-2031, Employment Growth (CAGR) year1-2031, Expansion year1-2031, Replacement year1-2031, Total Job Openings year1-2031 for the AGGREGATED INDUSTRIES

  for (j in 1:nlevels(temp$`Sector`)) {
    # loop through each sector

    # We are going to get the employment year1 data for the geography and sector first
    temp_employment_year1 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date == year1) # filter to only include geography/sector and year to year1
    employment_year1 <-
      sum(as.numeric(temp_employment_year1$Employment)) # we need to sum this value to get employment for the sector
    agg_industry_employment <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or sector) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1",
        "Level" = "Sector",
        "Level Value" = levels(temp_employment_year1$`Sector`)[j],
        "Value" = employment_year1
      )


    # Calculate the employment growth from year1 to 2026: This is calculated as the (employment in 2026/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR for year1 to 2026
    CAGR1 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for 2026 to 2031
    temp_employmentgrowth_year2_year3 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% c(year2, year3))

    CAGR2 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year2)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for year1 to 2031
    temp_employmentgrowth_year1_year3 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% c(year1, year3))

    CAGR3 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year1)$Employment
        )
      )^(1 / 10)) - 1)


    # We assign each of these CAGRS to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level or Sector), Level Value (the name assigned to the industry or sector) and Value (numeric value for the variable)

    agg_industry_employment2 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year2",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = CAGR1
      )

    agg_industry_employment3 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year2-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = CAGR2
      )

    agg_industry_employment4 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = CAGR3
      )



    # Calculate the expansion demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the sector and geography)
    expansion <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    expansion <- sum(expansion$`Expansion Demand`)

    # Calculate the replacement demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the sector and geography)
    replacement <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    replacement <- sum(replacement$`Replacement Demand`)

    # Calculate the job openings from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the sector and geography)
    job_openings_tot <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    job_openings_tot <- sum(job_openings_tot$`Job Openings`)


    # We assign expansion demand, replacement demand and job openings year1-2031 to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or sector) and Value (numeric value for the variable)

    agg_industry_employment5 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = expansion
      )
    agg_industry_employment6 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement
      )
    agg_industry_employment7 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = job_openings_tot
      )


    # Calculate the average annual replacement rate
    avg <- NULL

    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement <- sum(date2$`Replacement Demand`)
      percent <- as.numeric(replacement) / as.numeric(emp)
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    agg_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Annual Replacement Rate",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement_rate
      )


    # Now we bind all of the sector information together into one data frame - this loop will repeat again for each level of aggregate industries for this specific geography
    by_sector_industry <-
      rbind(
        agg_industry_employment,
        agg_industry_employment2,
        agg_industry_employment3,
        agg_industry_employment4,
        agg_industry_employment5,
        agg_industry_employment6,
        agg_industry_employment7,
        agg_industry_employment8,
        by_sector_industry
      )
  }
  # Once the variables have been calculate for each sector in the geography, the loop will be exited. We bind the results of all the aggregate industries for that geography into the data frame "by_sector" and then we go on to the next geography in the loop and repeat
  by_sector <-
    rbind(by_sector_industry, by_sector)
}

# Ensure there are no duplicate entries in the data frame which may cause double counting.
by_sector <- unique(by_sector)
by_sector <- remove_missing(by_sector)
by_sector <- filter(by_sector, Level.Value != "Total")

# 1.7_Prep_Growth_Rates_Aggregate_Industries---------
# We need to calculate growth rates for the individual industries, the aggregate industries and at the sector level because these will be too difficult to aggregate directly in Tableau. This section loops over the aggregate industries.
# Create two empty data frames to act as storage as we loop through
by_agg_industry <- NULL # placeholder for first loop
by_aggregated_industry <- NULL # placeholder for second loop

# Handle by aggregate industry first, for each geography and at each aggregate industry level we are going to calculate employment year1, Employment Growth (CAGR) year1-2026, Employment Growth (CAGR) 2026-2031, Employment Growth (CAGR) year1-2031, Expansion year1-2031, Replacement year1-2031, Total Job Openings year1-2031

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

for (i in 1:nlevels(jobs_employment$`Geographic Area`)) {
  # loop through each geographic area
  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # filter a temporary data frame from "job_employment" that only includes the geographic area of interest
  temp$`Aggregate Industry` <-
    as.factor(temp$`Aggregate Industry`) # ensure that "aggregate industry code" is a factor

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-2026, Employment Growth (CAGR) 2026-2031, Employment Growth (CAGR) year1-2031, Expansion year1-2031, Replacement year1-2031, Total Job Openings year1-2031 for the AGGREGATED INDUSTRIES

  for (j in 1:nlevels(temp$`Aggregate Industry`)) {
    # loop through each aggregate industry

    # We are going to get the employment year1 data for the geography and aggregate industry first
    temp_employment_year1 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date == year1) # filter to only include geography/aggregate industry and year to year1
    employment_year1 <-
      sum(as.numeric(temp_employment_year1$Employment)) # we need to sum this value to get employment for the aggregate industry
    agg_industry_employment <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp_employment_year1$`Aggregate Industry`)[j],
        "Value" = employment_year1
      )


    # Calculate the employment growth from year1 to 2026: This is calculated as the (employment in 2026/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR for year1 to 2026
    CAGR1 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for 2026 to 2031
    temp_employmentgrowth_year2_year3 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% c(year2, year3))

    CAGR2 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year2)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for year1 to 2031
    temp_employmentgrowth_year1_year3 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% c(year1, year3))

    CAGR3 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year1)$Employment
        )
      )^(1 / 10)) - 1)


    # We assign each of these CAGRS to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

    agg_industry_employment2 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year2",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = CAGR1
      )

    agg_industry_employment3 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year2-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = CAGR2
      )

    agg_industry_employment4 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = CAGR3
      )



    # Calculate the expansion demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the aggregate industry and geography)
    expansion <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    expansion <- sum(expansion$`Expansion Demand`)

    # Calculate the replacement demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the aggregate industry and geography)
    replacement <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    replacement <- sum(replacement$`Replacement Demand`)

    # Calculate the job openings from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the aggregate industry and geography)
    job_openings_tot <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    job_openings_tot <- sum(job_openings_tot$`Job Openings`)


    # We assign expansion demand, replacement demand and job openings year1-2031 to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

    agg_industry_employment5 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = expansion
      )
    agg_industry_employment6 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = replacement
      )
    agg_industry_employment7 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = job_openings_tot
      )


    # Calculate the average annual replacement rate
    avg <- NULL

    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement <- sum(date2$`Replacement Demand`)
      percent <- as.numeric(replacement) / as.numeric(emp)
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    agg_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Annual Replacement Rate",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = replacement_rate
      )


    # Now we bind all of the aggregate industry information together into one data frame - this loop will repeat again for each level of aggregate industries for this specific geography
    by_agg_industry <-
      rbind(
        agg_industry_employment,
        agg_industry_employment2,
        agg_industry_employment3,
        agg_industry_employment4,
        agg_industry_employment5,
        agg_industry_employment6,
        agg_industry_employment7,
        agg_industry_employment8,
        by_agg_industry
      )
  }
  # Once the variables have been calculate for each aggregate industry in the geography, the loop will be exited. We bind the results of all the aggregate industries for that geography into the data frame "by_aggregated_industry" and then we go on to the next geography in the loop and repeat
  by_aggregated_industry <-
    rbind(by_agg_industry, by_aggregated_industry)
}

# Ensure there are no duplicate entries in the data frame which may cause double counting.
by_aggregated_industry <- unique(by_aggregated_industry)

# 1.8_Prep_Growth_Rates_LMO_Industries----------------
# We need to calculate growth rates for the individual industries, the aggregate industries and at the sector level because these will be too difficult to aggregate directly in Tableau. This section loops over the industries.
# We need to do the same thing but now for the individual industries
# Create two empty data frames for storage as we go through the loop
by_ind_industry <- NULL
by_individual_industry <- NULL


for (i in 1:nlevels(jobs_employment$`Geographic Area`)) { # Loop through each geographic area

  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # filter the jobs_employment data frame to only include the geography of interest
  temp$Industry <- as.factor(temp$Industry) # ensure the industry column is a factor

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-2026, Employment Growth (CAGR) 2026-2031, Employment Growth (CAGR) year1-2031, Expansion year1-2031, Replacement year1-2031, Total Job Openings year1-2031 for the levels of the LMO 61 industries

  for (j in 1:nlevels(temp$Industry)) { # Loop over each of the LMO 61 industries

    # We are going to get the employment year1 data for the geography and aggregate industry first
    temp_employment_year1 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date == year1) # filter to only include geography/industry and year to year1

    employment_year1 <-
      sum(as.numeric(temp_employment_year1$Employment)) # we need to sum this value to get employment for the industry
    ind_industry_employment <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1",
        "Level" = "Industry",
        "Level Value" = levels(temp_employment_year1$Industry)[j],
        "Value" = employment_year1
      )

    # Calculate the employment growth from year1 to 2026: This is calculated as the (employment in 2026/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR from year1-2026
    CAGR1 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
        )
      )^(1 / 5)) - 1)

    temp_employmentgrowth_year2_year3 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% c(year2, year3))

    # Calculate the CAGR from 2026-2031
    CAGR2 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year2_year3, Date %in% year2)$Employment
        )
      )^(1 / 5)) - 1)

    temp_employmentgrowth_year1_year3 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% c(year1, year3))

    # Calculate the CAGR from year1-2031
    CAGR3 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year3)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year3, Date %in% year1)$Employment
        )
      )^(1 / 10)) - 1)

    # place the CAGRS into individual data frames: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

    ind_industry_employment2 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year2",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = CAGR1
      )

    ind_industry_employment3 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year2-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = CAGR2
      )

    ind_industry_employment4 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment Growth year1-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = CAGR3
      )


    # Calculate the expansion demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the industry and geography)
    expansion <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    expansion <- sum(expansion$`Expansion Demand`)

    # Calculate the replacement demand from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the industry and geography)
    replacement <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    replacement <- sum(replacement$`Replacement Demand`)

    # Calculate the job openings from year1-2031 (filter the data frame to include years from 2020-2031 and sum for the industry and geography)
    job_openings_tot <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    job_openings_tot <- sum(job_openings_tot$`Job Openings`)


    # place the expansion demand, replacement demand and job openings into individual data frames: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

    ind_industry_employment5 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = expansion
      )
    ind_industry_employment6 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement
      )
    ind_industry_employment7 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = job_openings_tot
      )


    # Calculate the Annual Replacement Rate and place into data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
    avg <- NULL
    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement <- sum(date2$`Replacement Demand`)
      percent <- replacement / emp
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    ind_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Annual Replacement Rate",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement_rate
      )

    # Now we bind all of theindustry information together into one data frame - this loop will repeat again for each level of industries for this specific geography
    by_ind_industry <-
      rbind(
        ind_industry_employment,
        ind_industry_employment2,
        ind_industry_employment3,
        ind_industry_employment4,
        ind_industry_employment5,
        ind_industry_employment6,
        ind_industry_employment7,
        ind_industry_employment8,
        by_ind_industry
      )
  }

  # Once the variables have been calculate for each industry in the geography, the loop will be exited. We bind the results of all the industries for that geography into the data frame "by_individual_industry" and then we go on to the next geography in the loop and repeat
  by_individual_industry <-
    rbind(by_ind_industry, by_individual_industry)
}

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

wages_cleaned <-
  wages_raw[, c(
    "NOC",
    "Economic.Region",
    "Low.Wage.($).2020.(1st.decile)",
    "Median.Wage.($).2020",
    "High.Wage.($).2020.(9th.decile)"
  )] # we don't need some of the columns (ie NOC Code or NOC Title)
colnames(wages_cleaned) <-
  c("NOC", "Region", "Low Wage", "Median Wage", "High Wage") # rename columns so they are cleaner
wages_cleaned <- remove_missing(wages_cleaned) # remove any rows where there is missing information (would just result in NAs in Tableau)
wages_cleaned <- filter(wages_cleaned, Region != "National") # Remove national for region

# Factor the regions so the names of the regions are consistent
wages_cleaned$Region <-
  factor(
    wages_cleaned$Region,
    levels = levels(as.factor(wages_cleaned$Region)),
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
  )

# We also want to do this by occupation group (ie HOO group)
colnames(group) <- c("NOC", "Occupation Group", "Region") # Rename columns to NOC, Region and Occupation Group
group$NOC <- as.factor(group$NOC) # change the NOC level to a factor
group$`Occupation Group` <- as.factor(group$`Occupation Group`) # change the occupation group to a factor

group_and_wages <- merge(group, wages_cleaned, all = TRUE) # merge the group data with the wages data
# group_and_wages <- remove_missing(group_and_wages) #again remove rows with missing data as it will just result in a NA category in Tableau

# we only need to keep certain columns, select those columns and order them
occ_characteristics <-
  occ_char_raw[, c(
    "NOC",
    "Description",
    "Education:.Typical.Background",
    "Education:.Alternative.Background",
    "Interest1",
    "Interest2",
    "Interest3",
    "Skill1",
    "Skill2",
    "Skill3",
    "Interests",
    "Skills:.Top.3"
  )]

# Rename the columns so they are consisten
colnames(occ_characteristics) <-
  c(
    "NOC",
    "Occupation Title",
    "Typical Education",
    "Alternative Education",
    "Interest1",
    "Interest2",
    "Interest3",
    "Skill1",
    "Skill2",
    "Skill3",
    "Interests",
    "Top 3 Skills and Competencies"
  )

occ_characteristics <- filter(occ_characteristics, NOC != "#T") # filter data to not include the NOC #T (total), we can calculate this in Tableau if necessary
occ_characteristics$NOC <- as.factor(occ_characteristics$NOC) # ensure the NOCS are a factor

# Merge this cleaned data frame with the groups and wages data frame
group_wages_characteristics <-
  merge(group_and_wages, occ_characteristics)

# To this we need to add the job openings data for year1-2031
jo <-
  jobs_employment[, c("NOC", "Date", "Geographic Area", "Job Openings")]
jo <- filter(jo, Date != year1 - 1) %>% filter(Date != year1) # filter to only include dates from 2020-2031 used to calculate the total
jo <- aggregate(`Job Openings` ~ NOC + `Geographic Area`, jo, sum) # aggregate job openings by NOC and geographic area
colnames(jo) <- c("NOC", "Region", "Job Openings") # rename the columns

# We need to add Employment year1
emp <-
  jobs_employment[, c("NOC", "Date", "Geographic Area", "Employment")]
emp <- filter(emp, Date %in% year1) # filter to only include 2019
emp <- aggregate(Employment ~ NOC + `Geographic Area`, emp, sum) # aggregate employment by NOC and geographic area
colnames(emp) <- c("NOC", "Region", "Employment year1") # rename the columns

# we need to merge these to our data frame, match up by NOC and Region
group_wages_characteristics <-
  merge(group_wages_characteristics, jo, by = c("NOC", "Region"))
group_wages_characteristics <-
  merge(group_wages_characteristics, emp, by = c("NOC", "Region"))
group_wages_characteristics <- group_wages_characteristics[!is.na(group_wages_characteristics$`Occupation Group`), ]

# 1.10_Write_to_File---------------------------------------------------------------------------------------------------------
fwrite(Occupation2, here("Tableau Tool Inputs", "Occupations_regional.csv"))
fwrite(Jobs_and_Industry, here("Tableau Tool Inputs", "Jobs_and_Industry.csv"))
fwrite(individual_industry_agg_industry, here("Tableau Tool Inputs", "Employment_Growth_Rates.csv"))
fwrite(group_wages_characteristics, here("Tableau Tool Inputs", "occ_characteristics_wage.csv"))
