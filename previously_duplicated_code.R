
# LIBRARIES------------
library("data.table")
library("lubridate")
library("tidyverse")
library("here")
library("dplyr")
library("openxlsx")
# "CONSTANTS"... that change every year------------
year1 <- as.numeric(year(today())) - 1 # delete the -1 once we have current data.
year2 <- year1 + 5
year3 <- year1 + 10

# Read in the most recent wage data
# Columns include NOC (with #), NOC.Code (no #), Noc.Title, Economic.Region.code, Economic.Region, Low Wage, High Wage, Median Wage
wages_raw <- read.xlsx(here(
  "Tableau Tool Inputs",
  "2020Preliminary wages.xlsx" #THIS FILE NAME LIKELY TO CHANGE
))
# Read in Jobs data
# Columns of the data include NOC, Description, Industry, Variable, Geographic Area, Date, Value
JO_raw <- read_csv(here(
  "LMO Master Databases",
  "JO single variables.csv"
),
locale = readr::locale(encoding = "latin1")
) %>%
  filter(NOC != "#T")
# Read in industry characteristics file to get aggregate industry & industry
ind_char_raw <- read.xlsx(here(
  "LMO Master Databases",
  "Industry characteristics.xlsx")) %>%
  mutate(Industry=trimws(tolower(Industry)))
# Read in Employment data
# Columns of the data include NOC, Description, Industry Code, Industry, Variable, Geographic Area, Date and Employment
Employment_raw <- read_csv(here(
  "LMO Master Databases",
  "Emp single variables.csv"
),
locale = readr::locale(encoding = "latin1"))%>%
  mutate(Industry=trimws(tolower(Industry)))

# add in the industry code

Employment_raw <- merge(Employment_raw, ind_char_raw[, c("Industry.Code", "Industry")], all = TRUE)%>%
  rename(`Industry Code` = Industry.Code,
         Employment = Value)%>%
  filter(NOC != "#T")

# Read in demand/supply data
# Columns include NOC, Description, Industry, Variable, Geographic Area, Date, Value
DS_raw <- fread(here(
  "LMO Master Databases",
  "DS single variables.csv"
))
# Read in NOC Mappings
# Columns include Level, Hierarchical structure, code, class title, class definition
noc_mappings_raw <- fread(here(
  "Tableau Tool Inputs",
  "NOC Mappings.csv"
))
# Read in occupation characteristics file and assign to data frame education occupation
education_occupation_raw <- read.xlsx(here(
  "LMO Master Databases",
  "Occupation characteristics.xlsx"
))
# Read in descriptions file - the industry profiles descriptions gives a description of the LMO industries at the sector, aggregate industry and sector level
descriptions_raw <- read.xlsx(here(
  "Tableau Tool Inputs",
  "IndustryProfiles_Descriptions.xlsx"
))
# We will read in the occupation characteristics file again
occ_char_raw <- read.xlsx(here(
  "LMO Master Databases",
  "Occupation characteristics.xlsx"
))

# Group contains the columns: NOC, Geographic Area and Occupation Group

group <- occ_char_raw%>%
  select(NOC, 
         `Occ.Group:.STEM`, 
         `Occ.Group:.Trades`, 
         `Occ.Group:.Construction.trades`, 
         `OCC.Group:.Trades.Mandatory.Certification`,
         `OCC.Group:.Technicians.&.Technologists`, 
         `OCC.Group:.Clean.(Green).Occupations`, 
         `Occ.Group:.Caring`)%>% 
  filter(NOC != "#T")%>%
  mutate(`All Occupations` = "All Occupations")%>%
  pivot_longer(cols = -NOC, names_to = "name", values_to = "Occupation Group") %>%
  select(NOC, `Occupation Group`)

# get NOC and GEO from JO so we don't have to do this manually
noc_geo <- unique(JO_raw[, c("NOC", "Geographic Area")]) %>% filter(NOC != "#T")

group <- merge(group, noc_geo, all = TRUE)%>%
  mutate(NOC = as.factor(NOC))

HOO <- read.xlsx(here("LMO Master Databases", "HOO list.xlsx"))%>%
  mutate(`High.Opportunity.Occupation?` = factor(`High.Opportunity.Occupation?`, levels = c("Yes", "No"), labels = c("HOO", "Non-HOO")))%>%
  filter(NOC != "#T")
colnames(HOO) <- c("NOC", "Geographic Area", "Occupation Group")

group <- merge(HOO, group, by = c("NOC", "Occupation Group", "Geographic Area"), all = TRUE)

# 1.3_Prep_Job_Openings----------------
# The final output is a file with a column for Date, NOC, Description, Industry, Geographic Area, Deaths, Expansion Demand, Job Openings, Replacement Demand, Retirement, Employment and Aggregate Industry
# Create a data frame with NOC and Industry
NOC_Ind <- Employment_raw # Create a copy of the employment data frame

NOC_Ind <- NOC_Ind%>%
  mutate(Description = as.factor(trimws(str_split_fixed(Description, "\t", n = 2)[, 1])),
         NOC = as.factor(trimws(NOC)),
         `Industry Code` = as.factor(trimws(`Industry Code`)),
         Variable = as.factor(trimws(Variable)),
         Industry = as.factor(trimws(tolower(Industry)))
         )

# Create a duplicate of jobs data frame (can be used for testing scripts etc)

JO_duplicate <- JO_raw
JO <- JO_raw

JO <- JO%>%
  mutate(Variable=as.factor(Variable),
         NOC = as.factor(trimws(NOC)),
         Industry = as.factor(tolower(trimws(Industry))),
         Description = as.factor(trimws(str_split_fixed(Description, "\t", n = 2)[, 1]))
         )%>%
  filter(Industry != "all industries", # Note that if label "all industries" changes in the excel file, will need to change (**)
         Description != "Total",
         NOC != "#T")%>%
  pivot_wider(names_from = Variable, values_from = Value) %>%
  arrange(
    NOC,
    Description,
    Industry,
    `Geographic Area`,
    Date
  )
  
# Merge the Employment data frame (NOC_IND) with the job openings data frame

JO_Employment <-
  merge(
    JO,
    NOC_Ind,
    by = c("Date", "NOC", "Description", "Industry", "Geographic Area"),
    all = TRUE
  ) # merge using date, NOC, description, industry and geographic area to match
JO_Employment <-JO_Employment %>% 
  filter(Industry != "all industries")%>%
  select(Date,
         NOC, 
         Description, 
         Employment,
         Industry,
         `Geographic Area`, 
         `Job Openings`, 
         `Expansion Demand`, 
         `Replacement Demand`, 
         `Deaths`, 
         `Retirements`, 
         `Industry Code`)%>%
  mutate(Industry = tolower(as.character(Industry))) # convert industry to lower case

Industry_Aggregate <- ind_char_raw%>%
  select(Industry, `Aggregate Industry` = `Aggregate.Industry`)%>%
  mutate(Industry = tolower(Industry))

# merge to JO_Employment data frame so we have aggregate industry

JO_Employment <- merge(JO_Employment, Industry_Aggregate, by = "Industry", all = TRUE)%>%
  filter(!is.na(`Geographic Area`))%>%
  mutate(Industry = str_to_title(Industry))







