# 1.1.1 PRE-REQS----------------
#' ensure that the current versions of the following files are available in subdirectory "LMO Master Databases"
#' "JO single variables.csv"
#' "Industry characteristics.xlsx"
#' "Emp single variables.csv"
#' "DS single variables.csv"
#' "Occupation characteristics.xlsx"
#' "HOO list.xlsx"
#'
#' and the current version of these files are available in subdirectory "Tableau Tool Inputs"
#' "Jobs_and_Industry.csv"
#' "NOC Mappings.csv"
#' "IndustryProfiles_Descriptions.xlsx"
#' "2020Preliminary wages.xlsx": LIKELY THIS FILE NAME WILL CHANGE AND THEREFORE NEED TO CHANGE LINE 5 OF FILE "read_common_data.R"

# 1.1.2 fair bit of code duplication between the Rmd versions of LMO tool and industry tool. Load the common code.
source("previously_duplicated_code.R")

# 1.1.3 read specific data-------------------
jobs_NOC <- read.csv(here("Tableau Tool Inputs", "Jobs_and_Industry.csv"))
wages <- read.xlsx(here("Tableau Tool Inputs", "2020Preliminary wages.xlsx"))
wages_cleaned <- wages[, c("NOC", "Economic.Region", "Low.Wage.($).2020.(1st.decile)", "Median.Wage.($).2020", "High.Wage.($).2020.(9th.decile)")]
colnames(wages_cleaned) <- c("NOC", "Region", "Low Wage", "Median Wage", "High Wage")

# 1.2_Prep_Job_Openings---------
# The JO_Employment data frame has the columns: Date, NOC, Description, Industry, Geographic Area, Deaths, Expansion Demand, Job Openings, Replacement Demand, Retirement, Employment, Aggregate Industry

job_openings <- JO_Employment
job_openings$Industry <-
  as.factor(trimws(tolower(job_openings$Industry))) # factor, trim white space, lower case for industry column
job_openings$`Aggregate Industry` <- tolower(job_openings$`Aggregate Industry`)

ind_char <- ind_char_raw
ind_char$Industry <-
  as.factor(trimws(tolower(ind_char$Industry))) # trim white space, factor, to lower case for industry column
# Rename columns
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
ind_char2 <- filter(ind_char, Sector != "Total")
for (i in 1:nrow(ind_char2)) {
  if (ind_char2$`Ind group: Tech Intensive Industries`[i] == "Tech intensive") {
    ind_char2$Sector[i] <- "Technology"
  } else {
    next
  }
}

ind_char2 <- subset(ind_char2, select = -c(`Ind Group: Trades Intensive Industries`, `Ind group: Tech Intensive Industries`, `ITA Sector Advisory Group`))

# Merge these two data frames
# Columns are industry code, industry, date, noc, description, geographic area, jobs openings, expansion demand, replacement demand, deaths, retirements, employment, aggregate industry.x (code for aggregate industry from df 1),NAICS defintion, Aggregate Industry.y (code for aggregate industry from df2), Sector code (aggregate industry), Sector
Jobs_and_industry <-
  merge(job_openings,
    ind_char2,
    by = c("Industry Code", "Industry"),
    all = TRUE
  )
Jobs_and_industry <-
  subset(Jobs_and_industry, select = -`Aggregate Industry.x`) # don't need this column (letter code for aggregate industry)
Jobs_and_industry <-
  subset(Jobs_and_industry, select = -`Agg Industry Code`) # don't need this column (letter code for aggregate industry)

colnames(Jobs_and_industry)[which(colnames(Jobs_and_industry) == "Aggregate Industry.y")] <-
  "Aggregate Industry" # rename aggregate industry column

Jobs_and_industry$`Industry Code` <-
  as.factor(Jobs_and_industry$`Industry Code`) # factor industry code
Jobs_and_industry$Industry <-
  as.factor(Jobs_and_industry$Industry) # factor industry
Jobs_and_industry$Date <-
  as.factor(Jobs_and_industry$Date) # factor date
Jobs_and_industry$Description <-
  as.factor(Jobs_and_industry$Description) # factor description
Jobs_and_industry$`NAICS Definition` <-
  as.factor(Jobs_and_industry$`NAICS Definition`) # factor NAICS definitions
Jobs_and_industry$`Aggregate Industry` <-
  Jobs_and_industry$`Aggregate Industry` # factor aggregate industry
Jobs_and_industry$`Sector Code` <-
  as.factor(Jobs_and_industry$`Sector Code`) # factor sector code
Jobs_and_industry$Sector <-
  as.factor(Jobs_and_industry$Sector) # factor sector
Jobs_and_industry$`Geographic Area` <-
  as.factor(Jobs_and_industry$`Geographic Area`) # factor geographic area
Jobs_and_industry <-
  filter(Jobs_and_industry, Industry != "all industries") # remove all industries

# factor hierarchical structure column
# Broad occupational category, major group, minor group, unit group
noc_mappings <- noc_mappings_raw
noc_mappings$`Hierarchical structure` <-
  as.factor(noc_mappings$`Hierarchical structure`)


# Do broad occupational categories first
noc_broad_occ <-
  filter(
    noc_mappings,
    noc_mappings$`Hierarchical structure` == "Broad occupational category"
  ) # filter to only include broad occupational category
noc_broad_occ <-
  noc_broad_occ[, c("Code", "Class title")] # we only want code and class title
noc_broad_occ$Code <-
  paste0("#", noc_broad_occ$Code) # put a pound symbol in front of the one digit code
colnames(noc_broad_occ) <-
  c("NOC1", "NOC1 Description") # rename these NOCs to be NOC1 and NOC1 Description

# Do major groups next
noc_major_group <-
  filter(
    noc_mappings,
    noc_mappings$`Hierarchical structure` == "Major group"
  )
# Need to fix up 01-05 and 07-09, so there are individual lines for each code
fix <- filter(noc_major_group, Code %in% c("01-05", "07-09"))
fix_bind <-
  as.data.frame(rbind(fix[1, ], fix[1, ], fix[1, ], fix[1, ], fix[1, ], fix[2, ], fix[2, ], fix[2, ]))
fix_bind$Code <- c("01", "02", "03", "04", "05", "07", "08", "09")

# remove from original data frame and add in fixed codes
noc_major_group <-
  filter(noc_major_group, Code != "01-05") %>% filter(Code != "07-09")
noc_major_group <- rbind(fix_bind, noc_major_group)
noc_major_group <- noc_major_group[, c("Code", "Class title")]

for (i in 1:nrow(noc_major_group)) {
  if (nchar(noc_major_group$Code[i]) < 2) {
    noc_major_group$Code[i] <- paste0("0", noc_major_group$Code[i])
  }
}
noc_major_group$Code <-
  paste0("#", noc_major_group$Code) # put a pound symbol in front of the two digit codes
colnames(noc_major_group) <-
  c("NOC2", "NOC2 Description") # rename these NOCS to be NOC2 and NOC2 Description

# Do minor groups next
noc_minor_group <-
  filter(
    noc_mappings,
    noc_mappings$`Hierarchical structure` == "Minor group"
  ) # filter to only include minor groups
noc_minor_group <-
  noc_minor_group[, c("Code", "Class title")] # only need code and class title

# If there are only two digits, add a zero out front, if there is only one digit add two zeros out front
for (i in 1:nrow(noc_minor_group)) {
  if (nchar(noc_minor_group$Code[i]) == 2) {
    noc_minor_group$Code[i] <- paste0("0", noc_minor_group$Code[i])
  } else {
    if (nchar(noc_minor_group$Code[i]) < 2) {
      noc_minor_group$Code[i] <- paste0("00", noc_minor_group$Code[i])
    } else {
      noc_minor_group$Code[i] <- noc_minor_group$Code[i]
    }
  }
}

noc_minor_group$Code <-
  paste0("#", noc_minor_group$Code) # put a pound symbol in front of the three digit codes
colnames(noc_minor_group) <-
  c("NOC3", "NOC3 Description") # rename these NOCS to be NOC3 and NOC3 Description


# Finally do the unit group
noc_unit <-
  filter(
    noc_mappings,
    noc_mappings$`Hierarchical structure` == "Unit group"
  )
noc_unit <- noc_unit[, c("Code", "Class title")]

# If there are only three digits, put one zero in front, if only two digits, put two zeros out front
for (i in 1:nrow(noc_unit)) {
  if (nchar(noc_unit$Code[i]) == 3) {
    noc_unit$Code[i] <- paste0("0", noc_unit$Code[i])
  } else {
    if (nchar(noc_unit$Code[i]) == 2) {
      noc_unit$Code[i] <- paste0("00", noc_unit$Code[i])
    } else {
      noc_unit$Code[i] <- noc_unit$Code[i]
    }
  }
}

noc_unit$Code <-
  paste0("#", noc_unit$Code) # put a pound symbol in front of the four digit codes
colnames(noc_unit) <-
  c("NOC", "NOC4 Description") # Rename these nocs to be NOC (to match our current data frame) and NOC4 description


jobs_employment <-
  Jobs_and_industry # make a copy of Jobs_and_industry data frame, and name it jobs_employment
jobs_employment <-
  filter(jobs_employment, Industry != "all industries") # remove the "all industries" rows
jobs_employment$`Industry Code` <-
  as.factor(jobs_employment$`Industry Code`) # factor industry code column
jobs_employment$Industry <-
  as.factor(tolower(jobs_employment$Industry)) # factor and lower case for the industry column
jobs_employment$NOC <-
  as.factor(jobs_employment$NOC) # factor the NOC column
jobs_employment$`Geographic Area` <-
  as.factor(jobs_employment$`Geographic Area`) # factor the geographic area column
jobs_employment$`Aggregate Industry` <-
  as.factor(jobs_employment$`Aggregate Industry`)

education_occupation <- education_occupation_raw
education_occupation <-
  education_occupation[, c(1, 6, 7, 8, 10)] # We only want the columns with the various NOCS and Typical Education Background
education_occupation$NOC <-
  as.factor(education_occupation$NOC) # Factor 4 digit NOC
education_occupation$NOC1 <-
  as.factor(education_occupation$NOC1) # Factor 1 digit NOC
education_occupation$NOC2 <-
  as.factor(education_occupation$NOC2) # Factor 2 digit NOC
education_occupation$NOC3 <-
  as.factor(education_occupation$NOC3) # Factor 3 digit NOC

jobs_employment <-
  merge(jobs_employment, education_occupation, by = c("NOC")) # Merge jobs_employment data frame with education data

# we need to have typical education background, NOC (hierarchy), occupation title, employment, expansion year1-year3, replacement year1-year3, job openings year1-year3 (wages too?) (**)
jobs_employment$Industry <-
  tolower(jobs_employment$Industry) # ensure that industry is lower case (easier to match up)
jobs_employment$Industry <-
  as.factor(jobs_employment$Industry) # ensure that industry is a factor

# 1.3_Aggregated_Industries-----------------

# we need to calculate growth rates for both the sectors, individual industries and the aggregate industries because these cannot be aggregated directly in Tableau
# we will start by looping over the aggregated industries first

# Create two empty data frames to act as storage as we loop through
by_agg_industry <- NULL # placeholder for first loop
by_aggregated_industry <- NULL # placeholder for second loop

# Handle by aggregate industry first, for each geography and at each aggregate industry level we are going to calculate employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

# Need number of job openings for BC (year1) and (year1-year3)

JO_BC_year1 <- filter(jobs_employment, `Geographic Area` == "British Columbia") %>% filter(Date == year1)
JO_BC_year1_year3 <- filter(jobs_employment, `Geographic Area` == "British Columbia") %>% filter(Date %in% c(as.numeric(year1 + 1):year3))


for (i in 1:nlevels(jobs_employment$`Geographic Area`)) {
  # loop through each geographic area
  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # filter a temporary data frame from "job_employment" that only includes the geographic area of interest
  temp$`Aggregate Industry` <-
    as.factor(temp$`Aggregate Industry`) # ensure aggregate industries area a factor

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3 for the AGGREGATED INDUSTRIES

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


    # Calculate the employment growth from year1 to year2: This is calculated as the (employment in year2/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR for year1 to year2
    CAGR1 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for year2 to year3
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

    # Calculate the CAGR for year1 to year3
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



    # Calculate the expansion demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    expansion <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year3
        )
      )
    expansion <- sum(expansion$`Expansion Demand`)


    expansion_year1_year2 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year2
        )
      )
    expansion_year1_year2 <- sum(expansion_year1_year2$`Expansion Demand`)

    expansion_year2_year3 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year2 + 1):year3
        )
      )
    expansion_year2_year3 <- sum(expansion_year2_year3$`Expansion Demand`)

    # Calculate the replacement demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    replacement <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year3
        )
      )
    replacement <- sum(replacement$`Replacement Demand`)


    replacement_year1_year2 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year2
        )
      )
    replacement_year1_year2 <- sum(replacement_year1_year2$`Replacement Demand`)

    replacement_year2_year3 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year2 + 1):year3
        )
      )
    replacement_year2_year3 <- sum(replacement_year2_year3$`Replacement Demand`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    job_openings_tot <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year3
        )
      )
    job_openings_tot <- sum(job_openings_tot$`Job Openings`)


    # We assign expansion demand, replacement demand and job openings year1-year3,year1-year2,year2-year3 to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

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


    agg_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year2",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = expansion_year1_year2
      )

    agg_industry_employment9 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year2-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = expansion_year2_year3
      )

    agg_industry_employment10 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year2",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = replacement_year1_year2
      )

    agg_industry_employment11 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year2-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = replacement_year2_year3
      )

    # Calculate the job openings from year1-year2 (filter the data frame to include years from 2020-year2 and sum for the aggregate industry and geography)
    job_openings_tot_year1_year2 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year1 + 1):year2
        )
      )
    job_openings_tot_year1_year2 <- sum(job_openings_tot_year1_year2$`Job Openings`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    job_openings_tot_year2_year3 <-
      unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(
        Date %in% c(
          as.numeric(year2 + 1):year3
        )
      )
    job_openings_tot_year2_year3 <- sum(job_openings_tot_year2_year3$`Job Openings`)

    agg_industry_employment12 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year2-year3",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = job_openings_tot_year2_year3
      )

    agg_industry_employment13 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year2",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = job_openings_tot_year1_year2
      )


    # Calculate Employment as a share of BC

    employment_year1_shareBC <- 100 * employment_year1 / sum(JO_BC_year1$Employment)

    agg_industry_employment14 <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1 as a Share of BC Emplyoment year1",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp_employment_year1$`Aggregate Industry`)[j],
        "Value" = employment_year1_shareBC
      )



    # We need job openings (year1-year3 as a share of BC, and replacement year1-year3 as a share of BC)

    job_openings_share <- 100 * job_openings_tot / sum(JO_BC_year1_year3$`Job Openings`)

    agg_industry_employment15 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3 as a Share of BC",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = job_openings_share
      )


    replacement_share <- 100 * replacement / job_openings_tot

    agg_industry_employment16 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3 as a Share of BC",
        "Level" = "Aggregate Industry",
        "Level Value" = levels(temp$`Aggregate Industry`)[j],
        "Value" = replacement_share
      )





    # Calculate the average annual replacement rate
    avg <- NULL

    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$`Aggregate Industry` == levels(temp$`Aggregate Industry`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement2 <- sum(date2$`Replacement Demand`)
      percent <- as.numeric(replacement2) / as.numeric(emp)
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    agg_industry_employment17 <-
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
        agg_industry_employment9,
        agg_industry_employment10,
        agg_industry_employment11,
        agg_industry_employment12,
        agg_industry_employment13,
        agg_industry_employment14,
        agg_industry_employment15,
        agg_industry_employment16,
        agg_industry_employment17,
        by_agg_industry
      )
  }
  # Once the variables have been calculate for each aggregate industry in the geography, the loop will be exited. We bind the results of all the aggregate industries for that geography into the data frame "by_aggregated_industry" and then we go on to the next geography in the loop and repeat
  by_aggregated_industry <-
    rbind(by_agg_industry, by_aggregated_industry)
}

# Ensure there are no duplicate entries in the data frame which may cause double counting.
by_aggregated_industry <- unique(by_aggregated_industry)


# 1.4_Sectors----------

# we need to calculate growth rates for both the sectors, individual industries and the aggregate industries because these cannot be aggregated directly in Tableau
# we will start by looping over the aggregated industries first

# Create two empty data frames to act as storage as we loop through
by_sector <- NULL # placeholder for first loop
by_sectors <- NULL # placeholder for second loop

# Handle by aggregate industry first, for each geography and at each aggregate industry level we are going to calculate employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

# Need number of job openings for BC (year1) and (year1-year3)

JO_BC_year1 <- filter(jobs_employment, `Geographic Area` == "British Columbia") %>% filter(Date == year1)
JO_BC_year1_year3 <- filter(jobs_employment, `Geographic Area` == "British Columbia") %>% filter(Date %in% c(as.numeric(year1 + 1):year3))


for (i in 1:nlevels(jobs_employment$`Geographic Area`)) {
  # loop through each geographic area
  temp <-
    filter(
      jobs_employment,
      `Geographic Area` == levels(jobs_employment$`Geographic Area`)[i]
    ) # filter a temporary data frame from "job_employment" that only includes the geographic area of interest
  temp$`Sector` <-
    as.factor(temp$`Sector`) # ensure that "sector code" (which is actually are aggregate industries) is a factor

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3 for the AGGREGATED INDUSTRIES

  for (j in 1:nlevels(temp$`Sector`)) {
    # loop through each aggregate industry

    # We are going to get the employment year1 data for the geography and aggregate industry first
    temp_employment_year1 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date == year1) # filter to only include geography/aggregate industry and year to year1
    employment_year1 <-
      sum(as.numeric(temp_employment_year1$Employment)) # we need to sum this value to get employment for the aggregate industry
    agg_industry_employment <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1",
        "Level" = "Sector",
        "Level Value" = levels(temp_employment_year1$`Sector`)[j],
        "Value" = employment_year1
      )


    # Calculate the employment growth from year1 to year2: This is calculated as the (employment in year2/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR for year1 to year2
    CAGR1 <-
      100 * (((
        sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
        ) / sum(
          filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
        )
      )^(1 / 5)) - 1)

    # Calculate the CAGR for year2 to year3
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

    # Calculate the CAGR for year1 to year3
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


    # We assign each of these CAGRS to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

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



    # Calculate the expansion demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    expansion <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year3
        )
      )
    expansion <- sum(expansion$`Expansion Demand`)


    expansion_year1_year2 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year2
        )
      )
    expansion_year1_year2 <- sum(expansion_year1_year2$`Expansion Demand`)

    expansion_year2_year3 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year2) + 1):year3
        )
      )
    expansion_year2_year3 <- sum(expansion_year2_year3$`Expansion Demand`)

    # Calculate the replacement demand from year1-year3 (filter the data frame to include years from year1-year3 and sum for the aggregate industry and geography)
    replacement <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year3
        )
      )
    replacement <- sum(replacement$`Replacement Demand`)


    replacement_year1_year2 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year2
        )
      )
    replacement_year1_year2 <- sum(replacement_year1_year2$`Replacement Demand`)

    replacement_year2_year3 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year2 + 1):year3
          )
        )
      )
    replacement_year2_year3 <- sum(replacement_year2_year3$`Replacement Demand`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from year1-year3 and sum for the aggregate industry and geography)
    job_openings_tot <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year3
        )
      )
    job_openings_tot <- sum(job_openings_tot$`Job Openings`)


    # We assign expansion demand, replacement demand and job openings year1-year3,year1-year2,year2-year3 to a data frame with the same format as before: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

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


    agg_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year2",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = expansion_year1_year2
      )

    agg_industry_employment9 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year2-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = expansion_year2_year3
      )

    agg_industry_employment10 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year2",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement_year1_year2
      )

    agg_industry_employment11 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year2-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement_year2_year3
      )

    # Calculate the job openings from year1-year2 (filter the data frame to include years from 2020-year2 and sum for the aggregate industry and geography)
    job_openings_tot_year1_year2 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year1) + 1):year2
        )
      )
    job_openings_tot_year1_year2 <- sum(job_openings_tot_year1_year2$`Job Openings`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    job_openings_tot_year2_year3 <-
      unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(
        Date %in% c(
          (as.numeric(year2) + 1):year3
        )
      )
    job_openings_tot_year2_year3 <- sum(job_openings_tot_year2_year3$`Job Openings`)

    agg_industry_employment12 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year2-year3",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = job_openings_tot_year2_year3
      )

    agg_industry_employment13 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year2",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = job_openings_tot_year1_year2
      )


    # Calculate Employment as a share of BC

    employment_year1_shareBC <- 100 * employment_year1 / sum(JO_BC_year1$Employment)

    agg_industry_employment14 <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1 as a Share of BC Emplyoment year1",
        "Level" = "Sector",
        "Level Value" = levels(temp_employment_year1$`Sector`)[j],
        "Value" = employment_year1_shareBC
      )



    # We need job openings (year1-year3 as a share of BC, and replacement year1-year3 as a share of BC)

    job_openings_share <- 100 * job_openings_tot / sum(JO_BC_year1_year3$`Job Openings`)

    agg_industry_employment15 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3 as a Share of BC",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = job_openings_share
      )


    replacement_share <- 100 * replacement / job_openings_tot

    agg_industry_employment16 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3 as a Share of BC",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement_share
      )





    # Calculate the average annual replacement rate
    avg <- NULL

    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$`Sector` == levels(temp$`Sector`)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement2 <- sum(date2$`Replacement Demand`)
      percent <- as.numeric(replacement2) / as.numeric(emp)
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    agg_industry_employment17 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Annual Replacement Rate",
        "Level" = "Sector",
        "Level Value" = levels(temp$`Sector`)[j],
        "Value" = replacement_rate
      )


    # Now we bind all of the aggregate industry information together into one data frame - this loop will repeat again for each level of aggregate industries for this specific geography
    by_sector <-
      rbind(
        agg_industry_employment,
        agg_industry_employment2,
        agg_industry_employment3,
        agg_industry_employment4,
        agg_industry_employment5,
        agg_industry_employment6,
        agg_industry_employment7,
        agg_industry_employment8,
        agg_industry_employment9,
        agg_industry_employment10,
        agg_industry_employment11,
        agg_industry_employment12,
        agg_industry_employment13,
        agg_industry_employment14,
        agg_industry_employment15,
        agg_industry_employment16,
        agg_industry_employment17,
        by_sector
      )
  }
  # Once the variables have been calculate for each aggregate industry in the geography, the loop will be exited. We bind the results of all the aggregate industries for that geography into the data frame "by_sectors" and then we go on to the next geography in the loop and repeat
  by_sectors <-
    rbind(by_sector, by_sectors)
}

# Ensure there are no duplicate entries in the data frame which may cause double counting.
by_sectors <- unique(by_sectors)
by_sectors <- filter(by_sectors, Level.Value != "Total")

# 1.6_Industries-------
# We need to do the same thing but now for the individual industries

# Handle by aggregate industry first, for each geography and at each industry level we are going to calculate employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

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

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3 for the levels of the LMO 61 industries

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

    # Calculate the employment growth from year1 to year2: This is calculated as the (employment in year2/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
    temp_employmentgrowth_year1_year2 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% c(year1, year2))

    # Calculate the CAGR from year1-year2
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

    # Calculate the CAGR from year2-year3
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

    # Calculate the CAGR from year1-year3
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


    # Calculate the expansion demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
    expansion <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    expansion <- sum(expansion$`Expansion Demand`)


    expansion_year1_year2 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year2)
      )
    expansion_year1_year2 <- sum(expansion_year1_year2$`Expansion Demand`)

    expansion_year2_year3 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year2 + 1):year3)
      )
    expansion_year2_year3 <- sum(expansion_year2_year3$`Expansion Demand`)

    # Calculate the replacement demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
    replacement <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year3)
      )
    replacement <- sum(replacement$`Replacement Demand`)

    replacement_year1_year2 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year2)
      )
    replacement_year1_year2 <- sum(replacement_year1_year2$`Replacement Demand`)

    replacement_year2_year3 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year2 + 1):year3)
      )
    replacement_year2_year3 <- sum(replacement_year2_year3$`Replacement Demand`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
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

    ind_industry_employment8 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year1-year2",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = expansion_year1_year2
      )

    ind_industry_employment9 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Expansion year2-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = expansion_year2_year3
      )

    ind_industry_employment10 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year2",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement_year1_year2
      )

    ind_industry_employment11 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year2-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement_year2_year3
      )


    # Calculate the Annual Replacement Rate and place into data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
    avg <- NULL
    for (z in 3:nlevels(as.factor(temp$Date))) {
      date1 <-
        unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      date2 <-
        unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
      emp <- sum(date1$Employment)
      replacement2 <- sum(date2$`Replacement Demand`)
      percent <- replacement2 / emp
      avg <- c(percent, avg)
    }
    replacement_rate <- mean(avg) * 100

    ind_industry_employment12 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Annual Replacement Rate",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement_rate
      )

    # Calculate the job openings from year1-year2 (filter the data frame to include years from 2020-year2 and sum for the aggregate industry and geography)
    job_openings_tot_year1_year2 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(as.numeric(year1 + 1):year2)
      )
    job_openings_tot_year1_year2 <- sum(job_openings_tot_year1_year2$`Job Openings`)

    # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
    job_openings_tot_year2_year3 <-
      unique(filter(temp, temp$Industry == levels(temp$Industry)[j])) %>% filter(
        Date %in% c(
          as.numeric(year2 + 1):year3
        )
      )
    job_openings_tot_year2_year3 <- sum(job_openings_tot_year2_year3$`Job Openings`)

    ind_industry_employment13 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year2-year3",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = job_openings_tot_year2_year3
      )

    ind_industry_employment14 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year2",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = job_openings_tot_year1_year2
      )


    # Calculate Employment as a share of BC

    employment_year1_shareBC <- 100 * employment_year1 / sum(JO_BC_year1$Employment)

    ind_industry_employment15 <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Employment year1 as a Share of BC Emplyoment year1",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = employment_year1_shareBC
      )



    # We need job openings (year1-year3 as a share of BC, and replacement year1-year3 as a share of BC)

    job_openings_share <- 100 * job_openings_tot / sum(JO_BC_year1_year3$`Job Openings`)

    ind_industry_employment16 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Job Openings year1-year3 as a Share of BC",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = job_openings_share
      )


    replacement_share <- 100 * replacement / job_openings_tot

    ind_industry_employment17 <-
      data.frame(
        "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
        "Variable" = "Replacement year1-year3 as a Share of BC",
        "Level" = "Industry",
        "Level Value" = levels(temp$Industry)[j],
        "Value" = replacement_share
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
        ind_industry_employment9,
        ind_industry_employment10,
        ind_industry_employment11,
        ind_industry_employment12,
        ind_industry_employment13,
        ind_industry_employment14,
        ind_industry_employment15,
        ind_industry_employment16,
        ind_industry_employment17,
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
by_sectors$`Aggregate Industry` <- NA

# Bind the aggregated industry data frame to the LMO 61 industry data frame
individual_industry_agg_industry <- rbind(by_individual_industry, by_aggregated_industry, by_sectors)
individual_industry_agg_industry$Level.Value <-
  str_to_title(individual_industry_agg_industry$Level.Value) # convert industries to title case
individual_industry_agg_industry <- # ensure there are no duplicates
  unique(individual_industry_agg_industry)

# 1.7_All_Industries-------------
# We need to do the same thing but now for the sum over all industries, we just get rid of the loop that does over all industries

# Handle by aggregate industry first, for each geography and at each industry level we are going to calculate employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3

# The final data frame will have five columns: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

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

  # We need a column with labels for Employment year1, Employment Growth (CAGR) year1-year2, Employment Growth (CAGR) year2-year3, Employment Growth (CAGR) year1-year3, Expansion year1-year3, Replacement year1-year3, Total Job Openings year1-year3 for the levels of the LMO 61 industries


  # We are going to get the employment year1 data for the geography and aggregate industry first
  temp_employment_year1 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date == year1) # filter to only include geography/industry and year to year1

  employment_year1 <-
    sum(as.numeric(temp_employment_year1$Employment)) # we need to sum this value to get employment for the industry
  ind_industry_employment <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Employment year1",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = employment_year1
    )

  # Calculate the employment growth from year1 to year2: This is calculated as the (employment in year2/employment in year1 ^(1/n) - 1) (the CAGR, where n is the number of years)
  temp_employmentgrowth_year1_year2 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date %in% c(year1, year2))

  # Calculate the CAGR from year1-year2
  CAGR1 <-
    100 * (((
      sum(
        filter(temp_employmentgrowth_year1_year2, Date %in% year2)$Employment
      ) / sum(
        filter(temp_employmentgrowth_year1_year2, Date %in% year1)$Employment
      )
    )^(1 / 5)) - 1)

  temp_employmentgrowth_year2_year3 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date %in% c(year2, year3))

  # Calculate the CAGR from year2-year3
  CAGR2 <-
    100 * (((
      sum(
        filter(temp_employmentgrowth_year2_year3, Date %in% year3)$Employment
      ) / sum(
        filter(temp_employmentgrowth_year2_year3, Date %in% year2)$Employment
      )
    )^(1 / 5)) - 1)

  temp_employmentgrowth_year1_year3 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date %in% c(year1, year3))

  # Calculate the CAGR from year1-year3
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
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = CAGR1
    )

  ind_industry_employment3 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Employment Growth year2-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = CAGR2
    )

  ind_industry_employment4 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Employment Growth year1-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = CAGR3
    )


  # Calculate the expansion demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
  expansion <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year3
      )
    )
  expansion <- sum(expansion$`Expansion Demand`)

  expansion_year1_year2 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year2
      )
    )
  expansion_year1_year2 <- sum(expansion_year1_year2$`Expansion Demand`)

  expansion_year2_year3 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year2 + 1):year3
      )
    )
  expansion_year2_year3 <- sum(expansion_year2_year3$`Expansion Demand`)

  # Calculate the replacement demand from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
  replacement <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year3
      )
    )
  replacement <- sum(replacement$`Replacement Demand`)

  replacement_year1_year2 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year2
      )
    )
  replacement_year1_year2 <- sum(replacement_year1_year2$`Replacement Demand`)

  replacement_year2_year3 <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year2 + 1):year3
      )
    )
  replacement_year2_year3 <- sum(replacement_year2_year3$`Replacement Demand`)

  # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the industry and geography)
  job_openings_tot <-
    unique(filter(temp, temp$Industry != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year3
      )
    )
  job_openings_tot <- sum(job_openings_tot$`Job Openings`)


  # place the expansion demand, replacement demand and job openings into individual data frames: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)

  ind_industry_employment5 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Expansion year1-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = expansion
    )
  ind_industry_employment6 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Replacement year1-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = replacement
    )
  ind_industry_employment7 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Job Openings year1-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = job_openings_tot
    )

  ind_industry_employment8 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Expansion year1-year2",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = expansion_year1_year2
    )

  ind_industry_employment9 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Expansion year2-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = expansion_year2_year3
    )

  ind_industry_employment10 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Replacement year1-year2",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = replacement_year1_year2
    )

  ind_industry_employment11 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Replacement year2-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = replacement_year2_year3
    )

  # Calculate the Annual Replacement Rate and place into data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
  avg <- NULL
  for (z in 3:nlevels(as.factor(temp$Date))) {
    date1 <-
      unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
    date2 <-
      unique(filter(temp, temp$Industry != "all industries")) %>% filter(Date %in% levels(as.factor(temp$Date))[z])
    emp <- sum(date1$Employment)
    replacement2 <- sum(date2$`Replacement Demand`)
    percent <- replacement2 / emp
    avg <- c(percent, avg)
  }
  replacement_rate <- mean(avg) * 100

  ind_industry_employment12 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Annual Replacement Rate",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = replacement_rate
    )

  # Calculate the job openings from year1-year2 (filter the data frame to include years from 2020-year2 and sum for the aggregate industry and geography)
  job_openings_tot_year1_year2 <-
    unique(filter(temp, temp$`Sector Code` != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year1 + 1):year2
      )
    )
  job_openings_tot_year1_year2 <- sum(job_openings_tot_year1_year2$`Job Openings`)

  # Calculate the job openings from year1-year3 (filter the data frame to include years from 2020-year3 and sum for the aggregate industry and geography)
  job_openings_tot_year2_year3 <-
    unique(filter(temp, temp$`Sector Code` != "all industries")) %>% filter(
      Date %in% c(
        as.numeric(year2 + 1):year3
      )
    )
  job_openings_tot_year2_year3 <- sum(job_openings_tot_year2_year3$`Job Openings`)

  ind_industry_employment13 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Job Openings year2-year3",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = job_openings_tot_year2_year3
    )

  ind_industry_employment14 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Job Openings year1-year2",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = job_openings_tot_year1_year2
    )


  # Calculate Employment as a share of BC

  employment_year1_shareBC <- 100 * employment_year1 / sum(JO_BC_year1$Employment)

  ind_industry_employment15 <- # place this into a data frame: Geographic Area (specifying the region), Variable (Employment year1, Job Openings etc), Level (Aggregate Industry or LMO 61 Industry level), Level Value (the name assigned to the industry or aggregate industry) and Value (numeric value for the variable)
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Employment year1 as a Share of BC Emplyoment year1",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = employment_year1_shareBC
    )



  # We need job openings (year1-year3 as a share of BC, and replacement year1-year3 as a share of BC)

  job_openings_share <- 100 * job_openings_tot / sum(JO_BC_year1_year3$`Job Openings`)

  ind_industry_employment16 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Job Openings year1-year3 as a Share of BC",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = job_openings_share
    )


  replacement_share <- 100 * replacement / job_openings_tot

  ind_industry_employment17 <-
    data.frame(
      "Geographic Area" = levels(jobs_employment$`Geographic Area`)[i],
      "Variable" = "Replacement year1-year3 as a Share of BC",
      "Level" = "All Industries",
      "Level Value" = "all industries",
      "Value" = replacement_share
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
      ind_industry_employment9,
      ind_industry_employment10,
      ind_industry_employment11,
      ind_industry_employment12,
      ind_industry_employment13,
      ind_industry_employment14,
      ind_industry_employment15,
      ind_industry_employment16,
      ind_industry_employment17,
      by_ind_industry
    )



  # Once the variables have been calculate for each industry in the geography, the loop will be exited. We bind the results of all the industries for that geography into the data frame "by_individual_industry" and then we go on to the next geography in the loop and repeat
  by_individual_industry <-
    rbind(by_ind_industry, by_individual_industry)
}


by_individual_industry$Level.Value <- tolower(by_individual_industry$Level.Value) # convert to lower case to ensure the industries will match up
by_individual_industry$`Aggregate Industry` <- "All Industries" # merge to mapping, now we have a column with the Aggregate industries matching to the LMO 61 industries
individual_industry_agg_industry$Level.Value <- tolower(individual_industry_agg_industry$Level.Value)

# Bind the aggregated industry data frame to the LMO 61 industry data frame
individual_industry_agg_industry <- rbind(by_individual_industry, individual_industry_agg_industry)
individual_industry_agg_industry$Level.Value <-
  str_to_title(trimws(individual_industry_agg_industry$Level.Value)) # convert industries to title case
individual_industry_agg_industry$`Aggregate Industry` <- str_to_title(individual_industry_agg_industry$`Aggregate Industry`)
individual_industry_agg_industry <- # ensure there are no duplicates
  unique(individual_industry_agg_industry)


## ----1.8_NOC_Wage--------------------------------------------------------------------------------------------------
NOC4_jobs <- jobs_NOC[, c("Date", "Geographic.Area", "NOC", "NOC4.Description", "Industry", "Aggregate.Industry", "Sector", "Job.Openings", "Replacement.Demand", "Employment", "Expansion.Demand")]
NOC4_jobs$level <- "4 Digit NOC"
colnames(NOC4_jobs)[c(3, 4)] <- c("NOC", "NOC Description")
NOC4_jobs <- aggregate(. ~ Date + Geographic.Area + NOC + `NOC Description` + Industry + Aggregate.Industry + Sector + level, NOC4_jobs, sum)

# wages_cleaned <- remove_missing(wages_cleaned)
wages_cleaned <- filter(wages_cleaned, Region != "National")
wages_cleaned$Region <- factor(wages_cleaned$Region, levels = levels(as.factor(wages_cleaned$Region)), labels = c("British Columbia", "Cariboo", "Kootenay", "Mainland South West", "North Coast & Nechako", "North Coast & Nechako", "North East", "Thompson Okanagan", "Vancouver Island Coast"))
wages_cleaned$NOC <- as.factor(wages_cleaned$NOC)


## ----1.9_Occupation_Data-------------------------------------------------------------------------------------------

# for each geography
all_industries <- NULL
all_geographies <- NULL
occupations_by_geo <- NULL
occupations_by_industry <- NULL
occupations_industry <- NULL

# Really it only makes sense here to go with the 4-digit NOC level
NOC4_jobs$Geographic.Area <- as.factor(NOC4_jobs$Geographic.Area)
NOC4_jobs$NOC <- as.factor(NOC4_jobs$NOC)
NOC4_jobs$Industry <- as.factor(NOC4_jobs$Industry)
NOC4_jobs$Sector <- NOC4_jobs$Sector

# filtered geography (takes a long time)
for (i in 1:nlevels(as.factor(NOC4_jobs$Geographic.Area))) {
  filtered_geo <-
    filter(
      NOC4_jobs,
      Geographic.Area %in% levels(as.factor(NOC4_jobs$Geographic.Area))[i]
    )

  for (j in 1:length(unique(filtered_geo$Sector))) {
    filtered_industry <-
      filter(
        filtered_geo,
        Sector == unique(filtered_geo$Sector)[j]
      )

    if (nrow(filtered_industry) == 0) {
      next
    } else {
      # now we need to go by NOC level for the calculations

      filtered_noc <- filtered_industry


      # We need employment year1, job openings year1-year3, replacement rate annual (year1-year3)
      filtered_noc_employment_year1 <-
        filter(filtered_noc, Date == year1)
      filtered_noc_employment_year1 <-
        aggregate(
          Employment ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_employment_year1,
          sum
        )
      colnames(filtered_noc_employment_year1)[which(colnames(filtered_noc_employment_year1) ==
        "Employment")] <- "Employment year1"

      # Job Openings year1-year3
      filtered_noc_job_openings_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < (year3 + 1))
      filtered_noc_job_openings_year1_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_job_openings_year1_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year3)[which(colnames(filtered_noc_job_openings_year1_year3) ==
        "Job.Openings")] <- "Job Openings year1-year3"


      # Replacement Rate (annual)
      # Calculate the Annual Replacement Rate

      avg <- NULL
      replacement_rate_df <- NULL

      # but we also need to do this for each NOC.....

      for (y in 1:length(unique(filtered_noc$NOC))) {
        noc_filtered <-
          filter(
            filtered_noc,
            NOC %in% unique(filtered_noc$NOC)[y]
          )

        for (z in 3:nlevels(as.factor(noc_filtered$Date))) {
          date1 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          date2 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          emp <- sum(date1$Employment)
          replacement2 <- sum(date2$`Replacement.Demand`)
          percent <- replacement2 / emp
          avg <- c(percent, avg)
        }
        replacement_rate <- mean(avg) * 100
        df <-
          data.frame(
            "Geographic Area" = unique(noc_filtered$`Geographic.Area`),
            "NOC" = unique(filtered_noc$NOC)[y],
            "Replacement Rate" = replacement_rate,
            "NOC Description" = unique(noc_filtered$`NOC Description`),
            "Sector" = unique(noc_filtered$Sector),
            "level" = unique(noc_filtered$level)
          )

        replacement_rate_df <- rbind(df, replacement_rate_df)
      }

      colnames(replacement_rate_df) <- c("Geographic.Area", "NOC", "Replacement Rate", "NOC Description", "Sector", "level")


      # Let's add in additional variables that the user can pick from: job openings year2-year3, year1-year2, expansion demand, replacement demand (for all three intervals) etc

      # Job Openings year1-year2
      filtered_noc_job_openings_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_job_openings_year1_year2 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_job_openings_year1_year2,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year2)[which(colnames(filtered_noc_job_openings_year1_year2) ==
        "Job.Openings")] <- "Job Openings year1-year2"

      # Job Openings year2-year3
      filtered_noc_job_openings_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_job_openings_year2_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_job_openings_year2_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year2_year3)[which(colnames(filtered_noc_job_openings_year2_year3) ==
        "Job.Openings")] <- "Job Openings year2-year3"


      # Expansion Demand


      # Expansion year1-year3
      filtered_noc_expansion_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year1_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_expansion_year1_year3,
          sum
        )
      colnames(filtered_noc_expansion_year1_year3)[which(colnames(filtered_noc_expansion_year1_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year3"

      # Expansion year2-year3
      filtered_noc_expansion_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year2_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_expansion_year2_year3,
          sum
        )
      colnames(filtered_noc_expansion_year2_year3)[which(colnames(filtered_noc_expansion_year2_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year2-year3"
      # Expansion year1-year2
      filtered_noc_expansion_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_expansion_year1_year2 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_expansion_year1_year2,
          sum
        )
      colnames(filtered_noc_expansion_year1_year2)[which(colnames(filtered_noc_expansion_year1_year2) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year2"


      # Replacement Demand


      # Replacement year1-year3
      filtered_noc_replacement_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year1_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_replacement_year1_year3,
          sum
        )
      colnames(filtered_noc_replacement_year1_year3)[which(colnames(filtered_noc_replacement_year1_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year3"

      # Replacement year2-year3
      filtered_noc_replacement_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year2_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_replacement_year2_year3,
          sum
        )
      colnames(filtered_noc_replacement_year2_year3)[which(colnames(filtered_noc_replacement_year2_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year2-year3"
      # Replacement year1-year2
      filtered_noc_replacement_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_replacement_year1_year2 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Sector +
            level,
          filtered_noc_replacement_year1_year2,
          sum
        )
      colnames(filtered_noc_replacement_year1_year2)[which(colnames(filtered_noc_replacement_year1_year2) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year2"



      occupations_data_frame <- merge(filtered_noc_employment_year1, filtered_noc_job_openings_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, replacement_rate_df, all = TRUE)
      occupations_industry <- rbind(occupations_data_frame, occupations_industry)
      occupations_industry <- unique(occupations_industry)
    }
  }

  occupations_by_geo <- rbind(occupations_industry, occupations_by_geo)
  occupations_by_geo <- unique(occupations_by_geo)
}
# long running code above---------------------

occupations_by_geo$`Typical Education Background` <- NA
occupations_by_geo <- unique(occupations_by_geo)

education_NOC4 <- unique(education_occupation[, c(which(colnames(education_occupation) == "NOC"), which(colnames(education_occupation) == "Education:.Typical.Background"))])
education_NOC4 <- education_NOC4 %>% filter(NOC != "#T")
colnames(education_NOC4) <- c("NOC", "Typical Education Background")


for (i in 1:nrow(occupations_by_geo)) {
  df <- filter(education_NOC4, NOC %in% occupations_by_geo$NOC[i])
  occupations_by_geo$`Typical Education Background`[i] <- df$`Typical Education Background`
}


# now we need to add in the wages --> the wages are only going to be at the 4 digit NOC level
colnames(wages_cleaned)[which(colnames(wages_cleaned) == "Region")] <- "Geographic.Area"
occupations_sector_tool <- merge(occupations_by_geo, wages_cleaned)
occupations_sector_tool$Industry <- occupations_sector_tool$Sector
occupations_sector_tool$Level <- "Sector"

# By aggregate industry
# for each geography
all_industries <- NULL
all_geographies <- NULL
occupations_by_geo <- NULL
occupations_by_industry <- NULL
occupations_industry <- NULL

# Really it only makes sense here to go with the 4-digit NOC level
NOC4_jobs$Geographic.Area <- as.factor(NOC4_jobs$Geographic.Area)
NOC4_jobs$NOC <- as.factor(NOC4_jobs$NOC)
NOC4_jobs$Industry <- as.factor(NOC4_jobs$Industry)
NOC4_jobs$Aggregate.Industry <- NOC4_jobs$Aggregate.Industry

# filtered geography (REALLY long time to run)----------
for (i in 1:nlevels(as.factor(NOC4_jobs$Geographic.Area))) {
  filtered_geo <-
    filter(
      NOC4_jobs,
      Geographic.Area %in% levels(as.factor(NOC4_jobs$Geographic.Area))[i]
    )

  for (j in 1:length(unique(filtered_geo$Aggregate.Industry))) {
    filtered_industry <-
      filter(
        filtered_geo,
        Aggregate.Industry == unique(filtered_geo$Aggregate.Industry)[j]
      )

    if (nrow(filtered_industry) == 0) {
      next
    } else {
      # now we need to go by NOC level for the calculations

      filtered_noc <- filtered_industry


      # We need employment year1, job openings year1-year3, replacement rate annual (year1-year3)
      filtered_noc_employment_year1 <-
        filter(filtered_noc, Date == year1)
      filtered_noc_employment_year1 <-
        aggregate(
          Employment ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_employment_year1,
          sum
        )
      colnames(filtered_noc_employment_year1)[which(colnames(filtered_noc_employment_year1) ==
        "Employment")] <- "Employment year1"

      # Job Openings year1-year3
      filtered_noc_job_openings_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < (year3 + 1))
      filtered_noc_job_openings_year1_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_job_openings_year1_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year3)[which(colnames(filtered_noc_job_openings_year1_year3) ==
        "Job.Openings")] <- "Job Openings year1-year3"


      # Replacement Rate (annual)
      # Calculate the Annual Replacement Rate

      avg <- NULL
      replacement_rate_df <- NULL

      # but we also need to do this for each NOC.....

      for (y in 1:length(unique(filtered_noc$NOC))) {
        noc_filtered <-
          filter(
            filtered_noc,
            NOC %in% unique(filtered_noc$NOC)[y]
          )

        for (z in 3:nlevels(as.factor(noc_filtered$Date))) {
          date1 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          date2 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          emp <- sum(date1$Employment)
          replacement2 <- sum(date2$`Replacement.Demand`)
          percent <- replacement2 / emp
          avg <- c(percent, avg)
        }
        replacement_rate <- mean(avg) * 100
        df <-
          data.frame(
            "Geographic Area" = unique(noc_filtered$`Geographic.Area`),
            "NOC" = unique(filtered_noc$NOC)[y],
            "Replacement Rate" = replacement_rate,
            "NOC Description" = unique(noc_filtered$`NOC Description`),
            "Aggregate.Industry" = unique(noc_filtered$Aggregate.Industry),
            "level" = unique(noc_filtered$level)
          )

        replacement_rate_df <- rbind(df, replacement_rate_df)
      }

      colnames(replacement_rate_df) <- c("Geographic.Area", "NOC", "Replacement Rate", "NOC Description", "Aggregate.Industry", "level")


      # Let's add in additional variables that the user can pick from: job openings year2-year3, year1-year2, expansion demand, replacement demand (for all three intervals) etc

      # Job Openings year1-year2
      filtered_noc_job_openings_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_job_openings_year1_year2 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_job_openings_year1_year2,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year2)[which(colnames(filtered_noc_job_openings_year1_year2) ==
        "Job.Openings")] <- "Job Openings year1-year2"

      # Job Openings year2-year3
      filtered_noc_job_openings_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_job_openings_year2_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_job_openings_year2_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year2_year3)[which(colnames(filtered_noc_job_openings_year2_year3) ==
        "Job.Openings")] <- "Job Openings year2-year3"


      # Expansion Demand


      # Expansion year1-year3
      filtered_noc_expansion_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year1_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_expansion_year1_year3,
          sum
        )
      colnames(filtered_noc_expansion_year1_year3)[which(colnames(filtered_noc_expansion_year1_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year3"

      # Expansion year2-year3
      filtered_noc_expansion_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year2_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_expansion_year2_year3,
          sum
        )
      colnames(filtered_noc_expansion_year2_year3)[which(colnames(filtered_noc_expansion_year2_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year2-year3"
      # Expansion year1-year2
      filtered_noc_expansion_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_expansion_year1_year2 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_expansion_year1_year2,
          sum
        )
      colnames(filtered_noc_expansion_year1_year2)[which(colnames(filtered_noc_expansion_year1_year2) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year2"


      # Replacement Demand


      # Replacement year1-year3
      filtered_noc_replacement_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year1_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_replacement_year1_year3,
          sum
        )
      colnames(filtered_noc_replacement_year1_year3)[which(colnames(filtered_noc_replacement_year1_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year3"

      # Replacement year2-year3
      filtered_noc_replacement_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year2_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_replacement_year2_year3,
          sum
        )
      colnames(filtered_noc_replacement_year2_year3)[which(colnames(filtered_noc_replacement_year2_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year2-year3"
      # Replacement year1-year2
      filtered_noc_replacement_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_replacement_year1_year2 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Aggregate.Industry +
            level,
          filtered_noc_replacement_year1_year2,
          sum
        )
      colnames(filtered_noc_replacement_year1_year2)[which(colnames(filtered_noc_replacement_year1_year2) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year2"



      occupations_data_frame <- merge(filtered_noc_employment_year1, filtered_noc_job_openings_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, replacement_rate_df, all = TRUE)
      occupations_industry <- rbind(occupations_data_frame, occupations_industry)
      occupations_industry <- unique(occupations_industry)
    }
  }

  occupations_by_geo <- rbind(occupations_industry, occupations_by_geo)
  occupations_by_geo <- unique(occupations_by_geo)
}


occupations_by_geo$`Typical Education Background` <- NA
occupations_by_geo <- unique(occupations_by_geo)

education_NOC4 <- unique(education_occupation[, c(which(colnames(education_occupation) == "NOC"), which(colnames(education_occupation) == "Education:.Typical.Background"))])
education_NOC4 <- education_NOC4 %>% filter(NOC != "#T")
colnames(education_NOC4) <- c("NOC", "Typical Education Background")


for (i in 1:nrow(occupations_by_geo)) {
  df <- filter(education_NOC4, NOC %in% occupations_by_geo$NOC[i])
  occupations_by_geo$`Typical Education Background`[i] <- df$`Typical Education Background`
}

# now we need to add in the wages --> the wages are only going to be at the 4 digit NOC level
colnames(wages_cleaned)[which(colnames(wages_cleaned) == "Region")] <- "Geographic.Area"
occupations_aggregate_industry_tool <- merge(occupations_by_geo, wages_cleaned)
occupations_aggregate_industry_tool$Industry <- occupations_aggregate_industry_tool$Aggregate.Industry
occupations_aggregate_industry_tool$Level <- "Aggregate Industry"

# By individual industry
# for each geography
all_industries <- NULL
all_geographies <- NULL
occupations_by_geo <- NULL
occupations_by_industry <- NULL
occupations_industry <- NULL

# Really it only makes sense here to go with the 4-digit NOC level
NOC4_jobs$Geographic.Area <- as.factor(NOC4_jobs$Geographic.Area)
NOC4_jobs$NOC <- as.factor(NOC4_jobs$NOC)
NOC4_jobs$Industry <- as.factor(NOC4_jobs$Industry)
NOC4_jobs$Sector <- NOC4_jobs$Sector

# filtered geography (this loop really should be fixed... takes hours)
for (i in 1:nlevels(as.factor(NOC4_jobs$Geographic.Area))) {
  filtered_geo <-
    filter(
      NOC4_jobs,
      Geographic.Area %in% levels(as.factor(NOC4_jobs$Geographic.Area))[i]
    )

  for (j in 1:length(unique(filtered_geo$Industry))) {
    filtered_industry <-
      filter(
        filtered_geo,
        Industry == unique(filtered_geo$Industry)[j]
      )

    if (nrow(filtered_industry) == 0) {
      next
    } else {
      # now we need to go by NOC level for the calculations

      filtered_noc <- filtered_industry


      # We need employment year1, job openings year1-year3, replacement rate annual (year1-year3)
      filtered_noc_employment_year1 <-
        filter(filtered_noc, Date == year1)
      filtered_noc_employment_year1 <-
        aggregate(
          Employment ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_employment_year1,
          sum
        )
      colnames(filtered_noc_employment_year1)[which(colnames(filtered_noc_employment_year1) ==
        "Employment")] <- "Employment year1"

      # Job Openings year1-year3
      filtered_noc_job_openings_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < (year3 + 1))
      filtered_noc_job_openings_year1_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_job_openings_year1_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year3)[which(colnames(filtered_noc_job_openings_year1_year3) ==
        "Job.Openings")] <- "Job Openings year1-year3"


      # Replacement Rate (annual)
      # Calculate the Annual Replacement Rate

      avg <- NULL
      replacement_rate_df <- NULL

      # but we also need to do this for each NOC.....

      for (y in 1:length(unique(filtered_noc$NOC))) {
        noc_filtered <-
          filter(
            filtered_noc,
            NOC %in% unique(filtered_noc$NOC)[y]
          )

        for (z in 3:nlevels(as.factor(noc_filtered$Date))) {
          date1 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          date2 <-
            filter(noc_filtered, Date %in% levels(as.factor(noc_filtered$Date))[z])
          emp <- sum(date1$Employment)
          replacement2 <- sum(date2$`Replacement.Demand`)
          percent <- replacement2 / emp
          avg <- c(percent, avg)
        }
        replacement_rate <- mean(avg) * 100
        df <-
          data.frame(
            "Geographic Area" = unique(noc_filtered$`Geographic.Area`),
            "NOC" = unique(filtered_noc$NOC)[y],
            "Replacement Rate" = replacement_rate,
            "NOC Description" = unique(noc_filtered$`NOC Description`),
            "Industry" = unique(noc_filtered$Industry),
            "level" = unique(noc_filtered$level)
          )

        replacement_rate_df <- rbind(df, replacement_rate_df)
      }

      colnames(replacement_rate_df) <- c("Geographic.Area", "NOC", "Replacement Rate", "NOC Description", "Industry", "level")


      # Let's add in additional variables that the user can pick from: job openings year2-year3, year1-year2, expansion demand, replacement demand (for all three intervals) etc

      # Job Openings year1-year2
      filtered_noc_job_openings_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_job_openings_year1_year2 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_job_openings_year1_year2,
          sum
        )
      colnames(filtered_noc_job_openings_year1_year2)[which(colnames(filtered_noc_job_openings_year1_year2) ==
        "Job.Openings")] <- "Job Openings year1-year2"

      # Job Openings year2-year3
      filtered_noc_job_openings_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_job_openings_year2_year3 <-
        aggregate(
          Job.Openings ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_job_openings_year2_year3,
          sum
        )
      colnames(filtered_noc_job_openings_year2_year3)[which(colnames(filtered_noc_job_openings_year2_year3) ==
        "Job.Openings")] <- "Job Openings year2-year3"


      # Expansion Demand


      # Expansion year1-year3
      filtered_noc_expansion_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year1_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_expansion_year1_year3,
          sum
        )
      colnames(filtered_noc_expansion_year1_year3)[which(colnames(filtered_noc_expansion_year1_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year3"

      # Expansion year2-year3
      filtered_noc_expansion_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_expansion_year2_year3 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_expansion_year2_year3,
          sum
        )
      colnames(filtered_noc_expansion_year2_year3)[which(colnames(filtered_noc_expansion_year2_year3) ==
        "Expansion.Demand")] <- "Expansion Demand year2-year3"
      # Expansion year1-year2
      filtered_noc_expansion_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_expansion_year1_year2 <-
        aggregate(
          Expansion.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_expansion_year1_year2,
          sum
        )
      colnames(filtered_noc_expansion_year1_year2)[which(colnames(filtered_noc_expansion_year1_year2) ==
        "Expansion.Demand")] <- "Expansion Demand year1-year2"


      # Replacement Demand


      # Replacement year1-year3
      filtered_noc_replacement_year1_year3 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year1_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_replacement_year1_year3,
          sum
        )
      colnames(filtered_noc_replacement_year1_year3)[which(colnames(filtered_noc_replacement_year1_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year3"

      # Replacement year2-year3
      filtered_noc_replacement_year2_year3 <-
        filter(filtered_noc, Date > year2 & Date < as.numeric(year3 + 1))
      filtered_noc_replacement_year2_year3 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_replacement_year2_year3,
          sum
        )
      colnames(filtered_noc_replacement_year2_year3)[which(colnames(filtered_noc_replacement_year2_year3) ==
        "Replacement.Demand")] <- "Replacement Demand year2-year3"
      # Replacement year1-year2
      filtered_noc_replacement_year1_year2 <-
        filter(filtered_noc, Date > year1 & Date < as.numeric(year2 + 1))
      filtered_noc_replacement_year1_year2 <-
        aggregate(
          Replacement.Demand ~ Geographic.Area + NOC + `NOC Description` + Industry +
            level,
          filtered_noc_replacement_year1_year2,
          sum
        )
      colnames(filtered_noc_replacement_year1_year2)[which(colnames(filtered_noc_replacement_year1_year2) ==
        "Replacement.Demand")] <- "Replacement Demand year1-year2"



      occupations_data_frame <- merge(filtered_noc_employment_year1, filtered_noc_job_openings_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_job_openings_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_expansion_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year2_year3, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, filtered_noc_replacement_year1_year2, all = TRUE)
      occupations_data_frame <- merge(occupations_data_frame, replacement_rate_df, all = TRUE)
      occupations_industry <- rbind(occupations_data_frame, occupations_industry)
      occupations_industry <- unique(occupations_industry)
    }
  }

  occupations_by_geo <- rbind(occupations_industry, occupations_by_geo)
  occupations_by_geo <- unique(occupations_by_geo)
}


occupations_by_geo$`Typical Education Background` <- NA
occupations_by_geo <- unique(occupations_by_geo)

education_NOC4 <- unique(education_occupation[, c(which(colnames(education_occupation) == "NOC"), which(colnames(education_occupation) == "Education:.Typical.Background"))])
education_NOC4 <- education_NOC4 %>% filter(NOC != "#T")
colnames(education_NOC4) <- c("NOC", "Typical Education Background")


for (i in 1:nrow(occupations_by_geo)) {
  df <- filter(education_NOC4, NOC %in% occupations_by_geo$NOC[i])
  occupations_by_geo$`Typical Education Background`[i] <- df$`Typical Education Background`
}

# now we need to add in the wages --> the wages are only going to be at the 4 digit NOC level
colnames(wages_cleaned)[which(colnames(wages_cleaned) == "Region")] <- "Geographic.Area"
occupations_industry_tool <- merge(occupations_by_geo, wages_cleaned)
occupations_industry_tool$Level <- "Industry"
occupations_industry_tool <- merge(occupations_industry_tool, occupations_aggregate_industry_tool, all = TRUE)
occupations_industry_tool <- merge(occupations_industry_tool, occupations_sector_tool, all = TRUE)
occupations_industry_tool$Aggregate.Industry <- str_to_title(trimws(occupations_industry_tool$Aggregate.Industry))
colnames(occupations_industry_tool)[which(colnames(occupations_industry_tool) == "Aggregate.Industry")] <- "Aggregate Industry"

#'
#'
## ----1.10_Write_to_File--------------------------------------------------------------------------------------------
individual_industry_agg_industry[which(individual_industry_agg_industry$Level.Value == "Agrifood Sector"), "Level.Value"] <- "Agrifoods Sector"
individual_industry_agg_industry[which(individual_industry_agg_industry$Level.Value == "Technology"), "Level.Value"] <- "Technology Sector"

fwrite(individual_industry_agg_industry, here("Tableau Tool Inputs", "Industry_Long.csv"))

test <- read.csv(here("Tableau Tool Inputs", "Jobs_and_Industry.csv"))
test[which(test$Sector == "Technology"), "Sector"] <- "Technology Sector"
test[which(test$Sector == "Agrifood Sector"), "Sector"] <- "Agrifoods Sector"
names <- colnames(test)
colnames(test) <- gsub("\\.", " ", names)
colnames(test)[which(colnames(test) == "Geographic Area")] <- "Geographic Area"
test <- test[, c(1:5, 7:12, 6, 13:ncol(test))]

fwrite(test, here("Tableau Tool Inputs", "Jobs_and_Industry_IndustryTool.csv"))

fwrite(occupations_industry_tool, here("Tableau Tool Inputs", "Occupations_Industry_Tool.csv"))
