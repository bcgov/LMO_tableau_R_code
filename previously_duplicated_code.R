# LIBRARIES------------
library("lubridate")
library("tidyverse")
library("here")
library("readxl")
# "CONSTANTS"... that change every year------------
year1 <- as.numeric(year(today())) - 1 # delete the -1 once we have current data.
year2 <- year1 + 5
year3 <- year1 + 10
# Functions--------------
#takes a string, cleans it up, then converts to a factor
make_clean_factor <- function(strng) {
  strng %>%
    str_replace_all("\t", "") %>%
    trimws() %>%
    str_to_lower() %>%
    str_replace_all(" ", "_") %>%
    factor()
}
# takes a tibble, converts column names to camel_case, and converts character columns to clean factors.
clean_tbbl <- function(tbbl) {
  tbbl %>%
    janitor::clean_names() %>%
    mutate(across(where(is.character), make_clean_factor))
}
# Read in the dataframes------------------------
# wage data THIS WILL BREAK IF MULTIPLE FILES CONTAIN THE PATTERN wages.
wages_raw <- read_excel(here("Tableau Tool Inputs", list.files(here("Tableau Tool Inputs"), pattern = "wages")))

# jobs data
jo_raw <- read_csv(here("LMO Master Databases", "JO single variables.csv"), locale = readr::locale(encoding = "latin1")) %>%
  clean_tbbl() %>%
  filter(noc != "#t")

# industry characteristics
ind_char_raw <- read_excel(here("LMO Master Databases", "Industry characteristics.xlsx")) %>%
  clean_tbbl()

# employment data
employment_raw <- read_csv(here("LMO Master Databases", "Emp single variables.csv"),
  locale = readr::locale(encoding = "latin1")
) %>%
  clean_tbbl()

# demand/supply data
ds_raw <- read_csv(here("LMO Master Databases", "DS single variables.csv")) %>%
  clean_tbbl()

# NOC Mappings
noc_mappings_raw <- read_csv(here("Tableau Tool Inputs", "NOC Mappings.csv")) %>%
  clean_tbbl()

# occupation characteristics file
education_occupation_raw <- read_excel(here("LMO Master Databases", "Occupation characteristics.xlsx")) %>%
  clean_tbbl()

# high opportunity occupations
hoo <- read_excel(here("LMO Master Databases", "HOO list.xlsx")) %>%
  clean_tbbl() %>%
  mutate(high_opportunity_occupation = factor(high_opportunity_occupation, labels = c("non-hoo", "hoo"))) %>%
  rename(occupation_group = high_opportunity_occupation) %>%
  filter(noc != "#t")

#Processing---------------

# jo INPUT TO jo_employment AND ds_merged ---------------
jo <- jo_raw %>%
  filter(
    industry != "all_industries", # Note that if label "all industries" changes in the excel file, will need to change (**)
    description != "total",
    noc != "#t"
  ) %>%
  pivot_wider(names_from = variable, values_from = value)

# employment_industry INPUT TO jo_employment ---------------
columns_to_keep <- colnames(employment_raw)
employment_industry <- employment_raw %>%
  full_join(ind_char_raw, by = "industry") %>%
  filter(noc != "#t") %>%
  select(all_of(columns_to_keep), industry_code, aggregate_industry)

# jo_employment INPUT TO Clean_JO.csv AND jobs_and_industry----------------
jo_employment <-
  full_join(
    jo,
    employment_industry,
    by = c("date", "noc", "description", "industry", "geographic_area")
  )%>%
  filter(industry != "all_industries") %>%
  rename(employment = value) %>%
  select(
    date,
    noc,
    description,
    employment,
    industry,
    aggregate_industry,
    geographic_area,
    job_openings,
    expansion_demand,
    replacement_demand,
    deaths,
    retirements,
    industry_code
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




