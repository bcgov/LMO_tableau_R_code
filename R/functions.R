# this function take a tbbl with two columns, start and finish and returns either start OR a sequence between start and finish.
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
  if("Noc" %in% colnames(tbbl)){
    tbbl <- tbbl%>%
      rename(NOC=Noc)
  }
  write_csv(tbbl, here("processed_data", file_name))
}
get_ten_obs <- function(vec){
  if_else(is.factor(vec), 
          paste(head(levels(vec), n=10), collapse = ", "), 
          paste(head(vec, n=10), collapse = ", "))
}

get_levels <- function(vec){
  length(unique(vec))
}

# creates summary of a dataframe (columns and column types)
col_names_type_example <- function(df){
  cname <- colnames(get(df))
  ctype <- sapply(get(df), class)
  clevels <- sapply(get(df), get_levels)
  cexample <- sapply(get(df), get_ten_obs)
  tbbl <- tibble(column = cname, type = ctype, levels= clevels, ten_values = cexample)
}
nest_to_string <- function(tbbl){
  tbbl%>%
    pull()%>%
    toString()
}

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

