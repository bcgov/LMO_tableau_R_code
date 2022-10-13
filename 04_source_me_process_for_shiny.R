library(tidyverse)

get_jo <- function(tbbl){
  sum(tbbl$`Job Openings`)
}
get_emp <- function(tbbl){
  tbbl$Employment[1]
}
get_cagr <- function(tbbl){
  100*((tbbl$Employment[11]/tbbl$Employment[2])^(.1)-1)
}
get_exp <- function(tbbl){
  sum(tbbl$`Expansion Demand`)
}
get_rep <- function(tbbl){
  sum(tbbl$`Replacement Demand`)
}

agg_by_year <- function(tbbl){
  tbbl%>%
    group_by(Date)%>%
    summarize(across(.cols = where(is.numeric), .fns = sum))
}

wide <- read_csv(here::here("processed_data","Jobs_and_Industry.csv"))%>%
  unite(NOC1, NOC1, `NOC1 Description`, sep=" ")%>%
  unite(NOC2, NOC2, `NOC2 Description`, sep=" ")%>%
  unite(NOC3, NOC3, `NOC3 Description`, sep=" ")%>%
  unite(NOC4, NOC, `NOC4 Description`, sep=" ")


mapping <- unique(wide[c("NOC1","NOC2","NOC3","NOC4")])
two_to_one <- unique(mapping[c("NOC1","NOC2")])
three_to_one <- unique(mapping[c("NOC1","NOC2","NOC3")])

noc1 <- wide%>%
  group_by(NOC1, `Geographic Area`)%>%
  nest()%>%
  mutate(data=map(data, agg_by_year))%>%
  mutate(NOC2=NA,
         NOC3=NA,
         NOC4=NA)

noc2 <- wide%>%
  group_by(NOC2, `Geographic Area`)%>%
  nest()%>%
  mutate(data=map(data, agg_by_year))%>%
  inner_join(two_to_one)%>%
  mutate(NOC3=NA,
         NOC4=NA)

noc3 <- wide%>%
  group_by(NOC3, `Geographic Area`)%>%
  nest()%>%
  mutate(data=map(data, agg_by_year))%>%
  inner_join(three_to_one)%>%
  mutate(NOC4=NA)

noc4 <- wide%>%
  group_by(NOC4, `Geographic Area`)%>%
  nest()%>%
  mutate(data=map(data, agg_by_year))%>%
  inner_join(mapping)

long <- bind_rows(noc1, noc2, noc3, noc4)%>%
  mutate(`Job Openings`=map_dbl(data, get_jo),
         `Employment Level`=map_dbl(data, get_emp),
          `Annual Employment Growth`=map_dbl(data, get_cagr),
          `Job Openings as Share of Employment`=`Job Openings`/`Employment Level`,
         `Expansion Demand`=map_dbl(data, get_exp),
         `Replacement Demand`=map_dbl(data, get_rep)
         )%>%
  select(-data)%>%
  pivot_longer(cols=`Job Openings`:`Replacement Demand`, names_to = "name", values_to = "value")

write_csv(long, here::here("shiny_data","long.csv"))



