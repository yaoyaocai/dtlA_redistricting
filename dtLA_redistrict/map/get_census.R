library(tidycensus)
census <- read.csv("census_codebook.csv")
#years <- list(2018)
census$my_cen_vars <- as.character(census$my_cen_vars)
la_census <- get_acs(
      geography = "tract",
      variables = census$my_cen_vars, 
      state = "CA",
      county = '037',
      survey="acs5",
      year = 2018,
      geometry = TRUE
    )

la_census <- la_census%>%select("GEOID","NAME","variable","estimate","geometry")%>%
  spread( variable, estimate)
colnames(la_census) <- c("GEOID","NAME","white_only" ,"aa_pop" ,"asian_pop",
                         "api_pop" ,"hispanic_pop", "total_population" , "foreign_born" , "poverty_number" ,
                         "medi_income",  "renter_number" , "limited_english_totpop" ,"geometry" )
save.image('la_census.Rdata')

# multi_year <-
#   map(
#     years,
#     ~ get_acs(
#       geography = "tract",
#       variables = census$my_cen_vars, 
#       state = "CA",
#       county = '037',
#       survey="acs5",
#       year = 2018,
#       geometry = TRUE
#     )
#   )
# la_census <- multi_year[1]