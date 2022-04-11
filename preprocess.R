parking = read_csv('maindf.csv')
county_mean = parking %>%
  dplyr::select(County, State, 
                AvgTimeToPark:AvgTotalGeohashes, 
                PercentCar:PercentOther, 
                POPESTIMATE2020:RNETMIG2020) %>% 
  group_by(County, State) %>%
  mutate_at(vars(AvgTimeToPark:AvgTotalGeohashes, PercentCar:PercentOther), mean) %>%
  mutate(State = ifelse(
      test = State %in% state.name, 
      yes = state.abb[match(State, state.name)], 
      no = str_replace_all(gsub(" ", "", State), "[:lower:]+", ""))
  ) %>% 
  unique() %>%
  mutate(POPESTIMATE2020 = ifelse(is.na(POPESTIMATE2020), 0, POPESTIMATE2020)) %>%
  ungroup()
