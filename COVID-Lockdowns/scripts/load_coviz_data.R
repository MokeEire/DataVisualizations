library(jsonlite)
library(furrr)
plan(multisession)

# COVID Case Data ---------------------------------------------------------


folder_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

files = c("confirmed", "deaths")


# Because each file is in the folder, paste together the filenames
case_urls = str_c(folder_url, 
                  # e.g. time_series_covid19_confirmed_global.csv
                  "time_series_covid19_", files, "_global.csv") %>% 
  # Setting names so we can map over each file later
  set_names(files)

covid_data = imap_dfr(case_urls, 
                      # ~read_csv(file = .x), 
                      ~fread(.x),
                      .id = "case_type") %>% 
  select(`Province/State`, `Country/Region`, case_type, everything()) %>% 
  # Remove cruise ships
  filter(!(`Country/Region` %in% c("Diamond Princess", "MS Zaandam", 
                                   "Summer Olympics 2020", "Winter Olympics 2022"))) %>% 
  # clean up country names
  mutate(`Country/Region` = str_replace_all(`Country/Region`, c("Cote d'Ivoire" = "Côte d'Ivoire",
                                                                "Korea, South" = "South Korea",
                                                                "Burma" = "Myanmar",
                                                                "US" = "United States",
                                                                "Congo \\(Brazzaville\\)" = "Congo, Rep.",
                                                                "Congo \\(Kinshasa\\)" = "Congo, Dem. Rep.",
                                                                "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
                                                                "Saint Kitts and Nevis" = "St. Kitts and Nevis",
                                                                "Taiwan\\*" = "Taiwan")),
         country_coded = countrycode(sourcevar = `Country/Region`, origin = "country.name", destination = "iso2c", nomatch = NA_character_),
         country_coded = case_when(`Country/Region` == "Kosovo" ~ "XK",
                                   `Country/Region` == "Micronesia" ~ "FM",
                                   T ~ country_coded),
         # Make case_type a factor to bring deaths geom to front
         case_type = factor(case_type, levels = c("confirmed", "deaths", ordered = T))) %>% 
  group_by(`Country/Region`, country_coded, case_type) %>% 
  summarise(across(matches("2[0-2]"), sum), .groups = "drop")


# World Population Data ---------------------------------------------------


collect_un_pop_data = function(covid_df, start_year = "2020", end_year = "2022"){
  # Prepare Query using country codes from COVID data
  locs_to_query = str_c(unique(covid_df$country_coded), collapse = ",")
  
  # Convert ISO2 codes to location IDs for the API
  # https://population.un.org/dataportal/about/dataapi#locations
  loc_ids = fromJSON(str_c("https://population.un.org/dataportalapi/api/v1/locations/", locs_to_query, "?sort=id")) %>% 
    pull(id) %>% 
    str_c(collapse = ",")
  
  base_query = "https://population.un.org/dataportalapi/api/v1/data/indicators/49/locations/"
  
  initial_query = URLencode(str_c(base_query, loc_ids, 
                                  "?startYear=", start_year, "&endYear=", end_year,
                                  # Filter to median pop projection for both sexes
                                  "&variants=4&sexes=3"))
  
  results = fromJSON(initial_query)
  
  # Add the data the a df
  results_df = results$data
  
  # prepare queries for all the remaining pages
  remaining_pages = URLencode(str_c(base_query, loc_ids, 
                                "?startYear=", start_year, "&endYear=", end_year,
                                "&variants=4&sexes=3",
                                "&pageNumber=", 2:results$pages, "&pageSize=100"))
  
  # Map over them in parallel
  remaining_pages_df = future_map(remaining_pages, fromJSON) %>% 
    # Pluck the data element and combine into a df
    future_map_dfr(pluck, "data")
  
  # Combine all pages
  results_df = bind_rows(results_df, remaining_pages_df)
  
  as_tibble(results_df)
}

un_pop_df = collect_un_pop_data(covid_data)

un_pop_prep = un_pop_df %>% 
  select(locationId:iso2, timeLabel, pop_total = value) %>% 
  clean_names() %>% 
  # Clean up country names
  mutate(location_clean = str_replace_all(location, c("Bolivia \\(Plurinational State of\\)" = "Bolivia",
                                                      "Iran \\(Islamic Republic of\\)" = "Iran",
                                                      "China, Taiwan Province of China" = "Taiwan",
                                                      "Viet Nam" = "Vietnam",
                                                      "Republic of Korea" = "South Korea",
                                                      "Republic of Moldova" = "Moldova",
                                                      "United States of America" = "United States",
                                                      "Venezuela \\(Bolivarian Republic of\\)" = "Venezuela",
                                                      "Democratic Republic of the Congo" = "Congo, Dem. Rep.",
                                                      "^Congo$" = "Congo, Rep.",
                                                      "United Republic of Tanzania" = "Tanzania",
                                                      "Syrian Arab Republic" = "Syria",
                                                      "Russian Federation" = "Russia",
                                                      "Lao People's Democratic Republic" = "Laos",
                                                      "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
                                                      "Saint Kitts and Nevis" = "St. Kitts and Nevis",
                                                      "Brunei Darussalam" = "Brunei")),
         year = as.integer(time_label))


# COVID Policy Data -------------------------------------------------------


# covid_policies = read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", guess_max = 50000) %>%
covid_policies = fread("https://github.com/OxCGRT/covid-policy-tracker-legacy/raw/main/legacy_data_202207/OxCGRT_latest.csv") %>%
  clean_names() %>% 
  filter(jurisdiction == "NAT_TOTAL") %>% 
  mutate(date = ymd(date),
         across(matches("c[0-9]_flag"), ~factor(., levels = c(0, 1), labels = c("Targeted", "National"))))%>% 
  select(country_name, date, c1_school_closing:c8_international_travel_controls) %>%
  # rename policy vars
  rename_with(.cols = matches("c[0-9]_[^(flag)]"), .fn = ~str_replace_all(., "(?<=c[0-9]_).+", "response"))

policy_dict = c("c1" = "School closing",
                "c2" = "Workplace closing",
                "c3" = "Cancel public events",
                "c4" = "Restrictions on gatherings",
                "c5" = "Close public transport",
                "c6" = "Stay at home requirements",
                "c7" = "Restrictions on internal movement",
                "c8" = "International travel controls")





# Join COVID and Population data and aggregate --------------------------------------


# Join population data by country
covid_pop_df = covid_data %>% 
  # Pivot data first
  pivot_longer(cols = c(-1:-3), 
               names_to = "date", values_to = "total_cases",
               names_transform = list(date = mdy)) %>% 
  mutate(year = year(date), .before = date) %>% 
  clean_names()  %>% 
  # Join UN data
  left_join(
    un_pop_prep, by = c("country_coded" = "iso2", "year")
  ) %>% 
  # Order by country, region, case type, then date
  arrange(country_region, case_type, date) %>% 
  # Calculate daily cases for each case type at the province/state level
  group_by(country_region, case_type) %>% 
  mutate(daily_cases = total_cases - lag(total_cases, default = 0, order_by = date),
         daily_cases = if_else(daily_cases < 0, 0, daily_cases))


# Aggregate to country level
covid_country_level = covid_pop_df %>% 
  # Group by country, date, and case type, keep population constant
  group_by(country_coded, year, date, pop_total, .add=T) %>% 
  # Calculate Country level cases for each case type over time
  summarise(total_cases = sum(total_cases, na.rm=T),
            daily_cases = sum(daily_cases, na.rm=T),
            .groups = "drop") %>% 
  group_by(country_region, case_type) %>% 
  mutate(
    # Calculate rolling 7 day average for daily cases;
    # Right align means on a given day, calculate the average of the past 7 days (inclusive)
    roll_avg_7day = zoo::rollmean(daily_cases, k = 7, fill = NA, align = "right")
  ) %>% 
  ungroup()

# For monthly labels, I want the label to be in the middle of the month (including current month)
# As a result we can't really use today()

global_date_labels = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-01"), length.out = 7)




# Visualization utilities -------------------------------------------------

case_pal = set_names(viz_colours[c(3,7)], unique(covid_country_level$case_type))

save(covid_policies, file = "covid_policies.RData")
save(covid_country_level, file = "covid_country_level.RData")

# covid_country_level %>% filter(country_region == "Norway", case_type == "confirmed", date == ymd("2020-05-18"))
# A tibble: 1 x 7
# country_region case_type date       pop_total total_cases daily_cases roll_avg_7day
# <chr>          <fct>     <date>         <dbl>       <dbl>       <dbl>         <dbl>
#   1 Norway         confirmed 2020-05-18     5421.        8257           8          17.9
