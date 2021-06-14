library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)

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
  filter(!(`Country/Region` %in% c("Diamond Princess", "MS Zaandam"))) %>% 
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
         # Make case_type a factor to bring deaths geom to front
         case_type = factor(case_type, levels = c("confirmed", "deaths", ordered = T))) %>% 
  group_by(`Country/Region`, case_type) %>% 
  summarise(across(matches("20|21"), sum), .groups = "drop")


# World Population Data ---------------------------------------------------

# UN Population data - https://population.un.org/wpp/Download/Standard/CSV/
un_pop = fread("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")

un_pop_df = un_pop %>% 
  # Filter to 2020, use the Medium Variant and remove Aggregated regions
  filter(Time == 2020, Variant == "Medium", LocID <= 900) %>% 
  select(Location, LocID, PopMale:PopDensity) %>% 
  clean_names() %>% 
  # Clean up country names
  mutate(location = str_replace_all(location, c("Bolivia \\(Plurinational State of\\)" = "Bolivia",
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
                                                "Brunei Darussalam" = "Brunei")))


# COVID Policy Data -------------------------------------------------------


# covid_policies = read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", guess_max = 50000) %>%
covid_policies = fread("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv") %>%
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
  pivot_longer(cols = c(-1:-2), 
               names_to = "date", values_to = "total_cases",
               names_transform = list(date = mdy)) %>% 
  clean_names()  %>% 
  left_join(
    select(un_pop_df, -loc_id), by = c("country_region" = "location")
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
  group_by(date, pop_total, .add=T) %>% 
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
