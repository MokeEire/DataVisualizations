# Compare COVID Cases and Policy implementation
library(extrafont)
library(tidyverse)
library(janitor)
library(here)
library(lubridate)
library(ggtext)
library(ggrepel)
library(gghighlight)
library(reactable)
library(patchwork)

here("theme_mark.R") %>% 
  walk(source)

viz_colours = my_col_pal[-1:-4]
source_caption = function(sources){
  if(missing(sources)){
    stop("Need to provide a vector of sources to the sources argument")
  }
  
  str_c(
    "**Source", if(length(sources) > 1){"s"}, "**: ",
    "<span style='line-height:1.25;'>", str_c(sources, collapse = "<br>"), "</span>",
    "<br><br>",
    "<span style='font-size:12px;color:", viz_colours[7], 
    "'>Visualized by @MokeEire</span>"
  )
}

jhu_source = "2019 Novel Coronavirus Data Repository by Johns Hopkins CSSE"
oxford_source = "Oxford COVID-19 Government Response Tracker, Blavatnik School of Government"



# COVID Case Data ---------------------------------------------------------


folder_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

files = c("confirmed", "recovered", "deaths")


# Because each file is in the folder, paste together the filenames
case_urls = str_c(folder_url, 
                  # e.g. time_series_covid19_confirmed_global.csv
                  "time_series_covid19_", files, "_global.csv") %>% 
  # Setting names so we can map over each file later
  set_names(files)


# World Population Data ---------------------------------------------------

# Set filename
# Thanks to: https://blogdown-demo.rbind.io/2018/02/27/r-file-paths/
world_pop_file = "world_bank_pop_2019.csv"
# Clean the column names and then change country names to match the COVID data
world_pop = read_csv(world_pop_file) %>% 
  clean_names() %>% 
  rename(population = x2019_yr2019) %>% 
  mutate(country_name = case_when(
    country_name == "Korea, Rep." ~ "South Korea",
    country_name == "Iran, Islamic Rep." ~ "Iran",
    country_name == "Hong Kong SAR, China" ~ "Hong Kong",
    country_name == "United States" ~ "US",
    country_name == "Egypt, Arab Rep." ~ "Egypt",
    country_name == "Syrian Arab Republic" ~ "Syria",
    country_name == "Russian Federation" ~ "Russia",
    country_name == "American Samoa" ~ "Samoa",
    country_name == "Czech Republic" ~ "Czechia",
    country_name == "Kyrgyz Republic" ~ "Kyrgyzstan",
    country_name == "Lao PDR" ~ "Laos",
    country_name == "St. Lucia" ~ "Saint Lucia",
    country_name == "Slovak Republic" ~ "Slovakia",
    country_name == "Bolivia (Plurinational State of)" ~ "Bolivia",
    str_detect(country_name,"Ivoire") ~ "Cote d'Ivoire",
    country_name == "Moldova, Republic of" ~ "Moldova",
    country_name == "Russian Federation" ~ "Russia",
    country_name == "Venezuela, RB" ~ "Venezuela",
    country_name == "Gambia, The" ~ "Gambia",
    country_name == "Bahamas, The" ~ "Bahamas",
    country_name == "Brunei Darussalam" ~ "Brunei",
    country_name == "Cameroon, Republic of" ~ "Cameroon",
    country_name == "Yemen, Rep." ~ "Yemen",
    T ~ str_squish(country_name)
  ))


# COVID Policy Data -------------------------------------------------------

covid_policies = read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv")

glimpse(covid_policies)








# Read COVID Case Data ----------------------------------------------------

covid_data = imap_dfr(case_urls, 
                      ~read_csv(file = .x), 
                      .id = "case_type") %>% 
  select(`Province/State`, `Country/Region`, case_type, everything())

head(covid_data)%>% 
  select(1:10)

# Pivot the data 
covid_data_pivot = covid_data %>% 
  pivot_longer(cols = c(-1:-5), 
               names_to = "date", values_to = "total_cases",
               names_transform = list(date = mdy)) %>% 
  clean_names()

covid_data_pivot %>% head()

# Clean up country names, remove cruise ships
covid_data_clean = covid_data_new %>% 
  # Sort by country, state, and date
  arrange(country_region, province_state, date) %>% 
  # Correct country naming (we'll see why in a moment)
  mutate(country_region = case_when(
    country_region == "Korea, South" ~ "South Korea",
    country_region == "Burma" ~ "Myanmar",
    country_region == "Congo (Brazzaville)" ~ "Congo, Rep.",
    country_region == "Congo (Kinshasa)" ~ "Congo, Dem. Rep.",
    country_region == "Saint Vincent and the Grenadines" ~ "St. Vincent and the Grenadines",
    country_region == "Saint Kitts and Nevis" ~ "St. Kitts and Nevis",
    T ~ country_region
  )) %>% 
  # Remove the Diamond Princess
  filter(country_region != "Diamond Princess")

head(covid_data_clean)

covid_cases_by_day = covid_country_level %>% 
  group_by(country_region) %>% 
  # Keep only confirmed cases where we have a value for population
  filter(case_type == "confirmed", !is.na(population),
         max(total_cases) > 50000) %>% 
  arrange(country_region, date) %>% 
  # Identify date of first case in each country
  mutate(first_case = nth(date, min(which(total_cases > 0))),
         min_cases = nth(total_cases, min(which(total_cases > 0)))) %>% 
  ungroup() %>% 
  # Calculate the daily cases as a proportion of population
  mutate(daily_cases_pct = daily_cases/population) %>% 
  select(country_region, population, date, first_case, 
         total_cases, daily_cases, daily_cases_pct, min_cases)



# Join Population data and aggregate --------------------------------------


# Join population data by country
covid_pop_df = covid_data_clean  %>% 
  left_join(
    select(world_pop, country_name, population), by = c("country_region" = "country_name")
  )


# Aggregate to country level
covid_country_level = covid_pop_df %>% 
  group_by(country_region, date, case_type, population) %>% 
  # Calculate Country level cases for each case type over time
  summarise(total_cases = sum(total_cases, na.rm=T),
            daily_cases = sum(daily_cases, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate case rates relative to population size
  mutate(total_cases_proportion = total_cases/population,
         total_cases_per_100k = total_cases_proportion*100000,
         daily_cases_proportion = daily_cases/population,
         daily_cases_per_100k = daily_cases_proportion*100000)

# Aggregate to global level
covid_global = covid_country_level %>% 
  group_by(date, case_type) %>% 
  summarise(total_cases = sum(total_cases, na.rm=T),
            daily_cases = sum(daily_cases, na.rm=T)) %>% 
  ungroup() %>% # Always remember to ungroup!
  mutate(case_type = factor(case_type, levels = c("confirmed", "recovered", "deaths")))



# Create plot labels ------------------------------------------------------


# Use max values for each case type as the label position
case_labels = covid_global %>% 
  group_by(case_type) %>% 
  top_n(1, total_cases) %>% 
  ungroup() %>% 
  mutate(date = max(date) + days(2),
         total_cases = total_cases)

# For monthly labels, I want the label to be in the middle of the month (including current month)
# As a result we can't really use today()

global_date_labels = seq(to = max(covid_global$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_global$date), from = ymd("2020-01-01"), by = "1 month")

# Country labels
country_labels = covid_country_level %>% 
  # Group by country
  group_by(country_region) %>% 
  # Select confirmed cases on the most recent date
  filter(case_type == "confirmed", date == max(date)) %>% 
  ungroup() %>% 
  # Take the top 5, ordered by daily cases => this is now slice_max()
  slice_max(n = 5, total_cases) %>% 
  mutate(date =max(date) + days(2)) %>% # Add two days to the date for the label position
  arrange(-total_cases)

# Country labels
country_labels_per_100k = covid_country_level %>% 
  # Select confirmed cases on the most recent date
  filter(case_type == "confirmed", !is.na(population), date == max(date)) %>% 
  mutate(cases_per_100k = 100000*(total_cases/population)) %>% 
  # Take the top 5, ordered by daily cases => this is now slice_max()
  slice_max(n = 5, cases_per_100k) %>% 
  mutate(date =max(date) + days(2)) %>% # Add two days to the date for the label position
  arrange(-cases_per_100k)

top_country_pal = viz_colours[1:5]

coviz_axis = scale_x_date(breaks = global_date_ticks, date_minor_breaks = "1 week",
                          date_labels = "%b", 
                          position = "top")



# Specify Lockdown information --------------------------------------------

# Create boolean values for whether a lockdown was enacted
################## IMPORTANT:  ############################
# I specify a policy as a lockdown when the policy is required rather than recommended
lockdowns = covid_policies %>% 
  select(CountryName:`C8_International travel controls`, M1_Wildcard:ConfirmedDeaths) %>%
  clean_names() %>% 
  mutate(country_name = if_else(country_name == "United States", "US", country_name),
         date = ymd(date),
         lockdown_school_closing = (c1_school_closing > 1 & c1_flag == 1),
         lockdown_workplace_closing = (c2_workplace_closing > 1 & c2_flag == 1),
         lockdown_cancel_public_events = (c3_cancel_public_events > 1 & c3_flag == 1),
         lockdown_restrictions_on_gatherings = (c4_restrictions_on_gatherings > 0 & c4_flag == 1),
         lockdown_close_public_transport = (c5_close_public_transport > 0 & c5_flag == 1),
         lockdown_stay_at_home_requirements = (c6_stay_at_home_requirements  > 1 & c6_flag == 1),
         lockdown_restrictions_on_internal_movement = (c7_restrictions_on_internal_movement > 1 & c7_flag == 1),
         lockdown_international_travel_controls = (c8_international_travel_controls > 1)) %>% 
  pivot_longer(cols = starts_with("lockdown"), names_to = "lockdown_type", values_to = "lockdown_active", names_pattern = "lockdown_([a-z_]+)")

compare_cases_to_policies = function(country = "US"){
  # Cases
  cases_viz = covid_cases_by_day %>% 
    # Specify country
    filter(country_region == country) %>% 
    arrange(first_case, -min_cases, country_region, date) %>%
    mutate(country_region = fct_inorder(country_region)) %>%
    # Plot
    ggplot(., aes(x = date, y = daily_cases, group = country_region))+
    geom_area(alpha = .75, fill = viz_colours[1], colour = my_col_pal[3])+
    labs(title = str_c("COVID-19 cases and lockdown policies in ", country))+
    coviz_axis+
    scale_y_continuous(breaks = scales::pretty_breaks(), labels = scales::comma, position = "left")+
    theme_mark(md=T, base_size = 12, plot_margin = margin(0,20,-10,20))+
    theme(legend.position = "none",
          # hide the minor grid lines
          panel.grid.minor = element_blank(),
          # hide vertical major grid lines
          panel.grid.major.x = element_blank(),
          axis.title = element_blank(),
          axis.title.y.left = element_blank(),
          axis.title.x.top = element_blank(),
          strip.text.y.right = element_blank(),
          panel.spacing.y = unit(-.2, "lines"),
          axis.line = element_blank())
  # browser()
  # Policies
  lockdown_viz = covid_cases_by_day %>% 
    filter(country_region %in% c(country)) %>% 
    # I swear I'll clean up the country names so they all join later
    inner_join(lockdowns, by = c("country_region" = "country_name", "date" = "date")) %>% 
    arrange(first_case, -min_cases, country_region, date) %>%
    mutate(country_region = fct_inorder(country_region),
           lockdown_type = factor(str_to_title(str_replace_all(lockdown_type, "_", " ")),
                                  levels = c("School Closing", "Workplace Closing", 
                                             "Cancel Public Events", "Restrictions On Gatherings", 
                                             "Close Public Transport", "Stay At Home Requirements", 
                                             "Restrictions On Internal Movement", "International Travel Controls"), 
                                  ordered = T),
           lockdown_val = na_if(lockdown_active*as.numeric(lockdown_type)/2, 0)) %>% 
    ggplot(., aes(x = date, y = lockdown_val, colour = lockdown_type, group = lockdown_type))+
    geom_line(alpha = .85, size = 2)+
    labs(caption = source_caption(sources = c(jhu_source, oxford_source)))+
    scale_colour_manual(values = viz_colours, na.value = my_col_pal[1], 
                        name = NULL)+
    scale_x_date(breaks = global_date_ticks, date_minor_breaks = "1 week",
                 date_labels = "%b", 
                 position = "bottom")+
    facet_wrap(~lockdown_type, 
               ncol = 1, strip.position = "right", 
               scales = "free_y", shrink=F)+
    theme_mark(md=T, base_size = 12, plot_margin = margin(-5,20,-5,20))+
    theme(legend.position = "none",
          # hide the minor grid lines
          panel.grid.minor = element_blank(),
          # hide vertical major grid lines
          panel.grid.major.y = element_blank(),
          axis.title = element_blank(),
          axis.title.y.left = element_blank(),
          axis.title.y.right = element_blank(),
          axis.title.x.bottom = element_blank(),
          strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1, 
                                           margin = margin(0,0,0,0)),
          strip.text.y.right = element_text(angle = 0, vjust = 0.5, hjust = 0, 
                                            margin = margin(0,0,0,0)),
          panel.spacing.y = unit(0, "null"),
          # axis.text.x = element_blank(),
          axis.line = element_blank(),
          axis.text.y = element_blank())
  
  # Combine the two
  cases_viz / lockdown_viz + 
    plot_layout(design = c(
      area(t = 1, l = 1, b = 5, r = 4),
      area(t = 6, l = 1, b = 7, r = 4)
    ))+
    plot_annotation(theme = theme_mark(plot_margin = margin(10, 10, 0, 10)))
  
}
