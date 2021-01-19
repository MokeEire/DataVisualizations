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
    "Source", if(length(sources) > 1){"s"}, ": ",
    str_c(sources, collapse = "; "),
    "<br><br>",
    # "<span style='font-size:12px;color:", my_col_pal[3], 
    "Visualized by @MokeEire"
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

covid_data = imap_dfr(case_urls, 
                      ~read_csv(file = .x), 
                      .id = "case_type") %>% 
  select(`Province/State`, `Country/Region`, case_type, everything())

head(covid_data)%>% 
  select(1:10)








# World Population Data ---------------------------------------------------

# UN Population data - https://population.un.org/wpp/Download/Standard/CSV/
un_pop = read_csv("https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv")

un_pop_df = un_pop %>% 
  # Filter to 2020, use the Medium Variant and remove Aggregated regions
  filter(Time == 2020, Variant == "Medium", LocID <= 900) %>% 
  select(Location, LocID, PopMale:PopDensity) %>% 
  clean_names()



# Countries in Johns Hopkins data but not in UN
jh_not_un = covid_data %>% 
  distinct(`Country/Region`) %>% 
  anti_join(un_pop_df, by = c("Country/Region" = "location"))

# Countries in UN data but not in Johns Hopkins
un_not_jh = un_pop_df %>% 
  distinct(location) %>% 
  anti_join(covid_data, by = c("location" = "Country/Region"))

bind_cols(
  jh_not_un %>% 
    add_row(`Country/Region` = rep(NA_character_,
                                   (nrow(un_not_jh)-nrow(jh_not_un)))),
  un_not_jh
) %>% View()

un_pop_clean = un_pop_df %>% 
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

# Clean up country names, remove cruise ships
covid_data_clean = covid_data %>% 
  mutate(`Country/Region` = str_replace_all(`Country/Region`, c("Cote d'Ivoire" = "Côte d'Ivoire",
                                                                "Korea, South" = "South Korea",
                                                                "Burma" = "Myanmar",
                                                                "US" = "United States",
                                                                "Congo \\(Brazzaville\\)" = "Congo, Rep.",
                                                                "Congo \\(Kinshasa\\)" = "Congo, Dem. Rep.",
                                                                "Saint Vincent and the Grenadines" = "St. Vincent and the Grenadines",
                                                                "Saint Kitts and Nevis" = "St. Kitts and Nevis",
                                                                "Taiwan\\*" = "Taiwan"))) %>% 
  # Remove cruise ships
  filter(!(`Country/Region` %in% c("Diamond Princess", "MS Zaandam")))

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


covid_policies = read_csv("https://github.com/OxCGRT/covid-policy-tracker/raw/master/data/OxCGRT_latest.csv", guess_max = 50000) %>%
  clean_names() %>% 
  filter(jurisdiction == "NAT_TOTAL") %>% 
  mutate(date = ymd(date),
         across(matches("c[0-9]_flag"), ~factor(., levels = c(0, 1), labels = c("Targeted", "National"))))

glimpse(covid_policies)

policy_dict = c("c1" = "School closing",
                "c2" = "Workplace closing",
                "c3" = "Cancel public events",
                "c4" = "Restrictions on gatherings",
                "c5" = "Close public transport",
                "c6" = "Stay at home requirements",
                "c7" = "Restrictions on internal movement",
                "c8" = "International travel controls")

lockdowns = covid_policies %>% 
  select(country_name, date, c1_school_closing:c8_international_travel_controls) %>%
  clean_names() %>% 
  # rename policy vars
  rename_with(.cols = matches("c[0-9]_[^(flag)]"), .fn = ~str_replace_all(., "(?<=c[0-9]_).+", "response")) %>% 
  mutate(# Create boolean values for whether a lockdown was enacted
         ################## IMPORTANT:  ############################
         # I specify a policy as a lockdown when the policy is required rather than recommended
         c1_lockdown = (c1_response > 1),
         c2_lockdown = (c2_response > 1),
         c3_lockdown = (c3_response > 1),
         c4_lockdown = (c4_response > 0),
         c5_lockdown = (c5_response > 0),
         c6_lockdown = (c6_response  > 1),
         c7_lockdown = (c7_response > 1),
         c8_lockdown = (c8_response > 1)) %>% 
  # pivot to policy, response, flag
  pivot_longer(!c(country_name, date), names_to = c("policy", ".value"), names_sep = "_") %>% 
  # provide policy labels
  mutate(policy = str_replace_all(policy, policy_dict)) %>% 
  # Add factor labels to flag and make policy a factor
  mutate(policy = factor(policy,
                         levels = c("School closing", 
                                    "Workplace closing", 
                                    "Cancel public events",
                                    "Restrictions on gatherings", 
                                    "Close public transport", 
                                    "Stay at home requirements", 
                                    "Restrictions on internal movement",
                                    "International travel controls"), 
                         ordered = T),
         lockdown_val = na_if(lockdown*as.numeric(policy), 0))

covid_policies %>% 
  select(country_name:c8_international_travel_controls, m1_wildcard:confirmed_deaths) %>%
  mutate(country_name = if_else(country_name == "United States", "US", country_name),
         date = ymd(date),
         # Lockdown booleans
         lockdown_school_closing = (c1_school_closing > 1 & c1_flag == 1),
         lockdown_workplace_closing = (c2_workplace_closing > 1 & c2_flag == 1),
         lockdown_cancel_public_events = (c3_cancel_public_events > 1 & c3_flag == 1),
         lockdown_restrictions_on_gatherings = (c4_restrictions_on_gatherings > 0 & c4_flag == 1),
         lockdown_close_public_transport = (c5_close_public_transport > 0 & c5_flag == 1),
         lockdown_stay_at_home_requirements = (c6_stay_at_home_requirements  > 1 & c6_flag == 1),
         lockdown_restrictions_on_internal_movement = (c7_restrictions_on_internal_movement > 1 & c7_flag == 1),
         lockdown_international_travel_controls = (c8_international_travel_controls > 1)) %>% 
  pivot_longer(cols = starts_with("lockdown"), names_to = "lockdown_type", values_to = "lockdown_active", names_pattern = "lockdown_([a-z_]+)") %>% 
  select(country_name, date, contains("lockdown"))

# Add response labels
# `C1_School closing` = factor(`C1_School closing`, levels = 0:3, labels = c("no measures", 
#                                                                            "recommend closing or all schools open with alterations resulting in significant differences compared to non-Covid-19 operations", 
#                                                                            "require closing (only some levels or categories, eg just high school, or just public schools)", 
#                                                                            "require closing all levels"),
#                              ordered = T),
# `C2_Workplace closing` = factor(`C2_Workplace closing`, levels = 0:3, labels = c("no measures",
#                                                                                  "recommend closing (or recommend work from home)",
#                                                                                  "require closing (or work from home) for some sectors or categories of workers",
#                                                                                  "require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)"),
#                                 ordered=T),
# `C3_Cancel public events` = factor(`C3_Cancel public events`, 
#                                    levels = 0:2, 
#                                    labels = c("no measures",
#                                               "recommend cancelling",
#                                               "require cancelling"),
#                                    ordered = T), 
# `C4_Restrictions on gatherings` = factor(`C4_Restrictions on gatherings`, 
#                                          levels = 0:4, 
#                                          labels = c("no restrictions",
#                                                     "restrictions on very large gatherings (the limit is above 1000 people)",
#                                                     "restrictions on gatherings between 101-1000 people",
#                                                     "restrictions on gatherings between 11-100 people",
#                                                     "restrictions on gatherings of 10 people or less"),
#                                          ordered = T), 
# `C5_Close public transport` = factor(`C5_Close public transport`, 
#                                      levels = 0:2, 
#                                      labels = c("no measures",
#                                                 "recommend closing (or significantly reduce volume/route/means of transport available)",
#                                                 "require closing (or prohibit most citizens from using it)"),
#                                      ordered = T), 
# `C6_Stay at home requirements` = factor(`C6_Stay at home requirements`, 
#                                         levels = 0:3, 
#                                         labels = c("no measures",
#                                                    "recommend not leaving house",
#                                                    "require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips",
#                                                    "require not leaving house with minimal exceptions (eg allowed to leave once a week, or only one person can leave at a time, etc)"),
#                                         ordered = T), 
# `C7_Restrictions on internal movement` = factor(`C7_Restrictions on internal movement`, 
#                                                 levels = 0:2, 
#                                                 labels = c("no measures",
#                                                            "recommend not to travel between regions/cities",
#                                                            "internal movement restrictions in place"),
#                                                 ordered = T),
# `C8_International travel controls` = factor(`C8_International travel controls`, 
#                                             levels = 0:4, 
#                                             labels = c("no restrictions",
#                                                        "screening arrivals",
#                                                        "quarantine arrivals from some or all regions",
#                                                        "ban arrivals from some regions",
#                                                        "ban on all regions or total border closure"),
#                                             ordered = T)







# Read COVID Case Data ----------------------------------------------------


# Pivot the data 
covid_data_pivot = covid_data_clean %>% 
  pivot_longer(cols = c(-1:-5), 
               names_to = "date", values_to = "total_cases",
               names_transform = list(date = mdy)) %>% 
  clean_names()

covid_data_pivot %>% head()
  












# Join Population data and aggregate --------------------------------------


# Join population data by country
covid_pop_df = covid_data_pivot  %>% 
  left_join(
    select(un_pop_clean, -loc_id), by = c("country_region" = "location")
  ) %>% 
  # Order by country, region, case type, then date
  arrange(country_region, province_state, case_type, date) %>% 
  # Calculate daily cases for each case type at the province/state level
  group_by(country_region, province_state, case_type) %>% 
  mutate(daily_cases = total_cases - lag(total_cases, default = 0, order_by = date)) %>% 
  ungroup()


# Aggregate to country level
covid_country_level = covid_pop_df %>% 
  # Group by country, date, and case type, keep population constant
  group_by(country_region, date, case_type, pop_total) %>% 
  # Calculate Country level cases for each case type over time
  summarise(total_cases = sum(total_cases, na.rm=T),
            daily_cases = sum(daily_cases, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(country_region, case_type) %>% 
  mutate(
    # Calculate case rates relative to population size
    total_cases_proportion = total_cases/pop_total,
    total_cases_per_100k = total_cases_proportion*100000,
    daily_cases_proportion = daily_cases/pop_total,
    daily_cases_per_100k = daily_cases_proportion*100000,
    # Calculate rolling 7 day average for daily cases;
    # Right align means on a given day, calculate the average of the past 7 days (inclusive)
    roll_avg_7day = zoo::rollmean(daily_cases, k = 7, fill = NA, align = "right")
    )

# Aggregate to global level
covid_global = covid_country_level %>% 
  group_by(date, case_type) %>% 
  summarise(total_cases = sum(total_cases, na.rm=T),
            daily_cases = sum(daily_cases, na.rm=T)) %>% 
  ungroup() %>% # Always remember to ungroup!
  mutate(case_type = factor(case_type, levels = c("confirmed", "recovered", "deaths")))








# Cumulative Global Cases, by type ------------------------------------------------------

# Utility data for plot labels
# Create total labels for each case type
case_labels = covid_global %>% 
  group_by(case_type) %>% 
  slice_max(total_cases, n = 1) %>% 
  ungroup() %>% 
  mutate(date = max(date) + days(2),
         total_cases = total_cases)

# For monthly labels, I want the label to be in the middle of the month (including current month)
# As a result we can't really use today()

global_date_labels = seq.Date(to = max(covid_global$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_global$date), from = ymd("2020-01-01"), by = "1 month")

case_pal = set_names(viz_colours[c(3,7,6)], unique(covid_pop_df$case_type))







# Cumulative Confirmed Cases, by Country ----------------------------------




top_country_pal = viz_colours[1:5]

coviz_axis = scale_x_date(breaks = global_date_ticks, date_minor_breaks = "1 week",
                          date_labels = "%b", 
                          position = "top")











# Lockdown policies plot --------------------------------------------


compare_cases_to_policies = function(country = "Ireland", save=F){
  # Cases
  cases_viz = covid_country_level %>% 
    # Specify country, remove recovered cases
    filter(country_region == country, case_type == "confirmed") %>% 
    arrange(country_region, date) %>%
    # Plot
    ggplot(., aes(x = date, y = roll_avg_7day, fill = case_type))+
    # Geom
    geom_area(alpha = .75, colour = my_col_pal[3])+
    # Axes
    scale_x_date(breaks = global_date_ticks, 
                 date_labels = "%b", 
                 position = "top")+
    scale_y_continuous(breaks = scales::pretty_breaks(), 
                       labels = scales::comma, 
                       position = "left", expand = expansion(add = 50))+
    # Colour scale
    scale_fill_manual(values = case_pal)+
    # Theme
    theme_mark(md=T, base_size = 12, plot_margin = margin(0,0,0,0))+
    theme(legend.position = "none",
          # hide the minor grid lines
          panel.grid.minor = element_blank(),
          # hide vertical major grid lines
          panel.grid.major.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks.x.top = element_line(colour = my_col_pal[4]),
          axis.ticks.length.x.top = unit(1, "mm"),
          axis.text.x.top = element_text(margin = margin(b = 7.5)),
          axis.title = element_blank(),
          axis.title.y.left = element_blank(),
          axis.title.x.top = element_blank(),
          axis.text.y.left = element_text(vjust = 0),
          strip.text.y.right = element_blank(),
          panel.spacing.y = unit(-.2, "lines"))
  
  # browser()
  # Policies
  lockdown_viz = covid_pop_df %>% 
    filter(country_region %in% c(country)) %>% 
    distinct(country_region, date) %>% 
    # I swear I'll clean up the country names so they all join later
    inner_join(lockdowns, by = c("country_region" = "country_name", "date" = "date")) %>% 
    replace_na(list(flag = "National")) %>% 
    ggplot(., aes(x = date, y = lockdown_val, colour = policy, group = flag))+
    # Geom
    geom_line(aes(linetype = flag, size = flag), alpha = .95)+
    # Axes
    scale_x_date(breaks = global_date_ticks, date_minor_breaks = "1 week",
                 date_labels = "%b", 
                 position = "bottom")+
    # Colour, linetype, size
    scale_colour_manual(values = viz_colours, na.value = my_col_pal[1], 
                      name = NULL, guide = guide_none())+
    scale_linetype_manual(values = c("Targeted" = "dashed", "National" = "solid"), 
                          na.translate = F,
                          guide = guide_legend(title = "Policy scope",
                                               override.aes = list(size = c(2, 1)),
                                               keywidth = unit(.8, "cm"),
                                               label.position = "left", reverse = T))+
    scale_size_manual(values = c("Targeted" = 1, "National" = 2), na.value = 2, guide = guide_none())+
    # Facets
    facet_wrap(~policy, 
               ncol = 1, strip.position = "right", 
               scales = "free_y", shrink=F)+
    # Theme
    theme_mark(md=T, base_size = 12, plot_margin = margin(0,0,0,0))+
    theme(legend.position = c(-.1,0.5),
          legend.spacing = unit(0.5, "cm"),
          legend.background = element_rect(fill = "#F9FAFA", colour = "#E1EAE9"),
          legend.title = ggtext::element_markdown(colour = "#2C3535"),
          legend.text = ggtext::element_markdown(colour = my_col_pal[3]),
          # legend.margin = margin(0,0,0,0),
          # hide the minor grid lines
          panel.grid.minor = element_blank(),
          # hide vertical major grid lines
          panel.grid.major.y = element_blank(),
          axis.title = element_blank(),
          axis.title.y.left = element_blank(),
          axis.ticks.y.left = element_blank(),
          axis.text.y.left = element_blank(),
          axis.line.y.left = element_blank(),
          axis.title.y.right = element_blank(),
          axis.title.x.bottom = element_blank(),
          strip.text.y.left = element_text(size = 12*1.05, 
                                           margin = margin(0,0,0,0)),
          strip.text.y.right = element_text(size = 12*1.05, 
                                            margin = margin(0,0,0,0)),
          panel.spacing.y = unit(0, "null"),
          # axis.text.x = element_blank(),
          axis.line = element_blank())
  
  # Combine the two
  lockdown_x_cases = cases_viz / lockdown_viz + 
    plot_layout(heights = c(7,3))+
    plot_annotation(theme = theme_mark(md=T, plot_margin = margin(25, 40, 0, 60)), 
                    title = country,#"Cough and response", #str_c("Daily COVID-19 cases ", country),
                    subtitle = "New confirmed COVID-19 cases (rolling seven-day avg.) and implementation of lockdown policies",
                    caption = source_caption(sources = c("Johns Hopkins University CSSE", "Oxford University (OxCGRT)")))
  if(save){
    ggsave(plot = lockdown_x_cases, 
           filename = here("plots", "coviz", str_c("lockdown_cases_", country, ".png")), 
           device = "png", width = 12, height = 9)
  }
  
  
  lockdown_x_cases
}

compare_cases_to_policies(country = "Italy", save=F)







# Other Visualizations ----------------------------------------------------


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


