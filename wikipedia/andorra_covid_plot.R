# Make a better graph for Andorra's COVID page
# https://en.wikipedia.org/wiki/COVID-19_pandemic_in_Andorra#Statistics[35]
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


jhu_source = "2019 Novel Coronavirus Data Repository by Johns Hopkins CSSE"
oxford_source = "Oxford COVID-19 Government Response Tracker, Blavatnik School of Government"



# COVID Case Data ---------------------------------------------------------


folder_url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/"

# files = c("confirmed", "recovered", "deaths")


# Because each file is in the folder, paste together the filenames
case_urls = str_c(folder_url, 
                  # e.g. time_series_covid19_confirmed_global.csv
                  "time_series_covid19_", "confirmed", "_global.csv") %>% 
  # Setting names so we can map over each file later
  set_names("confirmed")

covid_data = imap_dfr(case_urls, 
                      ~read_csv(file = .x), 
                      .id = "case_type") %>% 
  select(`Province/State`, `Country/Region`, case_type, everything()) %>% 
  filter(`Country/Region` == "Andorra")

covid_pivot = covid_data %>% 
  pivot_longer(cols = c(-1:-5), 
               names_to = "date", values_to = "total_cases",
               names_transform = list(date = mdy)) %>% 
  clean_names()

monthly_labels = covid_pivot %>% 
  group_by(month(date)) %>% 
  filter(date == max(date))

quarterly_labels = covid_pivot %>% 
  group_by(quarter(date, with_year = T)) %>% 
  filter(date == max(date))

line_graph = covid_pivot %>% 
  ggplot(., aes(x = date, y = total_cases))+
  geom_line(colour = "#1f77b4", size = 1.3)+
  geom_text(data = quarterly_labels, 
            aes(label = scales::comma(total_cases)), 
            size = 6, colour ="#202122",
            family = "Arial",nudge_x = -9, nudge_y = 280)+
  scale_y_continuous(labels = scales::comma, 
                     expand = expansion(add = c(25,0)))+
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y", 
               expand = expansion())+
  coord_cartesian(xlim = range(covid_pivot$date), 
                  ylim = range(covid_pivot$total_cases), 
                  clip = "off")+
  labs(x = NULL, y = "No. of confirmed cases",
       title = "COVID-19 cases in Andorra", subtitle = str_c("Source: Johns Hopkins CSSE (as of ", stamp_date("Oct 20 1979")(today()), ")"))+
  theme_wiki(md=T, base_size = 16, plot_margin = margin(20,80,50,50))

ggsave(line_graph, filename = here("plots", "wiki_covid_andorra.png"), dev = "png", width = 16, height = 8)
