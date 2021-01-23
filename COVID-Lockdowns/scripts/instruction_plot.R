# Make instructional plot
library(extrafont)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(here)

source(here("COVID-Lockdowns", "scripts","theme_mark.R"))


policy_dict = c("c1" = "School closing",
                "c2" = "Workplace closing",
                "c3" = "Cancel public events",
                "c4" = "Restrictions on gatherings",
                "c5" = "Close public transport",
                "c6" = "Stay at home requirements",
                "c7" = "Restrictions on internal movement",
                "c8" = "International travel controls")


# Set input parameters ----------------------------------------------------

input_country = "Ireland"

input_case_type = c("Confirmed", "Deaths")


# Case data ---------------------------------------------------------------
load(here("COVID-Lockdowns", "covid_country_level.RData"))

country_data = covid_country_level[covid_country_level$country_region == input_country & covid_country_level$case_type %in% tolower(input_case_type), ]

policy_input = list(
  c1 = 2, 
  c2 = 2,
  c3 = 2,
  c4 = 2,
  c5 = 1,
  c6 = 2,
  c7 = 2,
  c8 = 3
)
# Policy data -------------------------------------------------------------
load(here("COVID-Lockdowns", "covid_policies.RData"))

lockdown_data = covid_policies[covid_policies$country_name == input_country & covid_policies$date %in% unique(country_data$date), ] %>% 
  mutate(
    # Create boolean values for whether a lockdown was enacted
    c1_lockdown = (c1_response >= policy_input$c1),
    c2_lockdown = (c2_response >= policy_input$c2),
    c3_lockdown = (c3_response >= policy_input$c3),
    c4_lockdown = (c4_response >= policy_input$c4),
    c5_lockdown = (c5_response >= policy_input$c5),
    c6_lockdown = (c6_response >= policy_input$c6),
    c7_lockdown = (c7_response >= policy_input$c7),
    c8_lockdown = (c8_response >= policy_input$c8)) %>% 
  # pivot to policy, response, flag
  pivot_longer(!c(country_name, date), names_to = c("policy", ".value"), names_sep = "_") %>% 
  # provide policy labels
  mutate(policy = factor(str_replace_all(policy, policy_dict),
                         levels = c("School closing", 
                                    "Workplace closing", 
                                    "Cancel public events",
                                    "Restrictions on gatherings", 
                                    "Close public transport", 
                                    "Stay at home requirements", 
                                    "Restrictions on internal movement",
                                    "International travel controls"), 
                         ordered = T),
         # Add factor labels to flag and make policy a factor
         lockdown_val = na_if(lockdown*as.numeric(policy), 0)) %>% 
  replace_na(list(flag = "National"))



# Plotting utilities ------------------------------------------------------

global_date_labels = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-01"), by = "1 month")
case_pal = set_names(viz_colours[c(3,7,6)], unique(covid_country_level$case_type))



# Cases plot --------------------------------------------------------------

multiple_cases = (length(input_case_type) > 1)

cases_plot = country_data %>% 
  # Plot
  ggplot(., aes(x = date, y = roll_avg_7day, fill = case_type))+
  # Geom
  geom_area(alpha = .85, colour = my_col_pal[3], position = "identity")+
  # Axes
  scale_x_date(breaks = global_date_ticks, 
               date_labels = "%b", 
               position = "top")+
  scale_y_continuous(breaks = scales::pretty_breaks(), 
                     labels = scales::comma, 
                     position = "left", expand = expansion(add = c(0,50)))

if(multiple_cases){
  cases_plot = cases_plot+
    scale_fill_manual(values = case_pal, labels = str_to_title, name = "Case Type",
                      guide = guide_legend(ncol = 1))
} else {
  cases_plot = cases_plot+
    scale_fill_manual(values = case_pal, guide = guide_none())
}

cases_plot = cases_plot+
  # Theme
  theme_mark(md=T, base_size = 12, plot_margin = margin(0,0,5,0))+
  theme(legend.position = c(1.1, .5),
        legend.background = element_rect(fill = "#F9FAFA", colour = "#E1EAE9"),
        legend.margin = margin(10, 10, 10, 10),
        legend.key = element_rect(size = 10, colour = NA, fill = my_col_pal[1]),
        legend.key.height = unit(.6, "cm"),
        legend.title = ggtext::element_markdown(colour = "#2C3535", hjust = 0),
        legend.text = ggtext::element_markdown(colour = my_col_pal[3], vjust = .5), 
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
        axis.text.y.left = element_text(vjust = 0))


# Policy plot -------------------------------------------------------------

policy_plot = lockdown_data %>% 
  ggplot(., aes(x = date, y = lockdown_val, colour = policy, group = flag))+
  # Geom
  geom_line(aes(linetype = flag, size = flag), alpha = .95)+
  # Axes
  scale_x_date(breaks = global_date_ticks, 
               date_labels = "%b", 
               position = "bottom")+
  # Colour, linetype, size
  scale_colour_manual(values = viz_colours, na.value = my_col_pal[1], 
                      name = NULL, guide = guide_none())+
  scale_linetype_manual(values = c("Targeted" = "dashed", "National" = "solid"), 
                        na.translate = F,
                        guide = guide_legend(title = "Geographical scope",
                                             override.aes = list(size = c(1.75, .8)),
                                             keywidth = unit(.9, "cm"),
                                             label.position = "left", reverse = T, label.hjust = 1, title.hjust = -.25))+
  scale_size_manual(values = c("Targeted" = 1, "National" = 2), na.value = 2, guide = guide_none())+
  # Facets
  facet_wrap(~policy, 
             ncol = 1, strip.position = "right", 
             scales = "free_y", shrink=F)+
  # Theme
  theme_mark(md=T, base_size = 12, plot_margin = margin(0,0,0,0))+
  theme(legend.position = c(-.05,0.5),
        legend.margin = margin(10, 10, 10, 10),
        legend.spacing = unit(0.5, "cm"),
        legend.background = element_rect(fill = "#F9FAFA", colour = "#E1EAE9"),
        legend.title = ggtext::element_markdown(colour = "#2C3535", hjust = -.25),
        legend.text = ggtext::element_markdown(colour = my_col_pal[3]), 
        # hide the minor grid lines
        panel.grid.minor = element_blank(),
        # hide vertical major grid lines
        panel.grid.major.y = element_blank(),
        axis.title = element_blank(),
        axis.title.y.left = element_blank(),
        axis.ticks.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.text.x.bottom = element_text(margin = margin(b = 5)),
        axis.line.y.left = element_blank(),
        axis.title.y.right = element_blank(),
        axis.title.x.bottom = element_blank(),
        strip.text.y.left = element_text(size = 12*1.05, 
                                         margin = margin(0,0,0,0)),
        strip.text.y.right = element_text(size = 12*1.05, 
                                          margin = margin(0,0,0,0)),
        panel.spacing.y = unit(0, "null"),
        axis.line = element_blank())


# Patchwork ---------------------------------------------------------------

case_label = if_else(length(input_case_type) == 3, "All", tolower(str_c(input_case_type, collapse = " & ")))

cases_plot / policy_plot + 
  plot_layout(heights = c(7,3))+
  plot_annotation(theme = theme_mark(md=T, plot_margin = margin(10, 15, 10, 60)))

ggsave(here("plots", "coviz", "instruction_plot_base.svg"), device = "svg", width = 13.5, height = 7.75)
