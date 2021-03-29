# Make a nicer looking graph for Seattle's Homelessness page
# https://en.wikipedia.org/wiki/Homelessness_in_Seattle
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




# Data used in current chart ---------------------------------------------------------

dat = tribble(
  ~Year,~Total,~Unsheltered,#~KCPop,
  2006,7910,1946,#1832.259,
  2007,7839,2159,#1857.877,
  2008,8439,2631,#1885.368,
  2009,8916,2827,#1922.645,
  2010,8981,2759,#1938.351,
  2011,8922,2442,#1974.2,
  2012,8875,2594,#2011.197,
  2013,9106,2736,#2047.223,
  2014,9294,3123,#2085.225,
  2015,10091,3772,#2126.178,
  2016,10730,4505,#2166.35,
  2017,11643,5485,#2203.836,
  2018,12112,6320,#2228.364,
  2019,11199,5228,#2252.782,
  2020,11751,5578,#2277.2
)

# Compute Sheltered homeless population from Total minus Unsheltered
dat = dat %>% 
  mutate(Year, Sheltered = Total - Unsheltered, 
         .before = "Unsheltered")




ndat = dat %>% 
  pivot_longer(!Year, names_to="Homeless", values_to="Count") %>% 
  filter(Homeless != "Total")


plot = ggplot(ndat, aes(x=Year, y=Count, fill=Homeless)) + 
  geom_col(position = position_stack()) + 
  # Axes
  scale_x_continuous(breaks=scales::breaks_pretty(n = 7)) + 
  scale_y_continuous(breaks=scales::breaks_pretty(), 
                     labels = scales::comma,
                     expand = expansion())+
  # Colour scales
  scale_fill_manual(values = c("Sheltered" = "#08488C", "Unsheltered" = "#51A9C2"))+
  scale_colour_manual(values = c("Sheltered" = "#08488C", "Unsheltered" = "#51A9C2"))+
  # Labels
  labs(y = "Homeless Count", x = "",
       title = "Seattle Homeless Count 2006-2020",
       subtitle = "Count of sheltered and unsheltered population")+
  # Theme
  theme_wiki(md=T, plots_pane = F, base_size = 16)+
  theme(legend.position = c(.025, 1),
        legend.title = element_blank(),
        legend.justification = c("left", "top"),
        legend.box.just = "left")

plot

ggsave(plot, file="wikipedia/SeattleHomelessnessBarChart.svg", dev = "svg", width = 16, height = 8)


