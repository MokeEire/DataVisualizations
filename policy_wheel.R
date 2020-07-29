## trying out ggwaffle
library(tidyverse)
library(lubridate)
library(readxl)
library(here)
library(janitor)
library(extrafont)
library(gt)

## We want:
# Columns = States
# Rows = Policies
# Fill = Policies in place

## Create generic dataframe of states, this actually limits each state to only having three policies from the potential 5
policies = tibble(
  states = rep(1:50, 3),
  policy = sample(1:5, 150, replace = T)
)
  
policies %>% 
  ggplot(.)+
  geom_bar(aes(x=states, y = policy, fill = factor(policy)), stat = "identity", position = "stack")+
  coord_polar()+
  theme_minimal()+
  theme(axis.text.x = element_text())
## This kind of does what we want, what's missing?
#     - We want all factor levels appearing for each bar
#     - We want each bar to be the same height

# Try a diff approach
# Create an ordered factor using policies in figure
pols = factor(c("Allows Third Party Prescriptions", "Prescriber Civil Liability Protections", 
                "Prescriber Criminal Liability Protections",  "Lay Administration Civil Liability Protections", 
                "Lay Administration Criminal Liability Protections", "Controlled Substance/Paraphernalia Possession"), 
              levels = c("Allows Third Party Prescriptions", "Prescriber Civil Liability Protections", 
                         "Prescriber Criminal Liability Protections",  "Lay Administration Civil Liability Protections", 
                         "Lay Administration Criminal Liability Protections", "Controlled Substance/Paraphernalia Possession"), ordered=T)
# Relabel factors using str_wrap so they can fit in plot
pols = fct_relabel(pols, str_wrap, 40)
# Reverse factor levels because plotting often switches them
pols = fct_rev(pols)

# Create dataframe of states and their corresponding region (I think...whether the regions are correct is unclear)
state_df = tibble(states = state.abb, region = state.region)

# Get all combinations of states and policies with expand.grid
pol_df = expand.grid(states = state.abb, policies = pols) %>% 
  # Use count to get a vector of 1s
  count(states, policies) %>% 
  # Join in region variable
  left_join(state_df, by = "states") %>% 
  # Use bernoulli dist to select which policies are present in each state
  # Replace present variable with the policy if it is present in a state
  mutate(present = rbernoulli(n()),
         present = if_else(present, policies, NA_integer_)) %>% 
  # Arrange states in order of region and set them as a factor (ggplot doesn't care about df order unless it's a factor)
  arrange(region) %>% 
  mutate(states = fct_inorder(states))



pol_df %>% 
  ggplot(., aes(width = 1))+
  geom_bar(aes(x=states, y = n, fill = factor(present), group = policies), lwd=.5, colour="white", stat = "identity", position = "stack")+
  scale_y_discrete(expand = expand_scale(mult = c(1.1, -0.1)))+
  scale_fill_brewer(direction = -1, type = "qual", palette = "Accent", na.value = "gray84", breaks = pols, 
                    guide = guide_legend(title = "Enacted Legislation", reverse = T, keywidth = 2, keyheight = 3))+
  coord_polar(start = 2.25)+
  geom_segment(aes(x = 0, y = 0, yend = 9, xend = 0, group = policies))+
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_blank(), legend.position = c(-0.2,.75),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.grid = element_blank())



## Can this visualization be used with the Firearm Law Database?
fld_file = here("..", "..", "Datasets",  "TL-283-1 State Firearm Law Database 2.1.xlsx")
excel_sheets(fld_file)
sfld = read_excel(fld_file, sheet = "Database", na = c("N/A") )

# Dimensions of file
dim(sfld)
print(object.size(sfld),units="auto")
# names(sfld) = tolower(make.names(names(sfld), unique = T)) This is what janitor is for!
sfld = clean_names(sfld)
glimpse(sfld)

count(sfld, law_class)
count(sfld, law_class_subtype)


# What are different waiting periods?
sfld %>% 
  count(length_of_waiting_period_days_handguns)

sfld=sfld %>% 
  mutate(length_of_waiting_period_days_handguns = as.numeric(str_remove_all(length_of_waiting_period_days_handguns, "[:space:]days"))) %>% 
  mutate_at(vars(effective_date, supercession_date), ymd)

sfld %>% 
  count(law_class)

sfld %>% 
  filter(grepl("Open Carry", law.type, ignore.case = T)) %>% 
  arrange(effective_date)


# Background Checks -------------------------------------------------------

bg_checks = sfld %>% 
  filter(law_class == "background checks") %>% 
  arrange(effective_date)

bg_checks_handgun = bg_checks %>% 
  filter(str_detect(handguns_or_long_guns, "handgun"))

bg_checks_longgun = bg_checks %>% 
  filter(str_detect(handguns_or_long_guns, "long"))


bg_checks %>% 
  count(law_class, law_class_subtype)


policy_wheel = function(df, filename, title, subtitle, source, device = "png"){
  legend_items = levels(df$law_class_subtype)
  plot_title = if_else(missing(title), "", title)
  plot_subtitle = if_else(missing(subtitle), "", subtitle)
  plot_caption = if_else(missingArg(source), "", str_c("Source: ", source))
  
  df %>% 
    ggplot(., aes(x=states, y = n, fill = present, group = law_class_subtype, width = 1))+
    geom_bar(lwd=1, colour="white", stat = "identity", position = "stack")+
    # Scales
    scale_x_discrete(levels(.data$states))+
    scale_y_discrete(expand = expansion(mult = c(1.25, 0)), limits = restrictiveness_order)+
    scale_fill_manual(values = colours, na.value = "#CCCCCC", breaks = legend_items,
                      guide = guide_legend(title = "Law", reverse = F, keywidth = 1.75, keyheight = 2))+
    coord_polar(start=17/57.296, clip = "off")+
    # Annotations
    # Region split lines
    annotate("segment", x = 0.5, xend = 0.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
    annotate("segment", x = 9.5, xend = 9.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
    annotate("segment", x = 25.5, xend = 25.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
    annotate("segment", x = 35.5, xend = 35.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
    labs(title = plot_title,
         subtitle = plot_subtitle,
         caption = plot_caption)+
    theme_minimal(base_family = "Inter", base_size = 12)+
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(margin = margin(b = -10), colour = "#000614"),
          panel.grid = element_blank(),
          legend.text = element_text(colour = "#000614"),
          plot.caption = element_text(hjust = 0, margin = margin(l = 80), colour = "#181A1B"),
          plot.title = element_text(size = rel(1.5), hjust = 0, margin = margin(t = 20, b = 5), colour = "#000614"),
          plot.subtitle = ggtext::element_markdown(colour = "#000614"))
  
  output_file = str_c(if_else(missingArg(filename), "plot", filename), ".", device)
  
  ggsave(
    filename = output_file,
    plot = last_plot(),
    device = device,
    path = here(),
    dpi = "retina"
  )
}



# Questions:
#   1. What policies are implemented across the nation?
#   2. Which states are most similar to each other? Is there a reason for this?
#   3. How do the policies vary for handguns or long guns?
#   4. 

bg_checks_handgun_current = bg_checks_handgun %>% 
  filter(is.na(supercession_date), 
         # Remove rows where the content describes the lack of a law
         str_detect(content, "No law requiring background checks|No law restricting", negate=T),
         law_id != "DE1019") %>% 
  # Check if law was repealed
  group_by(state) %>% 
  mutate(repealed = (type_of_change == "Repeal")&(effective_date == max(effective_date)),
         dealer_sales_repealed = any(repealed)&str_detect(law_class_subtype, "sales from dealer"),
         # Convert subtypes to prettier names
         law_class_subtype = str_replace_all(law_class_subtype, c("permit to purchase" = "Background checks for permits to purchase", 
                                                                  "sales from dealer" = "Background checks for sales from dealers", 
                                                                  "^private sales(\\soptional)?" = "Background checks for private sales", 
                                                                  "extra time" = "Extra time/waiting period"))) %>% 
  # Remove repealed laws and federal laws
  filter(!dealer_sales_repealed, 
         str_detect(law_class_subtype, "Brady|federal", negate = T),
         type_of_change != "Repeal") %>% 
  ungroup() %>% 
  arrange(state, law_class_subtype, type_of_change, effective_date)

# TODO: double check this logic.  It is removing the cases I want to remove, but it doesn't look airtight

bg_checks_handgun_current %>% count(law_class_subtype)

# Select background checks which have not been superceded
current_bg_check_laws = bg_checks %>% 
  filter(is.na(supercession_date), 
         # Remove rows where the content describes the lack of a law
         str_detect(content, "No law requiring background checks|No law restricting", negate=T),
         # Remove rows where the type of change is a repeal
         type_of_change != "Repeal",
         law_id != "DE1019") %>% 
  mutate(law_class_subtype = str_replace_all(law_class_subtype, c("permit to purchase" = "Background checks for permits to purchase", 
                                                                  "sales from dealer" = "Background checks for sales from dealers", 
                                                                  "^private sales(\\soptional)?" = "Background checks for private sales", 
                                                                  "extra time" = "Extra time/waiting period"))) %>% 
  ungroup() %>% 
  arrange(handguns_or_long_guns, state)

restrictiveness_order = c("Background checks for permits to purchase",
                          "Background checks for private sales", 
                          "Background checks for sales from dealers", 
                          "Extra time/waiting period")


# Handguns ----------------------------------------------------------------



handgun_pols = bg_checks_handgun_current  %>% 
  # Identify the types of background check policies exist for handguns
  distinct(law_class_subtype, handguns_or_long_guns) %>% 
  pull(law_class_subtype)
                  
#gun_pols = fct_rev(gun_pols)
handgun_df = bg_checks_handgun_current %>% 
  count(state_postal_abbreviation, law_class_subtype)

# Create dataframe of all possible policies and join in existing policies
handgun_pol_df = expand_grid(states = state.abb, 
                         law_class_subtype = handgun_pols) %>% 
  left_join(handgun_df, 
            by = c("states" = "state_postal_abbreviation", "law_class_subtype")) %>% 
  mutate(present = if_else(!is.na(n), law_class_subtype, NA_character_),
         n = 1) %>% 
  add_count(present, name = "present_count") %>% 
  mutate(present_count = na_if(present_count, 131),
         law_class_subtype = factor(law_class_subtype, levels = restrictiveness_order, ordered = T)) %>% 
  left_join(state_df, by = "states") %>% 
  arrange(region) %>% 
  mutate(states = fct_inorder(states))
#gun_pol_df$law.type = fct_rev(gun_pol_df$law.type)



colours = c("#001b62","#194ccc","#288ce2","#6cc0ff")

names(colours) =  c("Background checks for permits to purchase",
                    "Background checks for private sales", 
                    "Background checks for sales from dealers", 
                    "Extra time/waiting period")

policy_wheel(df = handgun_pol_df, 
             filename = "handgun_policies", 
             title = "What kinds of background check laws do states have?", 
             source = "RAND State Firearm Law Database")


no_bg = handgun_pol_df %>% 
  group_by(states) %>% 
  summarise(no_laws = all(is.na(present))) %>% 
  pull(no_laws) %>% sum()

handgun_pol_df %>% 
  # Add in factor levels for the restrictiveness label and arrow
  # mutate(states = fct_relevel(fct_expand(states, c("label","arrow")), c("label","arrow"), after = 12)) %>%
  ggplot(., aes(x=states, y = n, fill = present, group = law_class_subtype, width = 1))+
  # Create squares
  geom_bar(lwd=1, colour="white", stat = "identity", position = "stack")+
  # Remove restrictiveness labels
  scale_x_discrete(levels(.data$states), labels = function(breaks) str_remove(breaks,pattern="arrow|label"), drop=F)+
  # Expand circle's limits
  scale_y_discrete(expand = expansion(mult = c(1.25, 0)), limits = restrictiveness_order)+
  # Fill the squares manually
  scale_fill_manual(values = colours, na.value = "#CCCCCC", breaks = levels(handgun_pol_df$law_class_subtype),
                    guide = guide_legend(title = "Law", reverse = F, keywidth = 1.75, keyheight = 2))+
  # Magnitude annotation
  # annotate("segment", x = 14.4, xend=14.1, y = 0.25, yend = 4, colour = "#333333", arrow = arrow(type = "closed", length = unit(.1, "inches")))+
  # annotate("text", x = 13, y = 2.15, label = "More Restrictive", colour = "#333333", vjust = 1, angle = 0)+
  # Region split lines
  annotate("segment", x = 0.5, xend = 0.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
  annotate("segment", x = 9.5, xend = 9.5, y = -.5, yend = 4, colour = "#333333", size = 1)+
  annotate("segment", x = 25.5, xend = 25.5, y = -1, yend = 4, colour = "#333333", size = 1)+
  annotate("segment", x = 35.5, xend = 35.5, y = -1, yend = 4, colour = "#333333", size = 1)+
  # Region names
  # annotate("text", x = 5, y = -.5, label = "Northeast", angle = 315)+
  # annotate("text", x = 18, y = -.5, label = "South", angle = 40)+
  # annotate("text", x = 30, y = -.75, label = "North\nCentral", angle = 315)+
  # annotate("text", x = 43, y = -.5, label = "West", angle = 40)+
  # annotation_custom(ne, xmin = 4.5, xmax = 5.5, ymin = 7, ymax = 7)+
  coord_polar(start=17/57.296, clip = "off")+
  labs(title = paste("Can anyone buy a handgun?"),
       subtitle = str_c("**",(no_bg),"**"," states have **no background check laws** in place for any point of purchase for handguns."),
       caption = "RAND State Firearm Law Database")+
  theme_minimal(base_family = "Inter", base_size = 12)+
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(margin = margin(b = -10, t = 20), colour = "#000614"),
        panel.grid = element_blank(),
        legend.text = element_text(colour = "#000614"),
        plot.caption = element_text(hjust = 0, margin = margin(l = 80), colour = "#181A1B"),
        plot.title = element_text(size = rel(1.75), hjust = 0, margin = margin(t = 20, b = 5), colour = "#000614"),
        plot.subtitle = ggtext::element_markdown(colour = "#000614"))


policy_wheel(handgun_pol_df, 
             filename = "handgun_bg",
             title = "(Not so) universal background checks",
             subtitle = str_c("**",no_bg,"**", " ", "states have **no background check laws** in place for any point of purchase for handguns."),
             source = "RAND State Firearm Law Database",
             device = "svg")

# Long Guns ---------------------------------------------------------------




longgun_pols = current_bg_check_laws %>% 
  count(law_class_subtype, handguns_or_long_guns) %>% 
  filter(str_detect(handguns_or_long_guns, "long gun"), 
         str_detect(law_class_subtype, "Brady|federal", negate = T)) %>% 
  pull(law_class_subtype)

longgun_df = current_bg_check_laws %>% 
  filter(str_detect(handguns_or_long_guns, "long gun")) %>% 
  count(state_postal_abbreviation, law_class_subtype)


longgun_pol_df = expand_grid(states = state.abb, 
                             law_class_subtype = longgun_pols) %>% 
  left_join(longgun_df, 
            by = c("states" = "state_postal_abbreviation", "law_class_subtype")) %>% 
  mutate(present = if_else(!is.na(n), law_class_subtype, NA_character_),
         n = 1) %>% 
  add_count(present, name = "present_count") %>% 
  mutate(present_count = na_if(present_count, 131),
         law_class_subtype = fct_rev(fct_reorder(law_class_subtype, present_count))) %>% 
  left_join(state_df, by = "states") %>% 
  arrange(region) %>% 
  mutate(states = fct_inorder(states))

longgun_pol_df %>% 
  ggplot(., aes(width = 1))+
  geom_bar(aes(x=states, y = n, fill = present, group = law_class_subtype), lwd=1, colour="white", stat = "identity", position = "stack")+
  scale_y_discrete(expand = expansion(mult = c(1.1, -0.1)))+
  scale_fill_manual(values = colours, na.value = "gray84", breaks = levels(longgun_pol_df$law_class_subtype),
                    guide = guide_legend(title = "Background Check Law", reverse = F, keywidth = 1.75, keyheight = 2))+
  coord_polar()+
  labs(title = "States' background check policies for long guns",
       caption = "Source: RAND State Firearm Law Database")+
  theme_minimal(base_family = "Inter", base_size = 14)+
  theme(axis.text.y = element_blank(), 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        # legend.position = "left",
        # legend.position = c(-.35,.6),
        # legend.title = element_text(size = 13),
        # legend.text = element_text(size = 11),
        panel.grid = element_blank(),
        plot.title = element_text(size = rel(2.5), hjust = 0, margin = margin(t = 20)))



ggsave(filename = "bgchecks_plot.png", width = 8, height = 4.5, units = "cm", scale = 3)