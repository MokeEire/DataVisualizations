#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(extrafont)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(here)

here("scripts", c("theme_mark.R", "load_coviz_data.R")) %>% 
    walk(source)

# load("covid_policies.RData")
# load("covid_country_level.RData")

policy_dict = c("c1" = "School closing",
                "c2" = "Workplace closing",
                "c3" = "Cancel public events",
                "c4" = "Restrictions on gatherings",
                "c5" = "Close public transport",
                "c6" = "Stay at home requirements",
                "c7" = "Restrictions on internal movement",
                "c8" = "International travel controls")
# global_date_labels = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-15"), by = "1 month")
# 
# global_date_ticks = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-01"), by = "1 month")
# case_pal = set_names(viz_colours[c(3,7,6)], unique(covid_country_level$case_type))




pickerInput2 <- function (inputId, label = NULL, choices, selected = NULL, multiple = FALSE, 
                          options = list(), choicesOpt = NULL, width = NULL, inline = FALSE, ratio = c(2,10)) 
{
    if (ratio[1] + ratio[2] != 12) stop("`ratio` has to add up 12.")
    choices <- shinyWidgets:::choicesWithNames(choices)
    selected <- restoreInput(id = inputId, default = selected)
    if (!is.null(options) && length(options) > 0) 
        names(options) <- paste("data", names(options), sep = "-")
    if (!is.null(width)) 
        options <- c(options, list(`data-width` = width))
    if (!is.null(width) && width %in% c("fit")) 
        width <- NULL
    options <- lapply(options, function(x) {
        if (identical(x, TRUE)) 
            "true"
        else if (identical(x, FALSE)) 
            "false"
        else x
    })
    maxOptGroup <- options[["data-max-options-group"]]
    selectTag <- tag("select", shinyWidgets:::dropNulls(options))
    selectTag <- tagAppendAttributes(tag = selectTag, id = inputId, 
                                     class = "selectpicker form-control")
    selectTag <- tagAppendChildren(tag = selectTag, shinyWidgets:::pickerSelectOptions(choices, 
                                                                                       selected, choicesOpt, maxOptGroup))
    if (multiple) 
        selectTag$attribs$multiple <- "multiple"
    divClass <- "form-group shiny-input-container"
    labelClass <- "control-label"
    if (inline) {
        divClass <- paste(divClass, "form-horizontal")
        selectTag <- tags$div(class = paste0("col-sm-", ratio[2]), selectTag)
        labelClass <- paste(labelClass, paste0("col-sm-", ratio[1]))
    }
    pickerTag <- tags$div(class = divClass, style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), if (!is.null(label)) 
            tags$label(class = labelClass, `for` = inputId, label), 
        selectTag)
    shinyWidgets:::attachShinyWidgetsDep(pickerTag, "picker")
}

ui <- navbarPage(selected = "National Level",
    theme = "app.css",

    # tags$head(
    #     tags$style(HTML("
    #   .form-group {
    #            display: flex;
    #            }
    # 
    # "))),
    
    # App title ----
    title = "COVID Lockdown Policies",
    
    
               tabPanel("National Level",
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                   
                   # Sidebar panel for inputs ----
                   sidebarPanel(
                     
                     HTML(str_c("<p style='color: var(--bg-col); font-size: 14px;'>Explore the timeline of nation's reported COVID cases and government containment policies (\"lockdowns\") using the ", 
                                "<a style='color: var(--bg-col);' href = 'https://github.com/CSSEGISandData/COVID-19'>COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University</a>.", 
                                " and the ", 
                                "<a style='color: var(--bg-col);' href ='https://github.com/OxCGRT/covid-policy-tracker'>Oxford Covid-19 Government Response Tracker (OxCGRT)</a>.</p>",
                                "<p style='color: var(--bg-col); font-size: 14px;'>The past year of uncertainty has likely made this obvious, but it's still important to say that you can't draw any causal relationships from these visualizations i.e. they won't tell you if closing schools stops the spread of COVID.",
                                "  If you look at a few different countries, you'll notice that the effects of introducing or lifting policies vary by country, time period, and a whole host of other factors.  ",
                                "I hope they will stimulate curiosity and make people ask questions.</p>")),
                     # tagList(
                     #     # p(style="color: var(--bg-col); font-size: 14px;",
                     #     #     "Compare COVID cases over time to the government containment policies implemented in the country."
                     #     # ), 
                     #     p(style="color: var(--bg-col); font-size: 14px;",
                     #         , a("COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University", href = 'https://github.com/CSSEGISandData/COVID-19', style="color: var(--bg-col);",), " and the ", a("Oxford Covid-19 Government Response Tracker (OxCGRT)", href = 'https://github.com/OxCGRT/covid-policy-tracker', style="color: var(--bg-col);"), "."),
                     #     p(style="color: var(--bg-col); font-size: 14px;",
                     #       str_c())
                     # ),
                     # Country and case type dropdowns
                     # Input: Slider for the number of bins ----
                     fluidRow(
                       column(6,
                              selectizeInput("country", #width = "250px", ratio = c(5,7),
                                          label = "Country", choices = sort(unique(covid_policies$country_name)), selected = "Ireland")
                              ),
                       column(6,
                              selectizeInput("case_type", width = "250px", #ratio = c(5,7),
                                          label = "Case Type", 
                                          choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed")
                              )
                     )
                     ,
                     # Input: Slider for the number of bins ----
                     hr(),
                     
                     h4("What constitutes a lockdown?", style="color: var(--bg-col);"),
                     
                     # TODO: Make these sliders somehow
                     # Policy options
                     fluidRow(
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c1",
                                label = "School closure",
                                choices = c("1 - Recommend closing or all schools open with alterations resulting in significant differences compared to non-COVID-19 operations" = 1,
                                            "2 - Require closing (only some levels or categories, e.g. just high school, or just public schools)" = 2,
                                            "3 - Require closing all levels" = 3),
                                selected = 2,
                                # inline=T
                              )
                       ),
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c2",
                                label = "Workplace closure", 
                                choices = c("1 - Recommend closing (or recommend work from home)" = 1, 
                                            "2 - Require closing (or work from home) for some sectors or categories of workers" = 2, 
                                            "3 - Require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)" = 3),
                                selected = 2,
                                # inline=T
                              )
                       )

                     ),
                     fluidRow(
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c3",
                                label = "Cancel public events", 
                                choices = c("1 - Recommend cancelling" = 1, 
                                            "2 - Require cancelling" = 2),
                                selected = 2,
                                # inline=T
                              )
                       ),
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c4",
                                label = "Restrictions on gatherings", 
                                choices = c("1 - Restrictions on very large gatherings (the limit is above 1000 people)" = 1, 
                                            "2 - Restrictions on gatherings between 101-1000 people" = 2, 
                                            "3 - Restrictions on gatherings between 11-100 people" = 3,
                                            "4 - Restrictions on gatherings of 10 people or fewer" = 4),
                                selected = 1,
                                # inline=T
                              )
                       )

                     ),
                     fluidRow(
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c5",
                                label = "Close public transport", 
                                choices = c("1 - Recommend closing (or significantly reduce volume/route/means of transport available)" = 1, 
                                            "2 - Require closing (or prohibit most citizens from using it)" = 2),
                                selected = 2,
                                # inline=T
                              )
                              ),
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c6",
                                label = "Stay at home requirements", 
                                choices = c("1 - Recommend not leaving house" = 1, 
                                            "2 - Require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips" = 2, 
                                            "3 - Require not leaving house with minimal exceptions (e.g. allowed to leave once a week, or only one person can leave at a time, etc.)" = 3),
                                selected = 2,
                                # inline=T
                              )
                              )

                     ),
                     fluidRow(
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c7",
                                label = "Restrictions on internal movement", 
                                choices = c("1 - Recommend not to travel between regions/cities" = 1, 
                                            "2 - Internal movement restrictions in place" = 2),
                                selected = 2,
                                # inline=T
                              )
                              ),
                       column(6,
                              selectizeInput(#ratio = c(4,8), width = "100%",
                                inputId = "c8",
                                label = "International travel control", 
                                choices = c("1 - Screening arrivals" = 1, 
                                            "2 - Quarantine arrivals from some or all regions" = 2, 
                                            "3 - Ban arrivals from some regions" = 3,
                                            "4 - Ban on all regions or total border closure" = 4),
                                selected = 3,
                                # inline=T
                              )
                              )

                     )

                     
                   ),
                   # Main panel for displaying outputs ----
                   mainPanel(
                     
                     # Output: Histogram ----
                     withSpinner(plotOutput(outputId = "whole_plot", height = "700px")),
                     uiOutput("caption")
                     
                   )
               )
    )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    
    # Histogram of the Old Faithful Geyser Data ----
    # with requested number of bins
    # This expression that generates a histogram is wrapped in a call
    # to renderPlot to indicate that:
    #
    # 1. It is "reactive" and therefore should be automatically
    #    re-executed when inputs (input$bins) change
    # 2. Its output type is a plot
    

    country_data = reactive({
        covid_country_level[covid_country_level$country_region == input$country & covid_country_level$case_type == tolower(input$case_type), ]
    })
    
    lockdown_data = reactive({
        policy_data = covid_policies %>% 
            mutate(# Create boolean values for whether a lockdown was enacted
                ################## IMPORTANT:  ############################
                # I specify a policy as a lockdown when the policy is required rather than recommended
                c1_lockdown = (c1_response >= input$c1),
                c2_lockdown = (c2_response >= input$c2),
                c3_lockdown = (c3_response >= input$c3),
                c4_lockdown = (c4_response >= input$c4),
                c5_lockdown = (c5_response >= input$c5),
                c6_lockdown = (c6_response >= input$c6),
                c7_lockdown = (c7_response >= input$c7),
                c8_lockdown = (c8_response >= input$c8)) %>% 
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
        
       country_data() %>% 
            distinct(country_region, date) %>% 
            # I swear I'll clean up the country names so they all join later
            inner_join(policy_data, by = c("country_region" = "country_name", "date" = "date")) %>% 
            replace_na(list(flag = "National"))
    })
    
    cases_viz = reactive({
        country_data() %>% 
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
                               position = "left", expand = expansion(add = c(0,50)))+
            # Colour scale
            scale_fill_manual(values = case_pal)+
            # Theme
            theme_mark(md=T, base_size = 12, plot_margin = margin(0,0,5,0))+
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
    })
    
    policy_viz = reactive({
        lockdown_data() %>% 
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
            theme(legend.position = c(-.025,0.5),
                  legend.spacing = unit(0.5, "cm"),
                  legend.background = element_rect(fill = "#F9FAFA", colour = "#E1EAE9"),
                  legend.title = ggtext::element_markdown(colour = "#2C3535", hjust = -.25),
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
    })
    
    output$caption = renderUI({
        
        tags$span(style = "color: var(--axis-col); font-family: var(--font-family-title); line-height: 1.75;",
            HTML("Visualized by <a href='https://github.com/MokeEire'>@MokeEire</a>")
        )
    })
    
    output$whole_plot <- renderPlot({
        
        cases_viz() / policy_viz() + 
            plot_layout(heights = c(7,3))+
            plot_annotation(theme = theme_mark(md=T, plot_margin = margin(0, 15, 0, 60)), 
                            title = str_c(input$country, "'s COVID response"),#"Cough and response", #str_c("Daily COVID-19 cases ", country),
                            subtitle = str_c(input$case_type, ", daily cases (rolling seven-day avg.) and Government lockdown policies"))
        
    })
    
    
}

shinyApp(ui = ui, server = server)