#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# dir.create('~/.fonts')
# file.copy("www/NotoSans-Regular.ttf", "~/.fonts")
# system('fc-cache -f ~/.fonts')
# file.copy("www/CabinCondensed-Regular.ttf", "~/.fonts")
# system('fc-cache -f ~/.fonts')
# file.copy("www/FiraSansExtraCondensed-Regular.ttf", "~/.fonts")
# system('fc-cache -f ~/.fonts')
library(extrafont)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(here)

here("scripts", 
     "theme_mark.R") %>%
     # c("theme_mark.R", "load_coviz_data.R")) %>%
    walk(source)

load("covid_policies.RData")
load("covid_country_level.RData")

global_date_labels = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-01"), by = "1 month")
case_pal = set_names(viz_colours[c(3,6,7)], unique(covid_country_level$case_type))


policy_dict = c("c1" = "School closing",
                "c2" = "Workplace closing",
                "c3" = "Cancel public events",
                "c4" = "Restrictions on gatherings",
                "c5" = "Close public transport",
                "c6" = "Stay at home requirements",
                "c7" = "Restrictions on internal movement",
                "c8" = "International travel controls")




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


    # App title ----
    title = "COVID Lockdown Policies",
    
    
               tabPanel("National Level", id = "main",
                 # Sidebar layout with input and output definitions ----
                 sidebarLayout(
                   
                   # Sidebar panel for inputs ----
                   sidebarPanel(
                     
                     

                     h3("Select which policies to show", style="color: var(--bg-col); margin-top: 0px;"),
                     
                     # TODO: Make these sliders somehow
                     # Policy options
                     awesomeRadio(
                         inputId = "c1",
                         label = "School closure",
                         choices = c("1 - Recommend closing or all schools open with alterations resulting in significant differences compared to non-COVID-19 operations" = 1,
                                     "2 - Require closing (only some levels or categories, e.g. just high school, or just public schools)" = 2,
                                     "3 - Require closing all levels" = 3),
                         selected = 2,
                         status = "warning"
                     ),
                     awesomeRadio(#ratio = c(4,8), width = "100%",
                         inputId = "c2",
                         label = "Workplace closure", 
                         choices = c("1 - Recommend closing (or recommend work from home)" = 1, 
                                     "2 - Require closing (or work from home) for some sectors or categories of workers" = 2, 
                                     "3 - Require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)" = 3),
                         selected = 2,
                         status = "warning"
                     ),
                     fluidRow(
                         column(6,
                                awesomeRadio(#ratio = c(4,8), width = "100%",
                                    inputId = "c3",
                                    label = "Cancel public events", 
                                    choices = c("1 - Recommend cancelling" = 1, 
                                                "2 - Require cancelling" = 2),
                                    selected = 2,
                                    status = "warning"
                                )
                                ),
                         column(6,
                                awesomeRadio(#ratio = c(4,8), width = "100%",
                                    inputId = "c7",
                                    label = "Restrictions on internal movement", 
                                    choices = c("1 - Recommend not to travel between regions/cities" = 1, 
                                                "2 - Internal movement restrictions in place" = 2),
                                    selected = 2,
                                    status = "warning"
                                )
                                )
                     ),
                     awesomeRadio(#ratio = c(4,8), width = "100%",
                         inputId = "c4",
                         label = "Restrictions on gatherings", 
                         choices = c("1 - Restrictions on very large gatherings (the limit is above 1000 people)" = 1, 
                                     "2 - Restrictions on gatherings between 101-1000 people" = 2, 
                                     "3 - Restrictions on gatherings between 11-100 people" = 3,
                                     "4 - Restrictions on gatherings of 10 people or fewer" = 4),
                         selected = 1,
                         status = "warning"
                     ),
                     awesomeRadio(#ratio = c(4,8), width = "100%",
                         inputId = "c5",
                         label = "Close public transport", 
                         choices = c("1 - Recommend closing (or significantly reduce volume/route/means of transport available)" = 1, 
                                     "2 - Require closing (or prohibit most citizens from using it)" = 2),
                         selected = 2,
                         status = "warning"
                     ),
                     awesomeRadio(#ratio = c(4,8), width = "100%",
                         inputId = "c6",
                         label = "Stay at home requirements", 
                         choices = c("1 - Recommend not leaving house" = 1, 
                                     "2 - Require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips" = 2, 
                                     "3 - Require not leaving house with minimal exceptions (e.g. allowed to leave once a week, or only one person can leave at a time, etc.)" = 3),
                         selected = 2,
                         status = "warning"
                     ),
                     awesomeRadio(#ratio = c(4,8), width = "100%",
                         inputId = "c8",
                         label = "International travel control", 
                         choices = c("1 - Screening arrivals" = 1, 
                                     "2 - Quarantine arrivals from some or all regions" = 2, 
                                     "3 - Ban arrivals from some regions" = 3,
                                     "4 - Ban on all regions or total border closure" = 4),
                         selected = 3,
                         status = "warning"
                     )
                   ),
                   # Main panel for displaying outputs ----
                   mainPanel(
                       fluidRow(
                           column(4,
                                  selectizeInput("country", #width = "250px", ratio = c(5,7),
                                                 label = "Country", choices = sort(unique(covid_policies$country_name)), 
                                                 selected = "Ireland") %>% 
                                      tagAppendAttributes(class = 'inline-label')
                           ) %>% 
                               tagAppendAttributes(style = 'display:flex; flex-flow: row; flex-shrink:0; flex-grow:1'),
                           column(4, 
                                  selectizeInput("case_type", width = "250px", #ratio = c(5,7),
                                                 label = "Case Type", multiple = T, 
                                                 choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed") %>% 
                                      tagAppendAttributes(class = 'inline-label')
                           ) %>% 
                               tagAppendAttributes(style = 'display:flex; flex-flow: row; flex-shrink:0; flex-grow:1'),
                           column(4)
                       ) %>% 
                           tagAppendAttributes(style = 'display:flex')
                       ,
                       fluidRow(style = "overflow:auto;",
                           # Output: Histogram ----
                           withSpinner(plotOutput(outputId = "whole_plot", height = "725px"), type = 8, color = viz_colours[2])
                           
                       ),
                       fluidRow(
                           uiOutput("caption")
                       )
                     
                     
                     
                   )
               )
    ),
    tabPanel("About", id = "about", fluid = TRUE, #icon = "info-circle",
             fluidRow(
                 column(6,
                        #br(),
                        h4("About the Project"),
                        p("The purpose of this project is to enable an exploration of the timeline of COVID cases and varying degree of government containment policies (\"lockdowns\") used to slow the spread of the virus. I saw a common sentiment that the lockdowns were putting a massive burden on populations while failing to actually slow transmission rates. This is an interesting question because it seems intuitive that lockdowns would have some kind of diminishing effect on transmission rates, but I had no prior knowledge of the effectiveness of containment policies. At the same time, whether lockdowns were ", em(strong("worth")), " the costs imposed on people is also an important question and one of the first steps in answering this is evaluating the potential benefits of lockdowns."),
                        p("Naturally we can't draw any causal relationships from these visualizations alone i.e. ", 
                          strong("they won't tell you if closing schools stops the spread of COVID"), ".  If you look at a few different countries, you'll notice that the effects of introducing or lifting policies vary by country, time period, and a whole host of other factors. At the same time, you might be able to find some patterns worth investigating further. You can find the source code, and leave any comments/suggestions,", a(" on github", href = "https://github.com/MokeEire/DataVisualizations/tree/master/COVID-Lockdowns"), "."),
                        hr(),
                        h4("Sources"),
                        tags$ul(
                            tags$li("COVID cases - ", a("JHU CSSE COVID-19 Data", href = "https://github.com/CSSEGISandData/COVID-19")),
                            tags$li("Lockdown policy information - ", a("Oxford Covid-19 Government Response Tracker (OxCGRT)", href = "https://github.com/OxCGRT/covid-policy-tracker"))
                        ),
                        br()

                 ),
                 column(6
                 )
             ),
             h4("Built with",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                "by",
                img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                ".")
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
        req(input$country)
        covid_country_level[covid_country_level$country_region == input$country & covid_country_level$case_type %in% tolower(input$case_type), ]
    })
    
    lockdown_data = reactive({
        req(input$case_type)

        covid_policies[covid_policies$country_name == input$country & covid_policies$date %in% unique(country_data()$date), ] %>% 
            mutate(
                # Create boolean values for whether a lockdown was enacted
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

    })
    
    # Cases viz ----
    cases_viz = reactive({
        multiple_cases = (length(input$case_type) > 1)
        
            
        plot = country_data() %>% 
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
            plot = plot+
                scale_fill_manual(values = case_pal, labels = str_to_title, name = "Case Type",
                                  guide = guide_legend(ncol = 1))
        } else {
            plot = plot+
                scale_fill_manual(values = case_pal, guide = guide_none())
        }
        plot+
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
    })
    
    # Policy viz ----
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
                  # axis.text.x = element_blank(),
                  axis.line = element_blank())
    })
    
    output$caption = renderUI({
        
        tags$span(style = "color: var(--axis-col); font-family: var(--font-family-title); line-height: 1.75;",
            HTML("Visualized by <a href='https://github.com/MokeEire'>@MokeEire</a>")
        )
    })
    # Combined Plot ----
    output$whole_plot <- renderPlot({
        case_label = if_else(length(input$case_type) == 3, "All case types", tolower(str_c(input$case_type, collapse = " & ")))
        
        cases_viz() / policy_viz() + 
            plot_layout(heights = c(7,3))+
            plot_annotation(theme = theme_mark(md=T, plot_margin = margin(10, 15, 10, 60)), 
                            title = str_c(input$country, "'s COVID response"),#"Cough and response", #str_c("Daily COVID-19 cases ", country),
                            subtitle = str_c("Daily cases, ", case_label, ", (rolling seven-day avg.) and the implementation of lockdown policies"))
        
    })
    
    
}

shinyApp(ui = ui, server = server)