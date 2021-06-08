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
# Libraries ----
library(extrafont)
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(here)
library(patchwork)
library(reactable)



load_new_data = F

if(load_new_data){
    here("scripts",
         # "theme_mark.R") %>%
         c("theme_mark.R", "load_coviz_data.R")) %>%
        walk(source)
} else {
    source(here("scripts", "theme_mark.R"))
    
    policy_dict = c("c1" = "School closing",
                    "c2" = "Workplace closing",
                    "c3" = "Cancel public events",
                    "c4" = "Restrictions on gatherings",
                    "c5" = "Close public transport",
                    "c6" = "Stay at home requirements",
                    "c7" = "Restrictions on internal movement",
                    "c8" = "International travel controls")
    
    load("covid_policies.RData")
    load("covid_country_level.RData")
    
    case_pal = set_names(viz_colours[c(3,6,7)], unique(covid_country_level$case_type))
    
}

options(reactable.theme = reactableTheme(
    color = "black",
    backgroundColor = "transparent",
    borderColor = my_col_pal[3],
    stripedColor = my_col_pal[4],
    highlightColor = my_col_pal[7],
    style = list(
        fontFamily = "Fira Sans Extra Condensed, sans-serif",
        fontSize = "16px",
        lineHeight = 1.1
    )
)
)







ui <- navbarPage(selected = "National Level",
    theme = "app.css",


    title = "COVID Lockdown Policies",
    tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            '),
    
    
    fluidRow(
        
        
        # column(3,
        dropdown(width = "860px", circle = F, icon = icon("pencil-square-o"), label = "Policies", 
                 margin = "0",
                 animate = animateOptions(
                     enter = animations$fading_entrances$fadeInLeftBig,
                     exit = animations$fading_exits$fadeOutLeftBig
                 ),
                 # tooltip = tooltipOptions(),
                 div(
                     class = "policy-box",
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
                         status = "warning",
                         width = "100%"
                     ),
                     awesomeRadio(width = "100%",
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
                                awesomeRadio(width = "100%",
                                             inputId = "c7",
                                             label = "Restrictions on internal movement", 
                                             choices = c("1 - Recommend not to travel between regions/cities" = 1, 
                                                         "2 - Internal movement restrictions in place" = 2),
                                             selected = 2,
                                             status = "warning"
                                ),
                                awesomeRadio(width = "100%",
                                             inputId = "c4",
                                             label = "Restrictions on gatherings", 
                                             choices = c("1 - Restrictions on very large gatherings (the limit is above 1000 people)" = 1, 
                                                         "2 - Restrictions on gatherings between 101-1000 people" = 2, 
                                                         "3 - Restrictions on gatherings between 11-100 people" = 3,
                                                         "4 - Restrictions on gatherings of 10 people or fewer" = 4),
                                             selected = 1,
                                             status = "warning"
                                )
                         ),
                         column(6,
                                awesomeRadio(width = "100%",
                                             inputId = "c8",
                                             label = "International travel control", 
                                             choices = c("1 - Screening arrivals" = 1, 
                                                         "2 - Quarantine arrivals from some or all regions" = 2, 
                                                         "3 - Ban arrivals from some regions" = 3,
                                                         "4 - Ban on all regions or total border closure" = 4),
                                             selected = 3,
                                             status = "warning"
                                ),
                                awesomeRadio(width = "100%",
                                             inputId = "c3",
                                             label = "Cancel public events", 
                                             choices = c("1 - Recommend cancelling" = 1, 
                                                         "2 - Require cancelling" = 2),
                                             selected = 2,
                                             status = "warning"
                                )
                                
                         )
                     ),
                     awesomeRadio(width = "100%",
                                  inputId = "c5",
                                  label = "Close public transport", 
                                  choices = c("1 - Recommend closing (or significantly reduce volume/route/means of transport available)" = 1, 
                                              "2 - Require closing (or prohibit most citizens from using it)" = 2),
                                  selected = 2,
                                  status = "warning"
                     ),
                     awesomeRadio(width = "100%",
                                  inputId = "c6",
                                  label = "Stay at home requirements", 
                                  choices = c("1 - Recommend not leaving house" = 1, 
                                              "2 - Require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips" = 2, 
                                              "3 - Require not leaving house with minimal exceptions (e.g. allowed to leave once a week, or only one person can leave at a time, etc.)" = 3),
                                  selected = 2,
                                  status = "warning"
                     )
                 )
                 
                 
                 # )
        ),
        selectizeInput("country", #width = "250px", ratio = c(5,7),
                       label = "Country", choices = sort(unique(covid_policies$country_name)), 
                       selected = "Ireland") %>% 
            tagAppendAttributes(class = 'inline-label'),
        selectizeInput("case_type", width = "250px", #ratio = c(5,7),
                       label = "Case Type", multiple = T, 
                       choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed") %>% 
            tagAppendAttributes(class = 'inline-label'),
        
        dropdownButton(width = "500px", right = T,circle = F, icon = icon("info-circle"), label = "About", 
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
                       h4("Built with",
                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                          "by",
                          img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                          ".")
        )
        
        # Country Filter -----
        # column(4,
        #        selectizeInput("country", #width = "250px", ratio = c(5,7),
        #                       label = "Country", choices = sort(unique(covid_policies$country_name)), 
        #                       selected = "Ireland") %>% 
        #            tagAppendAttributes(class = 'inline-label')
        # ) %>% 
        #     tagAppendAttributes(style = 'display:flex; flex-flow: row; flex-shrink:0; flex-grow:1'),
        # 
        # # Case Type Filter -----
        # column(4, 
        #        selectizeInput("case_type", width = "250px", #ratio = c(5,7),
        #                       label = "Case Type", multiple = T, 
        #                       choices = c("Confirmed", "Deaths", "Recovered"), selected = "Confirmed") %>% 
        #            tagAppendAttributes(class = 'inline-label')
        # ) %>% 
        #     tagAppendAttributes(style = 'display:flex; flex-flow: row; flex-shrink:0; flex-grow:1'),
        # 
        # # About Dropdown -----
        # column(1,
        #        dropdownButton(width = "500px", right = T,circle = F, icon = icon("info-circle"), label = "About", 
        #            h4("About the Project"),
        #            p("The purpose of this project is to enable an exploration of the timeline of COVID cases and varying degree of government containment policies (\"lockdowns\") used to slow the spread of the virus. I saw a common sentiment that the lockdowns were putting a massive burden on populations while failing to actually slow transmission rates. This is an interesting question because it seems intuitive that lockdowns would have some kind of diminishing effect on transmission rates, but I had no prior knowledge of the effectiveness of containment policies. At the same time, whether lockdowns were ", em(strong("worth")), " the costs imposed on people is also an important question and one of the first steps in answering this is evaluating the potential benefits of lockdowns."),
        #            p("Naturally we can't draw any causal relationships from these visualizations alone i.e. ", 
        #              strong("they won't tell you if closing schools stops the spread of COVID"), ".  If you look at a few different countries, you'll notice that the effects of introducing or lifting policies vary by country, time period, and a whole host of other factors. At the same time, you might be able to find some patterns worth investigating further. You can find the source code, and leave any comments/suggestions,", a(" on github", href = "https://github.com/MokeEire/DataVisualizations/tree/master/COVID-Lockdowns"), "."),
        #            hr(),
        #            h4("Sources"),
        #            tags$ul(
        #                tags$li("COVID cases - ", a("JHU CSSE COVID-19 Data", href = "https://github.com/CSSEGISandData/COVID-19")),
        #                tags$li("Lockdown policy information - ", a("Oxford Covid-19 Government Response Tracker (OxCGRT)", href = "https://github.com/OxCGRT/covid-policy-tracker"))
        #            ),
        #            h4("Built with",
        #               img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
        #               "by",
        #               img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
        #               ".")
        # 
        #        )
        # )
    ) %>% 
        tagAppendAttributes(style = 'display:flex;flex-flow:row;justify-content:space-around;max-width:80%;'),
    uiOutput("plot_header"),
    
    fluidRow(style = "overflow:auto;display:flex;justify-content:center;",
             
             column(width = 9, #style = "padding:0;",
                    withSpinner(plotOutput(outputId = "cases_viz", width = "100%", height = "auto"), #, height = "507px"
                                type = 8, color = viz_colours[2], hide.ui = F)
             ),
             column(width = 3, style = "display:flex; align-items:center;",#style = "position:relative; top: 507px;padding:0;width:18%;",
                    # plotOutput(outputId = "policy_viz_text", height = "210px")
                    HTML('<div class="callout">
                                            <div class="calloutHeading">Policy scope</div>
                                                <div class="calloutMessage">
                                                    <div class="row" style="
                                                        display: flex;
                                                        flex-flow: row;
                                                        margin: 0;
                                                        width: 100%;
                                                        align-items: baseline;">
                                                    <p>Targeted</p><span style="font-weight: 700;
                                                        font-size: 1.75rem;
                                                        margin-left: auto;
                                                        margin-right: 12%;
                                                    ">--</span>
                                                    </div>
                                                    <div class="row" style="
                                                        display: flex;
                                                        flex-flow: row;
                                                        margin: 0;
                                                        width: 100%;
                                                        align-items: baseline;
                                                    ">
                                                    <p>National</p><span style="font-weight: 700;
                                                        display: flex;
                                                        line-height: 0;
                                                        /* padding: 11px; */
                                                        transform: translate(2px, 9px);
                                                        font-size: 4.5rem;
                                                        margin-left: auto;
                                                        margin-right: 12%;">-</span>
                                                </div> 
                                                </div> 
                                            
                                          </div>')#,
                    # reactableOutput(outputId = "policy_viz_rt", height = "auto")#, height = "210px")
             )
             
    ),
    fluidRow(style = "overflow:auto;display:flex;justify-content:center;",
             column(width = 9, #style = "padding:0;",
                    withSpinner(plotOutput(outputId = "policy_viz", width = "100%", height = "auto"), #, height = "218px"
                                type = 8, color = viz_colours[2], hide.ui = F)
             ),
             column(width = 3,
                    reactableOutput(outputId = "policy_viz_rt", height = "auto")
             )
    ),
    uiOutput("caption")
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
    

    # Data ----
    country_data = reactive({
        req(input$case_type)
        covid_country_level[covid_country_level$country_region == input$country & covid_country_level$case_type %in% tolower(input$case_type), ]
    })
    
    lockdown_data = reactive({
        req(input$country)

        covid_policies[covid_policies$country_name == input$country & covid_policies$date %in% unique(covid_country_level$date), ] %>% 
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
    # output$whole_plot <- renderPlot({
    #     case_label = if_else(length(input$case_type) == 3, "All case types", tolower(str_c(input$case_type, collapse = " & ")))
    #     
    #     cases_viz() / policy_viz() + 
    #         plot_layout(heights = c(7,3))+
    #         plot_annotation(theme = theme_mark(md=T, plot_margin = margin(10, 15, 10, 60)))
    #     
    # })
    
    output$plot_header = renderUI({
        case_label = if_else(length(input$case_type) == 3, "All case types", tolower(str_c(input$case_type, collapse = " & ")))
        
        div(class = "tbl-header",
            h3(str_c(input$country, "'s COVID response"), class = "tbl-title"),
            h4(str_c("Daily cases, ", case_label, ", (rolling seven-day avg.) and the implementation of lockdown policies"),
               class = "tbl-subtitle")
        )
    })
    
    cases_height = reactive({
        req(input$dimension)
        input$dimension[2]*.4
    }, label = "cases_height") %>% throttle(1000)
    
    output$cases_viz = renderPlot({
        case_label = if_else(length(input$case_type) == 3, "All case types", tolower(str_c(input$case_type, collapse = " & ")))
        
        cases_viz()+
            scale_x_date(breaks = scales::breaks_pretty(n = 9), 
                         labels = scales::label_date_short(format = c("%Y", "%b")),
                         position = "top",
                         limits = c(ymd("2020-02-01"), max(country_data()$date)+1))+
            plot_annotation(theme = theme_mark(md=T, plot_margin = margin(10, 5, 0, 0)))
    }, height = function(){cases_height()})
    
    policy_height = reactive({
        req(input$dimension)
        input$dimension[2]*.25
    }) %>% throttle(1000)
    
    output$policy_viz = renderPlot({
        policy_viz()+
            scale_x_date(breaks = scales::breaks_pretty(n = 12), 
                         labels = scales::label_date_short(format = c("%Y", "%b")), 
                         position = "bottom",
                         limits = c(ymd("2020-02-01"), max(lockdown_data()$date)+1))+
            theme(strip.text.y.right = element_blank(), legend.position = "none", 
                  axis.text.y.left = element_text(colour = my_col_pal[1]))+
            plot_annotation(theme = theme_mark(md=T, plot_margin = margin(0, 5, 10, 0)))
    }, height = function(){policy_height()})
    
    output$policy_viz_text = renderPlot({
        policy_viz()+
            scale_x_date(limits = c(today(), today()))+
            theme(legend.position = "none",
                  axis.text.x.bottom = element_blank(), panel.grid.major.x = element_blank())+
            plot_annotation(theme = theme_mark(md=T, plot_margin = margin(0, 15, 10, 60)))
    }, execOnResize = T)
    
    output$policy_viz_rt = renderReactable({
        tibble(policy = levels(lockdown_data()$policy)) %>% 
            reactable(
                sortable = F, 
                resizable = F, 
                filterable = F, 
                searchable = F, 
                pagination = F, 
                borderless = T, 
                highlight = F, 
                wrap = F, 
                height = policy_height(), style = list(lineHeight = ((policy_height()/8)/16)-.6),
                defaultColDef = colDef(
                    header = "", 
                    headerStyle = list(display = "none"),
                    style = list(
                        padding = "2px 2px",
                        background = "transparent"
                    )
                )
            )
    })
    
    
}

shinyApp(ui = ui, server = server)