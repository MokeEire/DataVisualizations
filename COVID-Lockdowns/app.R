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
library(janitor)
library(data.table)
library(countrycode)
library(here)
library(patchwork)
library(reactable)



load_new_data = T

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
    

}

policy_circles = function(policy_df, policy, policy_inputs = 1, colour){
  policy_col = substitute(policy)
  max_policy_response = policy_df[, max(eval(policy_col), na.rm=T)]
  # Row div
  div(class = "circles-row", style = str_c("color:", colour),
      reduce(
        purrr::flatten(list(rerun(policy_inputs, icon("circle-dot")),
                            rerun(max_policy_response - policy_inputs, icon("circle")))), paste) %>% 
        HTML()
  )
}

# Visualization utilities
case_pal = set_names(viz_colours[c(3,7)], unique(covid_country_level$case_type))

# For monthly labels, I want the label to be in the middle of the month (including current month)
# As a result we can't really use today()

global_date_labels = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-15"), by = "1 month")

global_date_ticks = seq.Date(to = max(covid_country_level$date), from = ymd("2020-01-01"), length.out = 7)


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







ui <- fluidPage(title = "COVID Lockdown Policies",
    theme = "app.css", 


    fluidRow(class = "col-sm-offset-1", 
                h3("A Visualization of COVID Policy Responses"),
                h4("Explore how and when a country put lockdown policies in place")
            # div(class = "container-fluid",
    
        
            # )
        ),
    
    # Filters -----
    fluidRow(class = "col-sm-offset-1", style = 'display:flex; flex-flow:row;',
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
        column(width = 3, 
               selectizeInput("country", #width = "250px", ratio = c(5,7),
                              label = "Country", choices = sort(unique(covid_policies$country_name)), 
                              selected = "Ireland") %>% 
                   tagAppendAttributes(class = 'inline-label')
               ),
        column(width = 3, 
               selectizeInput("case_type", width = "250px", #ratio = c(5,7),
                              label = "Case Type", multiple = T, 
                              choices = c("Confirmed", "Deaths"), selected = "Confirmed") %>% 
                   tagAppendAttributes(class = 'inline-label')
               ),
        column(width = 2, style = "align-self:center;",
               dropdownButton(width = "500px", right = T,circle = F, icon = icon("info-circle"), label = "About", 
                              h4("About the Project"),
                              p("Explore how and when countries implemented containment policies (\"lockdowns\") over the course of the COVID-19 pandemic ",
                                "and compare the timing of these policies with the reported COVID cases and deaths."),
                              p("COVID case rates are ", strong("not"), " the true number of COVID cases or deaths. ",
                                "In fact, the Economist has estimated the death toll to be",  
                                a("2-4 times greater than official data", 
                                  href = "https://www.economist.com/briefing/2021/05/15/there-have-been-7m-13m-excess-deaths-worldwide-during-the-pandemic", target = "_blank"),
                                ". So any interpretations from this data should be taken not only with a few grains of salt, but also with the understanding that the changes in COVID cases in response to the same policy will not be consistent across countries."),
                              p(" In any case, this visualization does not isolate the effects of individual policies, but rather gives a broader picture of how countries responded to this world-changing crisis."),
                              h4("Understanding the lockdown timeline"),
                              p("Each line on the lockdown timeline represents a given category (e.g. school closures). A policy is considered ", 
                                em("active"), " when a country is implementing a policy ", strong("at least as strict"), " as the policy selected with the policy dropdown buttons."),
                              p("e.g. If you want to see when a country ", 
                                strong("(1) Recommended closing"), 
                                " workplaces, the policy will show up when the country ", 
                                strong("(2) Required closing workplaces for some sectors"), " too."),
                              # p("The purpose of this project is to enable an exploration of the timeline of COVID cases and varying degree of government containment policies (\"lockdowns\") used to slow the spread of the virus. I saw a common sentiment that the lockdowns were putting a massive burden on populations while failing to actually slow transmission rates. This is an interesting question because it seems intuitive that lockdowns would have some kind of diminishing effect on transmission rates, but I had no prior knowledge of the effectiveness of containment policies. At the same time, whether lockdowns were ", em(strong("worth")), " the costs imposed on people is also an important question and one of the first steps in answering this is evaluating the potential benefits of lockdowns."),
                              # p("Naturally we can't draw any causal relationships from these visualizations alone i.e. ", 
                              #   strong("they won't tell you if closing schools stops the spread of COVID"), ".  If you look at a few different countries, you'll notice that the effects of introducing or lifting policies vary by country, time period, and a whole host of other factors. At the same time, you might be able to find some patterns worth investigating further. You can find the source code, and leave any comments/suggestions,", a(" on github", href = "https://github.com/MokeEire/DataVisualizations/tree/master/COVID-Lockdowns", target = "_blank"), "."),
                              hr(style = "margin: 10px 0;"),
                              h4("Sources"),
                              tags$ul(
                                  tags$li("COVID cases - ", a("JHU CSSE COVID-19 Data", href = "https://github.com/CSSEGISandData/COVID-19", target = "_blank")),
                                  tags$li("Lockdown policy information - ", a("Oxford Covid-19 Government Response Tracker (OxCGRT)", href = "https://github.com/OxCGRT/covid-policy-tracker", target = "_blank"))
                              ),
                              p("Built with",
                                 a(img(src = "shiny.png", height = "30px"), href = "https://shiny.rstudio.com/", target = "_blank"),
                                 "by",
                                 a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"), href = "https://www.rstudio.com/", target = "_blank"),
                                 ".", style = "text-align: right;")
               )
               )

        
    ),
    
    # Plot title -----
    fluidRow(
        column(width = 11, offset = 1,
               uiOutput("plot_header")
               )
    ),
    
    # COVID cases row -----
    fluidRow(style = "display:flex;justify-content:center;",
             
             column(width = 8, offset = 1, #style = "padding:0;",
                    withSpinner(plotOutput(outputId = "cases_viz", width = "100%", height = "auto"), #, height = "507px"
                                type = 8, color = viz_colours[2], hide.ui = F)
             ),
             column(width = 3, style = "display:flex; align-self:flex-end;flex-flow: column wrap-reverse;",#style = "position:relative; top: 507px;padding:0;width:18%;",
                    # plotOutput(outputId = "policy_viz_text", height = "210px")
                    # This is the legend. It is terribly annoying to have a lot of raw HTML like this
                    #   but unfortunately this solution was faster than any other that popped into my head
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
                                            
                                          </div>'),
                    h4("Select policy severity")#,
                    # reactableOutput(outputId = "policy_viz_rt", height = "auto")#, height = "210px")
             )
             
    ),
    
    # Policy row -----
    fluidRow(style = "display:flex;justify-content:center;",
             column(width = 8, offset = 1, #style = "padding:0;",
                    withSpinner(plotOutput(outputId = "policy_viz", width = "100%", height = "auto"), #, height = "218px"
                                type = 8, color = viz_colours[2], hide.ui = F)
             ),
             column(3,
                    fluidRow(style = "display: flex;flex-flow: row;flex-wrap: nowrap;justify-content: center;",
                        column(3,style = "padding-right:0;",
                               uiOutput("circles")#, class = "policy-circles"),
                        ),
                        column(9, style = "padding-left:0;",
                               uiOutput("policy_dropdowns")#, class = "col-sm-3"))
                        )
                    )
                    )
             
    ),
    uiOutput("caption")
)







server <- function(input, output) {
    

    # Data ----
    country_data = reactive({
        req(input$case_type)
        covid_country_level[covid_country_level$country_region == input$country & covid_country_level$case_type %in% tolower(input$case_type), ]
    })
    
    lockdown_data = reactive({
        req(input$country, input$c1, input$c2, input$c3, input$c4, input$c5, input$c6, input$c7, input$c8)

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
                  # panel.grid.major.x = element_blank(),
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
            # h3(str_c(input$country, "'s COVID response"), class = "tbl-title"),
            h4(str_c("Daily cases, ", case_label, ", (rolling seven-day avg.) and the implementation of lockdown policies"),
               class = "tbl-subtitle")
        )
    })
    
    cases_height = reactive({
        req(input$dimension)
        input$dimension[2]*.45
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
        input$dimension[2]*.3
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
                height = policy_height(), style = list(lineHeight = round(((policy_height()/8)/16)-.6, 2)),
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
    
    # UI elements -----
    output$circles = renderUI({
        if(class(need(input$c1, label = "C1")) == "character"){
            div(class = "policy-circles",
                div(class = "circles-row", style = str_c("color:", viz_colours[1]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c1_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[2]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c2_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[3]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c3_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[4]),
                    reduce(flatten(list(rerun(1, icon("circle")),
                                        rerun(max(covid_policies$c4_response, na.rm=T) - 1, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[5]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c5_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[6]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c6_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[7]),
                    reduce(flatten(list(rerun(2, icon("circle")),
                                        rerun(max(covid_policies$c7_response, na.rm=T) - 2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",
                    reduce(flatten(list(rerun(3, icon("circle")),style = str_c("color:", viz_colours[8]),
                                        rerun(max(covid_policies$c8_response, na.rm=T) - 3, icon("circle-o")))), paste) %>% 
                        HTML()
                )
            ) %>% 
                tagAppendAttributes(style = str_c("line-height:", round(((policy_height()/8)/16)-.54, 2)))
            
        } else {
            policy_inputs = list(
                c1 = input$c1,
                c2 = input$c2,
                c3 = input$c3,
                c4 = input$c4,
                c5 = input$c5,
                c6 = input$c6,
                c7 = input$c7,
                c8 = input$c8
            ) %>% 
                map(as.numeric)
            
            div(class = "policy-circles",
                div(class = "circles-row", style = str_c("color:", viz_colours[1]),
                    reduce(flatten(list(rerun(policy_inputs$c1, icon("circle")),
                                        rerun(max(covid_policies$c1_response, na.rm=T) - policy_inputs$c1, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row",style = str_c("color:", viz_colours[2]),
                    reduce(flatten(list(rerun(policy_inputs$c2, icon("circle")),
                                        rerun(max(covid_policies$c2_response, na.rm=T) - policy_inputs$c2, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[3]),
                    reduce(flatten(list(rerun(policy_inputs$c3, icon("circle")),
                                        rerun(max(covid_policies$c3_response, na.rm=T) - policy_inputs$c3, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[4]),
                    reduce(flatten(list(rerun(policy_inputs$c4, icon("circle")),
                                        rerun(max(covid_policies$c4_response, na.rm=T) - policy_inputs$c4, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[5]),
                    reduce(flatten(list(rerun(policy_inputs$c5, icon("circle")),
                                        rerun(max(covid_policies$c5_response, na.rm=T) - policy_inputs$c5, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[6]),
                    reduce(flatten(list(rerun(policy_inputs$c6, icon("circle")),
                                        rerun(max(covid_policies$c6_response, na.rm=T) - policy_inputs$c6, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[7]),
                    reduce(flatten(list(rerun(policy_inputs$c7, icon("circle")),
                                        rerun(max(covid_policies$c7_response, na.rm=T) - policy_inputs$c7, icon("circle-o")))), paste) %>% 
                        HTML()
                ),
                div(class = "circles-row", style = str_c("color:", viz_colours[8]),
                    reduce(flatten(list(rerun(policy_inputs$c8, icon("circle")),
                                        rerun(max(covid_policies$c8_response, na.rm=T) - policy_inputs$c8, icon("circle-o")))), paste) %>% 
                        HTML()
                )
            ) %>% 
                tagAppendAttributes(style = str_c("line-height:", round(((policy_height()/8)/16)-.54, 2)))
        }
        
        
    })
    
    output$policy_dropdowns = renderUI({ 
        div(style = str_c("font-size:1.4rem;line-height:", round(((policy_height()/8)/16)-.65, 2), ";width:90%;"),
               # reactableOutput(outputId = "policy_viz_rt", height = "auto")
            dropdown(label = "School closing", circle = F, icon = icon("graduation-cap"), status = "policy", 
                        margin = "0", 
                        awesomeRadio(
                            inputId = "c1",
                            label = NULL, #"School closure",
                            choices = c("1 - Recommend closing or all schools open with alterations resulting in significant differences compared to non-COVID-19 operations" = 1,
                                        "2 - Require closing (only some levels or categories, e.g. just high school, or just public schools)" = 2,
                                        "3 - Require closing all levels" = 3),
                            selected = 2,
                            status = "warning",
                            width = "100%"
                        )
               ) %>% 
                   tagAppendAttributes(style = str_c("color:", viz_colours[1])),
               dropdown(label = "Workplace closing", circle = F, icon = icon("briefcase"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c2",
                                     label = NULL, #"Workplace closure",
                                     choices = c("1 - Recommend closing (or recommend work from home)" = 1, 
                                                 "2 - Require closing (or work from home) for some sectors or categories of workers" = 2, 
                                                 "3 - Require closing (or work from home) for all-but-essential workplaces (eg grocery stores, doctors)" = 3),
                                     selected = 2,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[2])),
               dropdown(label = "Cancel public events", circle = F, icon = icon("calendar-times-o"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c3",
                                     label = NULL, #"Cancel public events",
                                     choices = c("1 - Recommend cancelling" = 1, 
                                                 "2 - Require cancelling" = 2),
                                     selected = 2,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[3])),
               dropdown(label = "Restrictions on gatherings", circle = F, icon = icon("users"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c4",
                                     label = NULL, #"Restrictions on gatherings",
                                     choices = c("1 - Restrictions on very large gatherings (the limit is above 1000 people)" = 1, 
                                                 "2 - Restrictions on gatherings between 101-1000 people" = 2, 
                                                 "3 - Restrictions on gatherings between 11-100 people" = 3,
                                                 "4 - Restrictions on gatherings of 10 people or fewer" = 4),
                                     selected = 1,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[4])),
               dropdown(label = "Close public transport", up = T, circle = F, icon = icon("train"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c5",
                                     label = NULL, #"Close public transport",
                                     choices = c("1 - Recommend closing (or significantly reduce volume/route/means of transport available)" = 1, 
                                                 "2 - Require closing (or prohibit most citizens from using it)" = 2),
                                     selected = 2,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[5])),
               dropdown(label = "Stay at home requirements", up = T, circle = F, icon = icon("home"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c6",
                                     label = NULL, #"Stay at home requirements", 
                                     choices = c("1 - Recommend not leaving house" = 1, 
                                                 "2 - Require not leaving house with exceptions for daily exercise, grocery shopping, and 'essential' trips" = 2, 
                                                 "3 - Require not leaving house with minimal exceptions (e.g. allowed to leave once a week, or only one person can leave at a time, etc.)" = 3),
                                     selected = 2,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[6])),
               dropdown(label = "Restrictions on internal movement", up = T, circle = F, icon = icon("road"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c7",
                                     label = NULL, #"Restrictions on internal movement", 
                                     choices = c("1 - Recommend not to travel between regions/cities" = 1, 
                                                 "2 - Internal movement restrictions in place" = 2),
                                     selected = 2,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[7])),
               dropdown(label = "International travel controls", up = T, circle = F, icon = icon("globe"), status = "policy",
                        margin = "0",
                        awesomeRadio(width = "100%",
                                     inputId = "c8",
                                     label = NULL, #"International travel control", 
                                     choices = c("1 - Screening arrivals" = 1, 
                                                 "2 - Quarantine arrivals from some or all regions" = 2, 
                                                 "3 - Ban arrivals from some regions" = 3,
                                                 "4 - Ban on all regions or total border closure" = 4),
                                     selected = 3,
                                     status = "warning"
                        )
               ) %>% 
                tagAppendAttributes(style = str_c("color:", viz_colours[8]))
        )
    })
    
    
}

shinyApp(ui = ui, server = server)