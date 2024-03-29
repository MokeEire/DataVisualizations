library(extrafont)
library(tidyverse)
library(ggtext)
library(ggrepel)
library(reactable)
library(patchwork)

my_col_pal = str_c("#", 
                   c("f0fafa", # background
                     "051414", # text
                     "474d4d", # Axis line colour
                     "C2D6D4", # Grid line colour
                     "236c6c", # skobeloff
                     "e9724c", # burnt sienna
                     "6ca8a8", # cadet blue
                     "69b44b", # green
                     "941c2f", # crimson
                     "b35757", # lighter crimson
                     "072e25", # dark green
                     "6d435a"  # eggplant
                   )) 
viz_colours = my_col_pal[-1:-4]

options(reactable.theme = reactableTheme(
  color = my_col_pal[2],
  backgroundColor = my_col_pal[1],
  borderColor = my_col_pal[3],
  stripedColor = my_col_pal[4],
  highlightColor = my_col_pal[7],
  style = list(
    fontFamily = "Roboto, sans-serif",
    fontSize = "0.75rem"
  )
)
)

source_caption = function(sources, md=T){
  if(missing(sources)){
    stop("Need to provide a vector of sources to the sources argument")
  }
  
  str_c(
    "Source", if(length(sources) > 1){"s"}, ": ",
    str_c(sources, collapse = "; "),
    if_else(md, "<br><br>", "\n\n"),
    "Visualized by @MokeEire"
  )
}

theme_mark = function(title_family = "Cabin Condensed",
                      subtitle_family = "Fira Sans Extra Condensed",
                        text_family = "Noto Sans",
                        base_size = 13, 
                        plot_margin = margin(20,20,20,20),
                        plots_pane = FALSE,
                        md = FALSE,
                        colour_pal = my_col_pal) {
  
  bg_colour = colour_pal[1]
  text_colour = colour_pal[2]
  line_colour = colour_pal[3]
  grid_colour = colour_pal[4]
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            size = base_size,
                            colour = text_colour),
        title = element_text(family = title_family,
                             colour = text_colour),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.7,
                                     lineheight = 1.2),
        # Plot Margin & Caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = base_size),
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        # Axes
        axis.text = element_text(size = base_size * 1.25, colour = text_colour),
        axis.title = element_text(size = base_size * 1.6,
                                  hjust = 1,
                                  face = "italic"),
        axis.title.y.left = element_text(margin = margin(r = 15)),
        axis.title.y.right = element_text(hjust = 0, margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour),
        # Legend
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            size = base_size,
                            colour = text_colour),
        title = element_text(family = title_family,
                             colour = text_colour),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2,
                                              hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.4,
                                                 family = subtitle_family,
                                                 hjust = 0,
                                                 lineheight = 1,
                                                 margin = margin(5,0,15,0)), 
        # Plot margin & caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = line_colour, 
                                                margin = margin(t = 25, b = 15, l = 0, r = 0)),
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        
        # Axes
        axis.text.x = element_text(size = base_size * 1.2, colour = text_colour,
                                 margin = margin(t = 5, b = 5, l = 5, r = 5)),
        axis.text.y = element_text(size = base_size * 1.2, colour = text_colour,
                                   margin = margin(t = 5, b = 5, l = 5, r = 5)),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = 1,
                                              face = "italic",
                                              margin = margin(t = 10, b = 10, l = 10, r = 10)),
        axis.title.y.left = ggtext::element_markdown(size = base_size * 1.6,
                                                     hjust = 1,
                                                     face = "italic",
                                                     margin = margin(r = 10, l = 10)),
        axis.title.y.right = ggtext::element_markdown(size = base_size * 1.6,
                                                      hjust = 0,
                                                      face = "italic",
                                                      margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour),
        
        # Legend
        legend.title = ggtext::element_markdown(size = base_size * 1.2),
        legend.text = ggtext::element_markdown(size = base_size * .9),
        
        # Facets
        strip.text.y.left = element_text(size = base_size * 1.1, 
                                         angle = 0, 
                                         vjust = 0.5, 
                                         hjust = 1),
        strip.placement = "outside",
        strip.text.y.right = element_text(size = base_size * 1.1, 
                                          angle = 0, 
                                          vjust = 0.5, 
                                          hjust = 0),
        
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            colour = text_colour),
        title = element_text(family = title_family,
                             colour = text_colour),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2,
                                              hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(family = subtitle_family,
                                                 hjust = 0,
                                                 lineheight = 1),
        
        # Plot margin and caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = line_colour, 
                                                margin = margin(t = 15, b = 0, l = 0, r = 0)),
        
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        # panel.grid.major = element_line(colour = line_colour),
        
        # Axes
        axis.text = ggtext::element_markdown(colour = text_colour,
                                             family = text_family),
        axis.title = ggtext::element_markdown(hjust = 1, 
                                              colour = text_colour,
                                              family = text_family,
                                              face = "italic"),
        axis.title.y.left = ggtext::element_markdown(margin = margin(r = 15), 
                                                     colour = text_colour, 
                                                     family = text_family,
                                                     face = "italic"),
        axis.title.y.right = ggtext::element_markdown(hjust = 0, 
                                                      margin = margin(l = 15), 
                                                      colour = text_colour, 
                                                      family = text_family,
                                                      face = "italic"),
        axis.line = element_line(colour = line_colour),
        
        # Legend
        legend.title = ggtext::element_markdown(family = title_family),
        legend.text = ggtext::element_markdown(family = text_family)
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            colour = text_colour),
        title = element_text(family = title_family),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        # Plot margin and caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    colour = line_colour),
        
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        
        # Axes
        axis.title = element_text(hjust = 1,
                                  face = "italic"),
        axis.title.y.left = element_text(hjust = 1,
                                         face = "italic",
                                         margin = margin(r = 15)),
        axis.title.y.right = element_text(hjust = 0, 
                                          face = "italic",
                                          margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour)
      )
  }
}

theme_wiki = function(title_family = "Georgia",
                      subtitle_family = "Georgia",
                      text_family = "Arial",
                      base_size = 13, 
                      plot_margin = margin(20,20,20,20),
                      plots_pane = FALSE,
                      md = FALSE,
                      colour_pal = c("#ffffff", "#202122", "#a2a9b1", "#f6f6f6")) {
  
  bg_colour = colour_pal[1]
  text_colour = colour_pal[2]
  line_colour = colour_pal[3]
  grid_colour = colour_pal[4]
  
  if (plots_pane == FALSE & md == FALSE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            size = base_size,
                            colour = text_colour),
        title = element_text(family = title_family,
                             colour = text_colour),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = element_text(face = "bold",
                                  size = base_size * 2,
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(size = base_size * 1.7,
                                     lineheight = 1.2),
        # Plot Margin & Caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = base_size),
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        # Axes
        axis.text = element_text(size = base_size * 1.25, colour = text_colour),
        axis.title = element_text(size = base_size * 1.6,
                                  hjust = 1,
                                  face = "italic"),
        axis.title.y.left = element_text(margin = margin(r = 15)),
        axis.title.y.right = element_text(hjust = 0, margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour),
        # Legend
        legend.title = element_text(size = base_size * 1.3),
        legend.text = element_text(size = base_size * 1.1)
      )
  } else if (plots_pane == FALSE & md == TRUE) {
    ggplot2::theme_minimal() +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            size = base_size,
                            colour = text_colour),
        title = element_text(family = title_family,
                             colour = text_colour),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = ggtext::element_markdown(face = "bold",
                                              size = base_size * 2,
                                              lineheight = 1.2,
                                              hjust = 0,
                                              margin = margin(t = 15, b = 15)),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.4,
                                                 family = subtitle_family,
                                                 hjust = 0,
                                                 lineheight = 1,
                                                 margin = margin(b = 20)), 
        # Plot margin & caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                margin = margin(t = 20, b = 0, l = 0, r = 0)),
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        
        # Axes
        axis.text = element_text(size = base_size * 1.2, colour = text_colour,
                                 margin = margin(t = 10, b = 10, l = 10, r = 10)),
        axis.text.y.left = ggtext::element_markdown(size = base_size*1.2, colour = text_colour,
                                                    hjust = 1, margin = margin(t = 10, r = 10, l = 10, b = 10)),
        axis.text.x.bottom = ggtext::element_markdown(size = base_size*1.2, colour = text_colour,
                                                    hjust = .5, margin = margin(t = 10, b = 10)),
        axis.title = ggtext::element_markdown(size = base_size * 1.6,
                                              hjust = 1,
                                              face = "italic",
                                              margin = margin(t = 10, b = 10, l = 10, r = 10)),
        axis.title.y.left = ggtext::element_markdown(size = base_size * 1.6,
                                                     hjust = 1,
                                                     face = "italic",
                                                     margin = margin(r = 10, l = 10)),
        axis.title.y.right = ggtext::element_markdown(size = base_size * 1.6,
                                                      hjust = 0,
                                                      face = "italic",
                                                      margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour),
        axis.ticks = element_line(colour = line_colour),
        axis.ticks.length = unit(.2, "cm"),
        
        # Legend
        legend.title = ggtext::element_markdown(size = base_size * 1.2),
        legend.text = ggtext::element_markdown(size = base_size * .9),
        
        # Facets
        strip.text.y.left = element_text(size = base_size * 1.1, 
                                         angle = 0, 
                                         vjust = 0.5, 
                                         hjust = 1),
        strip.placement = "outside",
        strip.text.y.right = element_text(size = base_size * 1.1, 
                                          angle = 0, 
                                          vjust = 0.5, 
                                          hjust = 0),
        
      )
  } else if (plots_pane == TRUE & md == TRUE) {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            colour = text_colour),
        title = element_text(family = title_family),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = ggtext::element_markdown(face = "bold",
                                              lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(lineheight = 1.2),
        
        # Plot margin and caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0),
        
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        # panel.grid.major = element_line(colour = line_colour),
        
        # Axes
        axis.title = ggtext::element_markdown(hjust = 1, 
                                              colour = text_colour,
                                              family = text_family,
                                              face = "italic"),
        axis.title.y.left = ggtext::element_markdown(margin = margin(r = 15), 
                                                     colour = text_colour, 
                                                     family = text_family,
                                                     face = "italic"),
        axis.title.y.right = ggtext::element_markdown(hjust = 0, 
                                                      margin = margin(l = 15), 
                                                      colour = text_colour, 
                                                      family = text_family,
                                                      face = "italic"),
        axis.text = ggtext::element_markdown(colour = text_colour,
                                             family = text_family),
        axis.line = element_line(colour = line_colour),
        
        # Legend
        legend.title = ggtext::element_markdown(family = title_family),
        legend.text = ggtext::element_markdown(family = text_family)
      )
  } else {
    ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        # Main elements
        text = element_text(family = text_family,
                            colour = text_colour),
        title = element_text(family = title_family),
        line = element_line(colour = line_colour),
        panel.grid.major = element_line(colour = grid_colour),
        panel.grid.minor = element_blank(),
        
        # Plot titles
        plot.title = element_text(face = "bold",
                                  lineheight = 1.2),
        plot.title.position = "plot",
        plot.subtitle = element_text(lineheight = 1.2),
        # Plot margin and caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0),
        
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        
        # Axes
        axis.title = element_text(hjust = 1,
                                  face = "italic"),
        axis.title.y.left = element_text(hjust = 1,
                                         face = "italic",
                                         margin = margin(r = 15)),
        axis.title.y.right = element_text(hjust = 0, 
                                          face = "italic",
                                          margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour)
      )
  }
}
