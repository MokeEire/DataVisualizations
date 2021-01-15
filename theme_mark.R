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

source_caption = function(sources){
  if(missing(sources)){
    stop("Need to provide a vector of sources to the sources argument")
  }
  
  str_c(
    "**Source", if(length(sources) > 1){"s"}, "**: ",
    "<span style='line-height:1.25;'>", str_c(sources, collapse = "<br>"), "</span>",
    "<br><br>",
    "<span style='font-size:12px;color:", viz_colours[7], 
    "'>Visualized by @MokeEire</span>"
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
        #Main elements
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
                                              hjust = 0),
        plot.title.position = "plot",
        plot.subtitle = ggtext::element_markdown(size = base_size * 1.4,
                                                 family = subtitle_family,
                                                 hjust = 0,
                                                 lineheight = 1), 
        # Plot margin & caption
        plot.margin = plot_margin,
        plot.caption.position = "plot", 
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = line_colour, 
                                                margin = margin(t = 25, b = 25, l = 0, r = 0)),
        # Background
        plot.background = element_rect(fill = bg_colour,
                                       colour = bg_colour),
        
        # Axes
        axis.text = element_text(size = base_size * 1.25, colour = text_colour,
                                 margin = margin(t = 20, b = 20, l = 20, r = 20)),
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
        plot.caption = ggtext::element_markdown(hjust = 0, 
                                                colour = text_colour),
        
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
                                                     family = text_family),
        axis.title.y.right = ggtext::element_markdown(hjust = 0, 
                                                      margin = margin(l = 15), 
                                                      colour = text_colour, 
                                                      family = text_family),
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
        axis.title.y.left = element_text(margin = margin(r = 15)),
        axis.title.y.right = element_text(hjust = 0, margin = margin(l = 15)),
        axis.line = element_line(colour = line_colour)
      )
  }
}


min_wage_map = function(sf_data, map_year, fed_df){
  sf_data = filter(sf_data, year == map_year) %>% 
    mutate(min_wage_diff = min_wage - fed_min_wage)
  # non_repelled_data = filter(sf_data, is.na(repelled), min_wage_level != "Same", year == map_year)
  # repelled_data = filter(sf_data, repelled, min_wage_level != "Same", year == map_year)
  
  fed_mw = fed_df %>% filter(year == map_year) %>% pull(fed_min_wage)

  p = ggplot(sf_data, aes(fill = min_wage_diff, group = year)) +#min_wage_level, group = year)) +
    # Shapefile
    geom_sf(colour = my_col_pal[2]) +
    # State min_wage labels
    # geom_sf_text(data = non_repelled_data, 
    #              aes(label = scales::dollar(min_wage), group = year),
    #              size = 3.5,
    #              colour = my_col_pal[3],
    #              nudge_x = pull(non_repelled_data, x_nudge),
    #              nudge_y = pull(non_repelled_data, y_nudge),
    #              family = "Inter")+
    # Repel the smooshed (smushed?) up eastern states
    # https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/
    # geom_text_repel(data = repelled_data,
    #                 aes(label = scales::dollar(min_wage), 
    #                     geometry = geometry, group = year),
    #                 family = "Inter",
    #                 size = 3.5,
    #                 nudge_x = pull(repelled_data, x_nudge),
    #                 nudge_y = pull(repelled_data, y_nudge),
    #                 max.iter = 100, force = .5,seed = 1,
    #                 segment.colour = my_col_pal[3],
    #                 segment.alpha = .8,
    #                 colour = my_col_pal[3],
    #                 stat = "sf_coordinates")+
    coord_sf(crs = us_longlat_proj)+
    # scale_colour_identity()+
    labs(title = str_c("State vs. Federal Minimum Wage, ", map_year),
         subtitle = str_c("Difference ± between State and Federal Min. Wage (", scales::dollar(fed_mw), ")"))+
    # scale_fill_manual(name = NULL, values = c("Above" = my_col_pal[9], "Same" = my_col_pal[7], "Below" = my_col_pal[4]))+
    scale_fill_gradient2(low = my_col_pal[4], mid = my_col_pal[1], high = my_col_pal[9], 
                         labels = scales::dollar, name = NULL)+
    theme_mark(plots_pane = F)+
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.title.y.left = element_blank(),
          axis.line = element_blank())
  
  ggsave(p, filename = here("min_wage", str_c("min_wage_", map_year, ".png")), 
         device = "png", width = 12, height = 9, units = "cm", scale = 2)
}
