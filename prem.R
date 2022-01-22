pacman::p_load(tidyverse, polite, scales, ggimage, rvest, 
               glue, extrafont, showtext, ggrepel, magick, 
               ggforce, lubridate, cowplot, patchwork, rlang)
library(ggtext)
loadfonts()
library(worldfootballR)

prem_league_2021 <- get_season_team_stats(country = "USA", gender = "M", season_end_year = "2021", tier = "1st", stat_type = "league_table") %>%
  mutate(xG = xG / 34) 
prem_league_2020 <- get_season_team_stats(country = "USA", gender = "M", season_end_year = "2020", tier = "1st", stat_type = "league_table") %>%
  mutate(xG = xG / 23)

prem_league <- merge(prem_league_2021, prem_league_2020, all=TRUE) %>%
  select(Squad,Season_End_Year,xG) %>%
  mutate(xG = xG %>%
           round(digits = 2)) %>%
  pivot_wider(names_from = Season_End_Year, values_from = xG) %>%
  rename(xG_2020= "2020",
         xG_2021= "2021") %>% 
  filter(!is.na(xG_2021)) %>%
  mutate(xG_2020 = case_when(Squad == "CF Montréal" ~ 1.20,
                             TRUE ~ xG_2020),
    bump_2020 = if_else(xG_2020 < xG_2021,
                             xG_2020 - .05,
                             xG_2020 + .05),
    bump_2021 = if_else(xG_2020 < xG_2021,
                             xG_2021 + .05,
                             xG_2021 - .05)) %>%
  pivot_longer(cols= -Squad, names_to=c(".value","year"), names_sep="_") %>%
  filter(!is.na(xG))


MLS_plot <- prem_league %>%
  mutate(Squad = factor(Squad, levels = rev(unique(Squad)), ordered=TRUE)) %>%
  ggplot(aes(x=xG, y=Squad, color=year)) +
  geom_line(color="#4B6577", size=0.5, show.legend = FALSE) +
  geom_point(size=7, show.legend = FALSE) +
  geom_text(aes(label=xG, x=bump), show.legend = FALSE) +
  scale_color_manual(name=NULL,
                     breaks=c("2020","2021"),
                     values=c("#6CA6CE","#FF0200"),
                     labels=c("2020","2021")) +
  labs(x="Expected Goals", y=NULL,
       title="Expected Goals <span style = 'color: #6CA6CE' >last season</span> versus <span style = 'color: #FF0200' >this season</span>" ,
       subtitle = "Each metric 90 minutes played",
       caption ="<i>Data:fbref <br> Twitter:@AndresAnalytics </i>") +
  theme(
    plot.title = element_markdown(color = "white", face="bold"),
    plot.subtitle = element_text(color = "white", margin = margin(b=20)),
    plot.caption = element_markdown(color = "white", hjust = 0),
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#303030"),
    panel.background = element_rect(fill = "#303030"),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_text(color = "white"),
    axis.text.x = element_text(color = "white"),
    axis.text.y = element_text(color = "white")
  )

add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
  # Requires magick R Package https://github.com/ropensci/magick
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}
                     
ggsave(plot = MLS_plot, ".../MLS_xG_2020_2021.png",
       height = 4, width = 6)

plot_logo <- add_logo(plot_path = ".../MLS_xG_2020_2021.png",
                      logo_path = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/70/MLS_crest_logo_Mono_rev_black.svg/424px-MLS_crest_logo_Mono_rev_black.svg.png",
                      logo_position = "top left",
                      logo_scale = 23)

image_write(image = MLS_plot, ".../MLS_xG_2020_2021.png")
                     
                     
                     
                     
                     
                     


