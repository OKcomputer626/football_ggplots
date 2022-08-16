library(worldfootballR)
library(ggthemes)
pacman::p_load(tidyverse, polite, scales, ggimage, 
               ggforce, ggtext,
               rvest, glue, extrafont, ggrepel, magick)



df <- fb_teams_urls("https://fbref.com/en/comps/9/Premier-League-Stats")
p1 <- fb_team_match_log_stats(team_urls = df, stat_type = "shooting",time_pause = 10)

p1 <- p1 %>%
  filter(Comp == "Premier League",
         ForAgainst == "For")
p1 %>% 
  summarize(avg_xG = median(xG_Expected),
            shots = median(Sh_Standard))

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


plot <- p1 %>%
  ggplot(aes(x = Sh_Standard, y = xG_Expected, label = Team)) +
  geom_point(shape = 21, color = "white", fill = "#ADEFD1FF", size = 2) +
  geom_text_repel(color = "#ADEFD1FF") +
  geom_hline(yintercept = 0.95, color = "#9CC3D5FF", alpha = 0.6, linetype = "dashed") +
  geom_vline(xintercept = 11.5, color = "#9CC3D5FF", alpha = 0.6, linetype = "dashed") +
  annotate("text", family = "Helvetica", fontface = "bold",
           x = 0.235 , y = 1, hjust = 0, alpha = 0.3, color = "#ADEFD1FF",
           label = "Average Total Expected Shots") +
  annotate("text", family = "Helvetica", fontface = "bold",
           x = 11.8, y = 0.6, alpha = 0.3, angle = 90, color = "#ADEFD1FF",
           label = "Average Total Shots") +
  xlab("Number of Shots") + ylab("Expected Goals") +
  labs(title = "Number of Shots and Expected Goals",
       subtitle = "Premier League 2022/23 | Matchweek 1",
       caption = "Data: FBref | StatsBomb \n Andres Gonzalez, Twitter: @AndresAnalytics") +
  ggthemes::theme_solarized_2(light = FALSE, base_family = "Helvetica") +
  theme(panel.background = element_rect(color = "#89CFF0"),
        plot.title = element_text(size = 18, face = "bold", color = "white"),
        plot.subtitle = element_text(size = 12, color = "#00FFEF"),
        plot.caption = element_text(size = 14, color = "#89CFF0"),
        axis.title = element_text(size = 14, color = "white"),
        axis.text = element_text(size = 12, color = "#89CFF0"),
        panel.grid.minor.x = element_blank())

ggsave(plot = plot, 
       here::here("match-analysis/Premier League Matchweek 1.png"),
       height = 10, width = 12)

plot_logo <- add_logo(
  plot_path = here::here("match-analysis/Premier League Matchweek 1.png"),
  logo_path = "output-onlinepngtools.png",
  logo_position = "top right",
  logo_scale = 10)
plot_logo

image_write(image = plot_logo, 
            here::here("match-analysis/Premier League Matchweek 1_logo.png"))

