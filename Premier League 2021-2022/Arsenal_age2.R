pacman::p_load(tidyverse, polite, scales, ggimage, rvest, 
               glue, extrafont, ggrepel, magick, ggforce)
loadfonts()
library(worldfootballR)

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

session <- bow("https://www.transfermarkt.com/fc-tokyo/kader/verein/6631/plus/1/galerie/0?saison_id=2018")
result_name2 <- scrape(session) %>% 
  html_nodes("#yw1 .bilderrahmen-fixed") %>% 
  html_attr("title") 
result_bday <- scrape(session) %>% 
  html_nodes(".posrela+ .zentriert") %>% 
  html_text()
result_joinedteam <- scrape(session) %>% 
  html_nodes("td:nth-child(8)") %>% 
  html_text()
result_leaveteam <- scrape(session) %>% 
  html_nodes("td:nth-child(10)") %>% 
  html_text()

rect_df <- data.frame(
  xmin = 25, xmax = 29,
  ymin = -Inf, ymax = Inf
)

Arsenal %>% 
  ggplot(aes(x = player_age, y = minutes_played)) +
  geom_vline(xintercept = 25, alpha = 0.4, color = "grey20") +
  geom_hline(yintercept = 0.5, alpha = 0.4, color = "grey20") +
  geom_rect(data = rect_df,
            aes(x = NULL, y = NULL,
                xmin = xmin, xmax = xmax,
                ymin = ymin, ymax = ymax),
            fill = "#9C824A", alpha = 0.4) +
  geom_link(aes(x = join_age, xend = age_now,
                y = min_perc, yend = min_perc,
                alpha = stat(index)), 
            color = "#DD2220", size = 1.75) + 
  geom_point(color = "#DB0007", size = 2.5) +
  geom_text_repel(
    aes(label = player_name, family = "Roboto Condensed"),
    nudge_x = 0.5,
    seed = 6) +
  scale_y_continuous(
    expand = c(0.01, 0),
    limits = c(0, 1), 
    labels = percent_format()) +
  scale_x_continuous(
    breaks = pretty_breaks(n = 5)) +
  labs(
    x = "Age", 
    y = "Share of Minutes Played (%)",  
    title = "Arsenal Football Club | Squad Age Profile",
    subtitle = "English Premier League (Season 2021-22) | (100% = 1800 Total Minutes)",
    caption = glue("
                   Data: transfermarkt.com
                   Twitter: @gonzalez_afc")) +
  theme_bw() +
  theme(
    text = element_text(family = "Roboto Condensed"),
    panel.border = element_rect(color = "#ffffff", size = 1.25),
    plot.title = element_text(color = "#DB0007", size = 16, face = "bold"),
    plot.subtitle = element_text(size = 14),
    axis.title.y = element_text(vjust= 0.5),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)) -> Arsenal_plot

Arsenal_plot

ggsave(plot = Arsenal_plot, "C:/Users/16267/Downloads/arsenal_age2.png",
       height = 6, width = 8)

plot_logo <- add_logo(plot_path = "C:/Users/16267/Downloads/arsenal_age2.png",
                      logo_path = "https://upload.wikimedia.org/wikipedia/en/thumb/5/53/Arsenal_FC.svg/800px-Arsenal_FC.svg.png",
                      logo_position = "top right",
                      logo_scale = 18)
plot_logo

image_write(image = plot_logo, "C:/Users/16267/Downloads/arsenal_age2.png")