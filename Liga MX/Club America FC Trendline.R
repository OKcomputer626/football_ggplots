library(worldfootballR)
library(tidyverse)
library(showtext)
library(ggtext)

# Adding font
font_add_google(family = "montserrat", "Montserrat")
font_add_google(family = "Oswald", "Oswald")
showtext_auto()

# logo
add_logo <- function(plot_path, logo_path, logo_position, 
                     logo_scale = 10){
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

fb_teams_urls("https://fbref.com/en/comps/31/Liga-MX-Stats")
df <- fb_team_match_results("https://fbref.com/en/squads/18d3c3a3/America-Stats")

df_mx <- df %>%
  mutate(
    xG_roll = zoo::rollmean(xG, k = 5, fill = NA),
    xGA_roll = zoo::rollmean(xGA, k = 5, fill = NA)) %>%
  select(Date, xG_roll, xGA_roll) %>%
  pivot_longer(
    cols = c(xG_roll, xGA_roll),
    names_to = "Variables", 
    values_to = "count"
  ) %>%
  filter(!Date >= "2024-03-02")


df_mx %>%
  ggplot(aes(x = Date, y = count, group = Variables, color = Variables, fill = Variables)) +
  geom_line(linewidth = 0.5) +
  geom_point(size = 2.8, shape = 21, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  geom_vline(xintercept = c("2023-11-29", "2023-12-17"), color = "white", linetype = "dashed", size = 0.8) +
  annotate("text", x = "2023-09-30", y = 2.5, color = "white", label = "Apertura 2023 Regular Season", family = "montserrat", fontface = "bold", size = 8.5) +
  annotate("text", x = 20.5, y = 2.5, color = "white", label = "Liguilla 2023", family = "montserrat", fontface = "bold", size = 8.5) +
  annotate("text", x = 20.5, y = 2.425, color = "#D38D20", label = "CHAMPIONS", family = "Oswald", fontface = "bold", size = 10) +
  annotate("text", x = "2024-01-27", y = 2.5, color = "white", label = "Clausura 2024 Regular Season", family = "montserrat", fontface = "bold", size = 8.5) +
  labs(
    title = "Club América Expected Goals <span style = 'color:#ffeb00;'>For</span> & <span style = 'color:#e62b2f;'>Against</span> Trend Over Time",
    subtitle = "5 game rolling average | Liga MX | Apertura 2023 - Clausura 2024 MD 9",
    caption = "X: @AndresAnalytics | Data: fbref via Opta",
    x = "MatchDate",
    y = "Expected Goals Per Game"
  ) +
  scale_color_manual(values = c("#ffeb00","#e62b2f")) +
  scale_fill_manual(values = c("#ffeb00","#e62b2f")) +
  theme(
    text = element_text(family = "montserrat", color = "white"),
    plot.background = element_rect(fill = "#292929"),
    plot.title = element_markdown(face = "bold", size = 32, hjust = 0.5),
    plot.subtitle = element_text(size = 22, hjust = 0.5),
    plot.caption = element_text(face = "italic", size = 20),
    axis.title = element_text(face = "bold", size = 28),
    axis.text = element_text(face = "bold", size = 20, color = "white"),
    axis.text.x = element_text(angle = 90),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = "#292929"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", color = "grey25", size = 0.3),
    legend.position = "none"
  )

ggsave("America FC trendline.png", width = 12)

plot_logo <- add_logo(
  plot_path = here::here("America FC trendline.png"),
  logo_path = here::here("Club_América_crest.svg.png"),
  logo_position = "top right",
  logo_scale = 25)

magick::image_write(image = plot_logo, 
                    here::here("America FC trendline.png"))

