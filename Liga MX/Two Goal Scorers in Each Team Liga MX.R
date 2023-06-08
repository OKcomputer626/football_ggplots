library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggtext)
library(ggforce)

# Adding font
font_add_google(family = "montse", "Montserrat")

sysfonts::font_add("fb", "Font Awesome 6 Brands-Regular-400.otf")

showtext_opts(dpi = 300)
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

fotmob_mx <- fotmob_get_season_stats(
  league_id = 230,
  season_name = "2022/2023-Clausura",
  stat_name = "Goals per 90",
  team_or_player = "player"
)

top_two_players <- fotmob_mx %>%
  group_by(team_name) %>%
  arrange(desc(sub_stat_value)) %>%
  slice_head(n = 2) %>%
  mutate(place = ifelse(row_number() == 1, "first", "second"),
         bump_y = if_else(place == "first", 
                          sub_stat_value + 0.35, 
                          sub_stat_value - 0.55)) %>%
  ungroup() %>%
  arrange(desc(sub_stat_value)) %>%
  mutate(team_name = factor(team_name, levels = unique(team_name))) %>%
  select(team_name, participant_name, sub_stat_value, place, bump_y)

top_two_players <- top_two_players %>%
  mutate(last_name = stringr::str_extract(participant_name, "\\b\\w+$"))

labels <- c(
  "AME <br> <img src = 'CF America.png' width = '20' />",
  "ATS <br> <img src = 'Atlas FC.png' width = '20' />",
  "MNT <br> <img src = 'Monterrey CF.png' width = '20' />",
  "TUA <br> <img src = 'Tigres FC.png' width = '20' />",
  "GUA <br> <img src = 'Chivas FC.png' width = '20' />",
  "PUM <br> <img src = 'Pumas.png' width = '20' />",
  "SLA <br> <img src = 'Santos FC.png' width = '20' />",
  "TOL <br> <img src = 'Toluca FC.png' width = '20' />",
  "ASL <br> <img src = 'Atletico San Luis.png' width = '20' />",
  "CAZ <br> <img src = 'Cruz Azul CF.png' width = '20' />",
  "LEO <br> <img src = 'Leon FC.png' width = '20' />",
  "NXA <br> <img src = 'Necaxa FC.png' width = '20' />",
  "PCH <br> <img src = 'Pachuca CF.png' width = '20' />",
  "PUE <br> <img src = 'Puebla FC.png' width = '20' />",
  "TIJ <br> <img src = 'Tijuana FC.png' width = '20' />",
  "JUA <br> <img src = 'FC Juarez.png' width = '20' />",
  "MZA <br> <img src = 'Mazatlan FC.png' width = '20' />",
  "QUE <br> <img src = 'Queretaro FC.png' width = '20' />")

caption <- paste0(
  "<span style ='font-family:fb;'>&#xf099;</span> @AndresAnalytics"
)

top_two_players %>%
  ggplot(aes(x = team_name, y = sub_stat_value, color = place, group = team_name, label = last_name)) +
  geom_link2(linewidth = 1.2, alpha = 0.7) +
  geom_point(size = 5) +
  geom_text(aes(y = bump_y), vjust = 0, family = "montse", fontface= "bold") +
  scale_y_continuous(
    limits = c(0,15),
    breaks = seq(0,20,by = 2),
    expand = c(0,0)) +
  scale_color_manual(values = c("first" = "#00A4CCFF", "second" = "#F95700FF")) +
  scale_x_discrete(labels = labels) +
  labs(
    title = "Top Two Goal Scorers in Each Team",
    subtitle = "Liga MX | Clausura 2023",
    caption = caption,
    x = NULL,
    y = "Goals Scored") +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.title = element_text(face = "bold", size = 20),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12, color = "grey40"),
    plot.caption = element_markdown(face = "italic", size = 10),
    legend.position = "none",
    axis.title = element_text(face = "bold"),
    axis.text.x = element_markdown(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold"),
    axis.line = element_line(linewidth = 0.2),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed")
  )
  
ggsave("Two Goal Scorers in Each Team Liga MX.png",width = 15,height = 8)

plot_logo <- add_logo(
  plot_path = here::here("Two Goal Scorers in Each Team Liga MX.png"),
  logo_path = here::here("Liga MX logo.png"),
  logo_position = "top right",
  logo_scale = 12)

magick::image_write(image = plot_logo, 
                    here::here("Two Goal Scorers in Each Team Liga MX.png"))
