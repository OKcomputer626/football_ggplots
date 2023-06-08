# Load Libraries 
library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggalt)
library(ggtext)

# Adding font
font_add_google(family = "montse", "Montserrat")

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

liga_mx <- fotmob_get_season_stats(
  league_id = "230",
  season_name = "2022/2023-Clausura",
  stat_name = "Expected goals",
  team_or_player = "team"
)

liga_mx <- liga_mx %>%
  mutate(
    xG = if_else(stat_value < sub_stat_value,
                 stat_value - 1.1,
                 stat_value + 1.1),
    Goals = if_else(stat_value < sub_stat_value,
                    sub_stat_value + 1.1,
                    sub_stat_value - 1.1))

labels <- c(
  "<img src = 'Atlas FC.png' width = '15' /> <br> ATS",
  "<img src = 'Atletico San Luis.png' width = '15' /> <br> ASL",
  "<img src = 'CF America.png' width = '15' /> <br> AME",
  "<img src = 'Pumas.png' width = '15' /> <br> PUM",
  "<img src = 'Cruz Azul CF.png' width = '15' /> <br> CAZ",
  "<img src = 'FC Juarez.png' width = '15' /> <br> JUA",
  "<img src = 'Chivas FC.png' width = '15' /> <br> GUA",
  "<img src = 'Leon FC.png' width = '15' /> <br> LEO",
  "<img src = 'Mazatlan FC.png' width = '15' /> <br> MZA",
  "<img src = 'Monterrey CF.png' width = '15' /> <br> MNT",
  "<img src = 'Necaxa FC.png' width = '15' /> <br> NXA",
  "<img src = 'Pachuca CF.png' width = '15' /> <br> PCH",
  "<img src = 'Puebla FC.png' width = '15' /> <br> PUE",
  "<img src = 'Queretaro FC.png' width = '15' /> <br> QUE",
  "<img src = 'Santos FC.png' width = '15' /> <br> SLA",
  "<img src = 'Tigres FC.png' width = '15' /> <br> TUA",
  "<img src = 'Tijuana FC.png' width = '15' /> <br> TIJ",
  "<img src = 'Toluca FC.png' width = '15' /> <br> TOL")

liga_mx %>%
  ggplot(aes(x = stat_value, xend = sub_stat_value, y = participant_name)) + 
  geom_dumbbell(
    color = "grey85", colour_x = "#EA738D", colour_xend = "#89ABE3",
    size = 2, size_x = 3.5, size_xend = 3.5) +
  geom_text(aes(x = xG, y = participant_name, label = stat_value),
            family = "montse", color = "#EA738D", fontface = "bold") +
  geom_text(aes(x = Goals, y = participant_name, label = sub_stat_value),
            family = "montse", color = "#89ABE3", fontface = "bold") +
  labs(
    title = "Comparison of <span style = 'color:#EA738D;'>Expected</span> Goals and <span style = 'color:#89ABE3;'>Actual</span> Goals <br> in Liga MX Clausura 2023",
    subtitle = "Regular Season and Liguilla",
    caption = "Twitter: @AndresAnalytics",
    y = NULL,
    x = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0,45, by = 5),
    position = "top"
  ) +
  scale_y_discrete(labels = labels) +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.title = element_markdown(face = "bold", size = 18),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 14),
    plot.caption = element_text(face = "italic"),
    axis.ticks = element_blank(),
    axis.text.y = element_markdown(face = "bold"),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed")
    )

ggsave("Liga MX Clausura 2023 xG and Actual Goals.png")

plot_logo <- add_logo(
  plot_path = here::here("Liga MX Clausura 2023 xG and Actual Goals.png"),
  logo_path = here::here("Liga MX logo.png"),
  logo_position = "top right",
  logo_scale = 12)

magick::image_write(image = plot_logo, 
                    here::here("Liga MX Clausura 2023 xG and Actual Goals.png"))


