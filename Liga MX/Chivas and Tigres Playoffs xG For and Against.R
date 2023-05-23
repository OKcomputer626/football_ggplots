library(tidyverse)
library(worldfootballR)
library(showtext)
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

liga_mx_2023_results <- fb_match_results(country = "MEX", gender = "M", season_end_year = "2023", tier = "1st")

matches_url <- liga_mx_2023_results %>%
  filter(
    Date >= "2023-05-10" & Date <= "2023-05-22"
  ) %>%
  select(MatchURL) %>%
  pull(MatchURL)

time <- liga_mx_2023_results %>%
  filter(
    Date >= "2023-05-10" & Date <= "2023-05-22"
  ) %>%
  select(MatchURL, Wk)

match_summary <- fb_advanced_match_stats(matches_url, stat_type = "summary", team_or_player = "team")

matches <- left_join(match_summary,time, by = c("Game_URL" = "MatchURL"))

matches <- matches %>%
  mutate(Wk = as.numeric(Wk)) %>%
  select(!Game_URL)

test <- matches %>%
  mutate(Opponent = ifelse(Team == Away_Team, Home_Team, Away_Team))

# Chivas
chivas <- test %>%
  filter(Team %in% "Guadalajara" | Opponent %in% "Guadalajara") %>%
  mutate(Expected_Goals = ifelse(Team == "Guadalajara", "For","Against"))

labels <- c("<img src = 'atlas fc.png' width = '25' /><br> Away",
            "<img src = 'atlas fc.png' width = '25' /><br> Home",
            "<img src = 'image.png' width = '25' /><br> Home",
            "<img src = 'image.png' width = '25' /><br> Away")

chivas %>%
  ggplot(aes(x = Match_Date, y = xG_Expected, color = Expected_Goals, fill = Expected_Goals, group = Expected_Goals)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.3, shape = 21, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Chivas De Guadalajara Expected Goals <span style = 'color:#EA738D;'>For</span> and <span style = 'color:#89ABE3;'>Against</span> <br> Progression Through the Playoffs",
    subtitle = "Clausura 2023 Playoffs | Liga MX",
    x = "Playoff Match Date",
    y = "Expected Goals per Match",
    caption = "Twitter: @AndresAnalytics"
  ) +
  scale_color_manual(values = c("#89ABE3","#EA738D")) +
  scale_fill_manual(values = c("#89ABE3","#EA738D")) +
  scale_y_continuous(
    limits = c(0,2),
    breaks = seq(0,2, by = 0.5)) +
  scale_x_discrete(
    labels = labels) +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.title = element_markdown(face = "bold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_blank(),
    axis.text.x = element_markdown(),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", color = "grey90"),
    legend.position = "none"
  )

ggsave("Chivas De Guadalajara Expected Goals For and Against Playoffs.png")  

plot_logo <- add_logo(
  plot_path = here::here("Chivas De Guadalajara Expected Goals For and Against Playoffs.png"),
  logo_path = here::here("image copy.png"),
  logo_position = "top right",
  logo_scale = 10)

magick::image_write(image = plot_logo, 
                    here::here("Chivas De Guadalajara Expected Goals For and Against Playoffs.png"))

# Chivas
UANL <- test %>%
  filter(Team %in% "UANL" | Opponent %in% "UANL") %>%
  mutate(Expected_Goals = ifelse(Team == "UANL", "For","Against"))

labels <- c("<img src = 'toluca fc.png' width = '30' /><br> Home",
            "<img src = 'toluca fc.png' width = '30' /><br> Away",
            "<img src = 'image 3.png' width = '25' /><br> Home",
            "<img src = 'image 3.png' width = '25' /><br> Away")

UANL %>%
  ggplot(aes(x = Match_Date, y = xG_Expected, color = Expected_Goals, fill = Expected_Goals, group = Expected_Goals)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.3, shape = 21, color = "black") +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  labs(
    title = "Tigres UANL Expected Goals <span style = 'color:#EA738D;'>For</span> and <span style = 'color:#89ABE3;'>Against</span> <br> Progression Through the Playoffs",
    subtitle = "Clausura 2023 Playoffs | Liga MX",
    x = "Playoff Match Date",
    y = "Expected Goals per Match",
    caption = "Twitter: @AndresAnalytics"
  ) +
  scale_color_manual(values = c("#89ABE3","#EA738D")) +
  scale_fill_manual(values = c("#89ABE3","#EA738D")) +
  scale_y_continuous(
    limits = c(0,2.5),
    breaks = seq(0,2.5, by = 0.5)) +
  scale_x_discrete(
    labels = labels) +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.title = element_markdown(face = "bold", size = 16),
    plot.title.position = "plot",
    plot.subtitle = element_text(size = 12),
    plot.caption = element_text(face = "italic"),
    axis.title = element_text(face = "bold"),
    axis.ticks = element_blank(),
    axis.text.x = element_markdown(),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", color = "grey90"),
    legend.position = "none"
  )

ggsave("Tigres UANL Expected Goals For and Against Playoffs.png")  

plot_logo <- add_logo(
  plot_path = here::here("Tigres UANL Expected Goals For and Against Playoffs.png"),
  logo_path = here::here("image 4.png"),
  logo_position = "top right",
  logo_scale = 12)

magick::image_write(image = plot_logo, 
                    here::here("Tigres UANL Expected Goals For and Against Playoffs.png"))
