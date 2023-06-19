library(tidyverse)
library(worldfootballR)
library(ggtext)
library(showtext)
library(ggrepel)

# Adding font
font_add_google(family = "montse", "Montserrat")
font_add_google("Barlow","Barlow")

font_add("fb", "Font Awesome 6 Brands-Regular-400.otf")

showtext_auto()
showtext_opts(dpi = 300)

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
  stat_name = "Accurate passes per 90",
  team_or_player = "player"
)

fotmob_mx_filtered <- fotmob_mx %>%
  mutate(count = (stat_value/sub_stat_value) * 100) %>%
  filter(minutes_played >= median(minutes_played))

caption = paste0("<span style='font-family:Barlow;'>Data: FotMob via Opta</span><br>",
                 "<span style='font-family:fb;'>&#xf099;</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>.</span>",
                 "<span style='font-family:Barlow;'>@AndresAnalytics</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>.</span>",
                 "<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>.</span>",
                 "<span style='font-family:Barlow;'>OKcomputer626</span>"
                 )

fotmob_mx_filtered %>%
  ggplot(aes(x = count, y = sub_stat_value, fill = rank, label = participant_name)) +
  geom_point(size = 3.2, shape = 21) +
  geom_text_repel(
    data = subset(fotmob_mx_filtered, rank < 20),
    bg.color = "white",
    bg.r = 0.12,
    family = "montse",
    fontface = "bold",
    size = 3.2
  ) +
  labs(
    title = "Mastering Precision: Liga MX Clausura 2023",
    subtitle = "Unveiling the Balance between Frequency and Precision: <br> Passes per 90 vs Pass Success Rate Among Players Above the Minutes Played Median",
    caption = caption,
    x = "Passes per 90",
    y = "Pass success rate (%)"
  ) +
  paletteer::scale_fill_paletteer_c("grDevices::Purple-Orange") +
  scale_x_continuous(
    breaks = seq(0, 100, by = 10),
  ) +
  scale_y_continuous(
    limits = c(55,100),
    breaks = seq(0,100, by = 10),
    labels = scales::percent_format(scale = 1)
  ) +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.margin = margin(t = 20, r = 40, l = 15, b = 10),
    plot.title = element_textbox_simple(face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_textbox_simple(color = "grey40", margin = margin(b = 10)),
    plot.caption = element_textbox_simple(face = "italic"),
    legend.position = "none",
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    axis.text = element_text(face = "bold"),
    axis.line = element_line(),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed")
  )

ggsave("Liga MX Passes Clausura.png", units = "in", width = 9, height = 6)

plot_logo <- add_logo(
  plot_path = here::here("Liga MX Passes Clausura.png"),
  logo_path = here::here("Liga MX logo.png"),
  logo_position = "top right",
  logo_scale = 12)

magick::image_write(image = plot_logo, 
                    here::here("Liga MX Passes Clausura.png"))
