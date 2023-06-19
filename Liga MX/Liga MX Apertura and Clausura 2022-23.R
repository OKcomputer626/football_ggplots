library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggtext)

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

liga_mx <- fotmob_get_league_tables(
  league_id = 230,
  season = "2022/2023 - Apertura")

liga_mx_filtered <- liga_mx %>%
  filter(table_played == 17) %>%
  select(table_short_name,group_name, table_pts)

liga_mx_filtered <- liga_mx_filtered %>%
  pivot_wider(names_from = group_name,
              values_from = table_pts) %>%
  mutate(bump = case_when(
    Apertura > Clausura ~ Clausura + 0.65,
    Apertura < Clausura ~ Clausura - 0.65
  )) 

liga_mx_data <- liga_mx_filtered %>%
  pivot_longer(cols = c(Clausura,Apertura),
               names_to = "league",
               values_to = "pts")

arrow_data <- liga_mx_filtered %>%
  pivot_longer(cols = c(Apertura,bump),
               names_to = "line",
               values_to = "x")

caption = paste0("<span style='font-family:Barlow;'>Data: FotMob via Opta</span><br>",
                 "<span style='font-family:fb;'>&#xf099;</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>.</span>",
                 "<span style='font-family:Barlow;'>@AndresAnalytics</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>...</span>",
                 "<span style='font-family:fb;'>&#xf09b;</span>",
                 "<span style='font-family:Barlow;color:#FAF7F0;'>.</span>",
                 "<span style='font-family:Barlow;'>OKcomputer626</span>"
)

labels <- c(
  "<img src = 'Mazatlan FC.png' width = '15' /><br> MZA",
  "<img src = 'Necaxa FC.png' width = '15' /><br> NXA",
  "<img src = 'FC Juarez.png' width = '15' /><br> JUA",
  "<img src = 'Tijuana FC.png' width = '15' /><br> TIJ",
  "<img src = 'Pumas.png' width = '15' /><br> PUM",
  "<img src = 'Atletico San Luis.png' width = '15' /><br> ASL",
  "<img src = 'Santos FC.png' width = '15' /><br> SLA",
  "<img src = 'Puebla FC.png' width = '15' /><br> PUE",
  "<img src = 'Queretaro FC.png' width = '15' /><br> QUE",
  "<img src = 'Atlas FC.png' width = '15' /><br> ATS",
  "<img src = 'Cruz Azul CF.png' width = '15' /><br> CAZ",
  "<img src = 'Tigres FC.png' width = '12' /><br> TUA",
  "<img src = 'Leon FC.png' width = '13' /><br> LEO",
  "<img src = 'Pachuca CF.png' width = '15' /><br> PCH",
  "<img src = 'Toluca FC.png' width = '15' /><br> TOL",
  "<img src = 'CF America.png' width = '15' /><br> AME",
  "<img src = 'Chivas FC.png' width = '15' /><br> GUA",
  "<img src = 'Monterrey CF.png' width = '15' /><br> MNT")

liga_mx_data %>%
  arrange(league) %>%
  ggplot(aes(x = pts, y = table_short_name, label = pts, color = league, fill = league, group = table_short_name)) +
  geom_path(data = arrow_data, aes(x = x, y = reorder(table_short_name,Clausura) , group = table_short_name), arrow = arrow(angle = 25, length = unit(0.35, "cm"), type = "closed"), inherit.aes = FALSE) + 
  geom_point(shape = 21, size = 10) +
  geom_text(size = 3, family = "montse", fontface = "bold", show.legend = FALSE) +
  scale_fill_manual(values = c("#FAF7F0", "#FE7377")) +
  scale_color_manual(values = c("grey35", "black")) +
  scale_y_discrete(labels = labels) +
  labs(x = "Points",
       y = NULL,
       title = "The Ups and Downs: Liga MX Clausura vs Apertura Points",
       subtitle = "Following the trail of Liga MX teams through 2022-23 seasons, with a sharp decline in Santos Laguna's performance",
       caption = caption) +
  geom_text(x = 38, y = 7, label = "Santos Laguna's steep \n point drop", family = "montse", fontface = "bold", size = 3.5, color = "#00959F", show.legend = FALSE) +
  geom_text(x = 17, y = 17, label = "Impressive 12 point \n gain for Chivas", family = "montse", fontface = "bold", size = 3.5, color = "#5E495A", show.legend = FALSE) +
  theme(
    text = element_text(family = "montse"),
    plot.background = element_rect(fill = "#FAF7F0"),
    plot.margin = margin(t = 20, r = 20, l = 20, b = 10),
    plot.title = element_textbox_simple(face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_textbox_simple(color = "grey40", margin = margin(b = 10)),
    plot.caption = element_textbox_simple(face = "italic"),
    legend.title = element_blank(),
    legend.background = element_rect(fill = "#FAF7F0"),
    legend.key = element_rect(fill = "#FAF7F0"),
    legend.text = element_text(face = "bold"),
    legend.position = "top",
    axis.title.x = element_text(face = "bold", margin = margin(t = 10)),
    axis.text.y = element_markdown(face = "bold", size = 10),
    axis.text.x = element_text(face = "bold", size = 10),
    axis.line = element_line(),
    panel.background = element_rect(fill = "#FAF7F0"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linetype = "dashed")
  )


ggsave("Liga MX Apertura and Clausura 2022-2023.png", units = "in", width = 10, height = 10)

plot_logo <- add_logo(
  plot_path = here::here("Liga MX Apertura and Clausura 2022-2023.png"),
  logo_path = here::here("Liga MX logo.png"),
  logo_position = "top right",
  logo_scale = 15)

magick::image_write(image = plot_logo, 
                    here::here("Liga MX Apertura and Clausura 2022-2023.png"))

