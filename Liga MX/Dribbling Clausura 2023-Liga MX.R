library(tidyverse)
library(worldfootballR)
library(ggtext)
library(showtext)
library(gt)
library(gtExtras)

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

fotmob_mx <- fotmob_get_season_stats(
  league_id = 230,
  season_name = "2022/2023-Clausura",
  stat_name = "Successful dribbles per 90",
  team_or_player = "player"
)

fotmob_mx_modified <- fotmob_mx %>%
  mutate(dribbles = (stat_value/sub_stat_value) * 100) %>%
  filter(minutes_played >= 500 & dribbles > quantile(dribbles, 0.85))

player_imgs <- c("https://images.fotmob.com/image_resources/playerimages/150093.png",
                 "https://images.fotmob.com/image_resources/playerimages/728365.png",
                 "https://images.fotmob.com/image_resources/playerimages/822211.png",
                 "https://images.fotmob.com/image_resources/playerimages/793586.png",
                 "https://images.fotmob.com/image_resources/playerimages/977404.png",
                 "https://images.fotmob.com/image_resources/playerimages/726475.png",
                 "https://images.fotmob.com/image_resources/playerimages/1179357.png",
                 "https://images.fotmob.com/image_resources/playerimages/1031656.png",
                 "https://images.fotmob.com/image_resources/playerimages/658135.png",
                 "https://images.fotmob.com/image_resources/playerimages/616341.png",
                 "https://images.fotmob.com/image_resources/playerimages/206227.png",
                 "https://images.fotmob.com/image_resources/playerimages/889620.png",
                 "https://images.fotmob.com/image_resources/playerimages/855911.png",
                 "https://images.fotmob.com/image_resources/playerimages/720560.png",
                 "https://images.fotmob.com/image_resources/playerimages/688293.png",
                 "https://images.fotmob.com/image_resources/playerimages/655806.png",
                 "https://images.fotmob.com/image_resources/playerimages/425810.png",
                 "https://images.fotmob.com/image_resources/playerimages/1096623.png",
                 "https://images.fotmob.com/image_resources/playerimages/720737.png",
                 "https://images.fotmob.com/image_resources/playerimages/834965.png",
                 "https://images.fotmob.com/image_resources/playerimages/828972.png",
                 "https://images.fotmob.com/image_resources/playerimages/434180.png",
                 "https://images.fotmob.com/image_resources/playerimages/728323.png",
                 "https://images.fotmob.com/image_resources/playerimages/1256627.png",
                 "https://images.fotmob.com/image_resources/playerimages/1023022.png",
                 "https://images.fotmob.com/image_resources/playerimages/848182.png",
                 "https://images.fotmob.com/image_resources/playerimages/915602.png",
                 "https://images.fotmob.com/image_resources/playerimages/941660.png")

fotmob_mx_modified <- fotmob_mx_modified %>%
  mutate(img = player_imgs)

team_position <- fb_league_stats(
  country = "MEX",
  gender = "M",
  season_end_year = 2023,
  tier = "1st",
  stat_type = "shooting",
  team_or_player = "player"
)

team_position <- team_position %>%
  mutate(Squad = ifelse(Squad == "UNAM", "Pumas UNAM", Squad),
         Squad = ifelse(Squad == "Santos", "Santos Laguna", Squad),
         Squad = ifelse(Squad == "Tijuana", "Club Tijuana", Squad),
         Squad = ifelse(Squad == "UANL", "Tigres", Squad),
         Squad = ifelse(Squad == "Atlético", "Atlético San Luis", Squad),
         Player = ifelse(Player == "Francisco Antonio Figueroa", "Antonio Figueroa", Player),
         Player = ifelse(Player == "César Huerta", "Cesar Huerta", Player),
         Player = ifelse(Player == "Pavel Pérez", "Pavel Perez", Player),
         Player = ifelse(Player == "Maximiliano Araújo", "Maximiliano Araujo", Player),
         Player = ifelse(Player == "Leonardo Fernández", "Leonardo Fernandez", Player),
         Player = ifelse(Player == "Nicolás Benedetti", "Nicolas Benedetti", Player),
         Player = ifelse(Player == "Josué Colmán", "Josue Colman", Player),
         Player = ifelse(Player == "Carlos Rotondi", "Carlos Rodolfo Rotondi", Player),
         Player = ifelse(Player == "Julián Quiñones", "Julian Quinones", Player),
         Player = ifelse(Player == "Lucas Rodríguez", "Lucas Rodriguez", Player),
         Player = ifelse(Player == "Luis Enrique Quiñones", "Luis Quinones", Player),
         Player = ifelse(Player == "Francisco Sebastián Córdova", "Francisco Cordova", Player),
         Player = ifelse(Player == "Tomás Molina", "Tomas Molina", Player),
         Player = ifelse(Player == "Brian Rodríguez", "Brian Rodriguez", Player)
         )
         

merged_data <- fotmob_mx_modified %>%
  left_join(team_position, by = c("participant_name" = "Player", "team_name" = "Squad")) %>%
  mutate(primary_position = str_split(Pos, ",", simplify = TRUE)[,1],
         team_label = case_when(
           team_name == "Pumas UNAM" ~ "Pumas.png",
           team_name == "Guadalajara" ~ "Chivas FC.png",
           team_name == "Pachuca" ~ "Pachuca CF.png",
           team_name == "Santos Laguna" ~ "Santos FC.png",
           team_name == "León" ~ "Leon FC.png",
           team_name == "Toluca" ~ "Toluca FC.png",
           team_name == "Mazatlán" ~ "Mazatlan FC.png",
           team_name == "Club Tijuana" ~ "Tijuana FC.png",
           team_name == "Atlético San Luis" ~ "Atletico San Luis.png",
           team_name == "Cruz Azul" ~ "Cruz Azul CF.png",
           team_name == "Atlas" ~ "Atlas FC.png",
           team_name == "Tigres" ~ "Tigres FC.png",
           team_name == "FC Juárez" ~ "FC Juarez.png",
           team_name == "Necaxa" ~ "Necaxa FC.png",
           team_name == "América" ~ "CF America.png",
           TRUE ~ "No label.png"
           ))
  

dribbling <- merged_data %>%
  select(team_label, img, participant_name, primary_position, minutes_played, dribbles, sub_stat_value) %>%
  arrange(desc(dribbles)) %>%
  gt() %>%
  cols_label(
    team_label = "",
    img = "",
    participant_name = "",
    primary_position = "",
    minutes_played = "MIN. PLAYED",
    dribbles = "DRIBBLES PER 90",
    sub_stat_value = "SUCCESS RATE (%)"
  ) %>%
  tab_header(
    title = md("**THE ART OF DRIBBLING IN LIGA MX**"),
    subtitle = md("Analysis based on players in the 85th percentile of dribbles attempted per 90 and with at least 500 minutes played from the Clausura 2023.
    <br> Data sourced from Opta. Twitter: @AndresAnalytics.")
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  opt_table_font(
    font = google_font(name = "Montserrat")
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(3)),
    locations = cells_column_labels(everything())
  ) %>%
  tab_style(
    style = cell_borders(sides = "top", color = "black", weight = px(3)),
    locations = cells_column_labels(c("minutes_played", "dribbles", "sub_stat_value"))
  ) %>%
  tab_style(
    style = cell_borders(sides = c("top", "bottom"), color = "lightgrey", style = "dashed"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c(participant_name, primary_position)
    )
  ) %>%
  gt_plt_bar_pct(column = sub_stat_value, scaled = TRUE,
                 fill = "#000080") %>%
  fmt_number(
    columns = dribbles,
    decimals = 1
  ) %>%
  gt_img_rows(columns = team_label, img_source = "local") %>%
  gt_img_circle(img, border_color = "grey", height = 30) %>%
  cols_width(
    team_label ~ px(150),
    img ~ px(40),
    participant_name ~ px(250),
    primary_position ~ px(200),
    minutes_played ~ px(100),
    dribbles ~ px(100),
    sub_stat_value ~ px(100)
  ) %>%
  opt_table_lines("none") %>%
  opt_all_caps() %>%
  opt_row_striping() %>%
  tab_options(
    heading.title.font.size = px(32),
    heading.subtitle.font.size = px(18),
    heading.align = "left",
    row.striping.background_color = "#FAF7F0")

gtsave_extra(dribbling,filename = "dribbling Clausura 2023-Liga MX.png")
