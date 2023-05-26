library(worldfootballR)
library(tidyverse)
library(gt)
library(gtExtras)

liga_mx_2023_results <- fb_match_results(country = "MEX", gender = "M", season_end_year = "2023", tier = "1st")

write.csv(match_summary, "Clausura matches week 1-11.csv", row.names=FALSE)
write.csv(match_summary1, "Clausura matches week 12.csv", row.names=FALSE)
write.csv(match_summary2, "Clausura matches week 13-17.csv", row.names=FALSE)

wk_1_11 <- read_csv("Clausura matches week 1-11.csv")
wk_12 <- read_csv("Clausura matches week 12.csv")
wk_13_17 <- read_csv("Clausura matches week 13-17.csv")

matches <- wk_1_11 %>%
  full_join(wk_12) %>%
  full_join(wk_13_17)

write.csv(matches, "Clausura 2023 Matches.csv", row.names=FALSE)

matches <- left_join(matches,time, by = c("Game_URL" = "MatchURL"))

matches <- matches %>%
  mutate(Wk = as.numeric(Wk)) %>%
  select(!Game_URL)

test <- matches %>%
  mutate(Opponent = ifelse(Team == Away_Team, Home_Team, Away_Team),
         IsHomeTeam = ifelse(Team == Home_Team, 1, 0)) %>%
  select(Team, Opponent,Gls:Succ_Take_Ons, IsHomeTeam)

# Split the data into training and testing sets
set.seed(100) # for reproducibility
train_index <- sample(nrow(test), nrow(test)*0.8)
train_data <- test[train_index, ]
test_data <- test[-train_index, ]

null_model <- glm(Gls ~ Team + Opponent, family=poisson(link=log), data=train_data)

forward_model <- step(null_model, scope=list(lower=~Team + Opponent, upper=~ Team + Opponent + Ast + PK + PKatt + Sh + SoT + CrdY + CrdR + Touches + Tkl + Int + Blocks + xG_Expected + npxG_Expected +
                                               xAG_Expected + SCA_SCA + GCA_SCA + Cmp_Passes + Att_Passes + Cmp_percent_Passes + PrgP_Passes + Carries_Carries + PrgC_Carries + Att_Take_Ons + Succ_Take_Ons + IsHomeTeam))
summary(forward_model)

add1(forward_model, ~.+GCA_SCA*PK + GCA_SCA*SoT + GCA_SCA*SCA_SCA + GCA_SCA*Sh, test = 'Chisq')

mod <- update(forward_model, ~.+GCA_SCA*PK + GCA_SCA*SoT + GCA_SCA*SCA_SCA + GCA_SCA*Sh)
summary(mod)

anova(forward_model, mod, test = "Chisq")

# Fit the model on the training set
train_mod <- glm(Gls ~ Team + Opponent + GCA_SCA + PK + SoT + SCA_SCA + Sh + GCA_SCA:PK + 
                   GCA_SCA:SoT + GCA_SCA:SCA_SCA + GCA_SCA:Sh, data = train_data, family = poisson(link = log))

# Make predictions on the testing set
test_pred <- predict(train_mod, newdata = test_data, type = "response")

# Calculate the MSE
mse <- mean((test_data$Gls - test_pred)^2)
mse

test_data$predicted_gls <- round(predict(train_mod, test_data, type = "response"))

mean(abs(test_data$predicted_gls - test_data$Gls))

# Add a new column 'correct' to the 'test_data' data frame
test_data$correct <- test_data$Gls == test_data$predicted_gls

# Show the results
test_data %>%
  select(Team, Opponent, Gls, predicted_gls, correct) %>%
  group_by(correct) %>%
  count() %>%
  ungroup() %>%
  gt::gt()

df <- read_csv("Clausura matches Liguilla.csv")

df %>%
  group_by(Team) %>%
  summarise(GCA = mean(GCA_SCA),
            PK = mean(PK),
            SoT = mean(SoT),
            SCA = mean(SCA_SCA),
            Sh = mean(Sh))

simulate_match <- function(foot_model, homeTeam, awayTeam, max_goals=10){
  home_goals_avg <- predict(foot_model, 
                            data.frame(GCA_SCA = 3.2, Team= homeTeam, PK = 0.2,
                                       Opponent=awayTeam, SoT = 4.4, SCA_SCA = 20.8, Sh = 11.2), type="response")
  away_goals_avg <- predict(foot_model, 
                            data.frame(GCA_SCA = 1.5, Team=awayTeam, PK = 0,
                                       Opponent=homeTeam, SoT = 4.75, SCA_SCA = 23, Sh = 13.5), type="response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}

simulate_match(train_mod, "UANL", "Guadalajara", max_goals=4)

match_sim <- simulate_match(train_mod, "UANL", "Guadalajara", max_goals=10)

# Calculate the probabilities
tigres_wins <- sum(match_sim[lower.tri(match_sim, diag = FALSE)]) * 100 # Probability of América winning
draw <- sum(diag(match_sim)) * 100 # Probability of a draw
guadalajara_wins <- sum(match_sim[upper.tri(match_sim, diag = FALSE)]) * 100 # Probability of Guadalajara winning

# Print the probabilities
print(paste("Probability of Tigres winning: ", tigres_wins))
print(paste("Probability of a draw: ", draw))
print(paste("Probability of Guadalajara winning: ", guadalajara_wins))

# Create a data frame
df <- data_frame(
  home = "Tigres FC.png",
  vs = "vs.",
  away = "Chivas FC.png",
  Tigres_Win_Probability = round(tigres_wins,2),
  Draw_Probability = round(draw,2),
  Guadalajara_Win_Probability = round(guadalajara_wins,2),
)

probability_plot <- df %>%
  gt() %>%
  cols_label(
    Tigres_Win_Probability = md("**HOME WIN (%)**"),
    Draw_Probability = md("**TIE (%)**"),
    Guadalajara_Win_Probability = md("**AWAY WIN (%)**"),
    home = "",
    vs = md("**TEAM**"),
    away = "",
  ) %>%
  cols_align(
    align = "center"
  ) %>%
  gt_img_rows(home, img_source = "local",height = 60) %>%
  gt_img_rows(away, img_source = "local", height = 60) %>%
  tab_options(table.background.color = "#FAF7F0") %>%
  opt_table_font(
    font = google_font(name = "Montserrat")
  ) %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "black", weight = px(3)),
    locations = cells_column_labels(everything())
    ) %>%
  tab_style(
    style = list(cell_borders(sides = c("top", "bottom"), color = "lightgrey", style = "dashed"),
                 cell_text(
                   font = google_font("Teko"),
                   size = px(30))),
    locations = cells_body()
  ) %>%
  tab_header(
    title = md("**Probabilities for Tigres UANL vs. Guadalajara: Liga MX Final First Leg Prediction**"),
    subtitle = "Data sourced from FBref. Model predictions by @AndresAnalytics."
  ) %>%
  tab_style(
    style = cell_text(color = "#6F7378"),
    locations = cells_title(groups = "subtitle")
  ) %>%
  gt_color_rows(
    Tigres_Win_Probability,
    palette = "ghibli::MarnieLight1", domain = c(0,100)
  ) %>%
  gt_color_rows(
    Draw_Probability,
    palette = "ghibli::MarnieLight2", domain = c(0,100)
  ) %>%
  gt_color_rows(
    Guadalajara_Win_Probability,
    palette = "ghibli::LaputaLight", domain = c(0,100)
  ) %>%
  cols_width(
    Tigres_Win_Probability ~ px(150),
    Draw_Probability ~ px(150),
    Guadalajara_Win_Probability ~ px(150)
  ) %>%
  tab_options(
    heading.title.font.size = px(25),
    heading.align = "left",
    heading.padding = px(15),
    column_labels.padding = px(20),
    table.border.top.style = "hidden",
    table.border.bottom.color = "black",
    table.border.bottom.width = px(3),
    column_labels.border.top.style = "hidden"
    )

gtsave_extra(probability_plot, filename = "Tigres UANL vs Guadalajara Prediction.png")


