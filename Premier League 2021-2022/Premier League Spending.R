devtools::install_github("JaseZiv/worldfootballR")

library(worldfootballR)

pacman::p_load(tidyverse, polite, scales, ggimage, 
               ggforce, ggtext,
               rvest, glue, extrafont, ggrepel, magick)
loadfonts()

# let's get the transfer balances for the 2020/21 Bundesliga season
team_balances <- tm_team_transfer_balances(country_name = "England", start_year = 2021)


# to save the file as a csv:
write.csv(x= team_balances, file = "premierleague_transfer_balances2020_2021.csv", row.names = FALSE)

# let's start creating our plot:
team_balances %>% 
  # the new two lines use 'mutate()' from the dplyr package to create or change new columns
  # here, we create a net_transfer_income column that subtracts the expenditure form income
  mutate(net_transfer_income = income_euros - expenditure_euros) %>% 
  # and we can also come up with a flag for if the income is above or below 0
  mutate(green = net_transfer_income > 0) %>% 
  # ggplot is how we visualise our data
  ggplot(aes(x=net_transfer_income, y= squad, fill = green)) +
  geom_col() +
  # manually select colours to use:
  scale_fill_manual(values = c("darkred", "darkgreen"), name = "Made\nMoney?") +
  # change the data labels on the x-axis to be formatted to currency:
  scale_x_continuous(labels = scales::dollar, name = "Net Transfer Income") +
  # add a title, subtitle and a caption that sources the data:
  labs(title = "Premier League spending in the 2021/22 season *", 
       subtitle ="<b style='color: #DB0007'> Arsenal </b> and <b style='color: #DA0106'> Manchester United </b> are big net spenders this season,<br/> while <b style='color: #670E36'> Aston Villa </b> have made the most money on the transfer market",
       caption = "Data: transfermarkt <br/> Andres Gonzalez, Twitter: @gonzalez_afc") +
  # apply a pre-programmed general theme:
  theme_minimal() +
  # but then we can customise our plot even more - first we make the background black:
  theme(text = element_text(family = "Roboto Condensed"),
        plot.background = element_rect(fill = "antiquewhite"),
        # play around with the x and y gridlines:
        panel.grid.major.x = element_line(colour = "grey20", linetype = 2), panel.grid.minor.x = element_blank(), panel.grid.major.y = element_blank(),
        # change the title and subtitle format
        plot.title = element_markdown(size=28,face="bold"), plot.subtitle = element_markdown(size=18),
        # and change where the plot is aligned - in this case it's left-aligned
        plot.title.position = "plot", plot.caption.position = "plot",
        # change the colour and size of aixs titles and text, remove the y-axis title
        axis.title.x = element_text(size=16), axis.title.y = element_blank(), axis.text = element_text(size = 14),
        # remove the legend
        legend.position = "none",
        #format the plot caption:
        plot.caption = element_markdown(size = 12))  

# Save the plot
ggsave(
       here::here("C:/Users/16267/Downloads/premierleague_spending.png"),
       height = 10, width = 12)
