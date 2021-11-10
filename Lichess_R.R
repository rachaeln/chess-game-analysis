library(ggplot2)
library(tidyverse)

# Importing data

lichess <- readr::read_csv("Desktop/Data Projects/Lichess Data.csv", TRUE)

# Bar chart of opening playing moves -------------------- 

ggplot(lichess, aes(x = opening_ply, y = opening_ply)) + 
  geom_bar(stat = "identity") +
  
  geom_vline(aes(xintercept = signif(mean(opening_ply),1)), color = "red") +
  annotate(x = 15, y = 12000, label = paste("Average number of opening moves =", signif(mean(lichess$opening_ply),1)), vjust = 1, geom = "label", family = "Optima") +
  
  theme(text = element_text(family = "Optima")) +
  labs(title = "Opening play",
       subtitle = "The opening is paramount for developing pieces and taking control of the board.",
       x = "Number of opening moves",
       y = "Number of games") +
  
  theme(plot.title = element_text(face = "bold", size = 15)) + 
  theme(plot.subtitle = element_text(face = "italic"))
  
# Pie chart of winners (black, white, draw) -------------------- 

# table(lichess$winner)

# black  draw white 
# 9107   950 10001 

# Pie data
pie_data <- data.frame(
  group = c("Black — 9107", "White — 10,001", "Draw — 950"),
  value = c(9107, 10001, 950)
)

# Pie chart
ggplot(pie_data, aes(x = "", y = value, fill = group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) + 
  
  scale_fill_grey() +
  scale_color_discrete("") +
  theme_void() +
  
  theme(text = element_text(family = "Optima")) +
  labs(title = "Winners", 
       subtitle = "White usually has the winning advantage since it goes first.",
       fill = "") +
  
  theme(plot.title = element_text(face ="bold", size = 15)) +
  theme(plot.subtitle = element_text(face = "italic"))


# Bar chart of victory status (resign, mate, out of time, draw) -------------------- 

# table(lichess$victory_status)

# draw      mate   out of time    resign 
# 906      6325      1680         11147

# Bar data
victory_bar <- data.frame(
  name = c("Draw", "Mate", "Out of Time", "Resign"),
  value = c(906, 6325, 1680, 11147)
)

# Bar chart
ggplot(victory_bar, aes(x = reorder(name, value), y = value)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  
  theme(text = element_text(family = "Optima")) +
  labs(title = "Victory status",
       subtitle = "A chess game is won by either mate, resigning, or running out of time. \nIn a draw, neither side wins.",
       x = "",
       y = "Number of games") +
  
  geom_text(aes(label = value), vjust = .5, hjust = 1.5, color = "white", family = "Optima") +
  
  theme(plot.title = element_text(face = "bold", size = 15)) +
  theme(plot.subtitle = element_text(face = "italic"))
  

# Histogram of total turns per game --------------------

ggplot(lichess, aes(x = turns)) +
  geom_histogram(binwidth = 10) +
  
  geom_vline(aes(xintercept = signif(mean(turns),1)), color = "red") +
  annotate(x = 150, y = 2500, label = paste("Average number of turns =", signif(mean(lichess$turns),1)), vjust = 1, geom = "label", family = "Optima") +

  theme(text = element_text(family = "Optima")) +
  labs(title = "Total turns in a game",
       x = "Number of turns",
       y = "Number of games") +
  
  theme(plot.title = element_text(face = "bold", size = 15))


# Histogram of turns it takes black to win (via mate) --------------------

black_mate <- lichess %>% 
  filter(winner == "black" & victory_status == "mate")

ggplot(black_mate, aes(x = turns)) +
  geom_histogram(binwidth = 10) +
  
  geom_vline(aes(xintercept = signif(mean(black_mate$turns),2)), color = "red") +
  annotate(x = 150, y = 350, label = paste("Average number of turns to checkmate =", signif(mean(black_mate$turns),2)), vjust = 1, geom = "label", family = "Optima") +
  
  theme(text = element_text(family = "Optima")) +
  labs(title = "Total turns it takes for black to checkmate",
       subtitle = "Checkmate (often shortened to mate) is a game position in which a player's king is in check (threatened with capture) and there is no possible escape. \nCheckmating the opponent wins the game.",
       x = "Number of turns",
       y = "Number of games",
       caption = "Credit: https://en.wikipedia.org/wiki/Checkmate") +
  
  theme(plot.title = element_text(face = "bold", size = 15)) + 
  theme(plot.subtitle = element_text(face="italic")) +
  theme(plot.caption = element_text(face="italic"))


# Histogram of turns it takes for white to win (via mate) --------------------

white_mate <- lichess %>% 
  filter(winner == "white" & victory_status == "mate")

ggplot(white_mate, aes(x = turns)) +
  geom_histogram(binwidth = 10) +
 
  geom_vline(aes(xintercept = signif(mean(white_mate$turns),2)), color = "red") +
  annotate(x = 140, y = 350, label = paste("Average number of turns to checkmate =", signif(mean(white_mate$turns),2)), vjust = 1, geom = "label", family = "Optima") +
  
  theme(text = element_text(family = "Optima")) +
  labs(title = "Total turns it takes for white to checkmate",
       subtitle = "It takes black an average of 67 moves to checkmate compared to white's 64 moves.",
       x = "Number of turns",
       y = "Number of games") +
  
  theme(plot.title = element_text(face = "bold", size = 15)) + 
  theme(plot.subtitle = element_text(face = "italic")) +
  theme(plot.caption = element_text(face = "italic"))
