library(tidyverse)

file.game <- "https://raw.githubusercontent.com/jvcarolin/BDBdat/master/Data/games.csv"
games.sum <- read_csv(file.game)

file.plays <- "https://raw.githubusercontent.com/jvcarolin/BDBdat/master/Data/plays.csv"
plays.sum <- read_csv(file.plays)

file.players <- "https://raw.githubusercontent.com/jvcarolin/BDBdat/master/Data/players.csv"
players.sum <- read_csv(file.players)

file.tracking <- "https://raw.githubusercontent.com/jvcarolin/BDBdat/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)


xp.plays <- tracking.example %>%
  filter(event == "extra_point_attempt") %>%
  pull(playId) %>%
  unique()

tracking.example %>%
  filter(playId %in% xp.plays, team == "ball") %>%
  ggplot(aes(x, y)) +
  geom_point() +
  scale_y_continuous(lim = c(0,160/3)) +
  scale_x_continuous(lim = c(0, 120)) +
  facet_wrap(~playId) +
  labs(title = "Ball tracking on extra point plays")

tracking.example %>%
  mutate(x.new = ifelse(x < 60, 120 - x, x)) %>%
  filter(playId %in% xp.plays, team == "ball") %>%
  ggplot(aes(x.new, y)) +
  geom_point() +
  scale_y_continuous(lim = c(0,160/3)) +
  scale_x_continuous(lim = c(0, 120)) +
  facet_wrap(~playId) +
  labs(title = "Ball tracking on extra point plays, all kicks left to right")


kicker.ids <- players.sum %>% filter(PositionAbbr == "K") %>% pull(nflId)
kicker.play <- tracking.example %>%
  filter(playId %in%  xp.plays, nflId %in% kicker.ids) %>%
  select(displayName, playId) %>% unique()

tracking.example %>%
  filter(playId %in% xp.plays, team == "ball") %>%
  mutate(x.new = ifelse(x < 60, 120 - x, x),
         dist.120 = abs(x.new - 120)) %>%
  group_by(playId) %>%
  slice(which.min(dist.120)) %>%
  select(x, x.new, y, gameId, playId, dist.120) %>%
  left_join(kicker.play)


get_kicks <- function(gameId) {
  message(paste("reading", gameId, "data..."))
  file.read <- paste0("https://raw.githubusercontent.com/jvcarolin/BDBdat/master/Data/tracking_gameId_", gameId, ".csv")
  tracking.example <- suppressMessages(read_csv(file.read))
  
  xp.plays <- tracking.example %>%
    filter(event == "extra_point_attempt") %>%
    pull(playId) %>%
    unique()
  
  kicker.play <- tracking.example %>%
    filter(playId %in% xp.plays, nflId %in% kicker.ids) %>%
    select(displayName, playId) %>% unique()
  
  df.out <- tracking.example %>%
    filter(playId %in% xp.plays, team == "ball") %>%
    mutate(x.new = ifelse(x < 60, 120 - x, x),
           dist.120 = abs(x.new - 120)) %>%
    group_by(playId) %>%
    slice(which.min(dist.120)) %>%
    select(x, x.new, y, gameId, playId, dist.120) %>%
    left_join(kicker.play)
  
  return(df.out = df.out)
}

game.Ids <- games.sum %>% select(gameId) %>% pull()
df.kicks.out <- lapply(game.Ids, get_kicks)

kicks.all <- df.kicks.out %>%
  bind_rows() %>%
  mutate(kick.target = (160/3)/2,
    distance.off = y - kick.target,
    distance.off.abs = abs(distance.off))

upright.width <- 6.166667
kicks.all %>%
  group_by(displayName) %>%
  summarise(med.off = median(distance.off)) %>%
  right_join(kicks.all) %>%
  ggplot(aes(x = distance.off, y = fct_reorder(displayName, med.off))) +
  geom_vline(aes(xintercept = upright.width), color = "yellow", lwd = 2, alpha = 0.6) +
  geom_vline(aes(xintercept = -1*upright.width), color = "yellow", lwd = 2, alpha = 0.6) +
  geom_vline(aes(xintercept = 0)) +
  geom_vline(aes(xintercept = median(med.off)), color = "red", lwd = 1, alpha = 0.6) +
  xlim(c(-20, 20)) +
  geom_point() +
  geom_point(aes(x = med.off), pch = "|", color = "red") +
  theme_minimal() +
  labs(title = "Ball location at crossing of uprights",
       subtitle = "Kicker median coded in red",
       x =" Left of center                   Right of center", y = "")
