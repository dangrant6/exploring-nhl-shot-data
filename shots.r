#packages
library(tidyverse)
library(ggthemes)
library(ggridges)
library(flexclust)
library(ggdendro)

#read data (MoneyPuck.com shotdata)
playoff_shot_data = read.csv('C:/Users/Daniel/Desktop/shots_2021.csv') %>%
  dplyr::filter(isPlayoffGame == 1)

#data I'm using
playoff_shot_data = playoff_shot_data %>%
  dplyr::select(
  #Shooter Info:
  shooterPlayerId, shooterName, team, shooterLeftRight, 
  shooterTimeOnIce, shooterTimeOnIceSinceFaceoff,
  #Shot info:
  event, location, shotType, shotAngle, shotAnglePlusRebound, 
  shotDistance, shotOnEmptyNet, shotRebound, shotRush, 
  shotWasOnGoal, shotGeneratedRebound, shotGoalieFroze,
  #Adjusted for arena
  arenaAdjustedShotDistance, 
  arenaAdjustedXCord, arenaAdjustedYCord,
  #Goalie info:
  goalieIdForShot, goalieNameForShot,
  #Team context:
  teamCode, isHomeTeam, homeSkatersOnIce, awaySkatersOnIce,
  #Game context:
  game_id, homeTeamCode, awayTeamCode, homeTeamGoals,
  awayTeamGoals, time, period)
  
#exploring data structure
dim(playoff_shot_data)
#amount of shots
head(playoff_shot_data)
colnames(playoff_shot_data)
#Categorical variables: shooterName, event, shotGeneratedRebound
#Continuous variables: shooterTimeOnIce, shotAngle, shotDistance, time

#each row is a shot

#theme
theme_cust <- function(){
  theme_few()+ theme(
    plot.title = element_text(size = 18, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 8, hjust = 0.5),
    axis.title.x = element_text(size=12),
    axis.title.y = element_text(size=12)
  )
}
#exploring shooter time on ice and shots taken
#hypothesis: players shoot more the fresher they are ice time wise(just got on ice vs end of shift)
playoff_shot_data %>%
  filter(shooterTimeOnIce <= 150) %>%
  ggplot(aes(x = shooterTimeOnIce)) +
  geom_density(fill = "purple", alpha = 0.7) +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.25), linetype = "dashed") +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.50), linetype = "dashed") +
  geom_vline(xintercept = quantile(playoff_shot_data$shooterTimeOnIce,0.75), linetype = "dashed") +
  theme_cust() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Player's Time on Ice",
       y = "Density",
       title = "Player's Shoot Mostly Between 16 and 42 Seconds On Ice")
  
# #of shots and goals vs period

#Hypothesis: The current period would affect how many shots are taken 
#ie. teams would be aggressive at the start of games take more shots 

playoff_shot_data %>%
  filter(time<=3600) %>% 
  ggplot(aes(x = time)) + 
  geom_histogram(aes(fill = event)) +
  theme_cust() +
  scale_fill_brewer(palette = "Dark2") +
  geom_vline(xintercept = 1200) +
  geom_vline(xintercept = 2400) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  facet_grid(event ~., margins = TRUE, scales = "free_y") +
  labs(x = "Time (seconds)",
       y = "Count",
       title = "Different Event Frequencies",
       subtitle = "GOAL = goal, MISS = missed net, SHOT = shot on target") 

# What's more common, shots closer to the goal or further out?
#Hypothesis: There are more shots closer to the goal than further out

playoff_shot_data %>%
  ggplot(aes(x=shotDistance))+
  geom_density(fill = "red", alpha = 0.7)+
  theme_cust()+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  labs(x = "Shot Distance",
       y = "Density",
       title = "Most common shot distance")
#Conclusion: There are more shots closer to the goal (10-15ft)

# Clustering with player's stats ------------------------------
#getting our player-level data ready for clustering
player_shot_stats <- playoff_shot_data %>%
  group_by(shooterName) %>%
  summarize(shots = n(),
            avg_shot_distance = mean(shotDistance, na.rm = T),
            avg_shot_angle = mean(shotAngle, na.rm = T)) %>%
  filter(shots >= 10)
#creating a basic clustering algorithm
init_kmeans <- 
  kmeans(dplyr::select(player_shot_stats, avg_shot_distance, avg_shot_angle),
         algorithm = "Hartigan-Wong", centers = 4, nstart = 100)
#plotting the basic clustering algorithm
player_shot_stats %>%
  mutate(player_clusters = as.factor(init_kmeans$cluster)) %>%
  ggplot(aes(x = avg_shot_distance, y = avg_shot_angle,
             color = player_clusters)) +
  geom_jitter(alpha = 0.5, size = 3) +
  theme_bw() +
  ggthemes::scale_color_colorblind()
#kmeans++ 
nhl_kmeanspp <- 
  kcca(dplyr::select(player_shot_stats,
                     avg_shot_distance, avg_shot_angle), k = 4,
       control = list(initcent = "kmeanspp"))
#plotting
player_shot_stats %>%
  mutate(Cluster = 
           as.factor(nhl_kmeanspp@cluster)) %>% #must use the @ symbol 
  ggplot(aes(x = avg_shot_distance, y = avg_shot_angle,
             color = Cluster)) +
  geom_jitter(alpha = 0.7, size = 3) + 
  scale_fill_brewer(palette = "Set1") +
  theme_cust() +
  labs(x = "Average Shot Distance",
       y = "Average Shot Angle",
       title = "NHL Player Clustering Based on Shot Distance and Shot Angle",
       fill = "Cluster") +
  theme(legend.position = "bottom")

#hierarchical clustering
player_sdist <- dist(dplyr::select(player_shot_stats,
                                         avg_shot_distance, avg_shot_angle))

complete_hclust <- hclust(player_sdist, method = "complete")

ggdendrogram(complete_hclust, theme_dendro = FALSE,
             labels = FALSE, leaf_labels = FALSE) +
  labs(y = "Dissimilarity between clusters") +
  theme_bw() +
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_blank())
player_shot_stats %>%
  mutate(Cluster = as.factor(cutree(complete_hclust, k = 5))) %>%
  ggplot(aes(x = avg_shot_distance, y = avg_shot_angle,
             color = Cluster)) +
  geom_jitter(alpha = 0.6, size = 3) + 
  scale_fill_brewer(palette = "Set1") +
  theme_cust() +
  labs(x = "Average Shot Distance",
       y = "Average Shot Angle",
       title = "NHL Player Clustering Based on Shot Distance and Shot Angle",
       fill = "Cluster") +
  theme(legend.position = "bottom")
