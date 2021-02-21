
# Return strength of schedule according to games played so far. Created with the explanation from 
# \href{https://hackastat.eu/en/learn-a-stat-strength-of-schedule-sos/}{Hackastat}
#
#' @name schedule_of_strength
#' @title schedule_of_strength
#' @param df dataset that includes scores between teams, game number of the match and winner team. 
#' @return Returns strength of schedule of each team so far. Low value means, teams played with easier opponents so far, while high value indicated opposite.
#'
#' @export
#
schedule_of_strength <- function(scores) {
  df <- data.frame()
  for (i in unique(scores$TeamA)) {
    team_df <-  scores %>% filter(TeamA == i| TeamB == i) %>% arrange(gamenumber) %>% ungroup()%>% mutate(total_games =n(),team= i,win_count=sum(Winner==i),win_perc = round(win_count / total_games,2))
    df <- rbind(df,team_df)
    
  }
  ow_perc_df <- data.frame()
  for (i in unique(df$team)) {
    ow_mean <-  df %>% filter(team!=i) %>% group_by(team) %>% ungroup()%>%summarize(ow_perc = mean(win_perc)) %>%  unlist(use.names = FALSE)
    ow_df <- data.frame("Team"=i,"ow_perc" = ow_mean)
    ow_perc_df <- rbind(ow_df,ow_perc_df)    
  }
  df <- df %>% mutate(opponent = ifelse(TeamA==team,TeamB,TeamA))
  games_played <- df %>% group_by(team,opponent) %>% summarize(games_played = n(), .groups = 'drop')
  ow_perc_df$oow_perc <- ow_perc_df$ow_perc
  df <- df %>% left_join(games_played, c('team'='team','opponent'='opponent')) %>% left_join(ow_perc_df %>% select(Team,ow_perc),c('team'='Team')) %>% left_join(ow_perc_df %>% select(Team,oow_perc),c('opponent'='Team')) 
  df_oow_perc <- df %>% mutate(oow_perc = games_played * oow_perc)%>% group_by(team) %>% summarize(oow_final = sum(oow_perc) / total_games, .groups = 'drop') %>% distinct_all()
  
  df <- df %>% left_join(df_oow_perc,c('team'='team'))
  df <- df%>% group_by(team) %>% summarize(sos= (2*ow_perc + oow_final)/3, .groups = 'drop') %>% distinct_all()%>% arrange(sos)
  
  return(df)
}