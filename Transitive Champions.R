library(dplyr)

Women_NCAA = read.csv(file.choose(),header = T)

#Calculate score difference for every game
#Double each record so each Team exists in the 'Team' field once

T1 = Women_NCAA %>%
  mutate(Result = Score1 - Score2,
         Team=Team1,
         Opp=Team2) %>%
  select(Date,
         Team,
         Opp,
         Result)

T2 = Women_NCAA %>%
  mutate(Result = Score2 - Score1,
         Team=Team2,
         Opp=Team1) %>%
  select(Date,
         Team,
         Opp,
         Result)

Mod_Data = rbind(T1,T2)

#Create data frame with all champions
trans_champ = data.frame(Champ="Baylor")

#Starting values for conditions of while loop 
y=1
x=0
#Keep running loop until no additional transitive champions are added
while(y>x){
  
  temp = nrow(trans_champ)
  
  winners = Mod_Data %>%
    filter(Opp %in% trans_champ$Champ&
             Result>0) %>%
    select(Team) %>%
    distinct() %>%
    rename(Champ=Team)
  
  trans_champ = rbind(trans_champ,
                      winners) %>%
    distinct(Champ,.keep_all=T)
  
  x=temp
  y=nrow(trans_champ)
  
}

  
