# Description -------------------------------------------------------------

# This R file includes data manipulation.
# Data are taken from the database file and manipulated.
# After data manipulation, tidy data writes to the database file.
# When you click the "Become A Legend" button, tidy data will import the dashboard.
# The data is between 2016 to 2020. 
# The datasets contains all the statistics and playing attributes of all the players in the full version of FIFA Series.
# There is not a League variable in the datasets. I created a League variable.
# Value and Wage variables are categorical variables. I transformed thesee variables to continuous variables.
# There are some missing values in Preferred Foot variable.
# Player has a specific position. Position Class variable is added. The Class variable contains GK, DEF, MID and FORW classes. 
# Measurement Unit is changed to KG (Weight) and CM (Height).


# 1. FIFA 19 Data Import --------------------------------------------------

temp <- read.csv("FifaDash/database/data.csv", encoding = "UTF-8")[-1]

# 2. Create Leagues -------------------------------------------------------

# Subset Teams #

bundesliga <- c(
  "1. FC Nürnberg", "1. FSV Mainz 05", "Bayer 04 Leverkusen", "FC Bayern München",
  "Borussia Dortmund", "Borussia Mönchengladbach", "Eintracht Frankfurt",
  "FC Augsburg", "FC Schalke 04", "Fortuna Düsseldorf", "Hannover 96",
  "Hertha BSC", "RB Leipzig", "SC Freiburg", "TSG 1899 Hoffenheim",
  "VfB Stuttgart", "VfL Wolfsburg", "SV Werder Bremen"
)

premierLeague <- c(
  "Arsenal", "Bournemouth", "Brighton & Hove Albion", "Burnley",
  "Cardiff City", "Chelsea", "Crystal Palace", "Everton", "Fulham",
  "Huddersfield Town", "Leicester City", "Liverpool", "Manchester City",
  "Manchester United", "Newcastle United", "Southampton", 
  "Tottenham Hotspur", "Watford", "West Ham United", "Wolverhampton Wanderers"
  
)

laliga <- c(
  "Athletic Club de Bilbao", "Atlético Madrid", "CD Leganés",
  "Deportivo Alavés", "FC Barcelona", "Getafe CF", "Girona FC", 
  "Levante UD", "Rayo Vallecano", "RC Celta", "RCD Espanyol", 
  "Real Betis", "Real Madrid", "Real Sociedad", "Real Valladolid CF",
  "SD Eibar", "SD Huesca", "Sevilla FC", "Valencia CF", "Villarreal CF"
)

seriea <- c(
  "Atalanta","Bologna","Cagliari","Chievo Verona","Empoli", "Fiorentina","Frosinone","Genoa",
  "Inter","Juventus","Lazio","Milan","Napoli","Parma","Roma","Sampdoria","Sassuolo","SPAL",
  "Torino","Udinese"
  
)

superlig <- c(
  "Akhisar Belediyespor","Alanyaspor", "Antalyaspor","Medipol Başakşehir FK","BB Erzurumspor","Beşiktaş JK",
  "Bursaspor","Çaykur Rizespor","Fenerbahçe SK", "Galatasaray SK","Göztepe SK","Kasimpaşa SK",
  "Kayserispor","Atiker Konyaspor","MKE Ankaragücü", "Sivasspor","Trabzonspor","Yeni Malatyaspor"
)

ligue1 <- c(
  "Amiens SC", "Angers SCO", "AS Monaco", "AS Saint-Étienne", "Dijon FCO", "En Avant de Guingamp",
  "FC Nantes", "FC Girondins de Bordeaux", "LOSC Lille", "Montpellier HSC", "Nîmes Olympique", 
  "OGC Nice", "Olympique Lyonnais","Olympique de Marseille", "Paris Saint-Germain", 
  "RC Strasbourg Alsace", "Stade Malherbe Caen", "Stade de Reims", "Stade Rennais FC", "Toulouse Football Club"
)

eredivisie <- c(
  "ADO Den Haag","Ajax", "AZ Alkmaar", "De Graafschap","Excelsior","FC Emmen","FC Groningen",
  "FC Utrecht", "Feyenoord","Fortuna Sittard", "Heracles Almelo","NAC Breda",
  "PEC Zwolle", "PSV","SC Heerenveen","Vitesse","VVV-Venlo","Willem II"
)

liganos <- c(
  "Os Belenenses", "Boavista FC", "CD Feirense", "CD Tondela", "CD Aves", "FC Porto",
  "CD Nacional", "GD Chaves", "Clube Sport Marítimo", "Moreirense FC", "Portimonense SC", "Rio Ave FC",
  "Santa Clara", "SC Braga", "SL Benfica", "Sporting CP", "Vitória Guimarães", "Vitória de Setúbal"
)


# Leagues
temp %<>% mutate(League = if_else(Club %in% bundesliga, "Bundesliga",
                                  if_else(Club %in% premierLeague, "Premier League", 
                                          if_else(Club %in% laliga, "La Liga", 
                                                  if_else(Club %in% seriea, "Serie A", 
                                                          if_else(Club %in% superlig, "Süper Lig", 
                                                                  if_else(Club %in% ligue1, "Ligue 1", 
                                                                          if_else(Club %in% eredivisie, "Eredivisie",
                                                                                  if_else(Club %in% liganos, "Liga Nos", NA_character_)))))))),
                 
                 Country = if_else(League == "Bundesliga", "Germany",
                                   if_else(League == "Premier League", "UK",
                                           if_else(League == "La Liga", "Spain", 
                                                   if_else(League == "Serie A", "Italy", 
                                                           if_else(League == "Süper Lig", "Turkey", 
                                                                   if_else(League == "Ligue 1", "France", 
                                                                           if_else(League == "Liga Nos", "Portugal", 
                                                                                   if_else(League == "Eredivisie", "Netherlands", NA_character_))))))))) %>% 
  filter(!is.na(League)) %>% mutate_if(is.factor, as.character())


rm(bundesliga, premierLeague, laliga, seriea, superlig, ligue1, eredivisie, liganos)

# 3. Value & Wage ---------------------------------------------------------

# String Manipulation #

# Player Value
temp$Values <- str_remove_all(temp$Value,"€")
temp$Values <- str_replace_all(temp$Values,"K", "000")
temp$Values <- str_remove_all(temp$Values,"M")

temp$Values <- as.numeric(temp$Values)

# Player Wage
temp$Wages <- str_remove_all(temp$Wage,"€")
temp$Wages <- str_replace_all(temp$Wages,"K", "000")

temp$Wages <- as.numeric(temp$Wages)

temp <- temp  %>% mutate(Values = if_else(temp$Values < 1000 , Values * 1000000, Values))



# 4. Preferred Foot -------------------------------------------------------


# Preferred Foot #

temp %<>% filter(Preferred.Foot %in% c("Left", "Right")) 
temp$Preferred.Foot <- as.factor(as.character(temp$Preferred.Foot))



# 5. Position Class -------------------------------------------------------

# Create Position Class #
defence <- c("CB", "RB", "LB", "LWB", "RWB", "LCB", "RCB")
midfielder <- c("CM", "CDM","CAM","LM","RM", "LAM", "RAM", "LCM", "RCM", "LDM", "RDM")

temp %<>% mutate(Class = if_else(Position %in% "GK", "Goal Keeper",
                                 if_else(Position %in% defence, "Defender",
                                         if_else(Position %in% midfielder, "Midfielder", "Forward"))))

rm(defence, midfielder)


# 6. Name + Position ------------------------------------------------------

# Name + Position #

# Example: S. Agüero, ST
temp <- temp %>% arrange(Name) %>% mutate(Name.Pos = paste0(as.character(Name),", ", as.character(Position))) 


# 7. Height & Weight ------------------------------------------------------

# Height & Weight #

# From categorical to numeric
temp %<>%
  mutate(Height = round((as.numeric(str_sub(Height, start=1,end = 1))*30.48) + (as.numeric(str_sub(Height, start = 3, end = 5))* 2.54)),
         Weight = round(as.numeric(str_sub(Weight, start = 1, end = 3)) / 2.204623))




# 8. Rename Variables ----------------------------------------------------

temp %<>% 
  rename(
    "Heading.Accuracy"= HeadingAccuracy,
    "Short.Passing"= ShortPassing,
    "FK.Accuracy" = FKAccuracy,
    "Long.Passing"= LongPassing,
    "Ball.Control"= BallControl,
    "Sprint.Speed"= SprintSpeed,
    "Shot.Power"= ShotPower,
    "Long.Shots"= LongShots,
    "Standing.Tackle"= StandingTackle,
    "Sliding.Tackle"= SlidingTackle,
    "GK.Diving"= GKDiving,
    "GK.Handling"= GKHandling,
    "GK.Kicking"= GKKicking,
    "GK.Positioning"= GKPositioning,
    "GK.Reflexes"= GKReflexes
  )


# 9. Other FIFA Series ---------------------------------------------------

# Read Other FIFA Series & Manipulation #

f16 <- read.csv("FifaDash/database/players_16.csv", encoding = "UTF-8")
f17 <- read.csv("FifaDash/database/players_17.csv", encoding = "UTF-8")
f18 <- read.csv("FifaDash/database/players_18.csv", encoding = "UTF-8")
f20 <- read.csv("FifaDash/database/players_20.csv", encoding = "UTF-8")

f16_p <- f16 %>% select(sofifa_id, short_name, value_eur, club) %>% mutate(fifa = "FIFA 16")
f17_p <- f17 %>% select(sofifa_id, short_name, value_eur, club) %>% mutate(fifa = "FIFA 17")
f18_p <- f18 %>% select(sofifa_id, short_name, value_eur, club) %>% mutate(fifa = "FIFA 18")
f20_p <- f20 %>% select(sofifa_id, short_name, value_eur, club) %>% mutate(fifa = "FIFA 20")

f19 <- temp %>% select(ID, Name, Values, Position, Club)
f19 <- f19 %>% mutate(fifa = "FIFA 19") %>% rename("short_name" = Name,
                                                   "value_eur" = Values,
                                                   "sofifa_id" = ID,
                                                   "club" = Club) %>% distinct()

f <- rbind(f16_p, f17_p, f18_p, f20_p) %>% distinct() %>% 
  left_join((f19 %>% select(sofifa_id, Position)), by = "sofifa_id")
f <- rbind(f, f19)

f <- f %>% mutate(Name.Pos = paste0(as.character(short_name), ", ", as.character(Position))) %>% select(sofifa_id, Name.Pos, value_eur, fifa, club)
f$fifa <- ordered(f$fifa)

# League Team Value Change
f_t <- f %>% select(club, value_eur, fifa) %>% rename("Year" = fifa) %>% distinct()
f_t$Year <- ordered(f_t$Year)

# 10. Remove Unnecessary Variables ----------------------------------------



temp %<>% select(-ID, -Body.Type, -Real.Face, -Joined, -Loaned.From, -Release.Clause) 



# 11. Write Data ----------------------------------------------------------

write.csv(temp, "FifaDash/database/fifa19_data.csv", fileEncoding = "UTF-8")
write.csv(f, "FifaDash/database/fifa_series_players.csv", fileEncoding = "UTF-8")
write.csv(f_t, "FifaDash/database/fifa_series_teams.csv", fileEncoding = "UTF-8")


# 12. Remove Environment --------------------------------------------------

rm(f, f_t, f16, f17, f18, f19, f20, f16_p, f17_p, f18_p, f20_p, temp)



