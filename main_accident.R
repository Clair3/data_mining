library(readr)
library(ggplot2)
library(RgoogleMaps)
library(sp)
library(maps)
library(ggmap)
library(tidyr)
library(rpart)

# PCA to select features

# -------------------------------- DATA PREPARATION

caracteristiques_2005_to_2016 <- read.csv("caracteristics.csv", header = TRUE, 
                                          stringsAsFactors = FALSE, na.strings=c("NA", "NULL"))
caracteristiques_2005_to_2016 <- subset(caracteristiques_2005_to_2016, 
                                        select = c(-gps, -adr))
caracteristiques_2017 <- read.csv("caracteristiques-2017.csv", header = TRUE, 
                                  stringsAsFactors = FALSE, na.strings=c("NA", "NULL"))
caracteristiques_2017 <- subset(caracteristiques_2017, select = c(-gps, -adr))
caracteristiques_2018 <- read.csv("caracteristiques-2018.csv", header = TRUE, 
                                  stringsAsFactors = FALSE, na.strings=c("NA", "NULL"))
caracteristiques_2018 <- subset(caracteristiques_2018, select = c(-gps, -adr))
caracteristiques_2019 <- read.csv("caracteristiques-2019.csv", sep = ";", dec = ",",
                                 header = TRUE, stringsAsFactors = FALSE, 
                                  na.strings=c("NA", "NULL"))
caracteristiques_2019 <- subset(caracteristiques_2019, select = -adr)
caracteristiques_2019$an <- caracteristiques_2019$an - 2000

caracteristics <- rbind(caracteristiques_2005_to_2016, caracteristiques_2017)
caracteristics <- rbind(caracteristics, caracteristiques_2018)
caracteristics <- rbind(caracteristics, caracteristiques_2019)
caracteristics_original <- caracteristics


places <- read_csv("places.csv")
lieux_2017 <- read_csv("lieux-2017.csv")
lieux_2018 <- read_csv("lieux-2018.csv")
lieux_2019 <- read.csv("lieux-2019.csv", sep = ";", dec = ",",
                       header = TRUE, stringsAsFactors = FALSE, 
                       na.strings=c("NA", "NULL"))
#lieux_2019 <- read_csv("lieux_2019.csv")
places <- rbind(places, lieux_2017)
places <- rbind(places, lieux_2018)
places <- rbind(places, lieux_2019)
places <- subset(places, select = c(-v1, -v2, -pr, -pr1))



# ------------------------------ Accidents visualization on Saint-Etienne

# Nettoyage des données
caracteristics <- caracteristics %>% drop_na()
caracteristics <- caracteristics[caracteristics["lat"] > 40,]
caracteristics$lat <- as.numeric(as.character(caracteristics$lat)) * 10**-5
caracteristics$long <- as.numeric(as.character(caracteristics$long)) * 10**-5 
caracteristics <- rbind(caracteristics, caracteristiques_2019)
caracteristics$an <- as.factor(caracteristics$an)


register_google("AIzaSyCEQfR8eA-C3j1ZZWuALHBZjCvjdaZsvqQ")

ste_map <- get_map(location="saint-etienne, france", maptype = "satellite",
                           zoom = 14)

ggmap(ste_map) +
  geom_point(aes(x = long, y = lat, col = an),
             data=caracteristics)

# ------------------------------- Accidents hors agglomeration --------------------------

caracteristics <- caracteristics_original
caracteristics$an <- as.factor(caracteristics$an)

places_test = places[places["catr"] == c(2, 3),] 

caracteristics_hors_agg = caracteristics[places["catr"] == 2 || 3,]
caracteristics_hors_agg = caracteristics[caracteristics["agg"] == 1,]

acc_by_month = aggregate(caracteristics_hors_agg$Num_Acc, by=list(month=caracteristics_hors_agg$mois, year=caracteristics_hors_agg$an), FUN=length)
print(acc_by_month)

ggplot(acc_by_month, aes(x = month,
                         y = x,
                         col = year)) +     
  geom_point()


acc_by_month = aggregate(caracteristics$Num_Acc, by=list(month=caracteristics$mois, year=caracteristics$an), FUN=length)
print(acc_by_month)
plot(acc_by_month$x, type="l")

plot(acc_by_month$month, acc_by_month$x, col =" red ")

ggplot(acc_by_month,                                      # Grouped barplot using ggplot2
       aes(x = month,
           y = x,
           fill = sexe)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  scale_fill_discrete( labels = c("H", "F")) +
  labs(x = "Gravity accident", y = "Number of accidents") +
  scale_y_continuous(limits=c(0,600000)) +
  scale_x_discrete(breaks=c("1","2","3", "4"),
                   labels=c("uninjured", "slightly injured", "seriously injured", "dead"))


# --------------------------------------- Accident en fonction de l'age

acc_by_years = aggregate(users$Num_Acc, by=list(year=users$an_nais), FUN=length)
print(acc_by_years)
plot(acc_by_years$x, type="l", )
plot(x = acc_by_years$year, y = acc_by_years$x, type="l")

acc_by_years = aggregate(users$Num_Acc, by=list(year=users$an_nais), FUN=length)
print(acc_by_years)
plot(acc_by_years$x, type="l", )
plot(x = acc_by_years$year, y = acc_by_years$x, type="l")


# ----------------------------------------  Variation du nombre de mort par sexe


acc_by_sexe = aggregate(users$Num_Acc, by=list(sexe=as.factor(users$sexe), grav=as.factor(users$grav)), FUN=length)
acc_by_sexe_tot = aggregate(users$Num_Acc, by=list(sexe=as.factor(users$sexe)), FUN=length)
acc_by_sexe = rbind(acc_by_sexe, acc_by_sexe_tot)
print(acc_by_sexe_tot)
ggplot(acc_by_sexe,                                      # Grouped barplot using ggplot2
       aes(x = grav,
           y = x,
           fill = sexe)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  scale_fill_discrete( labels = c("H", "F")) +
  labs(x = "Gravity accident", y = "Number of accidents") +
  scale_y_continuous(limits=c(0,600000)) +
  scale_x_discrete(breaks=c("1","2","3", "4"),
                   labels=c("uninjured", "slightly injured", "seriously injured", "dead"))
  #scale_x_discrete(limits=c("uninjured", "slightly injured", "seriously injured", "dead", "A",  "dead", "A", "D"))

# ------------------------------------ DECISION TREE


users <- read_csv("users.csv")
usagers_2017 <- read_csv("usagers-2017.csv")
usagers_2018 <- read_csv("usagers-2018.csv")
# usagers_2019 <- read_csv("usagers-2019.csv")
users <- rbind(users, usagers_2017)
users <- rbind(users, usagers_2018)

users <- subset(users, select = c(-actp, -etatp, -num_veh))


set.seed(2568)
n <- nrow(users)

train <- sort(sample(1:n, floor(n*0.8)))
users.train <- users[train ,]
users.test <- users[-train ,]

rtree <- rpart(grav ~. ,
                  data = users[c(1,2,4:9)],
                  subset = train,
                  method = "class",
                  parms = list(split = "information"),
                  maxsurrogate= 0,
                  maxdepth= 5,
                  cp = 0, 
                  minsplit = 100,
                  minbucket = 4)

plot(rtree)
text(rtree)

# --------------------------- Influence de la place 

users <- users[users["place"] > 0,]
acc_by_place = aggregate(users$Num_Acc, by=list(place=as.factor(users$place), grav=as.factor(users$grav)), FUN=length)
acc_by_place_tot = aggregate(users$Num_Acc, by=list(place=as.factor(users$place)), FUN=length)
ggplot(acc_by_place,                                      # Grouped barplot using ggplot2
       aes(x = grav,
           y = x,
           fill = place)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  #scale_fill_discrete( labels = c("H", "F")) +
  labs(x = "Gravity accident", y = "Number of accidents") +
  scale_y_continuous(limits=c(0,600000)) +
  scale_x_discrete(breaks=c("1","2","3", "4"),
                   labels=c("uninjured", "slightly injured", "seriously injured", "dead"))

# ----------- Gravity taking into account the hours

total <- merge(users, caracteristics, by = c("Num_Acc"))

acc_by_hours = aggregate(total$Num_Acc, by=list(grav=as.factor(total$grav), hours=as.integer(as.numeric(total$hrmn) * 10**-2)), FUN=length)

print(acc_by_hours)
ggplot(acc_by_hours,                                      # Grouped barplot using ggplot2
         aes(x = hours,
             y = x,
             fill = grav)) +
  geom_bar(stat = "identity",
           position = "dodge2") +
  #scale_fill_discrete( labels = c("H", "F")) +
  labs(x = "Gravity accident", y = "Number of accidents") +
  scale_y_continuous(limits=c(0,90000)) +
  scale_x_discrete(breaks=waiver(),
    labels=acc_by_hours$hours)

# -------------- PCA
total <- merge(users, caracteristics, by = c("Num_Acc"))
total <- merge(total, places, by = c("Num_Acc"))
total <- subset(total, select = c(-locp, -actp, -etatp, -num_veh, -an, -mois, -jour, -lat, -long, -env1))

set.seed(2568)
n <- nrow(total)

train <- sort(sample(1:n, floor(n*0.8)))
total.train <- total[train ,]
total.test <- total[-train ,]

rtree <- rpart(grav ~. ,
               data = total[c(2:9)],
               method = "class",
               subset = train,
               parms = list(split = "information"),
               maxsurrogate= 0,
               maxdepth= 5,
               cp = 0, 
               minsplit = 100,
               minbucket = 4)

plot(rtree)
text(rtree)

              



