## Install the TidyTuesday package for easy data import
devtools::install_github("thebioengineer/tidytuesdayR")

## Install other packages for working with the data
library(tidyverse)
library(here)
library(ggpubr)

## Read in data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
nz_bird <- tuesdata$nz_bird

## Create Dataset with unique voterID corresponding to what would be each person's ballot.  I knew there were 43460
## individual ballots, so I made a new variable that sequentially numbered up to 43460 in segments of 5 rows.  This
## allowed me to have each row correspond to an individual.

## After the first round tally of first choice votes, this is how I removed birds but retained people's subsequent
## bird choice.  Using the distinct() command on the vote.id column let me retain the highest vote that remianed for each
## voter
nz_bird_id <- nz_bird %>%
  mutate(voter.id = rep(c(1:43460),each=5))

## Tally first choice votes
## Add percentage of vote
nz_bird1 <- nz_bird %>%
  group_by(bird_breed) %>%
  filter(bird_breed != "NA", vote_rank == "vote_1") %>%
  tally()%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  arrange(desc(n))%>%
  head(10)

## Plotting for the first chart  
p1 <- ggplot(nz_bird1, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
        geom_col(show.legend=F)+
        geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
        labs(subtitle="Round 1",
             x="",
             y="Votes")+
        scale_fill_brewer(palette="Set3")+
        ylim(0,12000)+
       theme(panel.background = element_rect(fill = "lightblue",
                                             colour = "lightblue"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(hjust=0.5, color = "black"),
            plot.caption = element_text(color = "black", face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1))

## Which bird gets eliminated after first round? This shows me the 20 lowest vote getters in the round 1 vote.
nz_bird %>%
  group_by(bird_breed) %>%
  filter(bird_breed != "NA", vote_rank == "vote_1") %>%
  tally()%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  arrange(n)%>%
  head(20)
  
## Little shag got the lowest vote total. 
## Remove those first place votes and replace with second choices
## I decided to go ahead and remove the 20 lowest vote getters to speed up the rounds
nz_bird2 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel"))%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)
  
## Plotting the second round chart
p2 <- ggplot(nz_bird2, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 2",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))
  
## To figure out the next 20 lowest vote getters in each round I took the "head(20)" out of the pipeline for the current 
## data set and then ran this code (replacing the dataset with whichever was current):
nz_bird2 %>% arrange(n) %>% head(20)

## This showed me the 20 lowest birds that I would then add to the removal of the next data subset.  Just make sure to 
## add the "head(20)" back before you plot or else you'll get a mess with too many birds


## Removing the next 20 lowest vote getters from the group
nz_bird3 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)
  
  
## Plotting for round 3
p3 <- ggplot(nz_bird3, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 3",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Remove the next 20 lowest voet getters from total
nz_bird4 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl"))%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)

## Plotting for round 4
p4 <- ggplot(nz_bird4, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 4",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Remove the next 20 lowest vote getters
nz_bird5 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl","Harrier","Kōkako","Mōhua","Orange-fronted Parakeet",
                            "Fairy Tern","Wrybill","Takahē","Southern Brown Kiwi","Fiordland Crested Penguin","Bittern","Rifleman",
                            "New Zealand Dotterel","Kingfisher","South Island Kōkako","Rockhopper Penguin","Stitchbird","Little Penguin",
                            "Morepork","Tūī"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)

## Plotting for round 5
p5 <- ggplot(nz_bird5, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 5",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

#Getting Closer: Remove the next 20 lowest vote getters
nz_bird6 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl","Harrier","Kōkako","Mōhua","Orange-fronted Parakeet",
                            "Fairy Tern","Wrybill","Takahē","Southern Brown Kiwi","Fiordland Crested Penguin","Bittern","Rifleman",
                            "New Zealand Dotterel","Kingfisher","South Island Kōkako","Rockhopper Penguin","Stitchbird","Little Penguin",
                            "Morepork","Tūī","Antipodean Albatross","Kea","Kākā","Blue Duck","Kererū","New Zealand Falcon"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)


## Plotting round 6
p6 <- ggplot(nz_bird6, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 6",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## For Real This Time: Remove Fantail.

nz_bird7 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl","Harrier","Kōkako","Mōhua","Orange-fronted Parakeet",
                            "Fairy Tern","Wrybill","Takahē","Southern Brown Kiwi","Fiordland Crested Penguin","Bittern","Rifleman",
                            "New Zealand Dotterel","Kingfisher","South Island Kōkako","Rockhopper Penguin","Stitchbird","Little Penguin",
                            "Morepork","Tūī","Antipodean Albatross","Kea","Kākā","Blue Duck","Kererū","New Zealand Falcon","Fantail"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)

## Plot round 7
p7 <- ggplot(nz_bird7, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 7",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Maybe one more will do it? Remove Banded Dottrel.

nz_bird8 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl","Harrier","Kōkako","Mōhua","Orange-fronted Parakeet",
                            "Fairy Tern","Wrybill","Takahē","Southern Brown Kiwi","Fiordland Crested Penguin","Bittern","Rifleman",
                            "New Zealand Dotterel","Kingfisher","South Island Kōkako","Rockhopper Penguin","Stitchbird","Little Penguin",
                            "Morepork","Tūī","Antipodean Albatross","Kea","Kākā","Blue Duck","Kererū","New Zealand Falcon","Fantail","Banded Dotterel"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  head(10)

## Plot round 8
p8 <- ggplot(nz_bird8, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 8",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))

## This has to be the last one. Remove Black Robin

nz_bird9 <- nz_bird_id %>%
  select(voter.id, vote_rank, bird_breed)%>%
  filter(!bird_breed %in% c("Little shag","Light-mantled Sooty Albatross","Westland Petrel",
                            "Pied Shag","Salvin's Mollymawk","Spotless Crake","Black shag",
                            "South Polar Skua","Campbell Black-Browed Albatross","White-faced Heron","NA",
                            "Whitehead","New Zealand King Shag","Variable Oystercatcher","Scaup","Arctic Skua",
                            "Shining Cuckoo","Buller's Mollymawk","Brown Teal","The Otago Shag","Black Petrel",
                            "Black-fronted Tern","Welcome Swallow","White Heron ","South Island Pied Oystercatcher",
                            "Whenua Hou Diving Petrel","Northern Royal Albatross","Spotted Shag","Bar-tailed Godwit",
                            "Fernbird","Hutton's Shearwater","Australasian Gannet","New Zealand Robin","Gibson's Albatross",
                            "Chatham Island Mollymawk","Pūkeko","Shore Plover","Southern Royal Albatross","White Heron","Grey Duck",
                            "Chatham Island Oystercatcher","Silvereye","Tomtit","Australasian Crested Grebe","Weka","North Island Brown Kiwi",
                            "Grey Warbler","New Zealand Dabchick","Great Spotted Kiwi","Royal Spoonbill","Black-billed Gull","Black Stilt",
                            "Little Spotted Kiwi","Rock Wren","Saddleback","Bellbird","Barn Owl","Harrier","Kōkako","Mōhua","Orange-fronted Parakeet",
                            "Fairy Tern","Wrybill","Takahē","Southern Brown Kiwi","Fiordland Crested Penguin","Bittern","Rifleman",
                            "New Zealand Dotterel","Kingfisher","South Island Kōkako","Rockhopper Penguin","Stitchbird","Little Penguin",
                            "Morepork","Tūī","Antipodean Albatross","Kea","Kākā","Blue Duck","Kererū","New Zealand Falcon","Fantail","Banded Dotterel",
                            "Black Robin"),
         bird_breed != "NA")%>%
  distinct(voter.id, .keep_all=T)%>%
  group_by(bird_breed)%>%
  tally()%>%
  arrange(desc(n))%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))

## Plot round 9
p9 <- ggplot(nz_bird9, aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
  geom_col(show.legend=F)+
  geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
  labs(subtitle="Round 9",
       x="",
       y="Votes")+
  scale_fill_brewer(palette="Set3")+
  ylim(0,12000)+
  theme(panel.background = element_rect(fill = "lightblue",
                                        colour = "lightblue"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
        plot.subtitle = element_text(hjust=0.5, color = "black"),
        plot.caption = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(angle = 45, hjust = 1))


## This is arranging several plots into one page for sharing.  The first two have 4 charts per page.  Figure 3 is just the 
## final chart.  Figure 4 puts all charts together into one big page. 
figure1 <- ggarrange(p1,p2,p3,p4)
annotate_figure(figure1,
                top = text_grob("New Zealand Bird of the Year Voting", color = "black", face = "bold", size = 18),
                bottom = text_grob("Data source: \n https://www.forestandbird.org.nz/", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10))%>%
  ggexport(filename="BOTY_1.png")

figure2 <- ggarrange(p5,p6,p7,p8)
annotate_figure(figure2,
                top = text_grob("New Zealand Bird of the Year Voting", color = "black", face = "bold", size = 18),
                bottom = text_grob("Data source: \n https://www.forestandbird.org.nz/", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10)) %>%
  ggexport(filename = "BOTY_2.png")

figure3 <- ggarrange(p9)
annotate_figure(figure3,
                top = text_grob("New Zealand Bird of the Year Voting", color = "black", face = "bold", size = 18),
                bottom = text_grob("Data source: \n https://www.forestandbird.org.nz/", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10))%>%
  ggexport(filename = "BOTY_3.png")

figure4<- ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9)
annotate_figure(figure4,
                top = text_grob("New Zealand Bird of the Year Voting", color = "black", face = "bold", size = 18),
                bottom = text_grob("Data source: \n https://www.forestandbird.org.nz/", color = "blue",
                                   hjust = 1, x = 1, face = "italic", size = 10))%>%
  ggexport(filename = "BOTY_Full.png")
  


  




