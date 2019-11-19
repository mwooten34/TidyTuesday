## Install the TidyTuesday package for easy data import
devtools::install_github("thebioengineer/tidytuesdayR")

## Install other packages for working with the data
library(tidyverse)
library(here)

## Read in data
tuesdata <- tidytuesdayR::tt_load("2019-11-19")
nz_bird <- tuesdata$nz_bird

## Tally first choice votes
## Add percentage of vote
## Send straight to plot
nz_bird %>%
  group_by(bird_breed) %>%
  filter(bird_breed != "NA", vote_rank == "vote_1") %>%
  tally()%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  arrange(desc(n))%>%
  head(10)%>%
  ggplot(., aes(x=reorder(bird_breed, desc(n)), y=n, fill=bird_breed))+
        geom_col(show.legend=F)+
        geom_text(aes(label=perc_vote, hjust=0.5, vjust=-0.5))+
        labs(title="New Zealand Bird of the Year Voting",
             subtitle="Round 1",
             x="",
             y="Votes",
             caption="Graphic by @mwooten34 \nData from https://www.forestandbird.org.nz/")+
        scale_fill_brewer(palette="Set3")+
       theme(panel.background = element_rect(fill = "lightblue",
                                             colour = "lightblue"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "black", size = 16, face = "bold"),
            plot.subtitle = element_text(hjust=0.5, color = "black"),
            plot.caption = element_text(color = "black", face = "italic"),
            axis.text.x = element_text(angle = 45, hjust = 1))

## Which bird gets eliminated after first round?
nz_bird %>%
  group_by(bird_breed) %>%
  filter(bird_breed != "NA", vote_rank == "vote_1") %>%
  tally()%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  arrange(n)
  head(10) 
  
## Little shag got the lowest vote total. 
## Remove those first place votes and replace with second choices
## I'm stuck here at the moment
nz_bird %>%
  count(bird_breed, vote_rank, sort=T, na.rm=T)%>%
  filter(vote_rank == "vote_1"& bird_breed != "Little shag")%>%
  mutate(perc_vote = round((n/sum(n)*100), digits=2))%>%
  arrange(n)
  




