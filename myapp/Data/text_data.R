
#Hent biblioteker
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(gutenbergr)

#Valg af data fra Gutenberg pakken i R. Her hentes Frankenstein, A tale of two cities, 
#The Great Gatsby og Moby Dick.

#Hent og definer data
Frankenstein <- gutenberg_download(84)
A_Tale_of_Two <- gutenberg_download(98)
Great_Gatsby <- gutenberg_download(64317)
Moby_Dick <- gutenberg_download(2701)

#gem uredigeret data
save(Frankenstein, file = "~/R_main/ShinyTextMining/myapp/Data/Frankenstein_data.RData")
save(A_Tale_of_Two, file = "~/R_main/ShinyTextMining/myapp/Data/A_Tale_of_Two_data.RData")
save(Great_Gatsby, file = "~/R_main/ShinyTextMining/myapp/Data/Great_Gatsby_data.RData")
save(Moby_Dick, file = "~/R_main/ShinyTextMining/myapp/Data/Moby_Dick_data.RData")

#Tjek af data og formater
glimpse(Frankenstein)
head(Frankenstein)

glimpse(Moby_Dick)
head(Moby_Dick)


#Klargør data til data mining
#Opdel tekst til enkelte ord samt fjern stopord

#Tidy af Frankenstein
tidy_Frankenstein <- Frankenstein %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af A Tale of Two Cities
tidy_A_Tale_of_Two <- A_Tale_of_Two %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af The Great Gatsby
tidy_Great_Gatsby <- Great_Gatsby %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af Moby Dick
tidy_Moby_Dick <- Moby_Dick %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tjek den nye dataopdeling 
tidy_Frankenstein
tidy_A_Tale_of_Two
tidy_Great_Gatsby
tidy_Moby_Dick

#gem tidy data
save(tidy_Frankenstein, file = "~/R_main/ShinyTextMining/myapp/Data/tidy_Frankenstein_data.csv")
save(tidy_A_Tale_of_Two, file = "~/R_main/ShinyTextMining/myapp/Data/tidy_A_Tale_of_Two_data.csv")
save(tidy_Great_Gatsby, file = "~/R_main/ShinyTextMining/myapp/Data/tidy_Great_Gatsby_data.csv")
save(tidy_Moby_Dick, file = "~/R_main/ShinyTextMining/myapp/Data/tidy_Moby_Dick_data.csv")

