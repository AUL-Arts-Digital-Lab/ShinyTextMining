
#Hent biblioteker
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(gutenbergr)
library(wordcloud)

#Valg af data fra Gutenberg pakken i R. Her hentes Frankenstein.
#Hent og definer data
frankenstein <- gutenberg_download(84)

#Tjek af data og formater
glimpse(frankenstein)
head(frankenstein)

#Klargør data til data mining
#Opdel tekst til enkelte ord samt fjern stopord
tidy_frankenstein <- frankenstein %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tjek den nye dataopdeling 
tidy_frankenstein

#Mest anvendte ord i Frankenstein}
tidy_frankenstein %>%
  count(word, sort = TRUE)

#Wordcloud
frankenstein_cloud <- tidy_frankenstein %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 50, colors = n, ordered.colors = TRUE))

#En anden visualisering, hvor tallene på, hvor ofte et ord nævnes, er inddraget
#Søjlediagram
tidy_frankenstein %>%
  count(word, sort = TRUE) %>%
  dplyr::filter(n > 75) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n, fill = n)) +
  geom_col() +
  labs(x = "Ord", y = "Hyppighed")

# Gemmer diagrammet som billede i mappen for billeder i shiny 
ggsave("frankenstein_diagram.png", path = "~/R_main/ShinyTextMining/myapp/www", width = 6, height = 6)

# Definer UI
ui <- fluidPage(
  titlePanel("Frankenstein af Mary Shelley"),
  
  sidebarLayout(
    sidebarPanel(
      h4("De mest anvendte ord i Frankenstein"),
      br(),
      em("Ordene er målt efter hyppigst forekomst")),
    mainPanel(
      img(src = "frankenstein_diagram.png", height = 500, width = 500)
    )
  )
  
)

# Definer server logic
server <- function(input, output) {
  
  
}



# Kør appen 
shinyApp(ui = ui, server = server)
