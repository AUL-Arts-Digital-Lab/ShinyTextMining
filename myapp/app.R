
#-----------------------------------Anvendte biblioteker-------------------------------

#Installer biblioteker
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("gutenbergr")
#install.packages("wordcloud")

#Hent biblioteker
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(gutenbergr)
library(wordcloud)

#-------------------------------------Dataindsamling-----------------------------------

#Valg af data fra Gutenberg pakken i R. Her hentes Frankenstein, A tale of two cities, 
#The Great Gatsby og Moby Dick.

#Hent og definer data
Frankenstein <- gutenberg_download(84)
A_Tale_of_Two <- gutenberg_download(98)
Great_Gatsby <- gutenberg_download(64317)
Moby_Dick <- gutenberg_download(2701)

#-------------------------------------Databehandling-----------------------------------
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

#Mest anvendte ord i Frankenstein
tidy_Frankenstein %>%
  count(word, sort = TRUE)

#Mest anvendte ord i Moby Dick
tidy_Moby_Dick %>%
  count(word, sort = TRUE)

#Tilføj kolonnen n, der indeholder ords fremkomst, til dataframe
#Frankenstein
tidy_Frankenstein <- tidy_Frankenstein %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#A Tale of Two Cities
tidy_A_Tale_of_Two <- tidy_A_Tale_of_Two %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#The Great Gatsby
tidy_Great_Gatsby<- tidy_Great_Gatsby %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#Moby Dick
tidy_Moby_Dick <- tidy_Moby_Dick %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

#-----------------------------------Shiny App----------------------------------------------

# Definer UI
ui <- fluidPage(
  titlePanel("Text Mining"),
  
  sidebarLayout(
    sidebarPanel(
      
      #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
      selectInput(inputId = "text_data", 
              label = "Vælg text",
              choices = c("Frankenstein",
                          "A Tale of two cities", 
                          "The Great Gatsby", 
                          "Moby Dick"), 
              selected = "Frankenstein"),
  ),
  
  mainPanel(
  #Danner et menu-layout, hvor det er muligt at skifte mellem visualiseringerne
    tabsetPanel(type = "tabs",
              tabPanel("Søjlediagram", plotOutput("viz_plot")),
              tabPanel("Wordcloud", plotOutput("viz_wordcloud")))
  )
)
)
  
# Definer server logic
server <- function(input, output) {
  #Søjlediagram
  #Får output til at matche input når der skiftes mellem texsterne
  output$viz_plot <- renderPlot({
    selected_text_data_plot <- switch(input$text_data,
                   "Frankenstein" = tidy_Frankenstein,
                   "A Tale of two cities" = tidy_A_Tale_of_Two,
                   "The Great Gatsby" = tidy_Great_Gatsby,
                   "Moby Dick" = tidy_Moby_Dick)
    
    #Visualisering af søjlediagram
    #overvej om det skal være muligt at gøre slice() til input og i såfald i hvilken form
    #Finde en måde, hvorpå selve frekvensen af ordet gøres tydligere - eventuelt i forbindelse med søgepromt 
    selected_text_data_plot %>%
      slice(1:10) %>%
      ggplot(., aes(x = word, y = n, fill = n)) +
      geom_col() +
      labs(x = "Ord", y = "Hyppighed for ordets forekomst", fill = "Hyppighed for ordets forekomst")
    
  })
  #Wordcloud
  output$viz_wordcloud <- renderPlot({
    #Får output til at matche input når der skiftes mellem texsterne
    selected_text_data_cloud <- switch(input$text_data,
                                 "Frankenstein" = tidy_Frankenstein,
                                 "A Tale of two cities" = tidy_A_Tale_of_Two,
                                 "The Great Gatsby" = tidy_Great_Gatsby,
                                 "Moby Dick" = tidy_Moby_Dick)
    #Visualisering af wordcloud
    #overvej om min.freq er mere relevant end max.words og om det skal være en 'aktiv' funktion, med inputs 
    selected_text_data_cloud %>%
      count(selected_text_data_cloud$word) %>%
      with(wordcloud(selected_text_data_cloud$word, selected_text_data_cloud$n, max.words = 50, colors = selected_text_data_cloud$n, ordered.colors = TRUE))
    
  })
  
}

# Kør appen 
shinyApp(ui = ui, server = server)


