
#-----------------------------------Anvendte biblioteker-------------------------------

#Installer biblioteker
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("gutenbergr")
#install.packages("ggwordcloud")

#Hent biblioteker
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(gutenbergr)
library(ggwordcloud)

#-------------------------------------Dataindsamling-----------------------------------

#Valg af data fra Gutenberg pakken i R. Her hentes Frankenstein, A tale of two cities, 
#The Great Gatsby og Moby Dick.

#Hent og definer data
#Frankenstein <- gutenberg_download(84)
#A_Tale_of_Two <- gutenberg_download(98)
#Great_Gatsby <- gutenberg_download(64317)
#Moby_Dick <- gutenberg_download(2701)

#Indlæs Frankenstein fra Data mappen
load("Data/Frankenstein_data.RData")
#Indlæs A Tale of Two Cities fra Data mappen
load("Data/A_Tale_of_Two_data.RData")
#Indlæs The Great Gatsby fra Data mappen
load("Data/Great_Gatsby_data.RData")
#Indlæs Moby Dick fra Data mappen
load("Data/Moby_Dick_data.RData")

#-------------------------------------Databehandling-----------------------------------
#Klargør data til data mining
#Opdel tekst til enkelte ord samt fjern stopord

#Tidy af Frankenstein - stopord fjernes
tidy_Frankenstein <- Frankenstein %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af A Tale of Two Cities - stopord fjernes
tidy_A_Tale_of_Two <- A_Tale_of_Two %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af The Great Gatsby - stopord fjernes
tidy_Great_Gatsby <- Great_Gatsby %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tidy af Moby Dick - stopord fjernes
tidy_Moby_Dick <- Moby_Dick %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

#Tjek den nye dataopdeling 
tidy_Frankenstein
tidy_A_Tale_of_Two
tidy_Great_Gatsby
tidy_Moby_Dick

#Lav en tidy version af teksterne, hvor stopordene stadig er i dataen
tidy_Frankenstein_with_stopwords <- Frankenstein %>%
  unnest_tokens(word, text)
tidy_A_Tale_of_Two_with_stopwords <- A_Tale_of_Two %>%
  unnest_tokens(word, text)
tidy_Great_Gatsby_with_stopwords <- Great_Gatsby %>%
  unnest_tokens(word, text)
tidy_Moby_Dick_with_stopwords <- Moby_Dick %>%
  unnest_tokens(word, text)


#Tilføj kolonnen n, der indeholder ords fremkomst, til dataframe
#Frankenstein
sorted_tidy_Frankenstein <- tidy_Frankenstein %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#A Tale of Two Cities
sorted_tidy_A_Tale_of_Two <- tidy_A_Tale_of_Two %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#The Great Gatsby
sorted_tidy_Great_Gatsby<- tidy_Great_Gatsby %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))
#Moby Dick
sorted_tidy_Moby_Dick <- tidy_Moby_Dick %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n))

#-----------------------------------Shiny App----------------------------------------------

#---------------------------------- Definer UI---------------------------------------------
ui <- fluidPage(
  titlePanel("Text Mining"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Navigation"),
      helpText("Brug knapperne til at navigere mellem forskellige visualiseringer"),
      
      br(),
      
      helpText("Info")

        
  ),
  
  mainPanel(
    
    #Danner et menu-layout, hvor det er muligt at skifte mellem visualiseringerne
    tabsetPanel(type = "tabs",
              tabPanel("Læs tekst", 
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen indeholder både tekster med og uden stopord"),
                       selectInput(inputId = "text_data_read", 
                                   label = "Vælg text",
                                   choices = c("Frankenstein",
                                               "Frankenstein (uden stopord)",
                                               "A Tale of Two Cities (uden stopord)",
                                               "A Tale of two cities", 
                                               "The Great Gatsby",
                                               "The Great Gatsby (uden stopord)",
                                               "Moby Dick",
                                               "Moby Dick (uden stopord)"), 
                                   selected = "Frankenstein"),
                       textOutput("viz_text")),
              tabPanel("Søjlediagram",
                       br(),
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_plot", 
                                   label = "Vælg text",
                                   choices = c("Frankenstein",
                                               "A Tale of two cities", 
                                               "The Great Gatsby", 
                                               "Moby Dick"), 
                                   selected = "Frankenstein"),
                       br(),
                       sliderInput(inputId = "slice_size",
                                   label = "Vælg antal ord i visualiseringen mellem 5 og 20",
                                   min = 5, max = 20, value = 10, step = 5), 
                       plotOutput("viz_plot")),
              tabPanel("Wordcloud",
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_cloud", 
                                   label = "Vælg text",
                                   choices = c("Frankenstein",
                                               "A Tale of two cities", 
                                               "The Great Gatsby", 
                                               "Moby Dick"), 
                                   selected = "Frankenstein"),
                       sliderInput(inputId = "word_freq_cloud", 
                                   label = "Vælg antal ord i visualiseringen mellem 5 og 30",
                                   min =5, max = 30, value = 20, step = 5),
                       plotOutput("viz_wordcloud")))
  )
)
)
  
#---------------------- Definer server logic -------------------------------------------
server <- function(input, output, session) {
  
#--------------------------- Læs_tekst -------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_text <- renderText({
    selected_text_data_read <- switch(input$text_data_read,
                    "Frankenstein" = tidy_Frankenstein_with_stopwords,
                    "Frankenstein (uden stopord)" = tidy_Frankenstein,
                    "A Tale of two cities" = tidy_A_Tale_of_Two_with_stopwords,
                    "A Tale of Two Cities (uden stopord)" = tidy_A_Tale_of_Two,
                    "The Great Gatsby" = tidy_Great_Gatsby_with_stopwords,
                    "The Great Gatsby (uden stopord)" = tidy_Great_Gatsby,
                    "Moby Dick" = tidy_Moby_Dick_with_stopwords,
                    "Moby Dick (uden stopord)" = tidy_Moby_Dick)
    
    #Visualisering af den fulde tekst
    head(selected_text_data_read$word, 1000)
  })
  
#------------------------ Søjlediagram --------------------------------------------------
  

  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_plot <- renderPlot({
    selected_text_data_plot <- switch(input$text_data_plot,
                   "Frankenstein" = sorted_tidy_Frankenstein,
                   "A Tale of two cities" = sorted_tidy_A_Tale_of_Two,
                   "The Great Gatsby" = sorted_tidy_Great_Gatsby,
                   "Moby Dick" = sorted_tidy_Moby_Dick)
    
    slice_size <- input$slice_size
    
    #Visualisering af søjlediagram
    selected_text_data_plot %>%
      head(slice_size) %>%
      ggplot(., aes(x = word, y = n, fill = n)) +
      geom_col() +
      coord_flip() +
      geom_label(aes(x = word, y = n, label = n), 
                 vjust = "top", hjust = "center",
                 fill = "white", color = "black", 
                 size = 3) +
      labs(x = "Ord", y = "Hyppighed for ordets forekomst", fill = "Hyppighed for ordets forekomst")
    
  })
#-------------------------------------- Wordcloud --------------------------------------------
  
  output$viz_wordcloud <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_cloud <- switch(input$text_data_cloud,
                                       "Frankenstein" = sorted_tidy_Frankenstein,
                                       "A Tale of two cities" = sorted_tidy_A_Tale_of_Two,
                                       "The Great Gatsby" = sorted_tidy_Great_Gatsby,
                                       "Moby Dick" = sorted_tidy_Moby_Dick)
    
    word_freq_cloud <- input$word_freq_cloud
    
    #Visualisering af wordcloud
    selected_text_data_cloud %>%
      head(word_freq_cloud) %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient(low = "lightblue", high = "darkblue")
  })
  
}

#--------------------------- Kør appen ------------------------------------------------------
shinyApp(ui = ui, server = server)


