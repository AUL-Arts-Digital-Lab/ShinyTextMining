
#-----------------------------------Anvendte biblioteker-------------------------------

#Installer biblioteker
#install.packages("shiny")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("tidytext")
#install.packages("dplyr")
#install.packages("gutenbergr")
#install.packages("ggwordcloud")
#install.package("janeaustenr")

#Hent biblioteker
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidytext)
library(dplyr)
library(gutenbergr)
library(ggwordcloud)
library(janeaustenr)

#-------------------------------------Dataindsamling-----------------------------------

#Valg af data fra Gutenberg pakken i R. 
#Teksterne er alle uden ophavsret og kan yderligere findes i data mappen

#Indlæs Frankenstein fra Data mappen
load("Data/Frankenstein_data.RData")
#Indlæs A Tale of Two Cities fra Data mappen
load("Data/A_Tale_of_Two_data.RData")
#Indlæs The Great Gatsby fra Data mappen
load("Data/Great_Gatsby_data.RData")
#Indlæs Moby Dick fra Data mappen
load("Data/Moby_Dick_data.RData")
#Indlæs Brother Grimm fra Data mappen
load("Data/Grimm_Brothers_corpus.RData")
#Indlæs H.C Andersen fra Data mappen
load("Data/HC_Andersen_corpus.RData")
#Indlæs Jane Austens romaner (stammer fra biblioteket janeaustenr)
Austen_corpora <- austen_books()

#-------------------------------------Databehandling-----------------------------------
#Klargør data til data mining

#Lav kolonne med lijenummer samt kapitlet, hvor linjen indgår i 
#Frankenstein
Frankenstein_pre_tidy <- Frankenstein %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\d0-9]",
                                           ignore_case = TRUE))))
#A Tale of two Cities
A_Tale_of_Two_pre_tidy <- A_Tale_of_Two %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE))))
# Jane Austen corpora
Austen_corpora_pre_tidy <- Austen_corpora %>%
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text,
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>% 
  ungroup()

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

#Tidy af HC Andersen
tidy_HC_Andersen <- HC_Andersen_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Tidy af Brother Grimm
tidy_Grimm_Brothers <- Grimm_Brothers_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Tidy af Austen
tidy_Austen <- Austen_corpora_pre_tidy %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Lav en tidy version af teksterne, hvor stopordene stadig er i dataen
tidy_Frankenstein_with_stopwords <- Frankenstein %>%
  unnest_tokens(word, text)
tidy_A_Tale_of_Two_with_stopwords <- A_Tale_of_Two %>%
  unnest_tokens(word, text)
tidy_Great_Gatsby_with_stopwords <- Great_Gatsby %>%
  unnest_tokens(word, text)
tidy_Moby_Dick_with_stopwords <- Moby_Dick %>%
  unnest_tokens(word, text)
tidy_HC_Andersen_with_stopwords <- HC_Andersen_corpus %>% 
  unnest_tokens(word, text)
tidy_Grimm_Brothers_with_stopwords <- Grimm_Brothers_corpus %>% 
  unnest_tokens(word, text)
tidy_Austen_with_stopwords <- Austen_corpora %>%
  unnest_tokens(word, text)

#Tilføj kolonnen n, der indeholder ords fremkomst, til dataframe
#Frankenstein
sorted_tidy_Frankenstein <- tidy_Frankenstein %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#A Tale of Two Cities
sorted_tidy_A_Tale_of_Two <- tidy_A_Tale_of_Two %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#The Great Gatsby
sorted_tidy_Great_Gatsby<- tidy_Great_Gatsby %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#Moby Dick
sorted_tidy_Moby_Dick <- tidy_Moby_Dick %>%
  count(word, sort = TRUE) %>%
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#H.C Andersen
sorted_tidy_HC_Andersen <- tidy_HC_Andersen %>%
  group_by(title) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#Grimm Brothers
sorted_tidy_Grimm_Brothers <- tidy_Grimm_Brothers %>%
  group_by(title) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup()
#Austen
sorted_tidy_Austen <- tidy_Austen %>% 
  group_by(book) %>% 
  count(word, sort = TRUE) %>% 
  mutate(word = reorder(word, n)) %>% 
  ungroup()
  
#-----------------------------------Shiny App----------------------------------------------

#---------------------------------- Definer UI---------------------------------------------
ui <- fluidPage(
  titlePanel("Text Mining"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
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
                       column(4,
                              br(),
                              selectInput(inputId = "text_data_read", 
                                   label = "Vælg tekst",
                                   choices = c("Frankenstein",
                                               "A Tale of two cities", 
                                               "The Great Gatsby",
                                               "Moby Dick",
                                               "Brødrene Grimms eventyr",
                                               "H.C Andersens eventyr",
                                               "Jane Austens Romaner",
                                   selected = "Frankenstein"))
                       ),
                       br(),
                       column(8,
                       radioButtons(inputId = "selected_stopword_view", 
                                    label = "Se tekst med eller uden stopord",
                                    choices = c("Med", "Uden"), 
                                    selected = "Med")
                       ),
                       br(),
                       textOutput("viz_text")),
              tabPanel("Søjlediagram",
                       br(),
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_plot", 
                                   label = "Vælg text",
                                   choices = c("Frankenstein",
                                               "A Tale of two cities", 
                                               "The Great Gatsby", 
                                               "Moby Dick",
                                               "Brødrene Grimms eventyr",
                                               "H.C Andersens eventyr",
                                               "Jane Austens Romaner",
                                   selected = "Frankenstein")),
                       br(),
                       sliderInput(inputId = "slice_size",
                                   label = "Vælg antal ord i visualiseringen mellem 1 og 20",
                                   min = 5, max = 20, value = 10, step = 5),
                       plotOutput("viz_plot")),
              tabPanel("Wordcloud",
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_cloud", 
                                   label = "Vælg text",
                                   choices = c("Frankenstein",
                                               "A Tale of two cities", 
                                               "The Great Gatsby", 
                                               "Moby Dick",
                                               "Brødrene Grimms eventyr",
                                               "H.C Andersens eventyr",
                                               "Jane Austens Romaner"), 
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
    #Skift mellem tekster med og uden stopord, alt efter hvilken knap, der er aktiveret
    if (input$selected_stopword_view == "Med"){
      selected_text_data_read <- switch(input$text_data_read,
                                        "Frankenstein" = tidy_Frankenstein_with_stopwords,
                                        "A Tale of two cities" = tidy_A_Tale_of_Two_with_stopwords,
                                        "The Great Gatsby" = tidy_Great_Gatsby_with_stopwords,
                                        "Moby Dick" = tidy_Moby_Dick_with_stopwords,
                                        "Brødrene Grimms eventyr" = tidy_Grimm_Brothers_with_stopwords,
                                        "H.C Andersens eventyr" = tidy_HC_Andersen_with_stopwords,
                                        "Jane Austens Romaner" = tidy_Austen_with_stopwords)
    } else if (input$selected_stopword_view == "Uden"){
      selected_text_data_read <- switch(input$text_data_read,
                                        "Frankenstein" = tidy_Frankenstein,
                                        "A Tale of two cities" = tidy_A_Tale_of_Two,
                                        "The Great Gatsby" = tidy_Great_Gatsby,
                                        "Moby Dick" = tidy_Moby_Dick,
                                        "Brødrene Grimms eventyr" = tidy_Grimm_Brothers,
                                        "H.C Andersens eventyr" = tidy_HC_Andersen,
                                        "Jane Austens Romaner" = tidy_Austen)
    }
    
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
                   "Moby Dick" = sorted_tidy_Moby_Dick,
                   "Brødrene Grimms eventyr" = sorted_tidy_Grimm_Brothers,
                   "H.C Andersens eventyr" = sorted_tidy_HC_Andersen,
                   "Jane Austens Romaner" = sorted_tidy_Austen)
    
    slice_size <- input$slice_size
    words_sum <- sum(as.numeric(selected_text_data_plot$n))
    
    if (input$text_data_plot == "Brødrene Grimms eventyr"){
      selected_text_data_plot %>%
        group_by(title) %>% 
        slice_max(n, n = slice_size) %>%
        ungroup() %>% 
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 6, scales = "free") +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                    vjust = "top", hjust = "center",
                    fill = "white", color = "black", 
                    size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", subtitle = paste("Det samlede antal ord i korporaet er:", words_sum), x = "Ord", y = "Hyppighed", fill = "Hyppighed")
    } else if (input$text_data_plot == "H.C Andersens eventyr"){
      selected_text_data_plot %>%
        group_by(title) %>% 
        slice_max(n, n = slice_size) %>%
        ungroup() %>% 
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 6, scales = "free") +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", subtitle = paste("Det samlede antal ord i korporaet er:", words_sum), x = "Ord", y = "Hyppighed", fill = "Hyppighed")
    } else if (input$text_data_plot == "Jane Austens Romaner"){
      selected_text_data_plot %>%
        group_by(book) %>% 
        slice_max(n, n = slice_size) %>%
        ungroup() %>% 
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ book, ncol = 3, scales = "free") +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", subtitle = paste("Det samlede antal ord i korporaet er:", words_sum), x = "Ord", y = "Hyppighed", fill = "Hyppighed")
    } else {
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
        labs(title = "Hyppigheden for ordenes forekomst i teksten", subtitle = paste("Det samlede antal ord i teksten er:", words_sum), x = "Ord", y = "Hyppighed", fill = "Hyppighed")
    }
    
  })
#-------------------------------------- Wordcloud --------------------------------------------
  
  output$viz_wordcloud <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_cloud <- switch(input$text_data_cloud,
                                       "Frankenstein" = sorted_tidy_Frankenstein,
                                       "A Tale of two cities" = sorted_tidy_A_Tale_of_Two,
                                       "The Great Gatsby" = sorted_tidy_Great_Gatsby,
                                       "Moby Dick" = sorted_tidy_Moby_Dick,
                                       "Brødrene Grimms eventyr" = sorted_tidy_Grimm_Brothers,
                                       "H.C Andersens eventyr" = sorted_tidy_HC_Andersen,
                                       "Jane Austens Romaner" = sorted_tidy_Austen)
    
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


