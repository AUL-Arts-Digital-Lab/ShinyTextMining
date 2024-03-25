
#-----------------------------------Anvendte biblioteker-------------------------------

#Hent biblioteker
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidytext)
library(dplyr)
library(ggwordcloud)
library(quanteda)
library(thematic)


#-------------------------------------Dataindsamling-----------------------------------

#Valg af data fra Project Gutenberg
#Teksterne er alle uden ophavsret og kan yderligere findes i data mappen

#Indlæs Brother Grimm fra Data mappen
load("Data/Grimm_Brothers_corpus.RData")
load("Data/Grimm_Brothers_corpus_da.RData")
#Indlæs H.C Andersen fra Data mappen
load("Data/HC_Andersen_corpus.RData")
load("Data/HC_Andersen_corpus_da.RData")
#Indlæs Jane Austens fra Data mappen
load("Data/Austen_corpus.RData")
#Indlæs St.Croix Avis 1878 fra Data mappen
load("Data/Croix_corpus.Rdata")

#Indlæs den danske stopordsliste fra Stop_Words mappen 
stop_words_da <- read.csv("Stopwords/stop_words_da.txt")

#-------------------------------------Dataforberedelse-----------------------------------

#Ændrer encodingen fra latin1 (mest optimal for de andre tekster) til UTF-8, der er den mest optimale for Austen
Encoding(Austen_corpus$text) <- "UTF-8"
Encoding(Grimm_Brothers_corpus$text) <- "UTF-8"

#------------------------------------ Forbered tidy ---------------------------------------

#Opdel tekst til enkelte ord samt fjern stopord
#Tidy af HC Andersen
tidy_HC_Andersen_en <- HC_Andersen_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_HC_Andersen_da <- HC_Andersen_corpus_da %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words_da)

#Tidy af Brother Grimm
tidy_Grimm_Brothers_en <- Grimm_Brothers_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_Grimm_Brothers_da <- Grimm_Brothers_corpus_da %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words_da)

#Tidy af Austen
tidy_Austen <- Austen_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Tidy af st. Croix
tidy_Croix <- Croix_corpus %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Lav en tidy version af teksterne, hvor stopordene stadig er i dataen
tidy_HC_Andersen_en_with_stopwords <- HC_Andersen_corpus %>% 
  unnest_tokens(word, text)
tidy_HC_Andersen_da_with_stopwords <- HC_Andersen_corpus_da %>% 
  unnest_tokens(word, text)
tidy_Grimm_Brothers_en_with_stopwords <- Grimm_Brothers_corpus %>% 
  unnest_tokens(word, text)
tidy_Grimm_Brothers_da_with_stopwords <- Grimm_Brothers_corpus_da %>% 
  unnest_tokens(word, text)
tidy_Austen_with_stopwords <- Austen_corpus %>%
  unnest_tokens(word, text)
tidy_Croix_with_stopwords <- Croix_corpus %>% 
  unnest_tokens(word, text)

#Forbered bigrams
#Tidy af HC Andersen
bigram_HC_Andersen_en <- HC_Andersen_corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigram_HC_Andersen_da <- HC_Andersen_corpus_da %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words_da$word) %>% 
  filter(!word2 %in% stop_words_da$word)

#Tidy af Brother Grimm
bigram_Grimm_Brothers_en <- Grimm_Brothers_corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

bigram_Grimm_Brothers_da <- Grimm_Brothers_corpus_da %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words_da$word) %>% 
  filter(!word2 %in% stop_words_da$word)

#Tidy af Austen
bigram_Austen <- Austen_corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word)

#Tidy af st. Croix
bigram_Croix <- Croix_corpus %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  filter(!is.na(bigram)) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word1 %in% stop_words_da$word) %>% 
  filter(!word2 %in% stop_words_da$word)


#------------------------------------ Forbered quanteda ------------------------------------

#Lav en corpus version af teksterne, der passer til quanteda pakken
HC_Andersen_context_corpus_en <- corpus(HC_Andersen_corpus)
HC_Andersen_context_corpus_da <- corpus(HC_Andersen_corpus_da)
Grimm_Brothers_context_corpus_en <- corpus(Grimm_Brothers_corpus)
Grimm_Brothers_context_corpus_da <- corpus(Grimm_Brothers_corpus_da)
Austen_context_corpus <- corpus(Austen_corpus)
Croix_context_corpus <- corpus(Croix_corpus)
                                     
#Rediger docnames til title, så de bliver meningsfulde og identificerbare
#HC Andersen
HC_docid_en <- paste(HC_Andersen_corpus$title)
docnames(HC_Andersen_context_corpus_en) <- HC_docid_en
HC_docid_da <- paste(HC_Andersen_corpus_da$title)
docnames(HC_Andersen_context_corpus_da) <- HC_docid_da
#Brother Grimm
Grimm_docid_en <- paste(Grimm_Brothers_corpus$title)
docnames(Grimm_Brothers_context_corpus_en) <- Grimm_docid_en
Grimm_docid_da <- paste(Grimm_Brothers_corpus_da$title)
docnames(Grimm_Brothers_context_corpus_da) <- Grimm_docid_da
#Austen
Austen_docid <- paste(Austen_corpus$title)
docnames(Austen_context_corpus) <- Austen_docid
#St. Croix
Croix_docid <- paste(Croix_corpus$month)
docnames(Croix_context_corpus) <- Croix_docid

#Tokenize korpus teksterne
HC_Andersen_context_corpus_en <- tokens(HC_Andersen_context_corpus_en, remove_separators = TRUE)
HC_Andersen_context_corpus_da <- tokens(HC_Andersen_context_corpus_da, remove_separators = TRUE)
Grimm_Brothers_context_corpus_en <- tokens(Grimm_Brothers_context_corpus_en, remove_separators = TRUE)
Grimm_Brothers_context_corpus_da <- tokens(Grimm_Brothers_context_corpus_da, remove_separators = TRUE)
Austen_context_corpus <- tokens(Austen_context_corpus, remove_separators = TRUE)
Croix_context_corpus <- tokens(Croix_context_corpus, remove_separators = TRUE)

#------------------------------------- Stop Words ---------------------------------------
#Lav en ny liste indeholdene selvvalgte stopord
my_stop_words <- data.frame(word = c("miss", "mrs", "sir", "mr"))

#-----------------------------------Shiny App----------------------------------------------

#---------------------------------- Definer UI---------------------------------------------
ui <- fluidPage(
  titlePanel("Text Mining"),
  thematic::thematic_shiny(),
  tags$img(src = "DKB_logo.png", heigth = 50, width = 150, align = "right"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
      br(),
      selectizeInput(inputId = "remove_word",
                     label = "Søg for at fjerne ord fra tekst",
                     choices = c("choose" = "", my_stop_words),
                     selected = NULL,
                     multiple = TRUE,
                     options = list(placeholder = "", create = TRUE)),
      #Liste til selvvalgte stopord
      verbatimTextOutput("list_removed_word"),
      h3("Stopord"),
      helpText("Den integrerede stopordsliste til engelske tekster stammer fra R pakken tidytext og indeholder ord fra de tre leksika: onix, SMART og snowball.",
               "Stopordslisten til de danske tekster er udarbejdet af informationsspecialister ved Det Kgl. Bibliotek"),

  ),
  
  mainPanel(
    
    #Danner et menu-layout, hvor det er muligt at skifte mellem visualiseringerne
    tabsetPanel(type = "tabs",
              tabPanel("Nærlæs tekst",
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen gør det muligt at se tekster, hvor stopord enten er fjernet eller stadig optræder i teksten."),
                       column(6,
                              br(),
                              selectInput(inputId = "text_data_read", 
                                   label = "Vælg tekst",
                                   choices = c("Brødrene Grimms eventyr (engelsk)",
                                               "Brødrene Grimms eventyr (dansk)",
                                               "H.C Andersens eventyr (engelsk)",
                                               "H.C Andersens eventyr (dansk)",
                                               "St. Croix Avis 1878",
                                               "Jane Austens Romaner"),
                                   selected = "Jane Austens Romaner")),
                       br(),
                       column(6,
                       radioButtons(inputId = "selected_stopword_view", 
                                    label = "Se tekst med eller uden stopord",
                                    choices = c("Med", "Uden"), 
                                    selected = "Med")),
                       br(),
                       textOutput("viz_text")),
              tabPanel("Søjlediagram",
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen viser de hyppigst forekommende ord i corpus som helhed og i de enkelte tekster"),
                       column(3,
                              br(),
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_plot", 
                                   label = "Vælg text",
                                   choices = c("Brødrene Grimms eventyr (engelsk)",
                                               "Brødrene Grimms eventyr (dansk)",
                                               "H.C Andersens eventyr (engelsk)",
                                               "H.C Andersens eventyr (dansk)",
                                               "St. Croix Avis 1878",
                                               "Jane Austens Romaner"),
                                   selected = "Jane Austens Romaner")),
                       column(3,
                              br(),
                       radioButtons(inputId = "selected_corpora_or_text",
                                    label = "Vis for hele corpus eller se fordelingen på tekster",
                                    choices = c("Hele corpus",
                                                "Tekster"),
                                    selected = "Hele corpus")),
                       column(3,
                              br(),
                       sliderInput(inputId = "slice_size",
                                   label = "Vælg antal ord i visualiseringen mellem 5 og 20",
                                   min = 5, max = 20, value = 5, step = 5)),
                       plotOutput("viz_plot")), 
              tabPanel("Wordcloud",
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen viser de hyppigst forekommende ord i corpus som helhed og i de enkelte tekster"),
                       column(3,
                              br(),
                      #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_cloud", 
                                   label = "Vælg text",
                                   choices = c("Brødrene Grimms eventyr (engelsk)",
                                               "Brødrene Grimms eventyr (dansk)",
                                               "H.C Andersens eventyr (engelsk)",
                                               "H.C Andersens eventyr (dansk)",
                                               "St. Croix Avis 1878",
                                               "Jane Austens Romaner"), 
                                   selected = "Jane Austens Romaner")),

                       column(3,
                              br(),
                       radioButtons(inputId = "selected_corpora_or_text_cloud",
                                    label = "Vis for hele corpus eller se fordelingen på tekster",
                                    choices = c("Hele corpus",
                                                "Tekster"),
                                    selected = "Hele corpus")),
                       column(3,
                              br(),
                       sliderInput(inputId = "word_freq_cloud", 
                                   label = "Vælg antal ord i visualiseringen mellem 5 og 30",
                                   min =5, max = 30, value = 20, step = 5)),
                       plotOutput("viz_wordcloud")),
              tabPanel("Kontekst",
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen viser konteksten, hvori et fremsøgt ord eller frase optræder"),
                       helpText("Søg for at se konteksten"),
                       br(),
                       column(3,
                              br(),
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_context", 
                                   label = "Vælg text",
                                   choices = c("Brødrene Grimms eventyr (engelsk)",
                                               "Brødrene Grimms eventyr (dansk)",
                                               "H.C Andersens eventyr (engelsk)",
                                               "H.C Andersens eventyr (dansk)",
                                               "St. Croix Avis 1878",
                                               "Jane Austens Romaner"), 
                                   selected = "Jane Austens Romaner")),
                       column(3,
                              br(),
                       #Definerer funktionen, der gør det muligt at fremsøge et keyword
                       textInput(inputId = "select_kwic",
                                 label = "Søg efter ord eller frase",
                                 value = "")),
                       column(3,
                              br(),
                       sliderInput(inputId = "window_context", 
                                   label = "Vælg antallet af ord før og efter søgeordet",
                                   min =1, max = 50, value = 1, step = 1)),
                       column(3,
                              br(),
                              textOutput("text_doc_sum"),
                              textOutput("text_token_sum"),
                              textOutput("text_token_unique")),
                       dataTableOutput("viz_context")),
              tabPanel("Bigrams",
                       br(),
                       h4("Info"),
                       helpText("Visualiseringen viser de ordpar, der forekommer i corpus. Juster minimusforekomsten af ordpar - ordparet fremgår i corpus mindst x antal gange"),
                       column(3,
                              br(),
                       #Definerer funktionen, hvor det er muligt at vælge mellem de forskellige tekster
                       selectInput(inputId = "text_data_bigrams", 
                                   label = "Vælg text",
                                   choices = c("Brødrene Grimms eventyr (engelsk)",
                                               "Brødrene Grimms eventyr (dansk)",
                                               "H.C Andersens eventyr (engelsk)",
                                               "H.C Andersens eventyr (dansk)",
                                               "St. Croix Avis 1878",
                                               "Jane Austens Romaner"), 
                                   selected = "Jane Austens Romaner")),
                       column(3,
                              br(),
                       #Definerer funktionen, hvor det er muligt at vælge minimum frekvens for ordene i visualiseringen
                       sliderInput(inputId = "wordpair_freq_bigrams", 
                                   label = "Vælg minimums frekvens for ordpar i visualiseringen mellem 1 og 50",
                                   min = 1, max = 50, value = 10, step = 2)),
                       plotOutput("viz_bigrams"))
  )
)
))
  
#------------------------- Definer server logic -------------------------------------------
server <- function(input, output, session) {
  
#------------------------ Fjern ord fra korpora --------------------------------------------
  #Lav dataframe reaktiv
  remove_word_df <- reactiveVal()
  #Definerer at de ord, der skal fjernes fra teksten stammer fra inputtet
  removed_word <- reactive({
    req(input$remove_word)
    data.frame(word = input$remove_word)
  })
  #Tilføj data til dataframe
  observeEvent(input$remove_word, {
    temp_df <- rbind(remove_word_df(), removed_word())
    remove_word_df(temp_df)
  })
  
#---------------------------- Nærlæs_tekst ----------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_text <- renderText({
    #Skift mellem tekster med og uden stopord, alt efter hvilken knap, der er aktiveret
    if (input$selected_stopword_view == "Med"){
      selected_text_data_read <- switch(input$text_data_read,
                                        "Brødrene Grimms eventyr (engelsk)" = tidy_Grimm_Brothers_en_with_stopwords,
                                        "Brødrene Grimms eventyr (dansk)"= tidy_Grimm_Brothers_da_with_stopwords,
                                        "H.C Andersens eventyr (engelsk)" = tidy_HC_Andersen_en_with_stopwords,
                                        "H.C Andersens eventyr (dansk)" = tidy_HC_Andersen_da_with_stopwords,
                                        "St. Croix Avis 1878" = tidy_Croix_with_stopwords,
                                        "Jane Austens Romaner" = tidy_Austen_with_stopwords)
    } else if (input$selected_stopword_view == "Uden"){
      selected_text_data_read <- switch(input$text_data_read,
                                        "Brødrene Grimms eventyr (engelsk)" = tidy_Grimm_Brothers_en,
                                        "Brødrene Grimms eventyr (dansk)" = tidy_Grimm_Brothers_da,
                                        "H.C Andersens eventyr (engelsk)" = tidy_HC_Andersen_en,
                                        "H.C Andersens eventyr (dansk)" = tidy_HC_Andersen_da,
                                        "St. Croix Avis 1878" = tidy_Croix,
                                        "Jane Austens Romaner" = tidy_Austen)
    }
   
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_read <- selected_text_data_read %>% 
        filter(!word %in% remove_word) 
    }
    
    #Visualisering af den fulde tekst
    head(selected_text_data_read$word, 1000)
      
  })
  
#------------------------ Søjlediagram --------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_plot <- renderPlot({
    selected_text_data_plot <- switch(input$text_data_plot,
                   "Brødrene Grimms eventyr (engelsk)" = tidy_Grimm_Brothers_en,
                   "Brødrene Grimms eventyr (dansk)" = tidy_Grimm_Brothers_da,
                   "H.C Andersens eventyr (engelsk)" = tidy_HC_Andersen_en,
                   "H.C Andersens eventyr (dansk)" = tidy_HC_Andersen_da,
                   "St. Croix Avis 1878" = tidy_Croix,
                   "Jane Austens Romaner" = tidy_Austen)
    
    #Definerer at antallet at ord i visualiseringen skal matche inputtet herfor
    slice_size <- input$slice_size
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_plot <- selected_text_data_plot %>% 
        filter(!word %in% remove_word) 
    }
    
    #Visualiseringer af søjlediagram baseret på hele korpora eller enkelte tekster
    #Brødrende Grimm (engelsk) - vis enkelte tekster
    if (input$text_data_plot == "Brødrene Grimms eventyr (engelsk)" & input$selected_corpora_or_text == "Tekster"){
      selected_text_data_plot %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(title = as.factor(title),
               word = reorder_within(word, n, title)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 5, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                    vjust = "top", hjust = "center",
                    fill = "white", color = "black", 
                    size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
    #Brødrende Grimm (dansk) - vis enkelte tekster
  } else if (input$text_data_plot == "Brødrene Grimms eventyr (dansk)" & input$selected_corpora_or_text == "Tekster"){
    selected_text_data_plot %>%
      group_by(title) %>%
      count(word, sort = TRUE) %>% 
      slice_max(n, n = slice_size, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(title = as.factor(title),
             word = reorder_within(word, n, title)) %>% 
      ggplot(aes(x = word, y = n, fill = n)) +
      geom_col() +
      facet_wrap( ~ title, ncol = 6, scales = "free") +
      coord_flip() +
      scale_x_reordered() +
      geom_label(aes(x = word, y = n, label = n), 
                 vjust = "top", hjust = "center",
                 fill = "white", color = "black", 
                 size = 3) +
      labs(title = "Hyppigheden for ordenes forekomst i teksten", 
           x = "Ord", 
           y = "Hyppighed", 
           fill = "Hyppighed")
    
  } #Brødrende Grimm (engelsk) - vis hele korpora 
    else if (input$text_data_plot == "Brødrene Grimms eventyr (engelsk)" & input$selected_corpora_or_text == "Hele corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>% 
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten",
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
    } #Brødrende Grimm (dansk) - vis hele korpora 
    else if (input$text_data_plot == "Brødrene Grimms eventyr (dansk)" & input$selected_corpora_or_text == "Hele corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>% 
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten",
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
    
      #H.C Andersen (engelsk) - vis enkelte tekster
    } else if (input$text_data_plot == "H.C Andersens eventyr (engelsk)" & input$selected_corpora_or_text == "Tekster"){
      selected_text_data_plot %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(title = as.factor(title),
               word = reorder_within(word, n, title)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 5, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
      #H.C Andersen (dansk) - vis enkelte tekster
    } else if (input$text_data_plot == "H.C Andersens eventyr (dansk)" & input$selected_corpora_or_text == "Tekster"){
      selected_text_data_plot %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(title = as.factor(title),
               word = reorder_within(word, n, title)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 6, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
      #H.C Andersen (engelsk)- vis hele korpora
    } else if (input$text_data_plot == "H.C Andersens eventyr (engelsk)" & input$selected_corpora_or_text == "Hele corpus"){
        selected_text_data_plot %>%
          count(word, sort = TRUE) %>%
          mutate(word = reorder(word, n)) %>%
          slice_max(n, n = slice_size, with_ties = FALSE) %>%
          ggplot(., aes(x = word, y = n, fill = n)) +
          geom_col() +
          coord_flip() +
          geom_label(aes(x = word, y = n, label = n), 
                     vjust = "top", hjust = "center",
                     fill = "white", color = "black", 
                     size = 3) +
          labs(title = "Hyppigheden for ordenes forekomst i teksten", 
               x = "Ord", 
               y = "Hyppighed", 
               fill = "Hyppighed")
      
      #H.C Andersen (dansk)- vis hele korpora
    } else if (input$text_data_plot == "H.C Andersens eventyr (dansk)" & input$selected_corpora_or_text == "Hele corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten",
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
      #St.Croix - vis enkelte tekster
    } else if (input$text_data_plot == "St. Croix Avis 1878" & input$selected_corpora_or_text == "Tekster"){
      selected_text_data_plot %>% 
        group_by(month) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(month = as.factor(month),
               word = reorder_within(word, n, month)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ month, ncol = 6, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
      #St. Croix - vis hele korpora
    } else if (input$text_data_plot == "St. Croix Avis 1878" & input$selected_corpora_or_text == "Hele corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
        
      #Jane Austen - vis enkelte tekster
    } else if (input$text_data_plot == "Jane Austens Romaner" & input$selected_corpora_or_text == "Tekster"){
      selected_text_data_plot %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ungroup() %>%
        mutate(title = as.factor(title),
               word = reorder_within(word, n, title)) %>% 
        ggplot(aes(x = word, y = n, fill = n)) +
        geom_col() +
        facet_wrap( ~ title, ncol = 3, scales = "free") +
        coord_flip() +
        scale_x_reordered() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten", 
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
      
      #Jane Austen - vis hele korpora
    } else if (input$text_data_plot == "Jane Austens Romaner" & input$selected_corpora_or_text == "Hele corpus"){
      selected_text_data_plot %>%
        count(word, sort = TRUE) %>%
        mutate(word = reorder(word, n)) %>%
        slice_max(n, n = slice_size, with_ties = FALSE) %>%
        ggplot(., aes(x = word, y = n, fill = n)) +
        geom_col() +
        coord_flip() +
        geom_label(aes(x = word, y = n, label = n), 
                   vjust = "top", hjust = "center",
                   fill = "white", color = "black", 
                   size = 3) +
        labs(title = "Hyppigheden for ordenes forekomst i teksten",
             x = "Ord", 
             y = "Hyppighed", 
             fill = "Hyppighed")
    } 
    
  })
#-------------------------------------- Wordcloud --------------------------------------------
  
  output$viz_wordcloud <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_cloud <- switch(input$text_data_cloud,
                                       "Brødrene Grimms eventyr (engelsk)" = tidy_Grimm_Brothers_en,
                                       "Brødrene Grimms eventyr (dansk)" = tidy_Grimm_Brothers_da,
                                       "H.C Andersens eventyr (engelsk)" = tidy_HC_Andersen_en,
                                       "H.C Andersens eventyr (dansk)" = tidy_HC_Andersen_da,
                                       "St. Croix Avis 1878" = tidy_Croix,
                                       "Jane Austens Romaner" = tidy_Austen)
    
    #Definerer at antal, der ønskes vist, kommer fra inputtet herfor
    word_freq_cloud <- input$word_freq_cloud
    #Definerer at ordet, der ønskes fjernet fra teksten, kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_cloud <- selected_text_data_cloud %>% 
        filter(!word %in% remove_word) 
    }
    #Visualiseringer af  wordcloud baseret på hele korpora eller enkelte tekster
    #Brødrende Grimm (engelsk) - vis enkelte tekster
    if (input$text_data_cloud == "Brødrene Grimms eventyr (engelsk)" & input$selected_corpora_or_text_cloud == "Tekster"){
      selected_text_data_cloud %>%
        group_by(title) %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = word_freq_cloud) %>%
        mutate(word = reorder(word, n)) %>%
        ungroup() %>% 
        ggplot(aes(label = word, size = n, color = n)) +
        geom_text_wordcloud() +
        facet_wrap(~title) +
        theme_minimal() +
        scale_size_area(max_size = 12) +
        scale_color_gradient()
      
      #Brødrende Grimm (dansk) - vis vis enkelte tekster
    } else if (input$text_data_cloud == "Brødrene Grimms eventyr (dansk)" & input$selected_corpora_or_text_cloud == "Tekster"){
      selected_text_data_cloud %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = word_freq_cloud) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(label = word, size = n, color = n)) +
        geom_text_wordcloud() +
        theme_minimal() +
        scale_size_area(max_size = 12) +
        scale_color_gradient()
      
      #Brødrende Grimm (engelsk) - vis hele korpora
    } else if (input$text_data_cloud == "Brødrene Grimms eventyr (engelsk)" & input$selected_corpora_or_text_cloud == "Hele corpus"){
    selected_text_data_cloud %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = word_freq_cloud) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(label = word, size = n, color = n)) +
        geom_text_wordcloud() +
        theme_minimal() +
        scale_size_area(max_size = 12) +
        scale_color_gradient()
      
      #Brødrende Grimm (dansk) - vis hele korpora
    } else if (input$text_data_cloud == "Brødrene Grimms eventyr (dansk)" & input$selected_corpora_or_text_cloud == "Hele corpus"){
      selected_text_data_cloud %>%
        count(word, sort = TRUE) %>%
        slice_max(n, n = word_freq_cloud) %>%
        mutate(word = reorder(word, n)) %>%
        ggplot(aes(label = word, size = n, color = n)) +
        geom_text_wordcloud() +
        theme_minimal() +
        scale_size_area(max_size = 12) +
        scale_color_gradient()
    
    #H.C Andersen (engelsk) - vis enkelte tekster
  } else if (input$text_data_cloud == "H.C Andersens eventyr (engelsk)" & input$selected_corpora_or_text_cloud == "Tekster"){
    selected_text_data_cloud %>%
      group_by(title) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ungroup() %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      facet_wrap(~title) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #H.C Andersen (dansk) - vis enkelte tekster
  } else if (input$text_data_cloud == "H.C Andersens eventyr (dansk)" & input$selected_corpora_or_text_cloud == "Tekster"){
    selected_text_data_cloud %>%
      group_by(title) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ungroup() %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      facet_wrap(~title) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #H.C Andersen (engelsk) - vis hele korpora
  } else if (input$text_data_cloud == "H.C Andersens eventyr (engelsk)" & input$selected_corpora_or_text_cloud == "Hele corpus"){
    selected_text_data_cloud %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #H.C Andersen (dansk) - vis hele korpora
  } else if (input$text_data_cloud == "H.C Andersens eventyr (dansk)" & input$selected_corpora_or_text_cloud == "Hele corpus"){
    selected_text_data_cloud %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #St. Croix - vis enkelte tekster
  } else if (input$text_data_cloud == "St. Croix Avis 1878" & input$selected_corpora_or_text_cloud == "Tekster"){
    selected_text_data_cloud %>%
      group_by(month) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ungroup() %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      facet_wrap(~month) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #St. Croix - vis hele korpora
  } else if (input$text_data_cloud == "St. Croix Avis 1878" & input$selected_corpora_or_text_cloud == "Hele corpus"){
    selected_text_data_cloud %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #Jane Austen - vis enkelte tekster
  } else if (input$text_data_cloud == "Jane Austens Romaner" & input$selected_corpora_or_text_cloud == "Tekster"){
    selected_text_data_cloud %>%
      group_by(title) %>%
      count(word, sort = TRUE) %>%
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ungroup() %>% 
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      facet_wrap(~title) +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
    
    #Jane Austen - vis hele korpora
  } else if (input$text_data_cloud == "Jane Austens Romaner" & input$selected_corpora_or_text_cloud == "Hele corpus"){
    selected_text_data_cloud %>%
      count(word, sort = TRUE) %>% 
      slice_max(n, n = word_freq_cloud) %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(label = word, size = n, color = n)) +
      geom_text_wordcloud() +
      theme_minimal() +
      scale_size_area(max_size = 12) +
      scale_color_gradient()
  }
  })
  #------------------------ Kontekst --------------------------------------------------
  
  #Visualisering af kwic som tabel
  output$viz_context <- renderDataTable({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_context <- switch(input$text_data_context,
                                         "Brødrene Grimms eventyr (engelsk)" = Grimm_Brothers_context_corpus_en,
                                         "Brødrene Grimms eventyr (dansk)" = Grimm_Brothers_context_corpus_da,
                                         "H.C Andersens eventyr (engelsk)" = HC_Andersen_context_corpus_en,
                                         "H.C Andersens eventyr (dansk)" = HC_Andersen_context_corpus_da,
                                         "St. Croix Avis 1878" = Croix_context_corpus,
                                         "Jane Austens Romaner" = Austen_context_corpus)
    #Definerer at antal ord, der ønskes vist, kommer fra inputtet herfor
    window_context <- input$window_context
    #Antal documenter i korporaet
    doc_sum <- ndoc(selected_text_data_context)
    output$text_doc_sum <- renderText({paste("Antal dokumenter:", doc_sum)})
    #Antal tokens i alt
    token_sum <- sum(ntoken(selected_text_data_context, remove_punct = TRUE))
    output$text_token_sum <- renderText({paste("Ord i alt:", token_sum)})
    #Antal unikke tokens
    token_unique <- sum(ntype(selected_text_data_context, remove_punct = TRUE))
    output$text_token_unique <- renderText({paste("Antal unikke ord:", token_unique)})
    
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_context <- selected_text_data_context %>% 
        tokens_remove(remove_word) 
    }
    
    #Definerer at keyworded kommer fra inputtet herfor
    select_kwic <- input$select_kwic
    #KWIC visualisering
    kwic(selected_text_data_context, pattern = phrase(select_kwic), window = window_context)
    
  })
  
  #------------------------- Bigrams --------------------------------------------------------
  #Visualisering ag Bigrams som netværksgraf
  output$viz_bigrams <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_bigrams <- switch(input$text_data_bigrams,
                                         "Brødrene Grimms eventyr (engelsk)" = bigram_Grimm_Brothers_en,
                                         "Brødrene Grimms eventyr (dansk)" =  bigram_Grimm_Brothers_da,
                                         "H.C Andersens eventyr (engelsk)" = bigram_HC_Andersen_en,
                                         "H.C Andersens eventyr (dansk)" = bigram_HC_Andersen_da,
                                         "St. Croix Avis 1878" = bigram_Croix,
                                         "Jane Austens Romaner" = bigram_Austen)
    
    #Definerer at antallet at ord i visualiseringen skal matche inputtet herfor
    wordpair_freq_bigrams <- input$wordpair_freq_bigrams
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_bigrams <- selected_text_data_bigrams %>% 
        filter(!word1 %in% remove_word) %>% 
        filter(!word2 %in% remove_word)
    }

    
    #Sikrer den samme visualisering
    set.seed(20)
    
    #Definer udseende på pilen, der markerer relationen mellem bigrams
    a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
    
    viz_bigrams_graph <- selected_text_data_bigrams %>%
      count(word1, word2, sort = TRUE) %>%
      filter(n >= wordpair_freq_bigrams) %>% 
      graph_from_data_frame()
    
      ggraph(viz_bigrams_graph, layout = "fr") +
      geom_edge_link(aes(edge_alpha = n),
                     show.legend = FALSE,
                     arrow = a,
                     end_cap = circle(.05, 'inches')) +
      geom_node_point(size = 2) +
      geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
      theme_void()
      
  })
  
}
#--------------------------- Kør appen ------------------------------------------------------
shinyApp(ui = ui, server = server)


