
#-----------------------------------Anvendte biblioteker-----------------------------------

#Hent biblioteker
library(shiny)
library(readtext)
library(thematic)
library(bslib)
library(tidyverse)
library(ggplot2)
library(ggraph)
library(igraph)
library(tidytext)
library(dplyr)
library(ggwordcloud)
library(quanteda)


#------------------------------------- Stop words -------------------------------------------

#Indlæs den danske stopordsliste fra Stop_Words mappen 
stop_words_da <- read.csv("Stopwords/stop_words_da.txt")
#Lav en ny liste indeholdene selvvalgte stopord
my_stop_words <- data.frame(word = c("miss", "mrs", "sir", "mr"))

#-----------------------------------Shiny App------------------------------------------------

#---------------------------------- Definer UI-----------------------------------------------

ui <- fluidPage(
  titlePanel("Text Mining"),
  thematic::thematic_shiny(),
  tags$img(src = "DKB_logo.png", heigth = 50, width = 150, align = "right"),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 br(),
                 #Upload fil/filer 
                 fileInput("files",
                           label="Upload filer",
                           multiple = TRUE),
                 br(),
                 #Vælg sprog for stopordsliste
                 radioButtons(inputId = "language_stopwords",
                              label = "Vælg sprog for stopordslisten",
                              choices = c("da",
                                          "en"),
                              selected = "en"),
                 br(),
                 #Fjern ord fra tekster
                 selectizeInput(inputId = "remove_word",
                                label = "Søg for at fjerne ord fra tekst",
                                choices = c("choose" = "", my_stop_words),
                                selected = NULL,
                                multiple = TRUE,
                                options = list(placeholder = "", create = TRUE)),
                 #Liste til selvvalgte stopord
                 verbatimTextOutput("list_removed_word"),
                 h3("Stopord"),
                 helpText("Den integrerede stopordsliste til engelske tekster stammer fra R pakken tidytext og indeholder ord fra de tre leksika: onix, SMART og snowball."),
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
                  tabPanel("Bigrams",
                           br(),
                           h4("Info"),
                           helpText("Visualiseringen viser de ordpar, der forekommer i corpus. Juster minimusforekomsten af ordpar - ordparet fremgår i corpus mindst x antal gange"),
                           column(3,
                                  br(),
                                  #Definerer funktionen, hvor det er muligt at vælge minimum frekvens for ordene i visualiseringen
                                  sliderInput(inputId = "wordpair_freq_bigrams", 
                                              label = "Vælg minimums frekvens for ordpar i visualiseringen mellem 1 og 50",
                                              min = 1, max = 50, value = 10, step = 2)),
                           plotOutput("viz_bigrams")),
                  tabPanel("Kontekst",
                           br(),
                           h4("Info"),
                           helpText("Visualiseringen viser konteksten, hvori et fremsøgt ord eller frase optræder"),
                           helpText("Søg for at se konteksten"),
                           br(),
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
                           dataTableOutput("viz_context"))

      )
    )
  )
)

#------------------------- Definer server logic -------------------------------------------

server <- function(input, output) {
  
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
  
#-------------------------- Indlæs og klargøring filer --------------------------------------------------------  
  
  #Tidy version uden stopord (engelsk)
  tidy_corpus <- reactive({
    #Læs teksten fra txt filerne
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words)
  })
  
  #Tidy version uden stopord (dansk)
  tidy_corpus_da <- reactive({
    #Læs teksten fra txt filerne
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words_da)
  })
  
  #Tidy version med stopord
  tidy_corpus_with_stopwords <- reactive({
    #Læs teksten fra txt filerne
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    raw_corpus %>% 
      unnest_tokens(word, text)
  })
  
  #Forbered tidy version af bigrams (engelsk)
  tidy_bigram <- reactive({
    #Læs teksten fra txt filerne
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word)
  }) 
  
  #Forbered tidy version af bigrams (dansk)
  tidy_bigram_da <- reactive({
    #Læs teksten fra txt filerne
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words_da$word) %>% 
      filter(!word2 %in% stop_words_da$word)
  }) 
  
  #Lav en corpus version af teksterne, der passer til quanteda pakken
  context_corpus <- reactive({
    tibbles_of_texts <<- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <<- bind_rows(tibbles_of_texts)
    #Lav corpus
    create_corpus <<- corpus(raw_corpus)
    #Rediger docnames til title, så de bliver meningsfulde og identificerbare
    context_docid <- paste(raw_corpus$title)
    docnames(create_corpus) <- context_docid
    #Tokenize corpus teksterne
    create_corpus <- tokens(create_corpus, remove_separators = TRUE)
  })
  
#---------------------------- Nærlæs_tekst ----------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$viz_text <- renderText({
    #Skift mellem tekster med og uden stopord, alt efter hvilken knap, der er aktiveret
    if (input$selected_stopword_view == "Med"){
      selected_text_data_read <- tidy_corpus_with_stopwords()

    } else if (input$selected_stopword_view == "Uden" & input$language_stopwords == "en"){
      selected_text_data_read <- tidy_corpus()
      
    } else if (input$selected_stopword_view == "Uden" & input$language_stopwords == "da"){
      selected_text_data_read <- tidy_corpus_da()
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
    if (input$language_stopwords == "en"){
      selected_text_data_plot <- tidy_corpus()
      
      }else if(input$language_stopwords == "da"){
        selected_text_data_plot <- tidy_corpus_da()
      }
    
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
    #vis enkelte tekster
    if (input$selected_corpora_or_text == "Tekster"){
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
      
    #vis hele corpus 
   } else if (input$selected_corpora_or_text == "Hele corpus"){
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
             fill = "Hyppighed")}
  })
  
#-------------------------------------- Wordcloud --------------------------------------------

output$viz_wordcloud <- renderPlot({
  #Får output til at matche input når der skiftes mellem teksterne
  if (input$language_stopwords == "en"){
    selected_text_data_cloud <- tidy_corpus()
    
  }else if(input$language_stopwords == "da"){
    selected_text_data_cloud <- tidy_corpus_da()
  }
  
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
  #vis enkelte tekster
  if (input$selected_corpora_or_text_cloud == "Tekster"){
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
    #vis hele corpus
  } else if (input$selected_corpora_or_text_cloud == "Hele corpus"){
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
  
#------------------------- Bigrams --------------------------------------------------------
  #Visualisering ag Bigrams som netværksgraf
  output$viz_bigrams <- renderPlot({
    #Får output til at matche input når der skiftes mellem teksterne
    if (input$language_stopwords == "en"){
      selected_text_data_bigrams <- tidy_bigram()
      
    }else if(input$language_stopwords == "da"){
      selected_text_data_bigrams <- tidy_bigram_da()
    }
    
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
  #------------------------ Kontekst --------------------------------------------------
  
  #Visualisering af kwic som tabel
  output$viz_context <- renderDataTable({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_context <- context_corpus()
      
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
}
#--------------------------- Kør appen ------------------------------------------------------
shinyApp(ui, server)

