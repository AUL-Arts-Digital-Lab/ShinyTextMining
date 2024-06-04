
#-----------------------------------Anvendte biblioteker-----------------------------------

#Hent biblioteker
library(shiny) #https://cran.r-project.org/web/packages/shiny/index.html
library(thematic) #https://cran.rstudio.com/web/packages/thematic/index.html
library(readtext) #https://cran.r-project.org/web/packages/readtext/index.html
library(writexl) #https://cran.r-project.org/web/packages/writexl/index.html
library(tidyverse) #https://cran.r-project.org/web/packages/tidyverse/index.html
library(tidytext) #https://cran.r-project.org/web/packages/tidytext/index.html
library(quanteda) #https://cran.r-project.org/web/packages/quanteda/index.html
library(quanteda.textstats) #https://cran.r-project.org/web/packages/quanteda.textstats/index.html
library(ggraph) #https://cran.r-project.org/web/packages/ggraph/index.html
library(igraph) #https://cran.r-project.org/web/packages/igraph/index.html
library(ggwordcloud) #https://cran.r-project.org/web/packages/ggwordcloud/index.html
library(RColorBrewer) #https://cran.r-project.org/web/packages/RColorBrewer/index.html

#------------------------------------- Stop words -------------------------------------------

#Indlæs den danske stopordsliste fra Stop_Words mappen 
stop_words_da <- read.csv("Stopwords/stop_words_da.txt")
#Lav en ny liste indeholdene selvvalgte stopord
my_stop_words <- data.frame(word = c("miss", "mrs", "sir", "mr"))

#-----------------------------------Shiny App------------------------------------------------

#---------------------------------- Definer UI-----------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "shinyLayout.css")
  ),   
  
  titlePanel(title = "Text Mining"),
  thematic::thematic_shiny(),
  
  sidebarLayout(
    sidebarPanel(width = 3,
                 br(),
                 #Upload fil/filer 
                 fileInput("files",
                           label="Upload filer",
                           multiple = TRUE),
                 textOutput("file_size_info"),
                 br(),
                 #Vælg encoding
                 radioButtons(inputId = "corpus_encoding",
                              label = "Vælg encoding for corpus",
                              choices = c("UTF-8",
                                          "latin1"),
                              selected = "UTF-8"),
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
                 br(),
                 textOutput("help_info"),
                 br(),
                 br(),
                 span(tags$img(src = "logo-digital-white.svg"))),
    mainPanel(
      #Danner et menu-layout, hvor det er muligt at skifte mellem visualiseringerne
      tabsetPanel(type = "tabs",
                  tabPanel("Oversigt",
                           br(),
                           h4("Info"),
                           helpText("På denne side får du et overblik over corpus"),
                           br(),
                                  textOutput("text_doc_sum"),
                                  br(),
                                  textOutput("text_token_sum"),
                                  br(),
                                  textOutput("text_token_unique"),
                                  br(),
                           DT::DTOutput("text_token_sum_doc")),
                  tabPanel("Nærlæs tekst",
                           br(),
                           h4("Info"),
                           helpText("Her er det muligt at se et udsnit fra corpus, hvor stopord enten er fjernet eller stadig optræder."),
                           column(12,
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
                                              min = 5, max = 30, value = 20, step = 5)),
                           plotOutput("viz_wordcloud")),
                  tabPanel("Bigrams",
                           br(),
                           h4("Info"),
                           helpText("Visualiseringen viser de ordpar, der forekommer i corpus. Juster minimusforekomsten af ordpar - ordparet fremgår i corpus mindst x antal gange"),
                           column(3,
                                  br(),
                                  radioButtons(inputId = "selected_corpora_or_text_bigrams",
                                               label = "Vis for hele corpus eller se fordelingen på tekster",
                                               choices = c("Hele corpus",
                                                           "Tekster"),
                                               selected = "Hele corpus")),
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
                           br(),
                           column(3,
                                  br(),
                                  #Definerer funktionen, der gør det muligt at fremsøge et keyword
                                  textInput(inputId = "select_kwic",
                                            label = "Søg her efter et ord eller en frase",
                                            value = "")),
                           column(3,
                                  br(),
                                  sliderInput(inputId = "window_context", 
                                              label = "Vælg størrelse på ordets kontekst",
                                              min = 1, max = 50, value = 1, step = 1)),
                           column(3,
                                  br(),
                                  downloadButton(outputId ="download_KWIC", 
                                                 label = "Download tabel")),
                                  br(),
                           DT::DTOutput("viz_context")),
                  tabPanel("Begrebsafklaring",
                           br(),
                           h4("Info"),
                           helpText("På denne side finder du definitioner og forklaringer på de begreber og visualiseringer, der bliver anvendt i applikationen"),
                           br(),
                           h4("Upload filer:"),
                           textOutput("term_info_text_1"),
                           br(),
                           h4("Vælg encoding for corpus:"),
                           textOutput("term_info_text_11"),
                           br(),
                           h4("Stopord:"),
                           textOutput("term_info_text_2"),
                           br(),
                           h4("Vælg sprog for stopordsliste:"),
                           textOutput("term_info_text_3"),
                           br(),
                           h4("Søg for at fjerne ord fra teksten:"),
                           textOutput("term_info_text_4"),
                           br(),
                           h4("Oversigt:"),
                           textOutput("term_info_text_10"),
                           br(),
                           h4("Nærlæs tekst:"),
                           textOutput("term_info_text_5"),
                           br(),
                           h4("Søjlediagram:"),
                           textOutput("term_info_text_6"),
                           br(),
                           h4("Wordcloud:"),
                           textOutput("term_info_text_7"),
                           br(),
                           h4("Bigrams:"),
                           textOutput("term_info_text_8"),
                           br(),
                           h4("Kontekst:"),
                           textOutput("term_info_text_9"),
                           br(),
                           h4("Hvis du vil vide mere:"),
                           textOutput("reference_info_text"),
                           br())
                  

      )
    )
  )
)

#------------------------- Definer server logic -------------------------------------------

server <- function(input, output) {
  #Gør det muligt at uploade filer på 30MB frem for default værdien på 5MB
  options(shiny.maxRequestSize=30*1024^2)
  
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
  #Info om ventetid ved store filer
  output$file_size_info <- renderText({
    paste("Store filer tager længere tid at uploade end små. Hav tålmodighed, hvis visualiseringerne ikke viser sig med det samme.")
  })
  #Info tekst om begrebsafklaring
  output$help_info <- renderText({
    paste("Er du i tvivl om et begreb, en formulering, en udregning eller andet? Find svar under fanen Begrebsafklaring.")
  })
  
#-------------------------- Indlæs og klargøring filer --------------------------------------------------------  
  
  #Tidy version uden stopord (engelsk)
  tidy_corpus <- reactive({
    req(input$files)
    #Læs teksten fra filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words)
  })
  
  #Tidy version uden stopord (dansk)
  tidy_corpus_da <- reactive({
    req(input$files)
    #Læs teksten fra filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text) %>% 
      anti_join(stop_words_da)
  })
  
  #Tidy version med stopord
  tidy_corpus_with_stopwords <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(word, text)
  })
  
  #Forbered tidy version af bigrams (engelsk)
  tidy_bigram <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words$word) %>% 
      filter(!word2 %in% stop_words$word)
  }) 
  
  #Forbered tidy version af bigrams (dansk)
  tidy_bigram_da <- reactive({
    req(input$files)
    #Læs teksten fra txt filerne
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    raw_corpus %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
      filter(!is.na(bigram)) %>% 
      separate(bigram, c("word1", "word2"), sep = " ") %>% 
      filter(!word1 %in% stop_words_da$word) %>% 
      filter(!word2 %in% stop_words_da$word)
  }) 
  
  #Lav en corpus version af teksterne, der passer til quanteda pakken
  context_corpus <- reactive({
    req(input$files)
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    #Lav corpus
    create_corpus <- corpus(raw_corpus)
    #Rediger docnames til title, så de bliver meningsfulde og identificerbare
    context_docid <- paste(raw_corpus$title)
    docnames(create_corpus) <- context_docid
    #Tokenize corpus teksterne
    create_corpus <- tokens(create_corpus, remove_separators = TRUE)
  })
  
  #Lav en corpus version af teksterne, der passer til quanteda pakkens readability funktion
  context_corpus_LIX <- reactive({
    req(input$files)
    tibbles_of_texts <- map_df(input$files$datapath, readtext) %>% 
      mutate(title = str_remove(input$files$name, ".txt")) %>% 
      mutate(title = str_remove(input$files$name, ".pdf"))
    #Forbind de enkelte filer/tekster i en dataframe
    raw_corpus <- bind_rows(tibbles_of_texts)
    #Definer encodingen i forhold til læsning af specialtegn
    if (input$corpus_encoding == "latin1"){
      Encoding(raw_corpus$text) <- "latin1"
    } else{
      Encoding(raw_corpus$text) <- "UTF-8"
    }
    #Lav corpus
    create_corpus <- corpus(raw_corpus)
  })

#---------------------------- Oversigt ---------------------------------------------------------
  
  #Får output til at matche input når der skiftes mellem teksterne
  output$text_token_sum_doc <- DT::renderDT({
    selected_text_data_sum <- context_corpus()
    selected_text_data_sum_LIX <- context_corpus_LIX()
    
    #Definerer at ordet, der ønskes fjernet fra teksten kommer fra inputtet herfor
    remove_word <- input$remove_word
    #Sorterer selvalgt stopord fra den valgte tekst
    if (!is.null(remove_word)){
      selected_text_data_sum <- selected_text_data_sum %>% 
        filter(!word %in% remove_word) 
    }
    #Antal documenter i korporaet
    doc_sum <- ndoc(selected_text_data_sum)
    output$text_doc_sum <- renderText({paste("Antal tekster i corpus: ", doc_sum)})
    #Antal tokens i alt
    token_sum <- sum(ntoken(selected_text_data_sum, remove_punct = TRUE))
    output$text_token_sum <- renderText({paste("Samlet antal ord i corpus: ", token_sum)})
    #Antal unikke tokens
    token_unique <- sum(ntype(selected_text_data_sum, remove_punct = TRUE))
    output$text_token_unique <- renderText({paste("Antal unikke ord i corpus: ", token_unique)})
    #Antal ord i de forskellige tekster
    tokens_in_texts <- ntoken(selected_text_data_sum, remove_punct = TRUE)
    
    #LIX beregning for de forskellige tekster
    LIX <- textstat_readability(selected_text_data_sum_LIX, "LIW", min_sentence_length = 2)
    
    #Lav en data frame over antal ord i hver tekst
    text_info_df <- data.frame(docnames(selected_text_data_sum), tokens_in_texts, LIX)
    #Fjern ekstra kolonne med information om tekstens placering i corpus
    text_info_df <-  subset(text_info_df, select = -document)
    
    #Ændrer navnet på kolonnerne
    names(text_info_df)[2] <- "Antal ord"
    names(text_info_df)[3] <- "LIX"
    
    #Fjern ekstra kolonne med samme værdi som tekst kolonnen
    text_info_df <- subset(text_info_df, select = c("Antal ord", "LIX"))
    text_info_df
  }, options = list(language = list(search = "Filtrer i tabellen")))
  
  
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
        scale_fill_gradient(low = "#CAF0FE", high = "#002E70")+
        facet_wrap( ~ title, ncol = 4, scales = "free") +
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
        scale_fill_gradient(low = "#CAF0FE", high = "#002E70") +
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
      scale_color_gradient(low = "#CAF0FE", high = "#002E70")
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
      scale_color_gradient(low = "#CAF0FE", high = "#002E70")
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
    
    #vis enkelte tekster
    if (input$selected_corpora_or_text_bigrams == "Tekster"){
    viz_bigrams_graph <- selected_text_data_bigrams %>%
      group_by(title) %>%
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
    } else if (input$selected_corpora_or_text_bigrams == "Hele corpus"){
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
    }
  })
  #------------------------ Kontekst --------------------------------------------------
  
  #Visualisering af kwic som tabel
  output$viz_context <- DT::renderDT({
    #Får output til at matche input når der skiftes mellem teksterne
    selected_text_data_context <- context_corpus()
      
    #Definerer at antal ord, der ønskes vist, kommer fra inputtet herfor
    window_context <- input$window_context
    
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
    KWIC <<- kwic(selected_text_data_context, pattern = phrase(select_kwic), window = window_context)

  }, options = list(language = list(zeroRecords = "Søg efter ord eller frase for at se resultaterne", search = "Filtrer i tabellen")))
  
  #Download KWIC tabel til en xlsx (Excel) fil
  output$download_KWIC <- downloadHandler(
    filename = function() { "kontekst_tabel.xlsx"},
    content = function(file) {write_xlsx(KWIC, path = file)
    })
  
#--------------------------- Begrebsafklaring ------------------------------------------------
  output$term_info_text_1 <- renderText({
    paste("Her kan du uploade en eller flere filer, der fungere som corpus i visualiseringerne. For at få det bedste resultat, skal filerne skal være i txt eller pdf format. Maximum filstørrelse er 30MB pr. fil.")
  })
  
  output$term_info_text_2 <- renderText({
    paste("Stopord er de ord, der optræder i en text eller et corpus uden at være meningsgivende herfor. Dvs. ord som de danske ord 'og', 'i', 'at' og de engelske ord 'the', 'a', 'an' etc.")
  })
  
  output$term_info_text_3 <- renderText({
    paste("Stopordslisterne indeholder henholdsvis engelske og danske stopord. Den integrerede stopordsliste til engelske tekster stammer fra R pakken tidytext og indeholder ord fra de tre leksika: onix, SMART og snowball. Stopordslisten til de danske tekster er udarbejdet af informationsspecialister ved Det Kgl. Bibliotek. Begge stopordslister kan findes i mappen Stopwords.")
  })
  
  output$term_info_text_4 <- renderText({
    paste("Fjern ord fra teksten, der ikke i forvejen er registreret som stopord, men derimod ord, der ikke er relevant for det pågældende corpus eller analyse.")
  })
  
  output$term_info_text_5 <- renderText({
    paste("I denne visualisering vises et tekstuddrag svarende til 1000 ord for det pågældende corpus. Her er det muligt at se en tekst med eller uden stopord. Dette er med til at illustrere, hvilken betydning det kan have for et corpus og dets indhold, når stopord fjernes.")
  })
  
  output$term_info_text_6 <- renderText({
    paste("I denne visualisering vises de hyppigst forekommende ord og antallet af disse forekomster som et søjlegram. Her er det muligt at se de hyppigst forekommende ord for hele corpus eller fordelt på enkelte tekster. Det er muligt at justere, hvor mange ord, der skal vises i visualiseringen - ønskes de hyppigste fem, ti eller tyve ord vist i visualiseringen.")
  })
  
  output$term_info_text_7 <- renderText({
    paste("I denne visualisering vises de hyppigst forekommende ord og antallet af disse forekomster som en ordsky. Her er det muligt at se de hyppigst forekommende ord for hele corpus eller fordelt på enkelte tekster. Det er muligt at justere, hvor mange ord, der skal vises i visualiseringen - ønskes de hyppigste fem, ti eller tyve ord vist i visualiseringen. Jo større et ord er i visualiseringen, des flere gange forekommer det i corpus eller i den enkelte tekst.")
  })
  
  output$term_info_text_8 <- renderText({
    paste("I denne visualisering vises ordpar, der forekommer i corpus. Her er det muligt at ændre minimumsforekomsten af et ordpar i visualiseringen - et ordpar forekommer mindst fem, ti eller tyve gange i corpus, før det vises i visualiseringen. Pilene markerer rækkefølgen ordene optræder i - peger pilen fra 'a' til 'b', står 'a' før 'b' i ordparet.")
  })
  
  output$term_info_text_9 <- renderText({
    paste("I denne visualiseringen vises konteksten, hvori et ord eller en frase optræder i. Denne metode indenfor text mining kaldes Key Word in Context (KWIC). Det er muligt at søge efter et enkelt ord eller en frase bestående af flere ord. Derudover er det muligt at anvende * i sin søgning. Dette vil give flere resultater, da man her kan søge efter alle endelser og ikke udelukkende det fremsøgte ord - søger man efter ordet 'træ' efterfulgt af * (træ*), vil man få resultater med træet, træer, træerne etc. Kolonnen 'docname' beskriver titlen på den tekst i corpus, hvor ordet og konteksten optræder. Navnet er efterfulgt af et tal, der giver en mere specifik beskrivelse af, hvor ordet eller frasen står. Kolonnerne 'from' og 'to' beskriver placeringen af ordet eller frasen i sætningen og er først relevant, når man er interesseret i deres specifikke placering. Kolonnerne 'pre' og 'post' beskriver konteksten ordet optræder i, dvs. ordene, der står før og efter det/de fremsøgte ord. Kolonnen 'keyword' indeholder informationer om det fremsøgte ord og hvordan det står skrevet i teksten. Kolonnen 'pattern' indeholder selve inputtet. Det er muligt at se det fremsøgte ords eller frases hyppighed i corpus nederst i tabellens venstre hjørne - her står det beskrevet som entries. ")
  })
  
  output$term_info_text_10 <- renderText({
    paste("Viser en oversigt over det pågældende corpus. Antallet af dokumenter, der optæder i corpus. Antal ord i corpus samt antal ord i de enkelte tekster. Antal unikke ord er en udregning af, hvor mange forskellige ord, der optræder i corpus. På den måde tæller hvert enkelt ord kun for en og er ikke påvirket af, om det optræder i corpus en eller flere gange. Lixtal er en beregning af en teksts sværhedsgrad i forhold til læsbarhed. Definitionen på LIX er fra Björnsson (1968). Udregningen af LIX er lavet ved følgende formel: ASL + ((100 * Nwsy >= 7)/Nw). Her er ASL = Average sentence length (dvs. det samlede antal ord divideret med antallet af sætninger), Nw = number of words og Nwsy = number of word syllables. Denne er er målrettet >= 7 (dvs. ord på mere end seks bogstaver). I udregningen anvendes yderligere et parameter: min_sentence_length = 2. Dette parameter har til formål at definere minimumslængden for en sætning på baggrund af antallet af ord. Her er en sætning defineret som det, der står foran et punktum. Ved at sætte grænsen ved to frem for en, undgår vi at tælle 'falske' sætninger med. Dvs. at sætninger der eksempelvis starter med '1.000' eller 'H.C. Andersen' ikke bliver anset som sætninger.")
  })
  
  output$term_info_text_11 <- renderText({
    paste("Gør det muligt at vælge mellem forskellige tekst afkodninger. Dette er særligt relevant i forbindelse med tekster indeholdende danske specialtegn: æ, ø og å. Her kan latin1 være en fordel. UTF-8 er den universelle standard for afkoding af tekst, og denne er ofte bedst til engelsksprogede tekster.")
  })
  
  output$reference_info_text <- renderText({
    paste("Text mining applikationen tager udgangspunkt i tidyverse og quanteda principper for databehandling og visualisering. Der tages udgangspunkt i tidyverse og tidytext i fanerne Nærlæs tekst, Søjlediagram, Wordcloud og Bigram. Du kan læse mere om dette i følgende: https://www.tidytextmining.com/. Fanerne indeholdende Oversigt og Kontekst er baseret på grundprincipper inden for quanteda. Læs mere om dette via følgende:http://quanteda.io/.")
  })  
}

#--------------------------- Kør appen ------------------------------------------------------
shinyApp(ui, server)

