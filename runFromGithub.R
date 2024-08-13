
#--------------------------------- Shiny Text Mining -------------------------------------

# Dette dokument indeholder kode, der gør det muligt at køre ShinyTextMining applikationen.
# For at køre applikationen skal du blot trykke ctrl + shift + enter 

#------------------------------------- Kør appen ------------------------------------------

# Her installeres og tilgås pakkerne, der skal bruges for, at køre applikationen. Pakkerne udvider basisfunktionaliteten i R og gør det blandt andet muligt at åbne og arbejde med applikationer med 
# en brugergrænseflade, hvilket er tilfældet med ShinyTextMining 
library(shiny)
library(thematic)
library(readtext)
library(writexl)
library(DT)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(ggraph)
library(igraph)
library(ggwordcloud)
library(tidygraph)

#Her fortæller vi R, at vi ønsker at åbne applikationen ShinyTextMining, der ligger på Github under AUL-Arts-Digital-Lab 
runGitHub("ShinyTextMining", "AUL-Arts-Digital-Lab") 



