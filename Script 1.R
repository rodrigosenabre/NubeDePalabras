install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")
install.packages("genius")
install.packages("tm")
install.packages("rvest")
library(rvest)
library (tm)
require(tidytext)
require(tidyverse)
library(wordcloud2)
require(genius)
library(ggplot2)
view(text_wordcounts)

#Michael Jackson- Thriller
MJ <- genius_album(artist = "Michael Jackson", album = "Thriller")
MJ <- MJ %>% select(lyric)
MJ <- paste(MJ, collapse = " ")
texto <- tibble(Text= MJ)
palabras <- texto %>% unnest_tokens(output = word, input = Text)
palabras <- palabras %>% anti_join(stop_words)
contarpalabras <- palabras %>% count(word, sort = TRUE)
contarpalabras <- contarpalabras %>% mutate(word= str_remove_all(word, "ssa|na|p.y.t|sa"))
contarpalabras

MJGraph <- wordcloud2(contarpalabras,
                      color = "random-dark", backgroundColor = "white",
                      shape = 'circle')

MJGraph

#Arctic Monkeys
AM <- genius_album(artist = "Arctic Monkeys", album = "AM")
AM <- AM %>% select(lyric)
view(AM)
AM <- paste(AM, collapse = " ")
AMtexto <- tibble(Text= AM)
AMpalabras <- AMtexto %>% unnest_tokens(output = word, input = Text)
AMpalabras <- AMpalabras %>% anti_join(stop_words)
AMcontarpalabras <- AMpalabras %>% count(word, sort = TRUE)
AMcontarpalabras <- AMcontarpalabras %>% mutate(word= str_remove_all(word, "wan|la|sp|oooh|wop|shoo|na"))
AMcontarpalabras

AMGraph <- wordcloud2(AMcontarpalabras,
                      color = "random-dark", backgroundColor = "white")

AMGraph

#RHCP
RHCP <- genius_album(artist = "Red Hot Chili Peppers", album = "By The Way")
RHCP <- RHCP %>% select(lyric)

view(RHCP)
RHCP <- paste(RHCP, collapse = " ")
RHCPtexto <- tibble(Text= RHCP)
RHCPpalabras <- RHCPtexto %>% unnest_tokens(output = word, input = Text)
RHCPpalabras <- RHCPpalabras %>% anti_join(stop_words)
RHCPcontarpalabras <- RHCPpalabras %>% count(word, sort = TRUE)
#RHCPcontarpalabras <- RHCPcontarpalabras %>% mutate(word= str_remove_all(word, "wan|la|sp|oooh|wop|shoo|na"))
RHCPcontarpalabras

RHCPGraph <- wordcloud2(RHCPcontarpalabras,
                      color = "random-dark", backgroundColor = "white",
                      shape = 'circle')
RHCPGraph

#SodaStereo
Soda <- genius_album(artist = "Soda Stereo", album = "Doble vida")
Soda <- Soda %>% select(lyric)


view(Soda)
Soda <- paste(Soda, collapse = " ")
Sodatexto <- tibble(Text= Soda)
custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                          lexicon = "custom"))
Sodapalabras <- Sodatexto %>% unnest_tokens(output = word, input = Text)
Sodapalabras <- Sodapalabras %>% anti_join(custom_stop_words)
Sodacontarpalabras <- Sodapalabras %>% count(word, sort = TRUE)
Sodacontarpalabras <- Sodacontarpalabras %>% mutate(word= str_remove_all(word, "yeh|uh|na"))
Sodacontarpalabras

SodaGraph <- wordcloud2(Sodacontarpalabras,
                      color = "random-dark", backgroundColor = "white",
                      shape = 'circle')
SodaGraph

#Bad Bunny
BB <- genius_album(artist = "Bad Bunny", album = "YHLQMDLG")
BB <- BB %>% select(lyric)

view(BB)
BB <- paste(BB, collapse = " ")
BBtexto <- tibble(Text= BB)
custom_stop_words <- bind_rows(stop_words,
                               tibble(word = tm::stopwords("spanish"),
                                      lexicon = "custom"))
BBpalabras <- BBtexto %>% unnest_tokens(output = word, input = Text)
BBpalabras <- BBpalabras %>% anti_join(custom_stop_words)
BBcontarpalabras <- BBpalabras %>% count(word, sort = TRUE)
BBcontarpalabras <- BBcontarpalabras %>% mutate(word= str_remove_all(word, "205f|ku"))
BBcontarpalabras

BBGraph <- wordcloud2(BBcontarpalabras,
                        color = "random-dark", backgroundColor = "white",
                        shape = 'circle')
BBGraph
