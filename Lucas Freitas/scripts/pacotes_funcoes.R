##### pacotes
library(tidytext)
library(tidyverse) 
library(stringr) 
library(quanteda) 
library(forcats) 
library(ggthemes) 
library(purrr)
library(foreach)
library(tokenizers)
library(stringi)
library(quanteda.textmodels)
library(tm)
library(stylo)
library(SnowballC)
library(caret)
library(doParallel)
library(readxl)
library(reshape2)
library(udpipe)
library(lattice)
library(lubridate)
library(data.table)
library(magrittr)
library(textstem)
library(tidytext)
library(sjmisc)
library(stopwords)
library(rslp)
library(keras)
library(tidymodels)
library(tensorflow)
library(pdftools)
library(purrr)
library(textrecipes)
library(quanteda.textstats)
library(tfdatasets)
library(reticulate)
library(justop)
library(devtools)
library(ggthemes) 
library(tokenizers)
library(tesseract)
library(textrank)
library(igraph)
library(ggraph)
library(wordcloud)
library(qdapRegex)
library(textclean)

##### funções

#Função para limpar os nomes das peças
limpa_nome = function(x){
  if(grepl("_P", x, fixed = TRUE) == T){
    nome = stri_match_first_regex(x, "(.*?)\\_P")[,2]
    nome = gsub("_","",nome)
  }else if(grepl("_A", x, fixed = TRUE) == T){
    nome = stri_match_first_regex(x, "(.*?)\\_A")[,2]
    nome = gsub("_","",nome)
  }else{
    nome = gsub("_","",x)
    nome = gsub(".pdf","",nome)
  }
  
  return(nome)
}

#função para truncar textos em, no máximo, k palavras
num_words = function(x,k){
  if(tokenizers::count_words(x) < k){
    resultado = tokenizers::count_words(x)
  } else {
    resultado = k
  }
  return(resultado)
}

#leitura e limpeza das peças
leitura = function(x){
  pdf_file <- file.path(caminho, x)
  texto = pdf_text(pdf_file)
  texto = tolower(texto)
  texto = gsub("documento assinado digitalmente conforme mp n° 2.200-2/2001 de 24/08/2001."," ", texto)
  texto = gsub("o documento pode ser acessado pelo endereço http://www.stf.jus.br/portal/autenticacao/autenticardocumento.asp sob o código"," ", texto)
  texto = gsub("supremo tribunal federal ementa e acórdão inteiro teor do acórdão página"," ", texto)
  texto = gsub("o documento pode ser acessado pelo endereço http stf jus br portal", " ", texto)
  texto = gsub("inteiro teor do acórdão página", " ", texto)
  texto = gsub("autenticacao autenticardocumento asp sob o código", " ", texto)
  texto = gsub("supremo tribunal ementa acórdão", " ", texto)
  texto = gsub("supremo tribunal", " ", texto)
  texto = gsub("$", " ", texto)
  texto = gsub("•", " ", texto)
  texto = gsub("ü", " ", texto)
  texto = gsub('[[:digit:]]+', ' ', texto)
  texto = gsub("\r\n", " ", texto)
  texto = gsub("\\[|\\]", " ", texto)
  texto = gsub('§',' ',texto)
  texto = gsub("º", " ", texto)
  texto = gsub('[.]', " ", texto)
  texto = gsub("-", " ", texto)
  texto = gsub("/", " ", texto)
  texto = gsub("\\\\", " ", texto)
  texto = gsub(":", " ", texto)
  texto = gsub(";", " ", texto)
  texto = gsub("[()]", " ", texto)
  texto = gsub("www", " ", texto)
  texto = gsub("’"," ", texto)
  texto = gsub("‘"," ", texto)
  texto = gsub("°", " ", texto)
  texto = gsub("“"," ", texto)
  texto = gsub("”"," ", texto)
  texto = gsub('"'," ", texto)
  texto = gsub(','," ", texto)
  texto = gsub("<"," ", texto)
  texto = gsub(">"," ", texto)
  texto = gsub("https"," ", texto)
  texto = gsub("http"," ", texto)
  texto = gsub("fls"," ", texto)
  texto = gsub("–"," ", texto)
  texto = gsub("~", " ", texto)
  texto = gsub("_", " ", texto)
  texto = gsub("'", " ", texto)
  texto = gsub("!", " ", texto)
  texto = gsub("&", " ", texto)
  texto = gsub("$", " ", texto)
  texto = gsub("#", " ", texto)
  texto = gsub("@", " ", texto)
  texto = gsub("=", " ", texto)
  texto = gsub("»", " ", texto)
  texto = gsub("%", " ", texto)
  texto = gsub("'", " ", texto)
  texto = gsub("~~", " ", texto)
  texto = gsub("senha", " ", texto)
  texto = gsub('plenário',' ',texto)
  texto = gsub('agravo regimental',' ',texto)
  texto = gsub('ementa',' ',texto)
  texto = gsub('cópia original assinado',' ',texto)
  texto = gsub("\\b[[:alpha:]]{11,}\\b", " ", texto)
  texto = gsub("número", " ", texto)
  texto = gsub("portal", " ", texto)
  texto = gsub("acessado", " ", texto)
  texto = gsub("eletrônico", " ", texto)
  texto = gsub("chaves", " ", texto)
  texto = gsub("lei nº ", " ", texto)
  texto = gsub(" ª ", " ", texto)
  texto = gsub("iii", " ", texto)
  texto = gsub("§", " ", texto)
  texto = gsub("ii", " ", texto)
  texto = gsub("arts", " ", texto)
  texto = gsub("nº § º", " ", texto)
  texto = gsub("art § º", " ", texto)
  texto = gsub("§ º", " ", texto)
  texto = gsub("art º", " ", texto)
  texto = gsub('\\b\\w{1,3}\\b','',texto)
  texto = tm::removeWords(texto,portugues$word)
  vetor_palavras = c("documento","e","podem","que", "de","abr","jan","mar","mai","jun",
                     "ago","set","out","nov","dez","fev","jul","a", "o", "dele", "dela", "ele", "ela",
                     "as", "os", "à", "mais", "nos", "por", "às", "na", "no", "porque", "porquê", "por quê",
                     "por que", "qual", "quais", "isso", "se", "por", "ainda", "ou", "das", "dos", "só", 
                     "em", "com", "esta", "está", "da", "do", "dar", "dá", "para", "fins", "sim", "não",
                     "seu", "sua", "meu", "nosso", "nossa","chave")
  palavras = c(vetor_palavras,letters, portugues$word)
  texto = tm::removeWords(texto,palavras)
  texto = trimws(texto)
  texto = str_squish(texto)
  texto = cbind(rep(str_extract(limpa_nome(x), "[^.]+"),length(x)),texto)
  df = as.data.frame(texto)
  
  df = df %>%
    purrr::set_names(c("id", "texto"))
  
  df = df %>% 
    group_by(id) %>% 
    summarise(texto = paste0(texto, sep = "", collapse = ". "))
}


#função para predição - keras
keras_predict <- function(model, baked_data, response) {
  predictions <- predict(model, baked_data)[, 1]
  tibble(
    .pred_1 = predictions,
    .pred_class = if_else(.pred_1 < 0.5, 0, 1),
    state = response
  ) %>%
    mutate(across(c(state, .pred_class),
                  ~ factor(.x, levels = c(1, 0))))  
}
