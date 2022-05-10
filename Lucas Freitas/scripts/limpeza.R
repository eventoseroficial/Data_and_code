#leitura do arquivo de pacotes e funções
source("scripts/pacotes_funcoes.R")

#leitura da base - hotsite 
dados_hotsite <- read_excel("dados/hotsite_14_03_2022.xlsx")

#removendo NAs
dados_hotsite = dados_hotsite[complete.cases(dados_hotsite),]

#melhorando o nome das variaveis
dados_hotsite = janitor::clean_names(dados_hotsite)

#manipulando a variavel desc_ods
dados_hotsite = dados_hotsite %>%
  mutate(desc_ods = recode(desc_ods,
                           "ODS 1 - Erradicação da Pobreza" = 1,
                           "ODS 2 - Fome Zero e Agricultura Sustentável" = 2,
                           "ODS 3 - Saúde e Bem-Estar" = 3, 
                           "ODS 4 - Educação de Qualidade" = 4,
                           "ODS 5 - Igualdade de Gênero" = 5,
                           "ODS 6 - Água Potável e Saneamento" = 6,
                           "ODS 7 - Energia Acessível e Limpa" = 7,
                           "ODS 8 - Trabalho Decente e Crescimento Econômico" = 8,
                           "ODS 9 - Industria, Inovação e Infraestrutura" = 9,
                           "ODS 10 - Redução das Desigualdades" = 10,
                           "ODS 11 - Cidades e Comunidades Sustentáveis" = 11,
                           "ODS 12 - Consumo e Produção Responsáveis" = 12,
                           "ODS 13 - Ação contra a Mudança Global do Clima" = 13,
                           "ODS 14 - Vida na Água" = 14,
                           "ODS 15 - Vida Terrestre" = 15,
                           "ODS 16 - Paz, Justiça e Instituições Eficazes" = 16,
                           "ODS 17 - Parcerias e Meios de Implementação" = 17
  )
)

#pivoteamento da tabela
dados_hotsite_bin <- dcast(dados_hotsite, id ~ desc_ods, fun.aggregate = any, value.var = "desc_ods") 
colnames(dados_hotsite_bin) = c("id",'ods_1','ods_2','ods_3','ods_4','ods_5','ods_6','ods_7','ods_8','ods_9','ods_10','ods_11','ods_12','ods_13','ods_14','ods_15','ods_16','ods_17')
dados_hotsite_bin = as.data.frame(dados_hotsite_bin)

#leitura das peças
caminho = "dados/peças"
file_list <- list.files(path = caminho, pattern = "*.pdf")
df = data.frame(matrix(ncol=0,nrow=0))
df = map_dfr(map(file_list, leitura), identity)

#verificando o número de palavras por texto
tokenizers::count_words(df$texto)

#usar as primeiras 1000 palavras, no máximo (opcional)
for(i in 1:nrow(df)){
  df$texto[i] = word(df$texto[i],
                           1, 
                           num_words(df$texto[i],1000))
}

#verificando o número de palavras por texto (< 1000 agora)
tokenizers::count_words(df$texto)

#eliminando textos NA
df = df %>% drop_na(texto)

#selecionando textos com número de palavras > 10
df = df %>% 
  filter(tokenizers::count_words(texto) > 10)

#subs "_" do id das peças para compatibilizar com id do hotsite
#opcional
df$id = gsub("_","",df$id)

#criação - base de treino
dados_hotsite_ods_16 = dados_hotsite_bin %>%
  select(id,ods_16)

df = inner_join(df,dados_hotsite_ods_16, by="id")

df$ods_16 = as.numeric(df$ods_16)

write.csv2(df,"dados/df.csv", row.names = T)
