#### Carregando pacotes necessários ####
# Manipulação de bases de dados
library(lubridate)
library(tidyverse)
library(splitstackshape)
library(reshape2)

# Leitura de dados
library(readxl)
library(read.dbc)
library(foreign)

# Criação de gráficos
library(ggplot2)
library(tmap)
library(colorspace)

# Dados espaciais
library(spdep)

# Análise de sobrevivência relativa
library(Epi)
library(popEpi)

# Modelos Bayesianos
library(rstan)
library(INLA)
library(loo)

#### Opções do Stan ####
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# Definindo diretório
dirname(rstudioapi::getSourceEditorContext()$path) |> 
  setwd()

setwd('data')

#### Definindo lista de municípios do RJ para uma variável mun ####
mun <- read_xlsx('rj_mun.xlsx') |> 
  tibble()

mun <- mun |> 
  dplyr::select(1, 2) |> 
  rename(mun = `Município [-]`,
         cod = `Código [-]`) |>
  mutate(cod = as.character(cod))

mun <- mun[1:92, ]

#### Carregando as bases RHC 2015 - 2019 ####
# rhc10 <- read.dbf('rhc10.dbf', as.is = T) |>
#   tibble()
# 
# rhc11 <- read.dbf('rhc11.dbf', as.is = T) |>
#   tibble()
# 
# rhc12 <- read.dbf('rhc12.dbf', as.is = T) |>
#   tibble()
# 
# rhc13 <- read.dbf('rhc13.dbf', as.is = T) |>
#   tibble()
# 
# rhc14 <- read.dbf('rhc14.dbf', as.is = T) |>
#   tibble()
# 
# rhc15 <- read.dbf('rhc15.dbf', as.is = T) |>
#   tibble()
# 
# rhc16 <- read.dbf('rhc16.dbf', as.is = T) |>
#   tibble()
# 
# rhc17 <- read.dbf('rhc17.dbf', as.is = T) |>
#   tibble()
# 
# rhc18 <- read.dbf('rhc18.dbf', as.is = T) |>
#   tibble()
# 
# rhc19 <- read.dbf('rhc19.dbf', as.is = T) |>
#   tibble()
# 
# rhc <- full_join(rhc10, rhc11) |>
#   full_join(rhc12) |>
#   full_join(rhc13) |>
#   full_join(rhc14) |>
#   full_join(rhc15) |>
#   full_join(rhc16) |>
#   full_join(rhc17) |>
#   full_join(rhc18) |>
#   full_join(rhc19)
# 
# remove(rhc10, rhc11, rhc12, rhc13,
#        rhc14, rhc15, rhc16, rhc17, rhc18, rhc19)
# 
# # Salvando dados como RDS para não repetir o processo acima a todo instante
# write_rds(rhc, 'rhc.rds')

# Lendo os dados para o arquivo
rhc <- read_rds('rhc.rds')

#### Tratando a base RHC ####

# Inserindo dados de município na base rhc e filtrando apenas para observações
# no estado do RJ
rhc <- rhc |>
  subset(PROCEDEN %in% mun$cod) |>
  inner_join(mun,
             by = c('PROCEDEN' = 'cod'))

# Tratando as variáveis na base
rhc$mun <- rhc$mun |> # município de residência
  factor()

rhc$IDADE <- rhc$IDADE |> # idade
  as.numeric()

rhc$LOCTUDET <- rhc$LOCTUDET |> # CID-10 do câncer
  factor()

rhc$DATAPRICON <- rhc$DATAPRICON |> # data da primeira consulta
  dmy()

rhc$DTDIAGNO <- rhc$DTDIAGNO |> # data do diagnóstico
  dmy()

rhc$DATAOBITO <- rhc$DATAOBITO |> # data do óbito, se este ocorreu 
  dmy()

# Mudando a variável IDADE de data de nascimento até data da primeira consulta
# para data de nascimento até data do diagnóstico
rhc <- rhc |> 
  mutate(IDADE = ifelse(!is.na(DTDIAGNO) & !is.na(DATAPRICON), # atualiza a idade com base na diferença em anos entre DTDIAGNO e DATAPRICON
                        IDADE + round(interval(DATAPRICON, DTDIAGNO) / years(1)),
                        IDADE))

# Selecionando as variáveis de interesse
rhc <- rhc |> 
  select(IDADE, SEXO, DTPRICON, DTDIAGNO, DATAOBITO, PROCEDEN, LOCTUDET, mun)

# Criando variável death que indica a morte do indivíduo no período de estudo (0
# se estiver vivo, 1 se foi observado o óbito)
rhc <- rhc |> 
  mutate(death = ifelse(!is.na(DATAOBITO) & (DATAOBITO < dmy('01/01/2020')),
                        1,
                        0))

# Definindo a coorte de pacientes com câncer de pulmão:
# Data de diagnóstico de 01/01/2010 até 01/01/2020
# Idade de 25 a 100 anos
# Diagnosticados com câncer de pulmão
rhc <- rhc |> 
  filter(IDADE >= 25 & IDADE <= 100 & LOCTUDET == 'C34' &
           DTDIAGNO < dmy('01/01/2020') & DTDIAGNO >= dmy('01/01/2010'))

# Criando variável surv que sendo esse o tempo de sobrevivência (tempo entre
# diagnóstico e morte em dias) e exit_, que indica quando o indivíduo saiu do
# estudo (data da morte, se aconteceu antes de 01/01/2020, ou 01/01/2020 se
# sobreviveu além desse tempo, ou seja, censurado à direita).
rhc <- rhc |> 
  mutate(exit_ = ifelse(as.logical(death) & (DATAOBITO < dmy('01/01/2020')),
                        as.character(DATAOBITO),
                        ifelse(DTDIAGNO + years(3) < dmy('01/01/2020'),
                               as.character(DTDIAGNO + years(3)),
                               '2020-01-01')),
         exit_ = ymd(exit_),
         surv = interval(DTDIAGNO, exit_) / days(1))

# Criando variável fx_eta na base rhc para representar a faixa etária dos
# indivíduos, sendo essas definidas como:
# 1: 25 até 54
# 2: 55 até 59
# 3: 60 até 64
# 4: 65 até 69
# 5: 70 em diante
rhc <- rhc |> 
  mutate(fx_eta = cut(x = IDADE,
                      breaks = c(24, 54, 59, 64, 69, Inf),
                      labels = c(1, 2, 3, 4, 5)))

#### Carregando dados de mortalidade e da população de cada área ####

##### ------------------------- #####
# Para evitar todo o trabalho a seguir, pode-se pular para a linha 371
##### ------------------------- #####

# Criando uma função import para auxiliar na importação de arquivos
import <- function(nome, ano) {
  arqF <- paste(nome, '_F.csv', sep = '') # arquivo referente ao sexo feminino
  arqM <- paste(nome, '_M.csv', sep = '') # arquivo referente ao sexo masculino
  
  
  baseF <- read.csv2(arqF) |> # base feminina
    tibble()
  
  baseF$sex <- 2
  baseF$year <- ano
  
  # passando dados da base para valores numéricos
  baseF[, 2:ncol(baseF)] <- lapply(baseF[, 2:ncol(baseF)], as.numeric)
  
  baseM <- read.csv2(arqM) |> # base masculina
    tibble()
  
  baseM$sex <- 1
  baseM$year <- ano
  
  baseM[, 2:ncol(baseM)] <- lapply(baseM[, 2:ncol(baseM)], as.numeric)
  
  return(full_join(baseF, baseM)) # juntando ambas as bases
}

# Lendo as bases de 2010 à 2019 do DATASUS de mortalidade (TabNet)
base10 <- import('mort10', 2010)
base11 <- import('mort11', 2011)
base12 <- import('mort12', 2012)
base13 <- import('mort13', 2013)
base14 <- import('mort14', 2014)
base15 <- import('mort15', 2015)
base16 <- import('mort16', 2016)
base17 <- import('mort17', 2017)
base18 <- import('mort18', 2018)
base19 <- import('mort19', 2019)

# As bases 11, 12, 13, 14, 15 e 19 devem receber o mesmo tratamento, sendo esse
# diferente das outras bases de dados. Isso se deve por causa do número de
# colunas serem diferentes e um merge entre eles causar problemas.
base1519 <- full_join(base15, base19) |> 
  full_join(base11) |> 
  full_join(base12) |> 
  full_join(base13) |> 
  full_join(base14)

base_resto <- full_join(base16, base17) |> 
  full_join(base18) |> 
  full_join(base10)

# Limpando o ambiente
remove(base10, base11, base12, base13, base14, base15, base16, base17, base18, base19)

#### Trabalhando nas bases de mortalidade ####

# Realizando algumas alterações na base mun para que ela seja compatível com a
# variável Município nas bases de mortalidade
mun <- mun |> 
  mutate(cod = substr(cod,1,nchar(cod)-1),
         cod_mun = paste(cod, mun, sep = ' '))

base1519 <- base1519 |> 
  base::subset(Município %in% mun$cod_mun) |> # Filtrando apenas por municípios no estado do RJ
  rename(mun = Município) |> # renomeando a variável Município
  mutate(cod = substr(mun, 1, 6), # separando o código do nome do município
         mun = substr(mun, 8, nchar(mun)))

base_resto <- base_resto |> 
  base::subset(Município %in% mun$cod_mun) |>
  rename(mun = Município) |> 
  mutate(cod = substr(mun, 1, 6),
         mun = substr(mun, 8, nchar(mun)))

# Tratando as bases base1519 e base_resto para que elas possam ser trabalhadas
# em conjunto
base1519 <- base1519 |> 
  mutate(X0.a.4.anos = select(base1519, X0.a.6.dias:X1.a.4.anos) |> rowSums(na.rm = T)) |> 
  select(cod, mun, year, sex, X0.a.4.anos, X5.a.9.anos:X80.anos.e.mais)

base_resto <- base_resto |> 
  mutate(X0.a.4.anos = select(base_resto, X0.a.6.dias:X1.a.4.anos) |> rowSums(na.rm = T)) |> 
  select(cod, mun, year, sex, X0.a.4.anos, X5.a.9.anos:X80.anos.e.mais)

# Juntando as duas bases agora que elas podem receber tratamentos iguais
popmort <- full_join(base_resto, base1519)

remove(base1519, base_resto)

# Agrupando os dados para criar uma variável fx_eta (faixa etária) tal que
# 1: 25 - 54 anos
# 2: 55 - 59 anos
# 3: 60 - 64 anos
# 4: 65 - 69 anos
# 5: 70+ anos
popmort <- popmort |> 
  mutate(fx1 = select(popmort, X25.a.29.anos:X50.a.54.anos) |> rowSums(na.rm = T),
         fx2 = select(popmort, X55.a.59.anos) |> rowSums(na.rm = T),
         fx3 = select(popmort, X60.a.64.anos) |> rowSums(na.rm = T),
         fx4 = select(popmort, X65.a.69.anos) |> rowSums(na.rm = T),
         fx5 = select(popmort, X70.a.74.anos:X80.anos.e.mais) |> rowSums(na.rm = T)) |>
  select(cod, mun, year, sex, fx1, fx2, fx3, fx4, fx5)

# Expandindo a base utilizando pivot_longer, de forma a transformar a faixa 
# etária em uma covariável assim como município e sexo
popmort <- popmort |>
  pivot_longer(cols = starts_with('fx'),
               names_to = 'faixa_etaria', values_to = 'value') |>
  group_by(cod, mun, sex, year, faixa_etaria) |>
  summarise(popmort = sum(value)) |> 
  mutate(faixa_etaria = substr(faixa_etaria, 3, 3))

#### Importando as bases de população residentes em cada área de 2015 a 2019 ####
pop10 <- import('pop10', 2010)
pop11 <- import('pop11', 2011)
pop12 <- import('pop12', 2012)
pop13 <- import('pop13', 2013)
pop14 <- import('pop14', 2014)
pop15 <- import('pop15', 2015)
pop16 <- import('pop16', 2016)
pop17 <- import('pop17', 2017)
pop18 <- import('pop18', 2018)
pop19 <- import('pop19', 2019)

# Todas as bases, nesse caso, podem receber tratamentos iguais. Portanto:
pop <- full_join(pop15, pop16) |> 
  full_join(pop17) |> 
  full_join(pop18) |> 
  full_join(pop19) |> 
  full_join(pop10) |>
  full_join(pop11) |> 
  full_join(pop12) |> 
  full_join(pop13) |> 
  full_join(pop14)

# Limpando o ambiente
remove(pop10, pop11, pop12, pop13, pop14, pop15, pop16, pop17, pop18, pop19)

#### Trabalhando na base pop ####
# Selecionando os dados populacionais apenas dos municípios do RJ
pop <- pop |> 
  base::subset(Município %in% mun$cod_mun) |> 
  rename(mun = Município) |> 
  mutate(cod = substr(mun, 1, 6),
         mun = substr(mun, 8, nchar(mun)))

# Agrupando os dados para criar uma variável fx_eta (faixa etária) tal que
# 1: 25 - 54 anos
# 2: 55 - 59 anos
# 3: 60 - 64 anos
# 4: 65 - 69 anos
# 5: 70+ anos
pop <- pop |>
  mutate(fx1 = select(pop, De.25.a.29.anos:De.50.a.54.anos) |> rowSums(na.rm = T),
         fx2 = select(pop, De.55.a.59.anos) |> rowSums(na.rm = T),
         fx3 = select(pop, De.60.a.64.anos) |> rowSums(na.rm = T),
         fx4 = select(pop, De.65.a.69.anos) |> rowSums(na.rm = T),
         fx5 = select(pop, De.70.a.74.anos:De.80.anos.ou.mais) |> rowSums(na.rm = T)) |> 
  select(cod, mun, year, sex, fx1, fx2, fx3, fx4, fx5)

# Utilizando a biblioteca reshape2 para criação da variável faixa_etaria, assim
# como feito anteriormente
pop <- pop |>
  pivot_longer(cols = starts_with('fx'),
               names_to = 'faixa_etaria', values_to = 'value') |>
  group_by(cod, mun, sex, year, faixa_etaria) |>
  summarize(pop = sum(value)) |> 
  mutate(faixa_etaria = substr(faixa_etaria, 3, 3))

# Juntando as bases de dados com inner_join
popmort <- inner_join(popmort,
                      pop,
                      by = c('cod', 'mun', 'faixa_etaria', 'sex', 'year')) |> 
  arrange(mun, cod, year, sex, faixa_etaria)

# Limpando o ambiente
remove(pop, import)

# Tratando as variáveis na base
popmort$mun <- factor(popmort$mun)
popmort$sex <- factor(popmort$sex)

popmort[is.na(popmort)] <- 0

# Escrevendo o arquivo popmort.rds para facilitar trabalhos futuros
write_rds(popmort, 'popmort.rds')

popmort <- read_rds('popmort.rds')

#### Análise de sobrevivência relativa ####

# Criando uma variável de risco (hazard) para cada grupo na base popmort
popmort$haz <- popmort$popmort/popmort$pop

# Expandindo as faixas etárias para anos individuais dentro das bases

# Criação de uma variável dummy de idade na base
# Criando uma função para expansão
# input: faixa etária, número de anos na faixa etária e intervalo de anos da faixa etária
expand <- function(base, fx, c, int) { 
  data <- base |> 
    subset(faixa_etaria == fx) |> 
    expandRows(count = c, count.is.col = F) |> 
    remove_rownames()
  
  data$age <- rep(int, times = nrow(data)/c)
  
  return(data)
}

# Utilizando a função acima para expansão da base
popmort <- bind_rows(expand(popmort, '1', 30, 25:54), # expansão de 30 anos
                     expand(popmort, '2', 5, 55:59), # expansão de 5 anos
                     expand(popmort, '3', 5, 60:64),
                     expand(popmort, '4', 5, 65:69),
                     expand(popmort, '5', 31, 70:100)) # expansão de 31 anos

# Ordenando por ano, sexo e idade
popmort <- popmort |> 
  ungroup() |> 
  select(mun, year, sex, age, haz, faixa_etaria) |> 
  arrange(mun, year, sex, age, faixa_etaria)

# Renomeando os dados de câncer
rhc <- rhc |> 
  rename(age = IDADE,
         sex = SEXO,
         dxdate = DTDIAGNO,
         dthdate = DATAOBITO) |> 
  mutate(year = year(dxdate))

# Alterando o nome da variável age e year para AGE e YEAR na base rhc
names(rhc)[c(1, 13)] <- toupper(names(rhc)[c(1, 13)])

# Renomeando a variável faixa etária
popmort <- popmort |> 
  rename(fx_eta = faixa_etaria)

# Tratando a variável sexo
rhc$sex <- factor(rhc$sex)
popmort$sex <- factor(popmort$sex)
rhc$fx_eta <- factor(rhc$fx_eta)
popmort$fx_eta <- factor(popmort$fx_eta)

# Criando um objeto Lexis
lexis_x <- Lexis(
  entry = list(FUT = 0, age = AGE, year = get.yrs(dxdate)),
  exit = list(year = get.yrs(exit_)),
  data = rhc,
  exit.status = factor(death, labels = c('alive', 'dead')),
  merge = TRUE)

# Removendo colunas duplicadas
lexis_x[, which(names(lexis_x) %in% c('YEAR', 'AGE'))] <- NULL

# Sobrevivência Relativa - Ederer II, 'at-risk' período de 2010 - 2019 com um
# período de follow-up anual (3 anos de follou-up)
st.e2 <- survtab(
  Surv(time = FUT, event = lex.Xst) ~ mun + fx_eta + sex,
  data = lexis_x,
  surv.type = "surv.rel",
  relsurv.method = "e2",
  breaks = list(FUT = seq(0, 3, 1)),
  pophaz = popmort
)

# Preparando a base de sobrevivência
surv <- st.e2 |> 
  inner_join(mun, by = 'mun') |> 
  select(Tstart, Tstop, d, d.exp, pyrs, cod, mun, fx_eta, sex)

#### Criando a matriz de covariáveis ####
Xc <- tibble(fx2 = as.numeric(surv$fx_eta == '2'), # Efeito de ser da faixa etária 2
            fx3 = as.numeric(surv$fx_eta == '3'), # Efeito de ser da faixa etária 3
            fx4 = as.numeric(surv$fx_eta == '4'), # Efeito de ser da faixa etária 4
            fx5 = as.numeric(surv$fx_eta == '5'), # Efeito de ser da faixa etária 5
            sex2 = as.numeric(surv$sex == 2)) |>  
  as.matrix()

#### Carregando e trabalhando no shapefile ####
setwd('..')

rj <- read_sf('shp', 'RJ_Municipios_2021')

# Criando coluna de identificação de área na base data
mun <- mun |> 
  mutate(id = 1:92)

# Incluindo a variável id no objeto rj
rj <- left_join(rj,
                mun |> select(id, mun),
                by=c('NM_MUN'='mun'))

# Definindo vizinhos de cada área
nb <- poly2nb(rj)

nb_sf <- as(nb2lines(nb, coords = sp::coordinates(as_Spatial(rj))), 'sf') |>
  st_set_crs(st_crs(st_as_sf(rj)))

ggplot(st_as_sf(rj)) +
  geom_sf(fill = 'salmon', color = 'white') +
  geom_sf(data = nb_sf,) +
  theme_minimal() +
  labs(y = "Latitude", x = "Longitude")

# Matriz de vizinhança
w <- matrix(rep(NA, times = length(nb)^2), length(nb))

for(i in 1:length(nb)) { # para cada coluna i na matriz
  for(j in 1:length(nb)) { # para cada linha j na matriz
    if(j == i) { # se for um elemento da diagonal principal
      w[j, i] <- length(nb[[i]]) # receber o número de vizinhos
    } else { # caso contrário
      if(j %in% nb[[i]]) { # se a linha j for vizinha da coluna 1
        w[j, i] <- -1 # receber o valor -1
      } else { # se não
        w[j, i] <- 0 # receber o valor 0
      }
    }
  }
}

# Matriz de adjacência
w_adj <- matrix(rep(NA, times = length(nb)^2), length(nb))

for(i in 1:length(nb)) { # para cada coluna i na matriz
  for(j in 1:length(nb)) { # para cada linha j na matriz
    if(j %in% nb[[i]]) { # se a linha j for vizinha da coluna 1
      w_adj[j, i] <- 1 # receber o valor 1
    } else { # se não
      w_adj[j, i] <- 0 # receber o valor 0
    }
  }
}

# Matriz diagonal de vizinhos
w_diag <-  matrix(rep(NA, times = length(nb)^2), length(nb))

for(i in 1:length(nb)) { # para cada coluna i na matriz
  for(j in 1:length(nb)) { # para cada linha j na matriz
    if(j == i) { # se for um elemento da diagonal principal
      w_diag[j, i] <- length(nb[[i]]) # receber o número de vizinhos
    } else {
      w_diag[j, i] <- 0
    }
  }
}

# Vizinhos adjacentes de cada área
adj <- c()
for (i in 1:92) {
  adj <- c(adj,
           nb[[i]])
}

# Número de vizinhos de cada área
n_adj <- c()
for (i in 1:92) {
  n_adj <- c(n_adj,
             length(nb[[i]]))
}

# Função para criar uma representação esparsa da matriz de adjacência
mungeCARdata4stan = function(adj,num) {
  N <- length(num)
  nn <- num
  N_edges <- length(adj) / 2
  node1 <- vector(mode="numeric", length=N_edges)
  node2 <- vector(mode="numeric", length=N_edges)
  
  iAdj <- 0
  iEdge <- 0
  
  for (i in 1:N) {
    for (j in 1:nn[i]) {
      iAdj <- iAdj + 1
      if (i < adj[iAdj]) {
        iEdge <- iEdge + 1
        node1[iEdge] <- i
        node2[iEdge] <- adj[iAdj]
      }
    }
  }
  return(list("N"=N,"N_edges"=N_edges,"node1"=node1,"node2"=node2))
}

nbs <- mungeCARdata4stan(adj, n_adj)

# Criando uma variável área que identifica unicamente cada região na base surv
area <- c()

# Preenchendo o vetor acima
for(i in 1:nrow(surv)) {
  area <- c(area,
            mun[mun$mun == surv$mun[i], ]$id)
}

# Geração de gráficos utilizando ggplot2
st.e2 |>
  filter(mun == 'Rio de Janeiro', sex == 1) |>
  ggplot(aes(x = surv.int, y = r.e2, colour = factor(fx_eta), fill = factor(fx_eta),
             group = fx_eta)) +
  geom_line(aes(linetype = factor(fx_eta)), size = 1) +
  # adicionando intervalo de 95% de confiança
  geom_ribbon(aes(ymin = r.e2.lo, ymax = r.e2.hi), alpha = 0.1, colour = NA) +
  labs(x = "Follow-up Interval", y = "Relative Survival") +
  scale_linetype("Faixa Etária") +
  scale_fill_discrete("Faixa Etária") +
  scale_colour_discrete("Faixa Etária") +
  labs(title = 'Sobrevivência relativa para o município do RJ, sexo masculino') +
  theme_light() +
  scale_x_continuous(breaks=c(1, 2, 3)) +
  ylim(c(0.1, 0.6))

# Exportando o gráfico
# ggsave('rs1.pdf', width = 30, height = 17, units='cm')

st.e2 |>
  filter(mun == 'Rio de Janeiro', sex == 2) |>
  ggplot(aes(x = surv.int, y = r.e2, colour = factor(fx_eta), fill = factor(fx_eta),
             group = fx_eta)) +
  geom_line(aes(linetype = factor(fx_eta)), size = 1) +
  geom_ribbon(aes(ymin = r.e2.lo, ymax = r.e2.hi), alpha = 0.1, colour = NA) +
  labs(x = "Follow-up Interval", y = "Relative Survival") +
  scale_linetype("Faixa Etária") +
  scale_fill_discrete("Faixa Etária") +
  scale_colour_discrete("Faixa Etária") +
  labs(title = 'Sobrevivência relativa para o município do RJ, sexo feminino') +
  theme_light() +
  scale_x_continuous(breaks=c(1, 2, 3)) +
  ylim(c(0.1, 0.6))

# Exportando o gráfico
# ggsave('rs2.pdf', width = 30, height = 17, units='cm')

#### Inferência ####

# Modelo Linear Generalizado sem considerar uma estrutura de dependência espacial
# fit_glm <- stan(file = "models/glm.stan",
#             data = list(N = length(nb), # Número de áreas
#                         Nd = nrow(surv), # Número de observações
#                         d = surv$d, # Número de mortes
#                         nc = ncol(Xc), # Número de covariáveis
#                         t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                         X = Xc, # Matriz de covariáveis
#                         dstar = surv$d.exp, # Número de mortes esperadas
#                         y = surv$pyrs, # Tempo em risco dos indivíduos
#                         RiskYear = surv$Tstart + 1, # Intervalo de tempo
#                         area = area),
#             iter = 8000,
#             warmup = 2000,
#             thin = 3)
#   
# write_rds(fit_glm, 'fit_glm.rds')

fit_glm <- read_rds('fit_glm.rds')

# IAR
# fit_iar <- stan(file = "models/icar.stan",
#                 data = list(N = length(nb), # Número de áreas
#                             N_edges = nbs$N_edges,
#                             node1 = nbs$node1,
#                             node2 = nbs$node2,
#                             Nd = nrow(surv), # Número de observações
#                             d = surv$d, # Número de mortes
#                             nc = ncol(Xc), # Número de covariáveis
#                             t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                             X = Xc, # Matriz de covariáveis
#                             dstar = surv$d.exp, # Número de mortes esperadas
#                             y = surv$pyrs, # Tempo em risco dos indivíduos
#                             RiskYear = surv$Tstart + 1, # Intervalo de tempo
#                             area = area),
#                 iter = 8000,
#                 warmup = 2000,
#                 thin = 3)
#  
# write_rds(fit_iar, 'fit_iar.rds')

fit_iar <- read_rds('fit_iar.rds')

# Bym
# fit_bym <- stan(file = "models/bym.stan",
#                 data = list(N = length(nb), # Número de áreas
#                             N_edges = nbs$N_edges,
#                             node1 = nbs$node1,
#                             node2 = nbs$node2,
#                             Nd = nrow(surv), # Número de observações
#                             d = surv$d, # Número de mortes
#                             nc = ncol(Xc), # Número de covariáveis
#                             t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                             X = Xc, # Matriz de covariáveis
#                             dstar = surv$d.exp, # Número de mortes esperadas
#                             y = surv$pyrs, # Tempo em risco dos indivíduos
#                             RiskYear = surv$Tstart + 1, # Intervalo de tempo
#                             area = area),
#                 iter = 8000,
#                 warmup = 2000,
#                 thin = 3)
# 
# write_rds(fit_bym, 'fit_bym.rds')

fit_bym <- read_rds('fit_bym.rds')

# Cressie
# fit_cressie <- stan(file = "models/cressie.stan",
#                 data = list(N = length(nb), # Número de áreas
#                             Nd = nrow(surv), # Número de observações
#                             d = surv$d, # Número de mortes
#                             nc = ncol(Xc), # Número de covariáveis
#                             t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                             X = Xc, # Matriz de covariáveis
#                             dstar = surv$d.exp, # Número de mortes esperadas
#                             y = surv$pyrs, # Tempo em risco dos indivíduos
#                             RiskYear =  surv$Tstart + 1, # Intervalo de tempo
#                             area = area,
#                             W = w_adj,
#                             W_n = sum(w_adj)/2),
#                 iter = 8000,
#                 warmup = 2000,
#                 thin = 3)
# 
# write_rds(fit_cressie, 'fit_cressie.rds')

fit_cressie <- read_rds('fit_cressie.rds')

# Leroux
# fit_leroux <- stan(file = "models/leroux.stan",
#                    data = list(N = length(nb), # Número de áreas
#                                W = w, # Matriz de vizinhança
#                                Nd = nrow(surv), # Número de observações
#                                d = surv$d, # Número de mortes
#                                nc = ncol(Xc), # Número de covariáveis
#                                t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                                X = Xc, # Matriz de covariáveis
#                                dstar = surv$d.exp, # Número de mortes esperadas
#                                y = surv$pyrs, # Tempo em risco dos indivíduos
#                                RiskYear = surv$Tstart + 1, # Intervalo de tempo
#                                area = area),
#                    iter = 8000,
#                    warmup = 2000,
#                    thin = 3)
# 
# write_rds(fit_leroux, 'fit_leroux.rds')

fit_leroux <- read_rds('fit_leroux.rds')

# BYM2

# Encontrando o fator de escala para o modelo BYM2
adj.matrix <- sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)
Q <-  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
Q_pert <- Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))
scaling_factor = exp(mean(log(diag(Q_inv))))

# Aplicando modelo BYM2
# fit_bym2 <- stan(file = "models/bym2.stan",
#                    data = list(N = length(nb), # Número de áreas
#                                N_edges = nbs$N_edges,
#                                node1 = nbs$node1,
#                                node2 = nbs$node2,
#                                Nd = nrow(surv), # Número de observações
#                                d = surv$d, # Número de mortes
#                                nc = ncol(Xc), # Número de covariáveis
#                                t = length(unique(surv$Tstart)), # Número de intervalos analisados
#                                X = Xc, # Matriz de covariáveis
#                                dstar = surv$d.exp, # Número de mortes esperadas
#                                y = surv$pyrs, # Tempo em risco dos indivíduos
#                                RiskYear = surv$Tstart + 1, # Intervalo de tempo
#                                area = area,
#                                scaling_factor = scaling_factor),
#                    iter = 8000,
#                    warmup = 2000,
#                    thin = 3)
# 
# write_rds(fit_bym2, 'fit_bym2.rds')

fit_bym2 <- read_rds('fit_bym2.rds')

log_lik_glm <- extract_log_lik(fit_glm, merge_chains = FALSE)
log_lik_icar <- extract_log_lik(fit_iar, merge_chains = FALSE)
log_lik_bym <- extract_log_lik(fit_bym, merge_chains = FALSE)
log_lik_cressie <- extract_log_lik(fit_cressie, merge_chains = FALSE)
log_lik_leroux_theta <- extract_log_lik(fit_leroux_theta, merge_chains = FALSE)
log_lik_leroux <- extract_log_lik(fit_leroux, merge_chains = FALSE)
log_lik_bym2 <- extract_log_lik(fit_bym2, merge_chains = FALSE)

tibble(Modelo = c('GLM', 'Intrínseco', 'BYM', 'Cressie', 'Leroux', 'BYM2'),
       WAIC = c(waic(log_lik_glm)$estimates['waic', 1],
                waic(log_lik_icar)$estimates['waic', 1],
                waic(log_lik_bym)$estimates['waic', 1],
                waic(log_lik_cressie)$estimates['waic', 1],
                waic(log_lik_leroux)$estimates['waic', 1],
                waic(log_lik_bym2)$estimates['waic', 1])) |> 
  arrange(WAIC) |> 
  write_rds('waic_comp.rds')

waic_comp <- read_rds('waic_comp.rds')

EHR_leroux <- apply(X = extract(fit_leroux)$phi,
                    MARGIN = 2,
                    FUN = median) |> 
  exp()

EHR_glm  <- apply(X = extract(fit_glm)$phi,
                 MARGIN = 2,
                 FUN = median) |> 
  exp()

# Calculando a probabilidade da exponencial do efeito efeito espacial ser maior que 1
# (equivale ao efeito espacial ser maior que 0)
xhs <- apply(extract(fit_leroux)$phi,
                                MARGIN = 2,
                                FUN = function(x){as.numeric(x>0)}) |> 
  apply(MARGIN = 2,
        FUN = sum)/8000

rj <- left_join(rj,
                tibble(id = 1:92,
                       EHR_leroux,
                       EHR_glm,
                       xhs),
                by='id')

# Passando sf para Spatial
rj <- as_Spatial(rj)

# Plotando mapas utilizando tmap
g_leroux <- tm_shape(rj) + 
  tm_polygons(col = 'EHR_leroux',
              midpoint=1,
              palette = divergingx_hcl(5, palette = 'Fall'),
              breaks=c(0.24, 0.51, 0.77, 1.18, 1.75, 2.32),
              title='Modelo Leroux')

g_leroux

g_glm <- tm_shape(rj) +
  tm_polygons(col = 'EHR_glm',
              midpoint=1,
              palette = divergingx_hcl(5, palette = 'Fall'),
              breaks=c(0.24, 0.51, 0.77, 1.18, 1.75, 2.32),
              title='Modelo GLM')

g_glm

g_xhs <- tm_shape(rj) +
  tm_polygons(col='xhs',
              breaks=c(0, 0.2, 0.8, 1),
              palette=c('#3C5941', '#FBF2C4', '#C7522B'),
              title='P(exp(S) > 1)')

g_xhs

# Exportando os mapas
# tmap_save(g_leroux, 'leroux.pdf', units = 'cm', width = 11.8, height = 8.5)
# 
# tmap_save(g_glm, 'glm.pdf', units='cm', width=11.8, height=8.5)
#
# tmap_save(g_xhs, 'xhs.pdf', units='cm', width=11.8, height=8.5)
