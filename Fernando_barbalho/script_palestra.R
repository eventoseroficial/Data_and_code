# install.packages("devtools")
#devtools::install_github("tchiluanda/Rgfs")

library(Rgfs)

Rgfs::dataAccountEvolutionByCountries(account = "Despesa", continent = "América do Sul")%>%
  Rgfs::graphAccountEvolutionByCountries()

Rgfs::dataHeatMap(account = "Despesa") %>%
  Rgfs::graphHeatMap()

Rgfs::dataCompleteTimeSeries(account="Despesa") %>%
  Rgfs::graphCompleteTimeSeries(selected_country="Brasil", text_max = TRUE )

Rgfs::dataAccountDistribution(year = 2019) %>%
  Rgfs::graphAccountDistribution()


#devtools::install_github("tchiluanda/Rcofog")

library(Rcofog)
Rcofog::dataExpenseFlow(year=2020) %>%
  Rcofog::graphExpenseFlow()

Rcofog::defineActionCluster(year = 2020)%>%
  Rcofog::graphGovernmentAction()

Rcofog::dataEconomicClassification(year = 2020)%>%
  Rcofog::graphEconomicClassification()


#devtools::install_github("tchiluanda/siconfiBD")

library(siconfiBD)

siconfiBD::setup_siconfi("nice-diorama-306223")

exp_mun<- siconfiBD::get_budgetary_expenses_municipality(year = 2019,
                                              municipality = 3550308,
                                              account = "Pessoal e Encargos Sociais")

exp_pib<- siconfiBD::get_budgetary_exp_by_gdp_range(gdp_range = c(1,100),
                                          gdp_year = 2018,
                                          exp_year = 2019,
                                          account = "Pessoal e Encargos Sociais")

exp_ce_saude_educacao<- siconfiBD::get_function_expenses_municipality_state(year = 2019,
                                                    state = "CE",
                                                    gov_function = c("saúde","educação"))

#devtools::install_github("tchiluanda/rtn")

library(rtn)


rtn::get_account_data_by_month(c("física","jurídica"),
                               month = c(1:12),
                               match_required = FALSE) %>%
  rtn::plot_rtn_series()

get_account_data_by_month(c("física"), month = c(1:12), match_required = FALSE) %>%
  plot_seasonality (value_type = "1")

