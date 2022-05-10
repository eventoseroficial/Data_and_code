###dados obtidos através do script limpeza
dados <- read.csv("dados/df.csv",header=T, dec=".",sep=";")
dados = dados[,-1]

###conjunto de treino
ods_split <- dados %>%
  initial_split()

ods_treino = training(ods_split)
ods_teste = testing(ods_split)

###sequencia de palavras e vocabulário
max_words <- 1000
max_length <- 30

ods_rec <- recipe(~ texto, data = ods_treino) %>%
  step_tokenize(texto) %>%
  step_tokenfilter(texto, max_tokens = max_words) %>%
  step_sequence_onehot(texto, sequence_length = max_length)

ods_prep <- prep(ods_rec)
ods_token_treino <- bake(ods_prep, new_data = NULL, composition = "matrix")

###primeira rede neural
model <- keras_model_sequential() %>%
  layer_embedding(input_dim = max_words + 1,
                  output_dim = 32,
                  input_length = max_length) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 32, activation = "linear") %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model

model %>% compile(
  optimizer = "adam",
  loss = "binary_crossentropy",
  metrics = c("accuracy")
)

###ODS 16
history_ods_16 <- model %>%
  fit(
    x = ods_token_treino,
    y = ods_treino$ods_16,
    callbacks = callback_early_stopping(
      monitor = "val_accuracy",
      patience = 50,
      restore_best_weights = T),
    batch_size = 2,
    epochs = 500,
    validation_split = 0.2,
    verbose = 1
  )

#avaliando o modelo - conjunto de teste
ods_token_teste = bake(ods_prep, new_data = ods_teste,
                    composition = "matrix")

teste <- keras_predict(model, ods_token_teste, ods_teste$ods_16)

teste %>%
  conf_mat(state, .pred_class) %>%
  autoplot(type = "heatmap")

metrics(teste, state, .pred_class)

teste_ods_16 = data.frame(ods_teste$id,as.integer(as.character(teste$.pred_class)),as.integer(ods_teste$ods_16))
teste_ods_16 = teste_ods_16 %>%
  purrr::set_names(c("id","pred","real"))

write.csv2(teste_ods_16,"dados/teste_ods_16.csv", row.names = T)

acc_16 = sum(abs(ods_teste$ods_16 - as.integer(as.character(teste$.pred_class))))
acc_16 = 1 - acc_16/length(ods_teste$ods_16)
acc_16
