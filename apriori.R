install.packages("arules")
library(arules)

data <- read.csv2("./datasets/PDA_PROUNI_2016_CSV.csv", header=TRUE)
data$ANO_CONCESSAO_BOLSA <- as.factor(data$ANO_CONCESSAO_BOLSA)
data$CODIGO_EMEC_IES_BOLSA <- as.factor(data$CODIGO_EMEC_IES_BOLSA)
data$DT_NASCIMENTO_BENEFICIARIO <- as.factor(format(as.Date(as.character(data$DT_NASCIMENTO_BENEFICIARIO), format="%d-%m-%Y"),"%Y"))



# exercicio com todos os cadastros
test1_data <- data[c(6, 9, 10, 11, 14, 15)]
test1_data_transactional <- as(test1_data, "transactions")
summary(test1_data_transactional)

# https://stackoverflow.com/a/33229854/6794792
rules <- apriori(data=test1_data_transactional, parameter=list(minlen=2, maxlen=10, supp=0.0001, conf=.4, target="rules"), appearance=list(rhs=paste0("NOME_CURSO_BOLSA=", unique(test1_data$NOME_CURSO_BOLSA))))
sorted_rules <- sort(rules, by="lift", decreasing=TRUE)
inspect(sorted_rules)



# exercicio para encontar regras de associacao mais frequentes entre principais universidades gauchas
universities <- c(
  "PONTIFÍCIA UNIVERSIDADE CATÓLICA DO RIO GRANDE DO SUL",
  "UNIVERSIDADE DO VALE DO RIO DOS SINOS",
  "UNIVERSIDADE DE CAXIAS DO SUL",
  "UNIVERSIDADE CATÓLICA DE PELOTAS",
  "UNIVERSIDADE LUTERANA DO BRASIL",
  "UNIVERSIDADE FEEVALE",
  "UNIVERSIDADE DE PASSO FUNDO",
  "UNIVERSIDADE REGIONAL DO NOROESTE DO ESTADO DO RIO GRANDE DO SUL",
  "UNIVERSIDADE DE SANTA CRUZ DO SUL",
  "UNIVERSIDADE REGIONAL INTEGRADA DO ALTO URUGUAI E DAS MISSÕES",
  "UNIVERSIDADE DE CRUZ ALTA",
  "UNIVERSIDADE DA REGIÃO DA CAMPANHA"
)

university_rules <- list()
for(i in seq_along(universities)){
  university_data <- data[data$NOME_IES_BOLSA == universities[i], ]
  relevant_university_data <- university_data[c(5, 6, 7, 9, 10, 11, 12, 14, 15)]
  university_data_transactional <- as(relevant_university_data, "transactions")

  rules <- apriori(data=university_data_transactional, parameter=list(minlen=2, maxlen=10, supp=0.1, conf=.8, target="rules"), appearance=list(rhs=paste0("NOME_CURSO_BOLSA=", unique(relevant_university_data$NOME_CURSO_BOLSA))))
  sorted_rules <- sort(rules, by="lift", decreasing=TRUE)

  university_rules[[i]] <- sorted_rules
}
names(university_rules) <- universities

university_rules

