install.packages("arules")
library(arules)

base_dados <- read.csv2("./datasets/PDA_PROUNI_2016_CSV.csv", header=TRUE)
base_dados$ANO_CONCESSAO_BOLSA <- as.factor(base_dados$ANO_CONCESSAO_BOLSA)
base_dados$CODIGO_EMEC_IES_BOLSA <- as.factor(base_dados$CODIGO_EMEC_IES_BOLSA)

relevant_base_dados <- base_dados[c(3, 5, 6, 9, 10, 11, 14, 15)]
relevant_base_dados$DT_NASCIMENTO_BENEFICIARIO <- as.factor(format(as.Date(as.character(relevant_base_dados$DT_NASCIMENTO_BENEFICIARIO), format="%d-%m-%Y"),"%Y"))
base_dados_transacional <- as(relevant_base_dados, "transactions")
summary(base_dados_transacional)

# https://stackoverflow.com/a/33229854/6794792
regras <- apriori(data=base_dados_transacional, parameter=list(minlen=1, supp=0.0001, conf=.8, target="rules"), appearance=list(rhs=paste0("NOME_CURSO_BOLSA=", unique(relevant_base_dados$NOME_CURSO_BOLSA))))
regras_ordenadas <- sort(regras, by="confidence", decreasing=TRUE)
inspect(regras_ordenadas)
