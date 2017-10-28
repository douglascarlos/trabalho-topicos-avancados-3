loadProuniData <- function(range, uf=NULL, city=NULL, college=NULL){
  all_data = data.frame()
  for(year in range){
    year_data <- loadProuniDataFromYear(year, uf, city, college)
    aggregated_data <- aggregateProuniData(year_data)
    all_data <- rbind(all_data, aggregated_data)
  }
  return(all_data)
}

loadProuniDataFromYear <- function(year, uf=NULL, city=NULL, college=NULL){
  path <- paste0("./datasets/PDA_PROUNI_", year, "_CSV.csv")
  prouni_data <- read.csv2(path, header=TRUE)

  relevant_data <- prouni_data[c(1, 3, 6, 14, 15)]
  relevant_data$ANO_CONCESSAO_BOLSA <- as.factor(relevant_data$ANO_CONCESSAO_BOLSA)
  
  filtered_data <- filterProuniData(relevant_data, uf, city, college)
  
  return(filtered_data)
}

filterProuniData <- function(data, uf=NULL, city=NULL, college=NULL){
  if(!is.null(college)){
    data <- data[data$NOME_IES_BOLSA == college, ]
  }
  if(!is.null(city)){
    data <- data[data$MUNICIPIO_BENEFICIARIO_BOLSA == city, ]
  }
  if(!is.null(uf)){
    data <- data[data$SIGLA_UF_BENEFICIARIO_BOLSA == uf, ]
  }
  return(data)
}

aggregateProuniData <- function(data){
  size <- nrow(data)
  aggregated_data <- aggregate(data, by=list(data$ANO_CONCESSAO_BOLSA, data$NOME_CURSO_BOLSA), FUN=length)
  aggregated_data <- cbind(aggregated_data, aggregated_data$ANO_CONCESSAO_BOLSA / size)

  colnames(aggregated_data)[1] <- "AGGREGATED_YEAR"
  colnames(aggregated_data)[2] <- "AGGREGATED_COURSE"
  colnames(aggregated_data)[8] <- "YEAR_PERCENTAGE"

  return(aggregated_data)
}

prouni_data <- loadProuniData(2014:2016, city="PORTO ALEGRE")
head(prouni_data)






