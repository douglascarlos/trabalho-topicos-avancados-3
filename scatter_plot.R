#install.packages("ggplot2")
library(ggplot2)

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

plotLineWithPoint <- function(data){
  plot <- ggplot(data, aes(x=AGGREGATED_YEAR, y=YEAR_PERCENTAGE)) + geom_point() 
  plot + geom_line(aes(colour = AGGREGATED_COURSE, group = AGGREGATED_COURSE)) + labs(x="Ano", y="Percentual de Bolsas Concedidas (%)", colour="Cursos")
}

plotLine <- function(data){
  plot <- ggplot(data, aes(x=AGGREGATED_YEAR, y=YEAR_PERCENTAGE, color=AGGREGATED_COURSE))
  plot + geom_line(aes(colour = AGGREGATED_COURSE, group = AGGREGATED_COURSE)) + labs(x="Ano", y="Percentual de Bolsas Concedidas (%)", colour="Cursos")
}

plotLineByCourse <- function(data){
  plot <- ggplot(prouni_data, aes(x=AGGREGATED_YEAR, y=YEAR_PERCENTAGE))
  plot + geom_line(aes(colour=AGGREGATED_COURSE))
  (plot <- plot + geom_line(group=1) + facet_wrap(~AGGREGATED_COURSE, ncol = 10) + labs(x="Ano", y="Percentual de Bolsas Concedidas (%)", colour="Cursos"))
}

filterByMostFrequentCourses <- function(data, quantity){
  aggregate_course <- aggregate(data, by=list(COURSE=data$AGGREGATED_COURSE),FUN=mean)
  subset <- subset(aggregate_course, YEAR_PERCENTAGE >= rev(sort(aggregate_course$YEAR_PERCENTAGE))[quantity])
  plot_data <- data[data$AGGREGATED_COURSE %in% subset$COURSE,]
  plot_data$YEAR_PERCENTAGE <- plot_data$YEAR_PERCENTAGE * 100
  return (plot_data)
}


# example
prouni_data <- loadProuniData(2005:2016, uf="RS")
plot_data <- filterByMostFrequentCourses(prouni_data, 10)

plotLineWithPoint(plot_data)
plotLine(plot_data)
plotLineByCourse(plot_data)
