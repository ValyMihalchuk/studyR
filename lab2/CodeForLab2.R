GetInfo <- function(state) {
  
  # Загрузим данные
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # Выберем данные по штату
  state <-data[which(data$State == state),]
  
  # Уровень смертности от инфаркта, находим минимум и максимум, не учитывая при этом нечисловые значения
  HeartAttackMortality<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
  HeartAttackMortality <- lapply(HeartAttackMortality, function(x) gsub("[^0-9.-]", NA, x))
  HeartAttackMortality <- lapply(HeartAttackMortality, function(x) as.numeric(x))
  HeartAttackMortality <- HeartAttackMortality[!is.na(HeartAttackMortality)]
  HAmin <- min(unlist(HeartAttackMortality), na.rm=TRUE)
  HAmax <- max(unlist(HeartAttackMortality), na.rm=TRUE)
  
  
  # Уровень смертности от остановки сердца, находим минимум и максимум, не учитывая при этом нечисловые значения
  HeartFailureMortality<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
  HeartFailureMortality <- lapply(HeartFailureMortality, function(x) gsub("[^0-9.-]", NA, x))
  HeartFailureMortality <- lapply(HeartFailureMortality, function(x) as.numeric(x))
  HeartFailureMortality <- HeartFailureMortality[!is.na(HeartFailureMortality)]
  HFmin <- min(unlist(HeartFailureMortality), na.rm=TRUE)
  HFmax <- max(unlist(HeartFailureMortality), na.rm=TRUE)
  
  # Уровень смертности от пневмонии, находим минимум и максимум, не учитывая при этом нечисловые значения
  PneumoniaMortality<-state[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",]
  PneumoniaMortality <- lapply(PneumoniaMortality, function(x) gsub("[^0-9.-]", NA, x))
  PneumoniaMortality <- lapply(PneumoniaMortality, function(x) as.numeric(x))
  PneumoniaMortality <- PneumoniaMortality[!is.na(PneumoniaMortality)]
  Pmin <- min(unlist(PneumoniaMortality), na.rm=TRUE)
  Pmax <-max(unlist(PneumoniaMortality), na.rm=TRUE)
  
  
  # Находим число больниц в штате
  Hcount <- length(unique(state$Hospital.Name))
  
  
  # Все эти значения добавим в список
  values <- list(HAmin, HAmax, HFmin,HFmax,Pmin,Pmax, Hcount)
  names(values) <- c("Heart Attack Mortality Min", "Heart Attack Mortality Max", "Heart Failure Mortality Min", "Heart Failure Mortality Max", "Pneumonia Mortality Min", "Pneumonia Mortality Max", "Hospital Count")
  
  return(values)
}

GetExpImpRegion<-function(path, my_str){
  load(path)
  
  ExpImp[-1] <- lapply(ExpImp[-1], function(x) gsub("-", 0, x)) # заменим "-" на 0
  ExpImp[-1] <- lapply(ExpImp[-1], function(x) as.numeric(x)) #переведем все в числа
  data <- ExpImp
  
  # Найдем индексы федеральных округов
  indxs <- which(grepl("федеральный округ",data$Регион) == TRUE)
  indxs <- append(indxs,nrow(data)+1)
  
  # Находим все регионы, которые "зажаты" между двумя округами в таблице (все они будут принадлежать верхнему округу в таблице)
  index1 <- which(my_str == data$Регион)
  index2 <- indxs[which(indxs == index1) + 1]
  regions_index <- (index1+1):(index2-1)
  region <- data[regions_index,]
  
  # Заведем столбцы суммарного экспорта и импорта, а так же их разницу:
  column_import <- region[,grepl("Импорт", colnames(region))]
  column_export <- region[,grepl("Экспорт", colnames(region))]
  binded <- cbind(region[, 1], column_import, column_export)
  
  
  binded['SumImp'] <- rowSums(column_import, na.rm = TRUE)
  binded['SumExp'] <- rowSums(column_export, na.rm = TRUE)
  binded['Delta']<-binded['SumExp']-binded['SumImp']
  
  # Нам нужны только такие, у которых экспорт больше импорта:
  result <- binded[which(binded['Delta'] > 0),]
  result <- result[1]
  return(result)
  
}
