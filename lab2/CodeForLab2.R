GetInfo <- function(state) {
  
  # �������� ������
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  # ������� ������ �� �����
  state <-data[which(data$State == state),]
  
  # ������� ���������� �� ��������, ������� ������� � ��������, �� �������� ��� ���� ���������� ��������
  HeartAttackMortality<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"]
  HeartAttackMortality <- lapply(HeartAttackMortality, function(x) gsub("[^0-9.-]", NA, x))
  HeartAttackMortality <- lapply(HeartAttackMortality, function(x) as.numeric(x))
  HeartAttackMortality <- HeartAttackMortality[!is.na(HeartAttackMortality)]
  HAmin <- min(unlist(HeartAttackMortality), na.rm=TRUE)
  HAmax <- max(unlist(HeartAttackMortality), na.rm=TRUE)
  
  
  # ������� ���������� �� ��������� ������, ������� ������� � ��������, �� �������� ��� ���� ���������� ��������
  HeartFailureMortality<- state[,"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"]
  HeartFailureMortality <- lapply(HeartFailureMortality, function(x) gsub("[^0-9.-]", NA, x))
  HeartFailureMortality <- lapply(HeartFailureMortality, function(x) as.numeric(x))
  HeartFailureMortality <- HeartFailureMortality[!is.na(HeartFailureMortality)]
  HFmin <- min(unlist(HeartFailureMortality), na.rm=TRUE)
  HFmax <- max(unlist(HeartFailureMortality), na.rm=TRUE)
  
  # ������� ���������� �� ���������, ������� ������� � ��������, �� �������� ��� ���� ���������� ��������
  PneumoniaMortality<-state[,"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",]
  PneumoniaMortality <- lapply(PneumoniaMortality, function(x) gsub("[^0-9.-]", NA, x))
  PneumoniaMortality <- lapply(PneumoniaMortality, function(x) as.numeric(x))
  PneumoniaMortality <- PneumoniaMortality[!is.na(PneumoniaMortality)]
  Pmin <- min(unlist(PneumoniaMortality), na.rm=TRUE)
  Pmax <-max(unlist(PneumoniaMortality), na.rm=TRUE)
  
  
  # ������� ����� ������� � �����
  Hcount <- length(unique(state$Hospital.Name))
  
  
  # ��� ��� �������� ������� � ������
  values <- list(HAmin, HAmax, HFmin,HFmax,Pmin,Pmax, Hcount)
  names(values) <- c("Heart Attack Mortality Min", "Heart Attack Mortality Max", "Heart Failure Mortality Min", "Heart Failure Mortality Max", "Pneumonia Mortality Min", "Pneumonia Mortality Max", "Hospital Count")
  
  return(values)
}

GetExpImpRegion<-function(path, my_str){
  load(path)
  
  ExpImp[-1] <- lapply(ExpImp[-1], function(x) gsub("-", 0, x)) # ������� "-" �� 0
  ExpImp[-1] <- lapply(ExpImp[-1], function(x) as.numeric(x)) #��������� ��� � �����
  data <- ExpImp
  
  # ������ ������� ����������� �������
  indxs <- which(grepl("����������� �����",data$������) == TRUE)
  indxs <- append(indxs,nrow(data)+1)
  
  # ������� ��� �������, ������� "������" ����� ����� �������� � ������� (��� ��� ����� ������������ �������� ������ � �������)
  index1 <- which(my_str == data$������)
  index2 <- indxs[which(indxs == index1) + 1]
  regions_index <- (index1+1):(index2-1)
  region <- data[regions_index,]
  
  # ������� ������� ���������� �������� � �������, � ��� �� �� �������:
  column_import <- region[,grepl("������", colnames(region))]
  column_export <- region[,grepl("�������", colnames(region))]
  binded <- cbind(region[, 1], column_import, column_export)
  
  
  binded['SumImp'] <- rowSums(column_import, na.rm = TRUE)
  binded['SumExp'] <- rowSums(column_export, na.rm = TRUE)
  binded['Delta']<-binded['SumExp']-binded['SumImp']
  
  # ��� ����� ������ �����, � ������� ������� ������ �������:
  result <- binded[which(binded['Delta'] > 0),]
  result <- result[1]
  return(result)
  
}
