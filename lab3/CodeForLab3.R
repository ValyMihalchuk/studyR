library(tidyverse)
load("trades.RData")


#Объединяем таблицы в одну
data <- bind_rows(trades)

#Убираем столбец с территорией торговли
data_without_geo <- select(data, -geo)

#Оставим только строки с экспортом и импортом
ExpImpData <- filter(data_without_geo, str_detect(indic_et, "Exports in|Imports in"))

#Вынесем данные по экспорту и импорту в отдельные переменные
pivoted <- pivot_wider(ExpImpData, names_from = indic_et, values_from = values) 
# И переименуем столбцы для удобства
pivoted <- rename(pivoted, product = "sitc06", export = "Exports in million of ECU/EURO", import = "Imports in million of ECU/EURO")

# Отфильтруем только данные для продуктов питания, напитков и табака
FoodDrinksTobaccoData <- filter(pivoted, str_detect(product, "Food, drinks and tobacco"))
# Для каждой даты найдем их суммарный экспорт и импорт
FoodDrinksTobaccoData <- group_by(FoodDrinksTobaccoData, time)
SumExpImpData <- summarise(FoodDrinksTobaccoData, export = sum(export), import = sum(import))



# Нарисуем график:
ggplot() + 
  geom_line(data = SumExpImpData, aes(x = time, y = import,colour="import")) +
  geom_line(data = SumExpImpData, aes(x = time, y = export,colour="export"))+
  geom_point(data = SumExpImpData, aes(x = time,y = import,colour="import"),size = 1)+
  geom_point(data = SumExpImpData, aes(x = time,y = export,colour="export"), size = 1)+
  geom_text(data = SumExpImpData, aes(x = time, y = import, label = import,colour="import"), size = 2,nudge_x = 268, nudge_y = -1500) +
  geom_text(data = SumExpImpData, aes(x = time, y = export, label = export, colour="export"), size = 2,nudge_x = -300, nudge_y = 1000) +
  xlab('time') +
  ylab('value')+
  labs(color = "Notation", title = "Food, Drinks, Tobacco Export/Import")


my_str = "Южный федеральный округ"
data <- drop_na(read_csv("reforest.csv", na = c("-", "NA")))

# Найдем индексы федеральных округов
indxs <- which(grepl("федеральный округ",data$Region) == TRUE)
indxs <- append(indxs,nrow(data)+1)

# Находим все регионы, которые "зажаты" между двумя округами в таблице (все они будут принадлежать верхнему округу в таблице)
index1 <- which(my_str == data$Region)
index2 <- indxs[which(indxs == index1) + 1]
regions_index <- (index1+1):(index2-1)
region <- data[regions_index,]

# "Расширим" таблицу, чтобы у нас теперь был столбец с годом
pivoted <- pivot_longer(region, !Region, names_to = "year", values_to = "value")
pivoted[-1] <- lapply(pivoted[-1], function(x) as.numeric(x)) #переведем все в числа


ggplot(pivoted) +
  geom_line(mapping = aes(x = year, y = value, colour = Region)) +
  geom_point(mapping = aes(x = year, y = value)) + 
  geom_text(aes(x = year, y = value, label = value), size = 2.3,nudge_x= -0.070, nudge_y = -0.045)+
  xlab("year") + 
  ylab("area") + 
  ggtitle(my_str)
