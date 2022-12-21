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
  geom_text(data = SumExpImpData, aes(x = time, y = import, label = import,colour="import"), size = 2,nudge_x = 255, nudge_y = -1500) +
  geom_text(data = SumExpImpData, aes(x = time, y = export, label = export, colour="export"), size = 2,nudge_x = -300, nudge_y = 1000) +
  xlab('time') +
  ylab('value')+
  labs(color = "Notation", title = "Food, Drinks, Tobacco Export/Import")


