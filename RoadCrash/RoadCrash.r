require(jsonlite)
require(lattice)
require(dplyr)
require(ggplot2)
require(ggmap)
require(lubridate)

#
# Загрузка данных
#

regions <- read.table("D:\\Projects\\RIntro_assignments\\RoadCrash\\regions.tsv",
                      sep = '\t',
                      header = TRUE,
                      encoding = 'UTF-8')

gibdd <- read.table("D:\\Projects\\RIntro_assignments\\RoadCrash\\cards.tsv",
                    sep = '\t',
                    header = TRUE,
                    encoding = 'UTF-8',
                    colClasses = c(reasons="character"))

saferoads <- stream_in(file("D:\\Projects\\RIntro_assignments\\RoadCrash\\saferoads.jsonl", open = "r"), flatten=TRUE)

saferoads <- saferoads %>% select(type, road.light)

#
# Распределение аварий по месяцам
#

gibdd <- gibdd %>% mutate(date = date(timestamp), time = time(timestamp), weekday = wday(timestamp, label = TRUE), month = month(timestamp, label = TRUE))

ggplot(data = gibdd, aes(x = date)) +
  geom_freqpoly(stat="count", colour = "#FF6666") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Месяц", y = "Число аварий")

#
# Распределение аварий по дням недели
#

ggplot(data = gibdd, aes(x = weekday)) +
  geom_bar(fill = "#FF6666") +
  labs(x = "День недели", y = "Число аварий")

#
# Самые аварийные города
#

cities_with_most_accidents <- head(as.data.frame(gibdd %>% group_by(city) %>% summarise(count=n())) %>% arrange(-count), 21)[-1,]
  
city_population <- c(12330126, # москва
                     5225690, # спб
                     1178079, # омск
                     1216965, # казань
                     1041876, # пермь
                     1266871, # нижний новгород
                     1066934, # красноярск
                     1191994,	# челябинск
                     1119875, # ростов-на-дону
                     1584138, # новосибирск 
                     635585, # барнаул 
                     720575, # тюмень
                     843460, # саратов
                     623424, # иркутск
                     1032382, # воронеж
                     1170910, # самара
                     1016137, # волгоград
                     606653,	# владивосток
                     531719,	# астрахань
                     524632 #  пенза
                     ) 

cities_with_most_accidents <- cbind(cities_with_most_accidents, city_population)

cities_with_most_accidents <- cities_with_most_accidents %>%
  mutate(ratio = count / city_population) %>%
  arrange(desc(ratio))

ggplot(cities_with_most_accidents, aes(x = reorder(city, ratio), y = ratio)) +
  geom_bar(stat="identity", fill="#FF6666") +
  coord_flip() +
  labs(x = "Отношение количества аварий к численности населения", y = "Город")
  

#
# Топ-4 самых аварийных городов х состояние дорог
#

a <- c("г Омск", "г Казань", "г Пермь", "г Нижний Новгород")
b <- gibdd %>%
  filter(city %in% c("г Омск", "г Казань", "г Пермь", "г Нижний Новгород")) %>%
  mutate(problems_present = (problems != "")) %>%
  group_by(city, problems_present) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(ratio = count / c(3456, 3456, 3411, 3411, 4262, 4262, 3434, 3434))


ggplot(data = b, aes(x = city, y = ratio, fill = problems_present)) +
  geom_bar(stat = "identity")

#
# Типы аварий
#

accidents_by_type <- gibdd %>%
  group_by(type) %>%
  summarise(count=n()) %>%
  filter(count > 200)

ggplot(data = accidents_by_type, aes(x = reorder(type, count), y = count)) +
  geom_bar(stat = "identity", fill="#FF6666") +
  labs(x = "Тип ДТП", y = "Число ДТП") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()

#
# Столкновения и наезды на пешеходов по месяцам
#

crashes_and_runovers <- gibdd %>%
  filter(type %in% c("Наезд на пешехода", "Столкновение"))

ggplot(data = crashes_and_runovers, aes(x = date, colour = type)) +
  geom_freqpoly(stat="count") +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Месяц", y = "Число аварий", colour = "Тип ДТП")

# 
# Аварии по невнимательности
#

selffaults <- saferoads %>% 
  filter(type %in% c("Наезд на препятствие", "Опрокидывание", "Наезд на стоящее ТС", "Съезд с дороги")) %>%
  filter(road.light != "") %>%
  group_by(road.light) %>%
  summarise(count = n())

ggplot(data = selffaults, aes(x = reorder(road.light, count), y = count)) +
  geom_bar(stat = "identity", fill="#FF6666") +
  labs(x = "Освещённость места ДТП", y = "Число ДТП") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()


# 
# [Исправлено: нормировка на число] Аварии по невнимательности
#

counts_by_light_condition <- saferoads %>%
  filter(road.light != "") %>%
  group_by(road.light) %>%
  summarise(total_count = n()) %>%
  select(total_count)

selffaults <- saferoads %>% 
  filter(type %in% c("Наезд на препятствие", "Опрокидывание", "Наезд на стоящее ТС", "Съезд с дороги")) %>%
  filter(road.light != "") %>%
  group_by(road.light) %>%
  summarise(count = n())

selffaults <- cbind(selffaults, counts_by_light_condition) %>%
  filter(road.light != "Не установлено") %>%
  mutate(ratio = count / total_count)

ggplot(data = selffaults, aes(x = reorder(road.light, ratio), y = ratio)) +
  geom_bar(stat = "identity", fill="#FF6666") +
  labs(x = "Освещённость места ДТП", y = "Число ДТП") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip()

#
# Места происшествий
#

accidents_by_place <- gibdd %>% 
  group_by(places) %>%
  summarise(count=n()) %>%
  filter(count > 1000)

levels(accidents_by_place$places)[156] <- "Нерег. п. п; нерег. перекрёсток неравнозначных улиц (дорог)"

ggplot(data = accidents_by_place, aes(x = reorder(places, count), y = count)) +
  geom_bar(stat = "identity", fill="#FF6666") +
  labs(x = "Тип места ДТП", y = "Число ДТП") +
  scale_y_continuous(labels = scales::comma) +
  theme(text = element_text(size=14)) +
  coord_flip()

#
# Наезды на пешеходов на регулируемых и нерегулируемых переходах по виновнику
#

crosswalk_accidents <- gibdd %>%
  filter(type == "Наезд на пешехода" & places %in% c("Регулируемый пешеходный переход", "Нерегулируемый пешеходный переход")) %>%
  filter(fault %in% c("Вод", "Пеш"))

ggplot(data = crosswalk_accidents, aes(x = fault, fill = places)) +
  geom_bar(position="dodge") +
  labs(fill = "Место происшествия", x = "Виновник", y = "Число ДТП")

#
# Нерегулируемые перекрёстки
#

unregulated_crossing_crashes <- gibdd %>%
  filter(places %in% c("Нерегулируемый перекрёсток неравнозначных улиц (дорог)", "Нерегулируемый перекрёсток равнозначных улиц (дорог)")) %>%
  filter(type == "Столкновение") %>%
  mutate(crossing_order_fault = (reasons == "Водитель: Несоблюдение очередности проезда")) %>%
  group_by(crossing_order_fault, places) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(ratio = count / c(25825, 1701, 25825, 1701))

ggplot(data = unregulated_crossing_crashes, aes(fill = crossing_order_fault, x = places, y = ratio)) +
  geom_bar(stat="identity", position="stack") +
  labs(x = "Место происшествия", fill = "Нарушена очерёдность проезда", y = "Число ДТП") +
  coord_flip()



ggsave("reasons_of_crossing_crashes.png", device="png", path = "C:\\Users\\falce\\Desktop\\Slides\\img", width = 10)

#
# Крупные аварии
#

major_accidents <- gibdd %>%
  filter(fatalities >= 5) %>%
  arrange(desc(fatalities)) %>%
  mutate(on_highway = (road != ""))

#
# Почти все -- на трассах
#

ggplot(data = major_accidents, aes(x = on_highway)) +
  geom_bar(fill="#FF6666") +
  labs(x = "ДТП произошло на трассе", y = "Число ДТП")

#
# Крупные аварии на карте
#

major_accidents_by_saferoads <- saferoads %>%
  filter(fatalities >= 5) %>%
  arrange(desc(fatalities))

major_accidents_map <- get_map(location = c(lon = mean(major_accidents_by_saferoads$coordinates.longitude), 
                                  lat = mean(major_accidents_by_saferoads$coordinates.latitude)), 
                     zoom = 4,
                     maptype = "roadmap",
                     scale = 2,
                     language = "ru-RU")

ggmap(major_accidents_map) + 
  geom_point(data = major_accidents_by_saferoads, 
             aes(x = coordinates.longitude, y = coordinates.latitude, fill = fatalities, alpha = 0.8), 
             size = 3, 
             shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#
# Крупные аварии по типам
#

ggplot(data = major_accidents, aes(x = type)) +
  geom_bar(fill="#FF6666") +
  labs(x = "Тип ДТП", y = "Количество") +
  coord_flip()

#
# Крупные аварии по числу автомобилей
#

ggplot(data = major_accidents, aes(x = vehicles)) +
  geom_bar(fill="#FF6666") +
  scale_x_continuous(breaks = 1:7) +
  labs(x = "Число автомобилей", y = "Число ДТП")

ggplot(data = 
         major_accidents %>%
          filter(reasons %in% c("Водитель: Нарушение правил обгона",
                                "Водитель: Выезд на полосу встречного движения",
                                "Водитель: Неправильный выбор дистанции",
                                "Водитель: Превышение установленной скорости движения",
                                "Водитель: Несоответствие скорости конкретным условиям движения",
                                "Водитель: Нарушение правил расположения ТС на проезжей части",
                                "Водитель: Другие нарушения ПДД")),
       aes(x = reasons)) +
  geom_bar(fill="#FF6666") +
  labs(x = "Причина ДТП", y = "Число ДТП") +
  coord_flip()

#
# Наезды на животных
#

animal_runovers <- saferoads %>% filter(type == "Наезд на животное")

#
# В целом
#

map_whole <- get_map(location = c(lon = mean(animal_runovers$coordinates.longitude), 
                                   lat = mean(animal_runovers$coordinates.latitude)), 
                      zoom = 4,
                      maptype = "roadmap",
                      scale = 2,
                      language = "ru-RU")
ggmap(map_whole) + 
  geom_point(data = animal_runovers, 
             aes(x = coordinates.longitude, y = coordinates.latitude, fill = "red", alpha = 0.8), 
             size = 3, 
             shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#
# На юге
#

map_south <- get_map(location = c(lon = 45, lat = 47),
                     maptype = "roadmap",
                     zoom = 6,
                     scale = 2,
                     language = "ru-RU")

ggmap(map_south) +
  geom_point(data = animal_runovers,
             aes(x = coordinates.longitude, y = coordinates.latitude, fill = "red", alpha = 0.8),
             size = 3,
             shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#
# В центре
#

map_center <- get_map(location = c(lon = 37, lat = 55),
                      maptype = "roadmap",
                      zoom = 6,
                      scale = 2,
                      language = "ru-RU")

ggmap(map_center) + 
  geom_point(data = animal_runovers, 
             aes(x = coordinates.longitude, y = coordinates.latitude, fill = "red", alpha = 0.8), 
             size = 3, 
             shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)

#
# На северо-западе
#

map_nw <- get_map(location = c(lon = 31, lat = 59),
                      maptype = "roadmap",
                      zoom = 6,
                      scale = 2,
                      language = "ru-RU")

ggmap(map_nw) + 
  geom_point(data = animal_runovers, 
             aes(x = coordinates.longitude, y = coordinates.latitude, fill = "red", alpha = 0.8), 
             size = 3, 
             shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)
