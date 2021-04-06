### Підключаємо необхідні бібліотеки

# library(devtools)
# install_github("amice13/drv")

library(readr)
library(tidyverse)
options(dplyr.summarise.inform = FALSE)

library(drv)
library(stringr)
library(TSP)
library(doParallel)
library(leaflet)
library(httr)

### Завантажуємо дані, які нас цікавлять

dir.create('./elections-data', showWarnings = FALSE)

download.file(
  url = 'https://ukr.vote/assets/export/language_1/election_8/ElectionProtocols_csv.zip',
  destfile = './elections-data/rada-protocols-2019.zip'
)

download.file(
  url = 'https://ukr.vote/assets/export/language_1/election_8/ElectionResults_csv.zip',
  destfile = './elections-data/rada-results-2019.zip'
)

download.file(
  url = 'https://ukr.vote/assets/export/language_1/election_9/ElectionProtocols_csv.zip',
  destfile = './elections-data/nardep-protocols-2019.zip'
)

download.file(
  url = 'https://ukr.vote/assets/export/language_1/election_9/ElectionResults_csv.zip',
  destfile = './elections-data/nardep-results-2019.zip'
)

### Зчитуємо дані до оперативної пам'яті

unzip('./elections-data/rada-protocols-2019.zip', list = TRUE)
unzip('./elections-data/rada-results-2019.zip', list = TRUE)
unzip('./elections-data/nardep-protocols-2019.zip', list = TRUE)
unzip('./elections-data/nardep-results-2019.zip', list = TRUE)

propProtocols2019 <- read_tsv(
  file = unzip('./elections-data/rada-protocols-2019.zip', 'ElectionProtocols.csv')
)

propResults2019 <- read_tsv(
  file = unzip('./elections-data/rada-results-2019.zip', 'ElectionResults.csv')
)

majorProtocols2019 <- read_tsv(
  file = unzip('./elections-data/nardep-protocols-2019.zip', 'ElectionProtocols.csv')
)

majorResults2019 <- read_tsv(
  file = unzip('./elections-data/nardep-results-2019.zip', 'ElectionResults.csv')
)

### Огляд даних

summary(propProtocols2019)
summary(majorResults2019)
summary(propResults2019)

### Очищення даних

# 1. Необхідні унікальні ключі

propProtocols2019$key <- paste0(propProtocols2019$district_no, '-', propProtocols2019$precinct_no)
majorProtocols2019$key <- paste0(majorProtocols2019$district_no, '-', majorProtocols2019$precinct_no)
propResults2019$key <- paste0(propResults2019$district_no, '-', propResults2019$precinct_no)
majorResults2019$key <- paste0(majorResults2019$district_no, '-', majorResults2019$precinct_no)

# 2. Необхідно видалити зайві змінні

dropProtocols <- c('election_name', 'election_type', 'election_date', 'koatuu', 
                   'district_desc', 'ballots_false_percent',
                   'ballots_received_home_percent', 'ballots_spoiled', 'voted_precinct',
                   'voted_home', 'voted_home_percent', 'voted_total_percent',
                   'ballots_invalid_percent', 'votes_againstall', 'votes_againstall_percent')
propProtocols2019 <- propProtocols2019[, !(names(propProtocols2019) %in% dropProtocols)]
majorProtocols2019 <- majorProtocols2019[, !(names(majorProtocols2019) %in% dropProtocols)]

dropResults <- c('election_name', 'election_type', 'election_date', 'region',
                 'rayon', 'community', 'city_rayon', 'koatuu',
                 'district_desc', 'precinct_desc', 'vote_percent')

propResults2019 <- propResults2019[, !names(propResults2019) %in% dropResults]
majorResults2019 <- majorResults2019[, !names(majorResults2019) %in% dropResults]
propResults2019$partisan <- NULL
propResults2019$candidate <- NULL
rm(dropProtocols, dropResults)

# 3. Необхідно відфільтрувати закордонні виборчі дільниці (ballots_amount)

propProtocols2019 <- propProtocols2019[propProtocols2019$district_no != 226, ]
propResults2019 <- propResults2019[propResults2019$district_no != 226, ]

# 4. Необхідно відфільтрувати спеціальні дільниці (voters_home)

propProtocols2019$special <- !grepl('^([см]\\.|смт|с-ще)', propProtocols2019$precinct_desc)
majorProtocols2019$special <- !grepl('^([см]\\.|смт|с-ще)', majorProtocols2019$precinct_desc)
propProtocols2019$precinct_desc <- NULL
majorProtocols2019$precinct_desc <- NULL
propResults2019$precinct_no <- NULL
majorResults2019$precinct_no <- NULL
propResults2019$district_no <- NULL

### Побудова вибірки для екзит-полу

propResults <- merge(propResults2019, propProtocols2019, by = "key")
propResults <- propResults %>%
  group_by(key) %>%
  mutate(prob = prop.table(vote_count))

keys <- unique(propResults$key)

# Задаємо крок відобору респондентів s, та кількість дільниць для вибірки sample size 
s = 10
sample_size = 30

# Визначаємо результат виборів
actual <- propResults %>%
  group_by(party) %>%
  summarize(vote_count = sum(vote_count)) %>%
  mutate(vote_count = prop.table(vote_count))

actual %>% print(n = Inf)

# Визначаємо оцінку результатів
estimate <- propResults %>%
  filter(key %in% sample(keys, size = sample_size, replace = F)) %>%
  filter(!is.na(prob)) %>%
  group_by(precinct_no) %>%
  summarize(party = sample(party, size = voted_total/s, replace = T, prob = prob)) %>%
  group_by(party) %>%
  summarize(vote_count = n()) %>%
  mutate(vote_prop = prop.table(vote_count))

estimate %>% print(n = Inf)

# Оцінка розміру похибки
makeEstimate <- function(s, sample_size) {
  estimate <- propResults %>%
    filter(key %in% sample(keys, size = sample_size, replace = F)) %>%
    filter(!is.na(prob)) %>%
    group_by(precinct_no) %>%
    summarize(party = sample(party, size = voted_total/s, replace = T, prob = prob)) %>%
    group_by(party) %>%
    summarize(vote_count = n()) %>%
    mutate(vote_prop = prop.table(vote_count))
  for (party in actual$party[!actual$party %in% estimate$party]) {
    estimate <- estimate %>% add_row(party = party, vote_prop = 0)
  }
  estimate <- estimate %>% arrange(party)
  # Єдина оцінка для похибки
  # max можна замінити на 95% процентиль або sum
  max(abs(actual$vote_count - estimate$vote_prop))
}

# Пірдахунок оцінки
s = 5
sample_size = 200
estimates <- replicate(30, makeEstimate(s, sample_size))
hist(estimates)

### Побудова вибірки для екзит-полу в рамках виборчого округу

okrug <- propResults %>%
  filter(district_no == '24')

keys <- unique(okrug$key)

s = 5
sample_size = 60

actual <- okrug %>%
  group_by(party) %>%
  summarize(vote_count = sum(vote_count)) %>%
  mutate(vote_count = prop.table(vote_count))

makeEstimate <- function(s, sample_size) {
  estimate <- okrug %>%
    filter(key %in% sample(keys, size = sample_size, replace = F)) %>%
    filter(!is.na(prob)) %>%
    group_by(precinct_no) %>%
    summarize(party = sample(party, size = voted_total/s, replace = T, prob = prob)) %>%
    group_by(party) %>%
    summarize(vote_count = n()) %>%
    mutate(vote_prop = prop.table(vote_count))
  for (party in actual$party[!actual$party %in% estimate$party]) {
    estimate <- estimate %>% add_row(party = party, vote_prop = 0)
  }
  estimate <- estimate %>% arrange(party)
  max(abs(actual$vote_count - estimate$vote_prop))
}

estimates <- replicate(100, makeEstimate(s, sample_size))
hist(estimates)

### Побудова вибірки для соціологічного дослідження в рамках округу

# Отримуємо перелік територі з ДРВ
regions <- get_regions()
territories <- get_territories(12)

# Отримуємо адреси необхідного населеного пунтка
locality <- get_addresses(14185)

# Створюємо масив унікальних домогосподарств
locality$Bld_Flats[is.na(locality$Bld_Flats)] <- 1
locality$Bld_Flats <- as.numeric(locality$Bld_Flats)

# Оцінємо пропорції домогосподарств у будинках
locality$prob <- prop.table(locality$Bld_Flats)

# Створюємо вибірку
sample_size = 1000
t <- table(sample(locality$Bld_ID, sample_size, prob = locality$prob, replace = T))
locality$sample = t[locality$Bld_ID]
locality$sample[is.na(locality$sample)] <- 0
locality$sample[locality$sample > locality$Bld_Flats] <- locality$Bld_Flats[locality$sample > locality$Bld_Flats]

# Функція для випадкового відбору квартир
f <- function(x) {
  paste0(sample(1:x['Bld_Flats'], x['sample'], replace = F), collapse=", ")
}

locality$flat <- apply(locality, 1, f)

### Вирішуємо логістичні питання

okrug <- get_polling_stations(12)

okrug$lng <- as.numeric(str_extract(okrug$PS_GeoDVK, '[\\d.]+'))
okrug$lat <- as.numeric(str_extract(okrug$PS_GeoDVK, '(?<=,)[\\d.]+'))

# Будуємо матрицю відстаней
dist_mat <- dist(
  okrug %>% select(lng, lat),
  method = 'euclidean'
)

# Починаємо вирішувати задачу
tsp_prob <- TSP(dist_mat)

# Додаємо один елемент даних, щоб пошук не створював маршрут із кільцем
tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')

# Додаємо можливість паралельного підрахунку
registerDoParallel()

# Шукаємо результат
tour <- solve_TSP(
  tsp_prob,
  method = 'two_opt',
  control = list(rep = 16)
)
path <- names(cut_tour(tour, 'dummy'))

# Візуалізація результата
okrug <- okrug %>% 
  mutate(id_order = order(as.integer(path)))

okrug %>% 
  arrange(id_order) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~lng,
    ~lat,
    fillColor = 'red',
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>% 
  addPolylines(~lng, ~lat)

### Якщо є декілька транспортних засобів

# Проводимо кластерний аналіз методами повного зв'язку
clusters <- hclust(dist_mat, method = 'complete')
plot(clusters)

rect.hclust(clusters, k=6, border="red")
groups <- cutree(clusters, k=6)

okrug$cluster = groups

# Повторюємо процедуру для кожного округу
dist_mat <- dist(
  okrug %>% filter(cluster == 1) %>% select(lng, lat),
  method = 'euclidean'
)

# Починаємо вирішувати задачу
tsp_prob <- TSP(dist_mat)

# Додаємо один елемент даних, щоб пошук не створював маршрут із кільцем
tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')

# Додаємо можливість паралельного підрахунку
registerDoParallel()

# Шукаємо результат
tour <- solve_TSP(
  tsp_prob,
  method = 'two_opt',
  control = list(rep = 16)
)
path <- names(cut_tour(tour, 'dummy'))

# Візуалізація результата
okrug <- okrug %>% filter(cluster == 1) %>%
  mutate(id_order = order(as.integer(path)))

okrug %>% filter(cluster == 1) %>%
  arrange(id_order) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~lng,
    ~lat,
    fillColor = 'red',
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>% 
  addPolylines(~lng, ~lat)

r <- POST('https://api.openrouteservice.org/v2/matrix/driving-car',
          body = list(locations = okrug %>% select(lng, lat) %>% data.matrix),
          encode = 'json',
          verbose(),
          add_headers(Authorization = Sys.getenv("OPEN_ROUTE_KEY")))
dist <- matrix(unlist(content(r)$durations), ncol = 11)
dist_mat <- as.dist(dist)

# Починаємо вирішувати задачу
tsp_prob <- TSP(dist_mat)

# Додаємо один елемент даних, щоб пошук не створював маршрут із кільцем
tsp_prob <- insert_dummy(tsp_prob, label = 'dummy')

# Додаємо можливість паралельного підрахунку
registerDoParallel()

# Шукаємо результат
tour <- solve_TSP(
  tsp_prob,
  method = 'two_opt',
  control = list(rep = 16)
)
path <- names(cut_tour(tour, 'dummy'))

# Візуалізація результата
okrug <- okrug %>% filter(cluster == 1) %>%
  mutate(id_order = order(as.integer(path)))

okrug %>% filter(cluster == 1) %>%
  arrange(id_order) %>% 
  leaflet() %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~lng,
    ~lat,
    fillColor = 'red',
    fillOpacity = 0.5,
    stroke = FALSE
  ) %>% 
  addPolylines(~lng, ~lat)
