### Підключаємо необхідні бібліотеки

# library(devtools)
# install_github("amice13/drv")

library(readr)
library(tidyverse)
library(stringr)
library(RColorBrewer)
library(broom)
library(leaflet)
library(drv)
library(jsonlite)

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

### Базовий аналіз даних

## Одиночні оцінки

# Кількість виборчих дільниць в Україні
nrow(propProtocols2019)

# Кількість виборців
sum(propProtocols2019$voters_amount)

# Загальна явка
sum(propProtocols2019$voted_total)/sum(propProtocols2019$voters_amount)

# Відсоток виборців, які проголосували за місцем перебування
sum(propProtocols2019$ballots_received_home)/sum(propProtocols2019$voted_total)

# Активність виборців за місцем перебування
sum(propProtocols2019$ballots_received_home)/sum(propProtocols2019$voters_home)

## Одномірні розподіли

# Кількість дільниць
propProtocols2019 %>%
  group_by(region) %>%
  summarize(n = n()) %>%
  print(n = Inf)

# Явка за регіонами
propProtocols2019 %>%
  group_by(region) %>%
  summarize(turnout = sum(voted_total)/sum(voters_amount)) %>%
  arrange(desc(turnout)) %>%
  print(n = Inf)

# Результати виборів
propResults2019 %>%
  group_by(party) %>%
  summarize(votes = sum(vote_count)/sum(propProtocols2019$voted_total)) %>%
  arrange(desc(votes)) %>%
  print(n = Inf)

# Результати виборів з урахуванням бар'єру
propResults2019 %>%
  group_by(party) %>%
  summarize(votes = sum(vote_count)/sum(propProtocols2019$voted_total)) %>%
  filter(votes >= 0.05) %>%
  mutate(votes = votes/sum(votes)) %>%
  arrange(desc(votes)) %>%
  print(n = Inf)

### Дослідження впливу партійності

majorResults2019 <- rename(majorResults2019, major_vote_count = vote_count)
partyMajor <- merge(propResults2019, majorResults2019, by = c('key', 'party'), all.x = T)
partyMajor <- partyMajor[!is.na(partyMajor$major_vote_count), ]
partyMajor$partisan[partyMajor$partisan != 'Безпартійний'] <- 'Від партії'

partyMajor <- merge(partyMajor, majorProtocols2019[c('key', 'votes_candidates')], by = 'key', all.x = T)
partyMajor$percent <- partyMajor$major_vote_count / partyMajor$votes_candidates

partyMajor %>%
  ggplot(., aes(x = partisan, y = percent, fill = partisan)) +
  geom_boxplot() + facet_wrap(~party) +
  theme(legend.position = 'bottom') +
  scale_fill_discrete(name = 'Партійність') +
  labs(title = 'Відсоток голосів за кандидатів (мажоритарна система)',
       subtitle = 'Порівняння партійних та безпартійних кандидатів',
       y = 'Відсоток голосів',
       x = NULL,
       color = NULL)

### Статистичні перевірки

#№ Додаткова обрабка даних

# Яквка для кожної виборчої дільниці
propProtocols2019$turnout <- propProtocols2019$voted_total / propProtocols2019$voters_amount * 100

### Первинний аналіз даних

# Надлишкова кількість зіпсованих бюлетенів
summary(propProtocols2019$ballots_invalid / propProtocols2019$voted_total)
quantile(propProtocols2019$ballots_invalid / propProtocols2019$voted_total, 0.95, na.rm = T)
propProtocols2019$spoilt_outliers <- (propProtocols2019$ballots_invalid / propProtocols2019$voted_total) >
  quantile(propProtocols2019$ballots_invalid / propProtocols2019$voted_total, 0.95, na.rm = T)

# Надвелика явка
quantile(propProtocols2019$turnout, 0.95)
table(!propProtocols2019$special & propProtocols2019$turnout > quantile(propProtocols2019$turnout, 0.95))
propProtocols2019$turnout_outliers <- !propProtocols2019$special & propProtocols2019$turnout > quantile(propProtocols2019$turnout, 0.95)

# Надвелика кількість голосуючих вдома

propProtocols2019 %>%
  filter(!special) %>%
  ggplot(., aes(x = voters_home/voters_amount, y = ballots_received_home/voters_home)) +
  geom_point(alpha = 0.2, size = 1) +
  labs(title = 'Зв\'язок явки та результатів голосування',
       y = 'Відсоток виборців, що були внесені до списку для голосування вдома',
       x = 'Відсоток виборців, що проголосували вдома',
       color = NULL)

propProtocols2019$home_oultiers <- propProtocols2019$voters_home/propProtocols2019$voters_amount > 0.25 & !propProtocols2019$special

### Створюємо набір з результатами

# Результати для кожної виборчої дільниці
propResults <- merge(propResults2019, propProtocols2019, by = "key")
propResults$vote_percent <- propResults$vote_count / propResults$votes_candidates * 100

# Вплив псування бюлетенів
propResults %>%
  filter(!is.na(spoilt_outliers)) %>%
  group_by(party, spoilt_outliers) %>%
  summarize(result = sum(vote_count)/sum(voted_total)) %>%
  ggplot(., aes(fill = spoilt_outliers, y = result, x = reorder(party, result))) + 
  geom_bar(position='dodge', stat='identity') + coord_flip() +
  theme(legend.position='bottom') + 
  scale_fill_discrete(name = 'Зіпсовані бюлетені', labels = c('Ні', 'Так')) +
  labs(title = 'Вплив зіпсованих бюлетенів',
       y = 'Результат у %',
       x = 'Партії',
       color = NULL)

# Вплив надвеликої явки
propResults %>%
  filter(!is.na(turnout_outliers)) %>%
  group_by(party, turnout_outliers) %>%
  summarize(result = sum(vote_count)/sum(voted_total)) %>%
  ggplot(., aes(fill = turnout_outliers, y = result, x = reorder(party, result))) + 
  geom_bar(position='dodge', stat='identity') + coord_flip() +
  theme(legend.position='bottom') + 
  scale_fill_discrete(name = 'Надвелика явка', labels = c('Ні', 'Так')) +
  labs(title = 'Вплив форсування явки',
       y = 'Результат у %',
       x = 'Партії',
       color = NULL)

# Вплив голосуючих вдома
propResults %>%
  filter(!is.na(home_oultiers)) %>%
  group_by(party, home_oultiers) %>%
  summarize(result = sum(vote_count)/sum(voted_total)) %>%
  ggplot(., aes(fill = home_oultiers, y = result, x = reorder(party, result))) + 
  geom_bar(position='dodge', stat='identity') + coord_flip() +
  theme(legend.position='bottom') + 
  scale_fill_discrete(name = 'Надвелика кількість голосувань вдома', labels = c('Ні', 'Так')) +
  labs(title = 'Вплив голосуючих вдома',
       y = 'Результат у %',
       x = 'Партії',
       color = NULL)

# Аналіз серденьоквадратичних відхилень
# Д. Блека, Е. Даунс, Р. Маккелві, К. Мей, Ч. Плотт, К. Ерроу

propResults %>%
  group_by(party) %>%
  summarize(sd = sd(vote_percent, na.rm = T)) %>%
  arrange(desc(sd)) %>%
  print(n = Inf)

# Перевірка розподілів останньої цифри (Метод Бебера-Скакко)

estimate <- replicate(1000, max(abs(prop.table(table(sample(0:9, nrow(propProtocols2019), replace = T))) - 0.1)))
estimate <- quantile(estimate, 0.95)

party_votes <- propResults2019
party_votes$one_number <- as.character(party_votes$vote_count)
party_votes$one_number <- sub('.*(.)$', '\\1', as.character(party_votes$vote_count))

# Усі партії для прикладу
party_votes %>%
  group_by(party, one_number) %>%
  summarize(n = n() / nrow(propProtocols2019) - 0.1) %>%
  ggplot(., aes(x = as.numeric(one_number), y = n)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 0.4) +
  facet_wrap(~party) +
  scale_x_discrete(limits = 0:9) +
  labs(title = 'Перевірка рівномірності розподілу останньої цифри кількості голосів',
       subtitle = 'Метод Бебера-Скакко',
       y = 'Відсоток цифр',
       x = NULL,
       color = NULL)

# Фільтрування даних до переможців
winners <- propResults2019 %>%
  group_by(party) %>%
  summarize(votes = sum(vote_count)/sum(propProtocols2019$voted_total)) %>%
  filter(votes >= 0.05)

# Повторний аналіз
party_votes %>%
  filter(party %in% winners$party) %>%
  group_by(party, one_number) %>%
  summarize(n = n() / nrow(propProtocols2019) - 0.1) %>%
  ggplot(., aes(x = as.numeric(one_number), y = n)) +
  geom_line() + geom_point() +
  scale_x_discrete(limits = 0:9) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 0.4) +
  geom_hline(yintercept = estimate, linetype = 'dashed', color = 'blue', size = 0.4) +
  geom_hline(yintercept = -estimate, linetype = 'dashed', color = 'blue', size = 0.4) +
  facet_wrap(~party) +
  labs(title = 'Перевірка рівномірності розподілу останньої цифри кількості голосів',
       subtitle = 'Метод Бебера-Скакко',
       y = 'Вірогідність цифр',
       x = NULL,
       color = NULL)

# Перевірка розподілів двох останніх цифр

party_votes$two_numbers <- sub('.*(..)$', '\\1', as.character(party_votes$vote_count))
party_votes$two_numbers <- str_pad(party_votes$two_numbers, 2, side = 'left', '0')

estimate100 <- replicate(1000, max(abs(prop.table(table(sample(0:100, nrow(propProtocols2019), replace = T))) - 0.01)))
estimate100 <- quantile(estimate, 0.95)

party_votes %>%
  filter(party %in% winners$party) %>%
  filter(substring(two_numbers, 1, 1) == substring(two_numbers, 2, 2)) %>%
  group_by(party, two_numbers) %>%
  summarize(n = n() / nrow(propProtocols2019) - 0.01) %>%
  ggplot(., aes(x = as.numeric(two_numbers), y = n)) +
  geom_line() + geom_point() +
  scale_x_discrete(limits = seq(0, 99, 11)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'red', size = 0.4) +
  geom_hline(yintercept = estimate100, linetype = 'dashed', color = 'blue', size = 0.4) +
  geom_hline(yintercept = -estimate100, linetype = 'dashed', color = 'blue', size = 0.4) +
  facet_wrap(~party) +
  labs(title = 'Перевірка рівномірності розподілу двої останнніх цифр кількості голосів',
       subtitle = 'Метод Бебера-Скакко',
       y = 'Вірогідність чисел',
       x = NULL,
       color = NULL)

rm(estimate, estimate100)

### Перевірка розподілу кількості виборчих дільниць із різними результатами
### голосування (метод Шпількіна)

propResults %>%
  filter(party %in% winners$party) %>%
  group_by(party, vp = round(vote_percent)) %>%
  summarize(n = n()) %>%
  ggplot(., aes(x = round(vp), y = n, color = party)) +
  geom_line() +
  scale_color_manual(values=c('#EE2136', '#FF6600', '#580526', '#0062BC', '#044832')) +
  theme(legend.position = 'bottom') +
  scale_x_discrete(limits = seq(0, 100, 10)) +
  labs(title = 'Розподіл виборчих дільниць з різними результатами голосування',
       subtitle = 'Метод перевірки С. Шпількіна',
       y = 'Кількість виборчих дільниць',
       x = 'Відсоток голосів виборців',
       color = NULL)

### Перевірка розподілу кількості голосів в залженості від явки

propResults %>%
  filter(party %in% winners$party) %>%
  mutate(half_turnout = round(turnout / 0.5) * 0.5) %>%
  group_by(party, half_turnout) %>%
  summarize(n = sum(vote_count)) %>%
  ggplot(., aes(x = as.numeric(half_turnout), y = n, color = party)) +
  geom_line() +
  scale_color_manual(values=c('#EE2136', '#FF6600', '#580526', '#0062BC', '#044832')) +
  theme(legend.position = 'bottom') +
  scale_x_discrete(limits = seq(0, 100, 10)) +
  labs(title = 'Розподіл кількості голосів виборців за партії на дільницях з різною явкою',
       subtitle = 'Метод перевірки С. Шпількіна (2)',
       y = 'Кількість голосів виборців',
       x = 'Явка з кроком 0.5%',
       color = NULL)

propResults %>%
  filter(party %in% winners$party) %>%
  group_by(party, vp = round(vote_percent)) %>%
  summarize(n = sum(vote_count)) %>%
  ggplot(., aes(x = vp, y = n, color = party)) +
  geom_line() +
  scale_color_manual(values=c('#EE2136', '#FF6600', '#580526', '#0062BC', '#044832')) +
  theme(legend.position = 'bottom') +
  scale_x_discrete(limits = seq(0, 100, 10)) +
  labs(title = 'Розподіл кількості голосів виборців за партії на дільницях з різним результатом партії',
       subtitle = 'Метод перевірки С. Шпількіна (2)',
       y = 'Кількість голосів виборців',
       x = 'Результат партії з кроком 1%',
       color = NULL)

### Двомірні графіки розподілу

propResults %>%
  filter(party %in% winners$party) %>%
  ggplot(., aes(x = turnout, y = vote_count/voters_amount, color = party, stroke = NULL)) +
  geom_point(alpha = 0.2, size = 1) +
  scale_color_manual(values=c('#EE2136', '#FF6600', '#580526', '#0062BC', '#044832')) +
  theme(legend.position = 'bottom') +
  labs(title = 'Зв\'язок явки та результатів голосування',
       subtitle = 'Метод Собяніна-Суховольського',
       y = 'Відсоток голосів виборців за партію',
       x = 'Явка з кроком 0.5%',
       color = NULL)

propResults %>%
  filter(party %in% winners$party) %>%
  group_by(party) %>%
  do(model = tidy(lm(vote_count/voters_amount * 100 ~ turnout, .))) %>%
  unnest(model)

# Двомірні гістограми

rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

propResults %>%
  filter(party %in% winners$party) %>%
  ggplot(., aes(x = turnout, y = vote_percent, color = party, stroke = NULL)) +
  stat_bin2d(bins=100) + scale_fill_gradientn(colours=r) +
  facet_wrap(~party) +
  theme(legend.position = 'none') +
  labs(title = 'Зв\'язок явки та результатів голосування (гістограма)',
       y = 'Відсоток голосів виборців за партію, %',
       x = 'Явка з кроком 1%',
       color = NULL)

# Перевірка з кумулятивнрю сумою (побудова сигмоїди)
propResults %>%
  filter(party %in% winners$party) %>%
  group_by(party, rt = round(turnout)) %>%
  summarize(cs = sum(vote_count)) %>%
  group_by(party) %>%
  mutate(cs = prop.table(cs)) %>%
  arrange(party, rt) %>%
  group_by(party) %>%
  mutate(cs = cumsum(cs)) %>%
  ggplot(., aes(x = rt, y = cs, color = party, stroke = NULL)) +
  geom_line() +
  scale_color_manual(values=c('#EE2136', '#FF6600', '#580526', '#0062BC', '#044832')) +
  theme(legend.position = 'none') +
  labs(title = 'Кумулятивне відображення відсотку отриманих партією голосів',
       y = 'Кумулятивний відсоток голосів виборців за партію, %',
       x = 'Явка з кроком 1%',
       color = NULL)
  

### Побудова мап

okrug <- get_polling_stations(12)

# Створюємо корисні функції 
createGeo <- function (v) {
  fromJSON(paste0('[', paste0(v, collapse = ','), ']'), simplifyVector = FALSE)
}

# Поєднуємо необхідні дані
okrug$key <- paste0(okrug$PS_Area, '-', okrug$PS_Num)
okrug <- merge(okrug, propResults[propResults$party == 'Слуга народу', ], by = "key")

# Задаємо стилі
hist(okrug$vote_percent)
bins <- c(quantile(okrug$vote_percent, probs = seq(0, 1, 0.15)), Inf)
pal <- colorBin("YlOrRd", domain = okrug$vote_percent, bins = bins)

okrug$geoJSON <- paste0(
  '{ "id": "', okrug$PS_Num, '",', 
  '  "geometry": ', okrug$PS_GeoData, ',',
  '  "type": "Feature",',
  '  "properties": {',
    '"style": {',
      '"fillColor": "', pal(okrug$vote_percent), '" }',
  '} } ')

# Створюємо формат необхідний для відображення
geojson <- createGeo(okrug$geoJSON)

# Малюємо мапу
leaflet() %>% setView(lng = 31.1828699, lat = 48.383022, zoom = 6) %>%
  addTiles() %>%
  addGeoJSON(geojson, weight = 2, fillOpacity = 0.5, color = 'white', dashArray = '3') %>%
  addLegend(pal = pal, values = bins, opacity = 0.7, title = NULL,
            position = "bottomright")

