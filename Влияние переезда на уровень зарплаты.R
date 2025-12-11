
# Установить необходимые библиотеки
#install.packages(c("readxl", "dplyr", "tidyr", "broom", "p.adjust", 'RItools',"MatchIt", "WeightIt", "tableone", "cobalt", "ggplot2", "survey", 'glmnet', 'hdm', 'grf', 'boot'))
{
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(broom)
  library(RItools)
  library(MatchIt) 
  library(WeightIt)
  library(tableone)
  library(cobalt)
  library(ggplot2)
  library(survey)
  library(glmnet) 
  library(hdm)
  library(grf)
  library(boot)
}

# Загрузка данных
data <- read_excel("C:/Users/tixon/Desktop/МГУ/Прикладная эконометрика/Дз2/wage_gap.xlsx")

{#первый пункт
# Определяем группы для каждого n
data_1 <- data %>%
  group_by(n) %>%
  mutate(
    group = case_when(
      SMSA_central[t == 1979] == 1 & SMSA_central[t == 1994] == 1 ~ "Всегда в central",
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 0 ~ "Никогда в central",
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 1 ~ "Переехал в central",
      SMSA_central[t == 1979] == 1 & SMSA_central[t == 1994] == 0 ~ "Уехал из central",
      TRUE ~ NA_character_  # Присваиваем NA, если условия не выполняются
    )
  )

# Список показателей
variables <- c("fam_size", "education", 'AFQT2', 'HGT_father', 'HGT_mother', 
               "self_conf", "size_of_firm", "risk")
#удаляем N/A
out_all <- data_1 %>%
  filter(if_any(all_of(variables), is.na)) %>%  
  distinct(n) %>%                               
  pull(n)
data_dropna <- data_1 %>%
  filter(!n %in% out_all)
#Cчитаем оставшихся людей
num_unique_dropna <- data_dropna %>% 
  summarise(Unique_N = n_distinct(n)) %>% 
  pull(Unique_N)
num_unique_dropna #1176 их оставим  
data_1 <- data_dropna

# Функция для выполнения t-теста и сохранения результатов
perform_t_tests <- function(data, var) {
  groups <- unique(data$group)
  results <- data.frame(Group1 = character(), Group2 = character(), p_value = numeric(), stringsAsFactors = FALSE)

  # Выполняем t-тесты для каждой пары групп
  for (i in 1:(length(groups) - 1)) {
    for (j in (i + 1):length(groups)) {
      group1 <- groups[i]
      group2 <- groups[j]
      
      t_test_result <- t.test(data[[var]][data$group == group1], 
                              data[[var]][data$group == group2])
      results <- rbind(results, data.frame(Group1 = group1, Group2 = group2, p_value = t_test_result$p.value))
    }
  }
  
  # Коррекция p-значений методом Бонферрони
  results$adj_p_value <- p.adjust(results$p_value, method = "holm")
  results$p_value <- NULL
  results$Balance <-  ifelse(results$adj_p_value < 0.05, "No", "Yes")
  
  return(results)
}


# Создание таблиц с результатами по каждому показателю
final_results <- list()
for (var in variables) {
  test_results <- perform_t_tests(data_1, var)
  final_results[[var]] <- test_results
}

# Вывод результатов
for (var in variables) {
  cat(paste("Results for:", var), "\n")
  print(final_results[[var]])
  cat("\n")
}


bal.tab(group ~ fam_size+education+AFQT2+HGT_father+HGT_mother+self_conf+size_of_firm+risk, 
        data = data_1,
        estimand = "ATE",
        m.threshold = .1)
}

{#второй пункт

# добавляем Treatment
data_2 <- data %>%
  group_by(n) %>%
  mutate(
    group = case_when(
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 0 ~ "Control",
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 1 ~ "Treatment",
      TRUE ~ NA_character_  # Присваиваем NA, если условия не выполняются
    )
  )

# Создание новой таблицы с нужными для нас данными
data_2 <- data_2 %>%
  # Группируем по n
  group_by(n) %>%
  # И затем для каждого группы выбираем нужные строки
  summarize(
    cpi_w = cpi_w[t == 1994],   # Получаем данные для cpi_w с 1994 года
    across(everything(), ~ .[t == 1979], .names = "{.col}") # Получаем данные для остальных переменных с 1979 года
  ) %>%
  ungroup()

data_2 <- data_2 %>%
  filter(!is.na(cpi_w))%>% 
  filter(!is.na(group))%>% 
  mutate(Treatment = ifelse(group == "Treatment", 1, 0))
count_control <- sum(data_2$Treatment == "0")
count_treatment <- sum(data_2$Treatment == "1")


model_1 <- lm(cpi_w ~ Treatment, data = data_2)
summary(model_1)
#оценка эффекта - оценка Treatment
}

{#третий пункт
#удаляем N/A по ковариатам
out_all <- data_2 %>%
  filter(if_any(all_of(variables), is.na)) %>%  
  distinct(n) %>%                               
  pull(n)
data_3 <- data_2 %>%
  filter(!n %in% out_all)
#ищем ближайшего соседа
match_1 <- matchit(Treatment ~ fam_size + AFQT2 + education + HGT_mother + 
                            HGT_father + self_conf + size_of_firm + risk,
                          data = data_3, method = "nearest", estimand = "ATT")
summary(match_1) #осталось по 43 человека в обеих группах, у них веса = 1, у остальных = 0
model_2 <- lm(cpi_w ~ Treatment + fam_size + AFQT2 + education + HGT_mother + 
                HGT_father + self_conf + size_of_firm + risk
              , data = data_3, weights = match_1$weight)
summary(model_2)
#показал, что эффекта от Тритмента нет
}

{#четвертый пункт

bal.tab(group ~ fam_size+education+AFQT2+HGT_father+HGT_mother+self_conf+size_of_firm+risk, 
        data = data_3, weights = match_1$weight, 
        estimand = "ATT", m.threshold = .1)
}

{#пятый пункт
ipw_1 <- weightit(Treatment ~ fam_size + AFQT2 + education + HGT_mother + 
                        HGT_father + self_conf + size_of_firm + risk,
                      data = data_3, estimand = "ATE", method = "ps")
summary(ipw_1) 
model_3 <- lm(cpi_w ~ Treatment + fam_size + AFQT2 + education + HGT_mother + 
                HGT_father + self_conf + size_of_firm + risk, 
              data = data_3, weights = ipw_1$weight)
summary(model_3)
}

{#шестой пункт

# добавляем Treatment
data_T <- data %>%
  group_by(n) %>%
  mutate(
    group = case_when(
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 0 ~ "0",
      SMSA_central[t == 1979] == 0 & SMSA_central[t == 1994] == 1 ~ "1",
      TRUE ~ NA_character_  # Присваиваем NA, если условия не выполняются
    )
  )
  
data_DRLasso <- data_T %>%
  # Группируем по n
  group_by(n) %>%
  # И затем для каждого группы выбираем нужные строки
  summarize(
    cpi_w = cpi_w[t == 1994],   # Получаем данные для cpi_w с 1994 года
    across(everything(), ~ .[t == 1979], .names = "{.col}") # Получаем данные для остальных переменных с 1979 года
  ) %>%
  ungroup() #1721 человек

variables_DR <- c("fam_size", "education", 'AFQT2', 'HGT_father', 'HGT_mother', 
                  "self_conf", "size_of_firm", "risk", "group", "cpi_w")

out_all <- data_DRLasso %>%
  filter(if_any(all_of(variables_DR), is.na)) %>%  
  distinct(n) %>%                               
  pull(n)
data_DRL <- data_DRLasso %>%
  filter(!n %in% out_all) #здесь 850 людей

formula2 <- cpi_w ~ fam_size + education + AFQT2 + HGT_father + HGT_mother + 
  self_conf + size_of_firm + risk # формула для регресии

X_1 <- model.matrix(data=data_DRL, formula2) # матрица для оценки модели
Y <- c(data_DRL$cpi_w)
Tmnt <- c(data_DRL$group)

model_DR <- rlassoEffect(X_1, # матрица иксов
                         Y, # зависимая переменная
                         Tmnt, # тритмент
                         method = 'partialling out') # по умолчанию 'partialling out', но можно и 'double selection'

summary(model_DR)
model_DR$selection.index
}

{#седьмой пункт
B <- 1000 # число бутстраповских выборок

# пишем функцию, которая выбирает данные
v  <- function(data, indeces){
  boot_sample <- data[sample(seq(1:nrow(data)), replace=TRUE),]
  boot_model <- lm(cpi_w ~ Treatment + fam_size + AFQT2 + education + HGT_mother + 
                     HGT_father + self_conf + size_of_firm + risk, 
                   data = boot_sample, weights = ipw_1$weight)
  return(coef(boot_model)[2])
}
       
# засовываем эту функцию в boot
results<- boot(data = data_3, 
               statistic = v ,
               R = B)
results$t # все бутстраповские оценки, 1000 штук
boot.ci(results, type="perc") # доверительный интервал

# Извлекаем бутстрапированные оценки
boot_estimates <- results$t

# Рассчитываем доверительный интервал
ci <- boot.ci(results, type="perc")

# Создаем датафрейм для ggplot
data_plot <- data.frame(Estimate = boot_estimates)

# Визуализируем
ggplot(data_plot, aes(x = Estimate)) +
  geom_histogram(bins = 30, aes(y = ..density..), alpha = 0.6, fill = "lightblue") +
  geom_vline(aes(xintercept = ci$percent[4]), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = ci$percent[5]), color = "red", linetype = "dashed") +
  labs(title = "Бутстрапированные оценки и доверительный интервал",
       x = "Оценки",
       y = "Плотность") +
  theme_minimal() +
  geom_density(alpha = 0.2, fill = "lightgreen")}
