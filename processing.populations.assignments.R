library(tidyverse)

# Загружаем данные из файла
data <- read.csv("test.data.csv", header = TRUE, stringsAsFactors = FALSE)

# Обновленная функция для обработки строки данных, исключая слово "Missing"
process_row <- function(row) {
  # Обработка текста, удаление "NA", "Missing" и замена ";" на " "
  text <- row %>%
    na.omit() %>%
    .[-c(1:2)] %>%
    paste(collapse = " ") %>%
    str_replace_all("NA", "") %>%
    str_replace_all("Missing", "") %>%
    str_replace_all(";", " ") %>%
    gsub("\\s+", " ", .)
  
  # Разбиваем на слова
  words <- unlist(strsplit(text, " "))
  # Удаляем пустые слова
  words <- words[words != ""]
  
  # Подсчёт частоты слов
  word_freq <- table(words)
  
  # Фильтруем слова, встречающиеся минимум 5 раз
  filtered_freq <- word_freq[word_freq >= 5]
  
  if (length(filtered_freq) == 0) {
    # Если нет слов, встречающихся 5 и более раз, возвращаем первое слово списка с процентом 100%
    first_word <- if(length(words) > 0) words[1] else "Недостаточно слов"
    return(list(first_word, "100%"))
  } else {
    # Сортируем и выбираем топ-4
    top_words <- sort(filtered_freq, decreasing = TRUE)[1:min(4, length(filtered_freq))]
    
    # Нормализуем частоты, чтобы сумма была 100%
    top_words_percent <- prop.table(top_words) * 100
    
    # Создаем список с парами "слово-процент"
    result_list <- map2(names(top_words_percent), top_words_percent, ~c(.x, paste(round(.y, 2), "%")))
    return(result_list)
  }
}

# Применяем функцию ко всем строкам и выводим результат
results <- apply(data, 1, process_row)

results_to_df <- function(results) {
  map_dfr(results, ~{
    # Разбиваем пары "слово-процент" на компоненты
    words_percents <- map(.x, ~str_split(.x, ",\\s*") %>% unlist())
    # Обработка случаев без подходящих слов
    if (length(words_percents) == 1 && startsWith(words_percents[[1]][1], "Нет слов")) {
      return(setNames(as_tibble(matrix(NA, ncol = 8)), c("Pop1", "Percent1", "Pop2", "Percent2", "Pop3", "Percent3", "Pop4", "Percent4")))
    }
    # Создаем вектор для заполнения датафрейма
    df_vector <- rep(NA, 8)
    vec_pos <- 1
    for (i in seq_along(words_percents)) {
      df_vector[vec_pos] <- words_percents[[i]][1] # Слово
      if (length(words_percents[[i]]) > 1) {
        df_vector[vec_pos + 1] <- words_percents[[i]][2] # Процент
      }
      vec_pos <- vec_pos + 2
    }
    # Преобразуем вектор в датафрейм
    return(setNames(as_tibble(matrix(df_vector, ncol = 8, byrow = TRUE)), c("Pop1", "Percent1", "Pop2", "Percent2", "Pop3", "Percent3", "Pop4", "Percent4")))
  })
}
# Преобразуем результаты в нужный формат
results_df <- results_to_df(results)
write_xlsx(results_df, "consensus.results.xlsx")