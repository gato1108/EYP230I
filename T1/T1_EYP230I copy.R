library(readxl)
library(dplyr)
library(ggplot2)
library(GGally) 
library(fastDummies)
library(car)


################
## Leer Datos ##
################
data <- read_excel("/Users/diego/U/S8/Regresión/T1/Tarea_1_2025_02.xlsx")


###################
## Generar Split ##
###################
set.seed(69420) 
n <- nrow(data)
idx <- sample(1:n, size = 0.8 * n, replace = FALSE)
train <- data[idx, ]
test  <- data[-idx, ]


######################
## Agregar Columnas ##
######################

train <- dummy_cols(train, select_columns = "hr", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
train <- dummy_cols(train, select_columns = "mnth", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
train <- dummy_cols(train, select_columns = "season", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
train <- dummy_cols(train, select_columns = "weathersit", remove_first_dummy = TRUE, remove_selected_columns = FALSE)

#train <- train %>% mutate(hr_peak = ifelse(hr %in% c(17, 18, 19), 1, 0))
#train <- train %>% mutate(hr_baja = ifelse(hr %in% c(0, 1, 2, 3, 4, 5, 6), 1, 0))
#train <- train %>% mutate(hr_mann = ifelse(hr %in% c(7, 8, 9, 10, 11), 1, 0))
train <- train %>% mutate(hr_almu = ifelse(hr %in% c(12, 13, 14, 15, 7, 9 ,20, 11), 1, 0))
#train <- train %>% mutate(season_1 = ifelse(season %in% c(1), 1, 0))
train <- train %>% mutate(summer = ifelse(mnth %in% c('Jan', 'July', 'June', 'May', 'Sept'), 1, 0))
train <- train %>% mutate(winter = ifelse(mnth %in% c('Jan', 'Feb', 'March'), 1, 0))
train <- train %>% mutate(hr_peak = ifelse(hr %in% c(17, 18), 1, 0))
train <- train %>% mutate(hr_baja = ifelse(hr %in% c(0, 1, 2, 3, 4, 5), 1, 0))
#test <- test %>% mutate(hr_mann = ifelse(hr %in% c(7, 8, 9, 10, 11), 1, 0))
#test <- test %>% mutate(hr_almu = ifelse(hr %in% c(12, 13, 14, 15), 1, 0))


#test <- test %>% mutate(season_1 = ifelse(season %in% c(1), 1, 0))
#test <- test %>% mutate(snow = ifelse(weathersit %in% c('light rain/snow'), 1, 0))
#test <- test %>% mutate(summer = ifelse(mnth %in% c('Aug', 'July', 'June', 'May', 'Oct', 'Sept'), 1, 0))


train <- train %>% select(-casual, -registered)

############################
## Gráficos de frecuencia ##
############################

mean_by_col <- train %>% group_by(hr) %>% summarise(mean_bikers = mean(bikers, na.rm = TRUE)) 
ggplot(mean_by_col, aes(x = factor(hr), y = mean_bikers)) + geom_col(fill = "blue")


#############
## Modelos ##
#############
mod1 <- lm(bikers ~ ., data = train)
summary(mod1)

# temp aporta individual mas que atemp
mod2 <- lm(bikers ~ workingday, data = train)
summary(mod2)

mod3 <- lm(bikers ~ temp +hum + season + winter + hr_baja+ hr_8 + hr_peak + hr_almu , data = train)
summary(mod3)

mod4 <- lm(bikers ~ temp + hum + season + winter + hr_peak + hr_baja + hr_8 + hr_19 , data = train)
summary(mod4)$adj.r.squared

mod5 <- lm(bikers ~ temp + hr + hum + season + winter , data = train)
summary(mod4)

#hum, winter hace que aumente poco
mod6 <- lm(bikers ~  atemp + hum + season + winter + hr_peak + hr_baja + hr_8 + hr_19, data = train)
summary(mod6)$adj.r.squared
vif(mod6)
#mod 6 se ve bueno

train_cuan <- train %>% select(-mnth, -weathersit)
r2_results <- sapply(names(train_cuan)[names(train_cuan) != "bikers"], function(var) {
  f <- reformulate(var, response = "bikers")   
  model <- lm(f, data = train_cuan)                 
  summary(model)$r.squared                     
})

# Convertir a data.frame ordenado
r2_results <- sort(r2_results, decreasing = TRUE)
r2_df <- data.frame(Variable = names(r2_results), R2 = r2_results)

print(r2_df)

################
## Predicción ##
################

test <- dummy_cols(test, select_columns = "hr", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
test <- dummy_cols(test, select_columns = "mnth", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
test <- dummy_cols(test, select_columns = "season", remove_first_dummy = TRUE, remove_selected_columns = FALSE)
test <- dummy_cols(test, select_columns = "weathersit", remove_first_dummy = TRUE, remove_selected_columns = FALSE)

test <- test %>% mutate(hr_almu = ifelse(hr %in% c(12, 13, 14, 15, 7, 9 ,20, 11), 1, 0))
test <- test %>% mutate(summer = ifelse(mnth %in% c('Jan', 'July', 'June', 'May', 'Sept'), 1, 0))
test <- test %>% mutate(winter = ifelse(mnth %in% c('Jan', 'Feb', 'March'), 1, 0))
test <- test %>% mutate(hr_peak = ifelse(hr %in% c(17, 18), 1, 0))
test <- test %>% mutate(hr_baja = ifelse(hr %in% c(0, 1, 2, 3, 4, 5), 1, 0))

test <- test %>% select(-casual, -registered)

predictions <- predict(mod6, newdata = test)
print(predictions)

error <- predictions-test$bikers

ggplot(predictions-test$bikers, aes(x = cut, y = counts))
summary(predictions-test$bikers)

hist(error,
     main = "Distribución de errores de predicción",
     xlab = "Error",
     ylab = "Frecuencia",
     col = "skyblue", border = "white", breaks = 20)
