# load libraries
library(tidyverse)
library(scales)
library(ggpubr)
read.csv(file = paste0('data/sockeye_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> sockeye

# historical dataset----
model <- lm(escapement ~ peak_live_count-1, data = sockeye)
peak_live_count <- c(5430,
                     7770,
                     13900,
                     23250,
                     11763,
                     3170,
                     8965,
                     14495,
                     3220,
                     4945, 
                     2260,
                     4985, 
                     1700)
newdata <- data.frame(peak_live_count)
predicted<-predict(model, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
fit_value <- predicted$fit
lwr_pi <- predicted$lwr
upr_pi <- predicted$upr
pred <- cbind(predicted, newdata)
pred %>%
  rename(escapement = fit,
         lwr_pii = lwr,
         upr_pii = upr) -> pred
predicted<-predict(model, newdata, interval="confidence", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
predicted %>%
  rename(lwr_cii = lwr,
         upr_cii = upr) -> predicted
pred <- cbind(predicted, pred)
#http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/#prediction-interval-or-confidence-interval

summary(model)
pred.int <- predict(model, interval = "prediction", level = 0.80)
pred.int %>%
  as.data.frame()%>%
  mutate(fit = fit,
         lwr_pi =  lwr,
         upr_pi =  upr) %>%
  dplyr::select(fit, lwr_pi, upr_pi)-> pi
pred.conf <- predict(model, interval = "confidence", level = 0.80)
pred.conf %>%
  as.data.frame()%>%
  mutate(fit = fit,
         lwr_ci =  lwr,
         upr_ci =  upr) %>%
  dplyr::select(lwr_ci, upr_ci)-> ci
formula <- y ~ x -1 # Regression with no intercept
sockeye %>% 
  cbind(., pi) %>%
  cbind(., ci)  %>%
  ggplot(aes(x = peak_live_count, y = escapement)) +
  geom_point(color ="grey50", size = 2) +
  geom_point(data = pred, color ="black", size = 2, pch=1) + 
  scale_x_continuous(labels = comma,breaks = seq(0, 30000, 5000), limits = c(0, 30000)) +
  scale_y_continuous(labels = comma,breaks = seq(-25000, 225000, 25000), limits = c(-25000, 225000)) +
  ggtitle(label = "McDonald Lake sockeye salmon", subtitle = "Regression through the origin") + 
  geom_errorbar(data = pred, ymax = pred$upr_pii, ymin = pred$lwr_pii, width =0.5, color = "grey50") +
  geom_errorbar(data = pred, ymax = pred$upr_cii, ymin = pred$lwr_cii, width =0.5, color = "blue", size=1) +
  geom_smooth(method = "lm", 
              orientation = "y", 
              formula = y ~ x -1, 
              color= "black", level = 0.8) + 
  labs(y = "Escapement", x =  "Peak live counts") +   geom_hline(yintercept = 0, lty=2, color = "grey80") +
  stat_regline_equation(
    aes(label =  paste(..eq.label..,..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.x =0, label.y = 225000) +
  geom_line(aes(y = lwr_pi), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr_pi), color = "red", linetype = "dashed") +
  geom_line(aes(y = lwr_ci), color = "blue", linetype = "dashed")+
  geom_line(aes(y = upr_ci), color = "blue", linetype = "dashed") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) -> plot1

# log model
read.csv(file = paste0('data/sockeye_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> sockeye
model <- lm(log(escapement) ~ log(peak_live_count)-1, data = sockeye)
peak_live_count <- c(5430,
                     7770,
                     13900,
                     23250,
                     11763,
                     3170,
                     8965,
                     14495,
                     3220,
                     4945, 
                     2260,
                     4985, 
                     1700)
newdata <- data.frame(peak_live_count)
newdata %>%
  as.data.frame() %>%
  mutate (peak_live_count_log = log(peak_live_count)) -> newdata
predicted<-predict(model, newdata, interval="prediction", level = 0.80) #prediction interval
sigma <- sigma(model)
predicted <- as.data.frame(predicted)
predicted %>%
  mutate(escapement = exp(predicted$fit)*exp(0.5*sigma*sigma),
         lwr_pii =  exp(predicted$lwr)*exp(0.5*sigma*sigma),
         upr_pii = exp(predicted$upr)*exp(0.5*sigma*sigma))%>%
  dplyr::select(escapement, lwr_pii, upr_pii) -> predicted
pred <- cbind(predicted, newdata)

conf<-predict(model, newdata, interval="confidence", level = 0.80) #prediction interval
sigma <- sigma(model)
conf <- as.data.frame(conf)
conf %>%
  mutate(lwr_cii =  exp(lwr)*exp(0.5*sigma*sigma),
         upr_cii = exp(upr)*exp(0.5*sigma*sigma)) %>%
  dplyr::select(lwr_cii, upr_cii) %>%
  cbind(., pred) -> pred
#http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/#prediction-interval-or-confidence-interval
summary(model)
pred.int <- predict(model, interval = "prediction", level = 0.80)
pred.int %>%
  as.data.frame()%>%
  mutate(fit = exp(fit)*exp(0.5*sigma*sigma),
         lwr_pi =  exp(lwr)*exp(0.5*sigma*sigma),
         upr_pi =  exp(upr)*exp(0.5*sigma*sigma) ) %>%
  dplyr::select(fit, lwr_pi, upr_pi)-> pi
pred.conf <- predict(model, interval = "confidence", level = 0.80)
pred.conf %>%
  as.data.frame()%>%
mutate(fit = exp(fit)*exp(0.5*sigma*sigma),
       lwr_ci =  exp(lwr)*exp(0.5*sigma*sigma),
       upr_ci =  exp(upr)*exp(0.5*sigma*sigma) ) %>%
   dplyr::select(lwr_ci, upr_ci)-> ci
formula <-log(y) ~ log(x) -1 # Regression with no intercept
sockeye %>% 
  cbind(., pi) %>%
  cbind(., ci)  %>%
  ggplot(aes(x = peak_live_count, y = escapement)) +
  geom_point(color ="grey50", size = 2) +
  geom_point(data = pred, color ="black", size = 2, pch=1) + 
  scale_x_continuous(labels = comma,breaks = seq(0, 30000, 5000), limits = c(0, 30000)) +
  scale_y_continuous(labels = comma,breaks = seq(-25000, 225000, 25000), limits = c(-25000, 225000)) +
  ggtitle(label = "McDonald Lake sockeye salmon", subtitle = "Log regression through the origin") + 
  geom_errorbar(data = pred, ymax = pred$upr_pii, ymin = pred$lwr_pii, width =0.5, color = "grey50") +
  geom_errorbar(data = pred, ymax = pred$upr_cii, ymin = pred$lwr_cii, width =5, color = "blue", size =1) +
  labs(y = "Escapement", x =  "Peak live counts") +   geom_hline(yintercept = 0, lty=2, color = "grey80") +
  stat_regline_equation(
    aes(label =  paste(..eq.label..,..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.x =0, label.y = 225000) +
  geom_line(aes(y = lwr_pi), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr_pi), color = "red", linetype = "dashed") +
  geom_line(aes(y = lwr_ci), color = "blue", linetype = "dashed")+
  geom_line(aes(y = upr_ci), color = "blue", linetype = "dashed") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) -> plot2

# log model (in log space)
read.csv(file = paste0('data/sockeye_data.csv'), header=TRUE, as.is=TRUE, strip.white=TRUE) -> sockeye
sockeye %>%
  mutate (peak_live_count = log(peak_live_count),
          escapement = log(escapement)) -> sockeye
model <- lm(escapement ~ peak_live_count-1, data = sockeye)
peak_live_count <- c(5430,
                     7770,
                     13900,
                     23250,
                     11763,
                     3170,
                     8965,
                     14495,
                     3220,
                     4945, 
                     2260,
                     4985, 
                     1700)
newdata <- data.frame(peak_live_count)
newdata %>%
  as.data.frame() %>%
  mutate (peak_live_count = log(peak_live_count)) -> newdata
predicted<-predict(model, newdata, interval="prediction", level = 0.80) #prediction interval
predicted <- as.data.frame(predicted)
pred <- cbind(predicted, newdata)
pred %>%
  mutate(escapement = fit,
         lwr_pii =  lwr,
         upr_pii = upr) %>%
dplyr::select(escapement, lwr_pii, upr_pii, peak_live_count)-> pred
conf<-predict(model, newdata, interval="confidence", level = 0.80) #prediction interval
conf <- as.data.frame(conf)
conf %>%
  mutate(lwr_cii =  lwr,
         upr_cii = upr) %>%
  dplyr::select(lwr_cii, upr_cii) %>%
  cbind(., pred) -> pred
#http://www.sthda.com/english/articles/40-regression-analysis/166-predict-in-r-model-predictions-and-confidence-intervals/#prediction-interval-or-confidence-interval
summary(model)
pred.int <- predict(model, interval = "prediction", level = 0.80)
pred.int %>%
  as.data.frame()%>%
  mutate(lwr_pi =  lwr,
         upr_pi =  upr) %>%
  dplyr::select(fit, lwr_pi, upr_pi)-> pi
pred.conf <- predict(model, interval = "confidence", level = 0.80)
pred.conf %>%
  as.data.frame()%>%
  mutate(lwr_ci =  lwr,
         upr_ci =  upr) %>%
  dplyr::select(lwr_ci, upr_ci)-> ci
formula <-y ~ x -1 # Regression with no intercept
sockeye %>% 
  cbind(., pi) %>%
  cbind(., ci)  %>%
  ggplot(aes(x = peak_live_count, y = escapement)) +
  geom_point(color ="grey50", size = 2) +
  geom_point(data = pred, color ="black", size = 2, pch=1) + 
  geom_smooth(method = "lm", 
              orientation = "y", 
              formula = y ~ x -1, 
              color= "black", level = 0.8) +
  scale_x_continuous(labels = comma,breaks = seq(7, 12, 1), limits = c(7, 12)) +
  scale_y_continuous(labels = comma,breaks = seq(7, 13, 1), limits = c(7, 13)) +
  ggtitle(label = "McDonald Lake sockeye salmon", subtitle = "Log regression through the origin (log space)") + 
  geom_errorbar(data = pred, ymax = pred$upr_pii, ymin = pred$lwr_pii, width =0.1, color = "grey50") +
  geom_errorbar(data = pred, ymax = pred$upr_cii, ymin = pred$lwr_cii, width =0.1, color = "blue") +
  labs(y = "Escapement", x =  "Peak live counts") +
  stat_regline_equation(
    aes(label =  paste(..eq.label..,..adj.rr.label.., sep = "~~~~")),
    formula = formula, label.x =7, label.y = 13) +
  geom_line(aes(y = lwr_pi), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr_pi), color = "red", linetype = "dashed") +
  geom_line(aes(y = lwr_ci), color = "blue", linetype = "dashed")+
  geom_line(aes(y = upr_ci), color = "blue", linetype = "dashed") +
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) -> plot3
cowplot::plot_grid(plot2,plot3, plot1,  align = "v", nrow = 2, ncol=2) 
ggsave(paste0("output/comparison.png"), dpi = 500, height = 8, width = 10, units = "in")