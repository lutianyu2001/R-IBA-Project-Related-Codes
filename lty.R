# Library for IBA Group Assignment

# Programmer:  Lu Tianyu (Sky)
# Student ID: 1930026092
# Date:2021/11/16

# How to use:
  #source("./lty.R")

# Example for discrete variable
# Bar Plot
  #save_dens_plot_dis(projects %>% select(funding_status))
# Lollipop Chart
  #save_lollipop_chart_dis(donors %>% select(state))

# Example for continuous variable
  #save_dens_plot_cont(projects %>% select(total_matched_base))

library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
options(max.print=20)

dir.create("pictures")

#projects <- read_csv("projects.csv")
#show(projects)
#donors <- read_csv("donors.csv")
#show(donors)

get_xlim <- function(data){
  # Convert tibble into list to avoid bug in tidyverse
  data_df <- as.data.frame(data)
  xlim_box <- boxplot.stats(data_df[,1])

  xlim_box <- xlim_box$stats
  xlim_left <- xlim_box[1]
  xlim_right <- xlim_box[5]

  return(c(xlim_left,xlim_right))
}

save_dens_plot_con <- function(data){ # for continuous variable
  plot1 <- data %>%
    ggplot(aes(x = get(colnames(data)))) +
    xlab(colnames(data)) +
    geom_density(fill = "#4D9DDA", alpha = 0.64) +
    ggtitle(paste("Density Plot for ",colnames(data), sep='')) +
    xlim(get_xlim(data))

  show(plot1)
  ggsave(paste("./pictures/",colnames(data),".png", sep = ''))
}

get_freq_data <- function(data){
  freq_data <- data %>% rename(cat_variable = colnames(data)) %>%
    count(cat_variable) %>% mutate(freq = n/sum(n))
  return(freq_data)
}

save_freq_plot_dis <- function(data){ # for discrete variables
  freq_data <- get_freq_data(data)
  plot1 <- freq_data %>%
    ggplot(aes(x = cat_variable, y = freq)) +
    xlab(colnames(data)) + ylab("frequency") +
    geom_bar(stat = "identity", fill = "#4D9DDA", alpha = 0.72) +
    geom_text(aes(label = round(freq, digit = 4)),
              size = 4, position = position_stack(vjust = 1)) +
    ggtitle(paste("Frequency Bar Plot for ",colnames(data), sep=''))

  show(plot1)
  ggsave(paste("./pictures/",colnames(data),".png", sep = ''))
}

save_lollipop_chart_dis <- function(data, cut = 0){ # for discrete variables
  freq_data <- get_freq_data(data)
  freq_data <- freq_data %>% filter(freq >= cut)
  plot_title_cut <- if(cut == 0) "" else paste(" (frequency >= ",cut,")",
                                               sep='')
  show(plot_title_cut)
  plot1 <- freq_data %>% ggdotchart(x = "cat_variable", y = "freq",
          add = "segments",
          add.params = list(color = "Grey48", size = 1),
          dot.size = 2,
          ggtheme = theme_bw(),
          title = paste("Frequency Lolipop Chart for ",colnames(data),
                        plot_title_cut, sep=''),
          xlab = colnames(data), ylab = "frequency")

  show(plot1)
  ggsave(paste("./pictures/",colnames(data),".png", sep = ''))
}

# abandoned since it can be replaced with lollipop chart
save_pie_plot_dis <- function(data){
  freq_data <- get_freq_data(data)
  plot1 <- freq_data %>% ggplot(aes(x = "", y = freq, fill = cat_variable)) +
    geom_bar(width = 1, stat = "identity", color = "white") +
    geom_text(aes(y = cumsum(freq) - 0.5*freq,
                  label = round(freq, digit = 2)), color = "white") +
    coord_polar("y", start = 0)

  show(plot1)
  ggsave(paste("./pictures/",colnames(data),".png", sep = ''))
}
