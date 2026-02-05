
dive_db <- function(data) {
  pivot_longer(data, -1, names_to = "freq", values_to = "db") %>% 
    mutate(freq = parse_number(freq)) %>% 
    filter(between(freq, 1000, 3500)) %>% 
    mutate(pressure = 10^(db / 10)) %>% 
    group_by(Time) %>% 
    summarize(pressure_1000_3500 = sum(pressure)) %>% 
    mutate(db_1000_3500 = 10 * log10(pressure_1000_3500))
  
}

dive_db_plot <- function(data) {
  ggplot(dive_db(data), aes(Time, db_1000_3500)) +
    geom_line() + 
    ylim(-72, -42) + 
    ggtitle(deparse(substitute(data)))
}
