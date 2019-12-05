library(tidyverse)

a <- readLines(pipe("journalctl | cut -c1-15"))
a <- a[!grepl("^\\s*$", a)]
a <- tibble(a = a) %>%
  extract(a, c("month", "day", "hour", "min", "sec"),
          regex = c("^(.*) (.*) (.*):(.*):(.*)$")) %>%
  na.omit() %>%
  mutate(month = c("Jun"=6,"Jul"=7,"Aug"=8,"Sep"=9,"Oct"=10,"Nov"=11,"Dec"=12)[month],
         day   = day %>% as.integer,
         hour  = hour %>% as.integer,
         min   = min %>% as.integer,
         date  = sprintf("%02d-%02d", month, day),
         time  = (24 * 60) - (hour * 60 + min)) %>%
  count(date, time)
a %>% write_tsv("alive-time.txt")

g <- a %>%
  ggplot(aes(x = date, y = time, fill = (n > 0))) +
  geom_tile() +
  scale_y_continuous(breaks = (0:24) * 60, labels = sprintf("%02d:00", 24:0)) +
  guides(fill = FALSE) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
g %>% ggsave(filename = "alive-time.png", width = 16, height = 8)
