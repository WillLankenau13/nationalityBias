library(tidyverse)

adj_r_squared <- data.frame(intercept = "test",
                            r_squared = 0)

write.csv(adj_r_squared, "adj_r_squared.csv")
