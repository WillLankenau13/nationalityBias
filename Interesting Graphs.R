library(tidyverse)
library(modelr)


ggplot(data = combined) +
  geom_bar(mapping = aes(x = overall))

ggplot(data = combined) +
  geom_bar(mapping = aes(x = npx_g_expected))

ggplot(data = combined, mapping = aes(x = npx_g_expected, y = shooting)) +
  geom_point(mapping = aes(color = nationality_group), position = "jitter")

ggplot(data = combined, mapping = aes(x = gls_standard, y = shooting)) +
  geom_point(mapping = aes(color = nationality_group), position = "jitter")

ggplot(data = combined, mapping = aes(x = np_g_minus_x_g_expected/npx_g_expected, y = shooting)) +
  geom_point(mapping = aes(color = nationality_group), position = "jitter")

ggplot(data = combined, mapping = aes(x = age, y = shooting)) +
  geom_point(mapping = aes(color = nationality_group), position = "jitter")

ggplot(data = combined, mapping = aes(x = dist_standard, y = shooting)) +
  geom_point(mapping = aes(color = pos), position = "jitter")
