library(tidyverse)
library(infer) 
library(ranger)

combined <- read_csv("~/R Stuff/nationalityBias/combined.csv") %>% 
  select(-1) #%>% 
  #filter(nationality_group == "Tier2" | nationality_group == "England")



two_nationality_groups <- c('Rest', 'England')
combined <- combined %>% 
  mutate(
    ## I dislike having calculations in the model formula :)
    np_goals = goals - pk_standard,
    log_np_goals = log(np_goals),
    log_np_goals = ifelse(is.infinite(log_np_goals), 0, log_np_goals),
    nationality_group2 = ifelse(
      nationality_group %in% c('England'), 
      two_nationality_groups[2],
      two_nationality_groups[1]
    ) %>% 
      factor(),
    ## can't have NAs
    dist_standard = ifelse(
      is.na(dist_standard),
      median(dist_standard, na.rm = TRUE), 
      dist_standard
    )
  )

counts <- combined %>% 
  count(nationality_group2, shooting)

counts_to_match <- counts %>% 
  ## convert implicit missingness to an explicit zero
  complete(nationality_group2, shooting, fill = list(n = 0L)) %>% 
  ## identify which group has the smaller count per shooting value
  group_by(shooting) %>% 
  slice_min(n, n = 1) %>% 
  ungroup() %>% 
  ## drop shooting values for which one group has a zero count (i.e. downsampling to zero)
  filter(n > 0L) %>% 
  select(shooting, n) %>% 
  crossing(
    nationality_group2 = two_nationality_groups
  )


matched_combined <- counts_to_match %>% 
  mutate(
    data = pmap(
      list(n, shooting, nationality_group2),
      ~{
        combined %>% 
          dplyr::filter(shooting == ..2, nationality_group2 == ..3) %>% 
          dplyr::slice_sample(n = ..1, replace = TRUE)
      }
    )
  ) %>% 
  select(data) %>% 
  unnest(data)

both_combined <- bind_rows(
  combined %>% mutate(source = "original"),
  matched_combined %>% mutate(source = "matched")
)

both_combined %>% 
  ggplot() +
  aes(x = nationality_group2, y = shooting) +
  geom_boxplot() +
  facet_wrap(~source)

both_combined %>% 
  count(source, nationality_group2, primary_position) %>% 
  ggplot() +
  aes(x = n, y = primary_position) +
  geom_col(aes(fill = nationality_group2), position = 'dodge') +
  facet_wrap(~source) +
  theme(legend.position = 'top')




lm_fifa_fit <- lm(
  shooting ~ log_np_goals + primary_position + squad + dist_standard + sh_per_game + age + season,
  data = matched_combined
)

lm_matched_preds <- tibble(
  shooting = matched_combined$shooting,
  pred = predict(lm_fifa_fit, matched_combined)
) %>% 
  mutate(
    resid = shooting - pred
  )


lm_matched_preds %>% 
  summarize(
    r2 = 1 - sum(resid^2) / sum( (shooting - mean(shooting) )^2 ),
    rmse = sqrt(mean(resid^2, na.rm = TRUE))
  )

fifa_fit <- ranger(
  shooting ~ np_goals + primary_position + squad + dist_standard + sh_per_game + age + season,
  data = matched_combined
)


matched_preds <- matched_combined %>% 
  mutate(
    pred = predict(fifa_fit, matched_combined)$predictions,
    resid = shooting - pred
  )

matched_preds %>% 
  summarize(
    r2 = 1 - sum(resid^2) / sum( (shooting - mean(shooting) )^2 ),
    rmse = sqrt(mean(resid^2, na.rm = TRUE))
  )

matched_preds %>% 
  ggplot() +
  aes(x = nationality_group2, y = resid) +
  geom_boxplot()

matched_preds %>%
  t_test(
    formula = resid ~ nationality_group2,
    order = two_nationality_groups,
    alternative = 'two-sided'
  )


resample_t_test <- function() {
  matched_combined <- counts_to_match |> 
    mutate(
      data = pmap(
        list(n, shooting, nationality_group2),
        ~{
          combined |> 
            filter(shooting == ..2, nationality_group2 == ..3) |> 
            slice_sample(n = ..1, replace = TRUE)
        }
      )
    ) |> 
    select(data) |> 
    unnest(data)
  
  fifa_fit <- ranger(
    shooting ~ np_goals + primary_position + squad + dist_standard + sh_per_game + age + season,
    data = matched_combined
  )
  
  matched_preds <- matched_combined |> 
    mutate(
      pred = predict(fifa_fit, matched_combined)$predictions,
      resid = shooting - pred
    )
  
  matched_preds |>
    t_test(
      formula = resid ~ nationality_group2,
      order = two_nationality_groups,
      alternative = 'two-sided'
    )
  

}

resampled_t_tests <- rerun(
  10,
  resample_t_test()
) |> 
  bind_rows() |> 
  mutate(
    i = row_number(),
    .before = 1
  )

resampled_t_tests |> 
  ggplot() +
  aes(x = estimate) +
  geom_histogram(bins = 30)

write.csv(resampled_t_tests, "~/R Stuff/nationalityBias/resampled_t_tests/testing/Resr_England")

