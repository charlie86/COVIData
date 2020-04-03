covid_counties %>% 
    group_by(fips) %>% 
    filter(max(cases) >= 50) %>% 
    arrange(date) %>% 
    mutate(new_cases = cases - lag(cases),
           new_cases_rolling = slide_dbl(new_cases, sum, .before = 7, .complete = F),
           log_new_cases = log(new_cases_rolling),
           log_cases = log(cases)) %>% 
    filter(state == 'New York') %>% 
    ggplot(aes(x = log_cases, y = log_new_cases, color = county)) +
    geom_line()


covid_states %>% 
    group_by(fips) %>% 
    filter(max(cases) >= 50) %>% 
    arrange(date) %>% 
    mutate(new_cases = cases - lag(cases),
           new_cases_rolling = slide_dbl(new_cases, sum, .before = 7, .complete = F),
           log_new_cases = log(new_cases_rolling),
           log_cases = log(cases)) %>% 
    # filter(state == 'Washington') %>% 
    ggplot(aes(x = log_cases, y = log_new_cases, color = state)) +
    geom_line()


head(iris)

iris$tmp <- ifelse(iris$Species == 'setosa', iris$Sepal.Length / 2, iris$Sepal.Length)
iris <- iris %>% 
    mutate(tmp2 = case_when(Species == 'setosa' ~ Sepal.Length / 2, TRUE ~ Sepal.Length))

