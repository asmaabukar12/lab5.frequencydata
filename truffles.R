#Step 1
truffles_data <- read_csv("https://whitlockschluter.zoology.ubc.ca/wp-content/data/chapter08/chap08q16Truffles.csv")
truffles_data

#Step 2
truffles_data <- 
  truffles_data %>% 
  rename(trufflesPerPlot = numberOfTrufflePerPlot)

#Step 3
ggplot(data = truffles_data) +
  geom_bar(mapping = aes(x = trufflesPerPlot), fill = "#a6192e") +
  labs(y = "Frequency", x = "Number of Truffles Per Plot")

#Step 4
truffles_data %>% 
  count(trufflesPerPlot, name = "observed_freq") %>% 
  print(n = Inf)

#Step 5
truffles_freq_table <-
 truffles_data %>% 
  count(trufflesPerPlot, name = "observed_freq") %>% 
  complete(
    trufflesPerPlot = 0:5, 
    fill = list(observed_freq = 0)
  ) %>% 
  print(n = Inf)


#Step 6 estimating the mean
mean_truffles <-
  truffles_data %>% 
  summarize(mean = mean(trufflesPerPlot)) %>% 
  pull(mean)
mean_truffles

#Step 7 expected frequencies
expected_proportions <- dpois(0:5, lambda = mean_truffles)
expected_proportions
---#Expected Proportions
truffles_freq_table <-
  truffles_freq_table %>% 
  mutate(
    expected_prop = expected_proportions, 
    expected_freq = expected_prop *126) %>% 
  print(n = Inf)

#Step 8 Graph from step 7
ggplot(data = truffles_freq_table,
       mapping = aes(x = trufflesPerPlot)) +
  geom_col(mapping = aes(y = observed_freq), fill = "#a6192e") +
  geom_line(mapping = aes(y = expected_freq), size = 2) +
  labs(y = "Frequency", x = "Number of Truffles")
  
chisq.test(x = truffles_freq_table,
           p = combined_freq_table$expected_freq / 126)
#Step 9
combined_freq_table <- 
  combined_freq_table %>% 
  mutate(trufflesPerPlot = fct_collapse(
    trufflesPerPlot,
    `0`  = "0",
    `1` = "1",
    `2` = "2",
    `3` = "3",
    `4` = "4",
   other_level = "1 or more",
  )) %>% 
  print() 
  
#Step 10  
missing_probabilities <- 128 - sum(combined_freq_table$expected_freq)
missing_probabilities  

#Step 11 adding factors
combined_freq_table %>% 
  add_row(
    trufflesPerPlot = factor("1 or more"),
    observed_freq = 0,
    expected_freq = missing_probabilities,
    .before = 1
  ) 

#Step 12
combined_freq_table <-
  combined_freq_table %>% 
  group_by(trufflesPerPlot) %>% 
  summarize(
    observed_freq = sum(observed_freq),
    expected_freq = sum(expected_freq)
  ) %>% 
  print()
chisq.test(x = combined_freq_table$observed_freq,
           p = combined_freq_table$expected_freq / 128)

#Step 13 goodness fit test
chisq_results <-                                
  chisq.test(
    x = combined_freq_table$observed_freq,
    p = combined_freq_table$expected_freq/6
  )
test_statistic <- chisq_results$statistic      

p_value <- 1 - pchisq(test_statistic, df = 15)   
p_value

#Lastly, Interpretation of results
#Because the P-value of 2.2^{-16} is less than the α-level of 0.05, I reject the null hypothesis. 
#Therefore, the number of truffles are not randomly located around the forest


