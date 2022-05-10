Step 1
cats_data <- read_csv("https://whitlockschluter3e.zoology.ubc.ca/Data/chapter08/chap08q19FallingCatsByMonth.csv")
cats_data



#Step 2
ggplot(data = cats_data) +
  geom_bar(mapping = aes(x = month))

months_of_year <- c("January", "February", "March", "April", 
                  "May", "June", "July","August","September","October",
                  "November","December")
months_of_year

#Step 3
cats_data <- 
  mutate( 
    cats_data, 
    month_fct = factor(month, levels = days_of_month),
    month_short = fct_relabel(month_fct, str_sub, start = 1, end = 3)
  )
cats_data

#Step 4
ggplot(data = cats_data) +
  geom_bar(mapping = aes(x = month_short), fill = "#a6192e") +
  labs(y = "Frequency", x = NULL)

#Step 5
cats_freq_table <- count(cats_data, month_fct)
cats_freq_table
#cats frequency
cats_freq <- cats_freq_table$n
cats_freq

cats_prob <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31) / 365
cats_prob

#Chi-squared
chisq.test (x = cats_freq, p = cats_prob)

#Since the P-value is 0.04 it is less than 0.05 in which I reject the null hypothesis
#Therefore, the frequency of cats falling is not same in each month and does not the fit the proportional model

