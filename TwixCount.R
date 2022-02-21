library(tidyverse) # if you have not already loaded it

Candy_Tom <- c(rep("Twix", 12), rep("Reese's", 8))
Candy_Jessy <- c(rep("Twix", 6), rep("Reese's", 14))

sims <- 1000
samp_size <- 10
Twix_counter <- numeric(sims)

for(i in 1:sims) {
  candies <- sample(Candy_Tom, 10, replace = F)
  samp_df <- data.frame(candies)
  candies_count <- samp_df %>% count(candies)
  Twix_count <- filter(candies_count, candies == "Twix")$n
  Twix_counter[i] <- Twix_count
}

Twix_sims_T <- data.frame(Twix_counter)

Twix_sims_T %>% ggplot(aes(x = Twix_counter)) +
  geom_bar(fill = "forestgreen") + 
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "firebrick") +
  scale_x_continuous(breaks = c(1:10),labels = c(1:10)) +
  labs(title =  paste(sims, "simulations of drawing", samp_size, "candies"),
       subtitle = "assuming Tom's claim is correct.",
       x = "Number of Twix in sample",
       y = "Number of simulations")

### 2nd Version

sims <- 1000
samp_size <- 10
sample_df <- data.frame(matrix(ncol = samp_size, nrow = 0))
colnames(sample_df) <- c(1:samp_size)

for(i in 1:sims) {
  candy_sample <- sample(Candy_Tom, samp_size, replace = F)
  sample_df[i,] <- t(candy_sample)
}

sample_df <- sample_df %>% mutate(Twix_count = rowSums(. == "Twix"))


sample_df %>% ggplot(aes(x = Twix_count)) +
  geom_bar(fill = "forestgreen") + 
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "firebrick") +
  scale_x_continuous(breaks = c(1:10),labels = c(1:10)) +
  labs(title =  paste(sims, "simulations of drawing", samp_size, "candies"),
       subtitle = "assuming Tom's claim is correct.",
       x = "Number of Twix in sample",
       y = "Number of simulations")
