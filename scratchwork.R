library(tidyverse)

Candy_Tom <- c(rep("Twix", 12), rep("Reese's", 8))
Candy_Jessy <- c(rep("Twix", 6), rep("Reese's", 14))

### --- change these as needed
sims <- 1000
samp_size <- 10
correct_person <- "Tom"
correct_person_dist <- Candy_Tom
### ---

sample_df <- data.frame(matrix(ncol = samp_size, nrow = 0))
colnames(sample_df) <- c(1:samp_size)


set.seed(42)
for(i in 1:sims) {
  candy_sample <- sample(correct_person_dist, samp_size, replace = F)
  sample_df[i,] <- t(candy_sample)
}

sample_df <- sample_df %>% mutate(Twix_count = rowSums(. == "Twix"))


sample_df %>% ggplot(aes(x = Twix_count)) +
  geom_bar(fill = "forestgreen") + 
  geom_vline(aes(xintercept = 5), linetype = "dashed", color = "firebrick") +
  scale_x_continuous(breaks = c(1:10),labels = c(1:10)) +
  labs(title =  paste(sims, "simulations of drawing", samp_size, "candies"),
       subtitle = paste("assuming ", correct_person, "'s claim is correct.", sep = ""),
       x = "Number of Twix in sample",
       y = "Number of simulations")

table(sample_df$Twix_count)
