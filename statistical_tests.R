# Author: Christopher Peters
# twitter: @statwonk

install.packages("MASS")
library(MASS)

install.packages("ggplot2")
library(ggplot2)

set.seed(1)
total.sample.size <-  25000
p_value <- rep(NA, (total.sample.size / 2) - 1)
probs_a <- rbinom(total.sample.size / 2, 1, prob = 0.02)
probs_b <- rbinom(total.sample.size / 2, 1, prob = 0.025)
for (i in 1:(total.sample.size / 2)) {
  print(i)
  if (sum(probs_a[1:i], na.rm = T) > 0 & sum(probs_b[1:i], na.rm = T) > 0) {
    p_value[i] <- chisq.test(rbind(table(probs_a[1:i]), table(probs_b[1:i])))$p.value
  }
}

df <- as.data.frame(list(total_observations = seq(1, total.sample.size / 2, 1),
                         p_value = p_value))

df$is_sig <- ifelse(p_value < 0.05, 1, 0)

ggplot(df, aes(x = total_observations, y = p_value, colour = factor(is_sig))) +
  geom_point() +
  coord_cartesian(ylim = c(0, 1))






?chisq.test
