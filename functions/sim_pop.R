# Function to simulate a population
sim_pop <- function(nind, nloci = 1, effect = 0.1, allfreq = 0.5) {

  p <- allfreq
  q <- 1 - p

  # Simulate genotypes in Hardy-Weinberg equilibrium
  data <- sample(
    x = 0:2,
    prob = c(q * q, 2 * p * q, p * p),
    size = nloci * nind,
    replace = TRUE
  )

  # Turn them into a table
  data <- matrix(data, nrow = nind)
  data <- as_tibble(data)

  # Compute phenotypes
  data <- data %>%
    mutate(ind = seq(n())) %>%
    pivot_longer(colnames(.)[colnames(.) != "ind"]) %>%
    group_by(ind) %>%
    summarize(z = sum(value)) %>%
    mutate(z = z * effect)

  # Plot
  data %>%
    ggplot(aes(x = z, y = after_stat(density))) +
    geom_histogram(bins = 50) +
    xlab("Phenotype") +
    ylab("Count") +
    geom_density()

}
