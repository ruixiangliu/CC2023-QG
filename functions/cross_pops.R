# Function to cross two populations
cross_pops <- function(noff, nloci = 100, effect = 0.1, p1 = 0, p2 = 1, ngens = 1) {

  # Simulate offspring genotypes
  data <- rbinom(noff * nloci, 1, p1) + rbinom(noff * nloci, 1, p2)

  # Make a table out of them
  data <- matrix(data, nrow = noff)
  data <- as_tibble(data)

  # While there are still some generations to compute...
  while (ngens > 1) {

    # Sample genotypes from the Fn to make up the Fn+1
    data <- data %>%
      pivot_longer(colnames(.)[colnames(.) != "ind"]) %>%
      group_by(name) %>%
      summarize(freq = sum(value) / (2 * n())) %>%
      mutate(z = map(freq, ~ rbinom(n = noff, size = 2, prob = .x))) %>%
      unnest(z) %>%
      mutate(ind = rep(seq(noff), n() / noff)) %>%
      select(-freq) %>%
      rename(value = "z") %>%
      pivot_wider()

    ngens <- ngens - 1

  }

  # Compute phenotypes from genotypes
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
