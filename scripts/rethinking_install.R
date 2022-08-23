


install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::install_cmdstan()

install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"), dependencies = TRUE)
devtools::install_github("rmcelreath/rethinking")