


install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

cmdstanr::install_cmdstan()

install.packages(c("mvtnorm","loo","coda","remotes"), repos="https://cloud.r-project.org/",dependencies=TRUE)
remotes::install_github("rmcelreath/rethinking")