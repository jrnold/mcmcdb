line <- mcmc4:::mcmc_list_from_csv(dir(".", pattern="\\.csv", full.names=TRUE), comment.char="#")
                               
## Mcmc.list
save(line, file="../data/line.rda")
