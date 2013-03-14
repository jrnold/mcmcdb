R = Rscript

all: line_stanfit.Rdata line_mcmc_list.Rdata

line_data.R: make_line_data.R 
	$(R) $^

line_stanfit.Rdata: line_stanfit.R line_data.R line.stan
	$(R) $^

line_mcmc_list.Rdata: line_mcmc_list.R line_data.R line.jag
	$(R) $^
