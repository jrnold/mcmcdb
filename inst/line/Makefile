R = Rscript

all: line_mcmc_list.Rdata ../../data/line_mcmcdbwide.Rdata

line_data.R: make_line_data.R 
	$(R) $^

line_mcmc_list.Rdata: line_mcmc_list.R line_data.R line.jag
	$(R) $^

../../data/line_mcmcdbwide.Rdata: make_mcmcdbwide.R line_mcmc_list.Rdata
	$(R) $^
