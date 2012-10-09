CREATE TABLE parvalues (
       parname VARCHAR NOT NULL,
       chainid INTEGER NOT NULL,
       iter INTEGER NOT NULL,
       parvalue NUMERIC NOT NULL,
       PRIMARY KEY (parname, chainid)
);

CREATE TABLE parnames (
       parname VARCHAR NOT NULL,
       pararray VARCHAR NOT NULL,
       parindex VARCHAR NOT NULL,
       PRIMARY KEY (parname)
);

CREATE TABLE pararrays (
       pararray VARCHAR NOT NULL,
       ndim INTEGER NOT NULL,
       dimsize VARCHAR NOT NULL,
       PRIMARY KEY (pararray)
);

CREATE TABLE chains (
       chainid INTEGER NOT NULL,
       -- For Stan
       stan_version_major INTEGER,
       stan_version_minor INTEGER,
       stan_version_patch INTEGER,
       iter INTEGER,
       datasrc VARCHAR,
       init VARCHAR, -- integer, but can be big
       append_samples INTEGER,
       save_warmup INTEGER,
       seed VARCHAR,g
       chain_id INTEGER,
       warmup INTEGER,
       equal_step_sizes INTEGER,
       leapfrog_steps INTEGER,
       max_treedepth INTEGER,
       epsilon NUMERIC,
       epsilon_pm INTEGER,
       delta NUMERIC,
       gamma NUMERIC,
       step_size NUMERIC,
       PRIMARY KEY (chainid)
);

CREATE TABLE parameter_chains (
       parname VARCHAR NOT NULL,
       chainid INTEGER NOT NULL,
       -- For Stan
       step_size_multipliers NUMERIC,
       PRIMARY KEY (parname, chainid)
);

CREATE TABLE chain_iters (
       chainid INTEGER NOT NULL,
       iter INTEGER NOT NULL,
       reject BOOLEAN,
       -- For Stan
       stepsize NUMERIC,
       treedepth INTEGER,
       PRIMARY KEY (chainid, iter)
);

-- parvalues
CREATE INDEX parvalues_parname_idx ON parvalues(parname);
CREATE INDEX parvalues_chainid_idx ON parvalues(chainid);
CREATE INDEX parvalues_iter_idx ON parvalues(iter);
CREATE INDEX parvalues_parname_chainid_idx ON parvalues(parname,chainid);
CREATE INDEX parvalues_parname_iter_idx ON parvalues(parname,iter);
CREATE INDEX parvalues_chainid_iter_idx ON parvalues(chainid,iter);
-- -- parnames
CREATE INDEX parnames_pararray_idx on parnames(pararray);
-- parameter_chains
CREATE INDEX parameter_chains_parname_idx on parameter_chains(parname);
CREATE INDEX parameter_chains_chainid_idx on parameter_chains(chainid);
-- chain_iters
CREATE INDEX chain_iters_chainid_idx on chain_iters(chainid);
CREATE INDEX chain_iters_iter_idx on chain_iters(iter);


