##
## Simulating data sets to test {gllvm}
##
## ---------------------------------------------------------------------------- ##

## Here we simulate data based on Eq. 1 from Niku et al. (2019) [Methods Ecol Evol 10:2173-2182],
## assuming 'alpha' (row effects) as zero, thus ignoring this term, i.e.:
##
## g(mu_ij) = eta_ij = alpha_i + beta0_j + X_i %*% Beta_j + U_i %*% Gamma_j
##
## where,
## eta_ij = the linear predictor,
## alpha_i = 0 (no effect, in our case),
## beta0_j = species-specific intercept,
## X_i = predictor(s) matrix,
## Beta_j = species-specific coefficients related to the predictors,
## U_i = latent variables (assumed, here, to be normal distributed),
## Gamma_j = species-specific coefficients related to the latent variables.
## (terms starting with a capital letter means it is a matrix)

### General settings (set.seed, nsp, nsite, nlv, X_i, U_i) ####

## As per the simulation purpose, set a seed to allow reproducibility
set.seed(1234)

## Number of species
n_sps = 15

## Number of sites
n_sites = 99

## Number of latent variables
# n_lv = 2

## X_i matrix (predictors): we are assuming the predictors as a dummy variable (0/1)
# "3 seasons" -- the 'intercept' is, say, "summer"
Xmat_winter = c(rep(0, (n_sites/3)), rep(1, (n_sites/3)), rep(0, (n_sites/3))) 
Xmat_spring = c(rep(0, (n_sites/3)*2), rep(1, (n_sites/3)))
X_i = cbind(Xmat_winter, Xmat_spring)

rm("Xmat_winter", "Xmat_spring")

## U_i matrix (the latent variables)
U_i = cbind(rnorm(n_sites), rnorm(n_sites))

### Parameters for data sets #### 

datasets_params <- list(
  # -- 1: likely no zeroes, 'high values' for all species
  dataset1 = list(
    ## Intercept (a value for each species)
    beta0_j = c(2,1,1,0.5,0.5, 2,1,1,0.5,0.5, 2,1,1,0.5,0.5) + 2,
    ## Beta_j matrix (species-specific coefficients related to covariates) 
    Beta_j = cbind(c(2, 0.5), c(2, 0.5), c(1.5, 0.5), c(1, 0.5), c(1, -0.5),
                   c(-2.5, 0.5), c(-2, 0), c(-1.5, 0.5), c(-1, 0.5), c(-0.5, 0),
                   c(0.5, 0), c(0.5, 1), c(0.5, 0.5), c(0.5, 1), c(0.5, 0.5)),
    ## Gamma_j matrix (species-specific coefficients related to latent variables) 
    # Easier to specify it in the 'transposed' format, then transpose it back as it should be.
    # To ensure that the matrix do not rotate, the diagonal has to be a positive number 
    # and above it 0 (see details in the Niku et al. 2019 paper, pp. 2174) -- 
    # for simplicity, we fixed the 'positive' value to 1, as implemented in {gllvm}.
    Gamma_j = t( cbind(c(1, rnorm(n_sps-1,0,0.5)), c(0,1, rnorm(n_sps-2,0,0.5))) )
  ),
  # -- 2: similar to dataset1, but smaller counts expected
  dataset2 = list(
    beta0_j = c(2,1,1,0.5,0.5, 2,1,1,0.5,0.5, 2,1,1,0.5,0.5),
    Beta_j = cbind(c(2, 0.5), c(2, 0.5), c(1.5, 0.5), c(1, 0.5), c(1, -0.5),
                   c(-2.5, 0.5), c(-2, 0), c(-1.5, 0.5), c(-1, 0.5), c(-0.5, 0),
                   c(0.5, 0), c(0.5, 1), c(0.5, 0.5), c(0.5, 1), c(0.5, 0.5)),
    Gamma_j = t( cbind(c(1, rnorm(n_sps-1, 0, 0.5)), c(0,1, rnorm(n_sps-2, 0, 0.5))) )
  ),
  # -- 3: sparse zeroes
  dataset3 = list(
    beta0_j = c(rep(1, n_sps)),
    Beta_j = cbind(c(2, 0.5), c(2, 0.5), c(1.5, 0.5), c(1, 0.5), c(1, -0.5),
                   c(-2.5, 0.5), c(-2, 0), c(-1.5, 0.5), c(-1, 0.5), c(-0.5, 0),
                   c(0.5, 0), c(0.5, 1), c(0.5, 0.5), c(0.5, 1), c(0.5, 0.5)),
    Gamma_j = t( cbind(c(1, rnorm(n_sps-1,0,0.5)), c(0,1, rnorm(n_sps-2,0,0.5))) )
  ),
  # -- 4: quite a few zeroes
  dataset4 = list(
    beta0_j = c(rep(0, n_sps)),
    Beta_j = cbind(c(2, 0.5), c(2, 0.5), c(1.5, 0.5), c(1, 0.5), c(1, -0.5),
                   c(-2.5, 0.5), c(-2, 0), c(-1.5, 0.5), c(-1, 0.5), c(-0.5, 0),
                   c(0.5, 0), c(0.5, 1), c(0.5, 0.5), c(0.5, 1), c(0.5, 0.5)),
    Gamma_j = t( cbind(c(1, rnorm(n_sps-1,0,0.5)), c(0,1, rnorm(n_sps-2,0,0.5))) )
  )
)

### Simulate data sets #### 

## List to store data sets
datasets <- list()

for (ii in 1:length(datasets_params)) {
  
  name <- names(datasets_params[ii])
  
  beta0 = datasets_params[[ii]][["beta0_j"]]
  Xmatrix = X_i
  betamat = datasets_params[[ii]][["Beta_j"]]
  umat = U_i
  gamma = datasets_params[[ii]][["Gamma_j"]]
  
  ## Get expected values (populate 'g(mu_ij)')
  gmu = matrix(NA, n_sites, n_sps)
  
  for(i in 1:n_sites){
    gmu[i,] = beta0 + Xmatrix[i,] %*% betamat + umat[i,] %*% gamma
  }
  
  ## As 'g' is a log-link function, take the exponential of the expected g(mu)
  ## to get 'real values'
  mu = exp(gmu)
  
  datasets[[name]] = cbind(
    season = c(rep("summer", (n_sites/3)), rep("winter", (n_sites/3)), rep("spring", (n_sites/3))),
    as.data.frame(matrix(data = rpois(n = length(mu), lambda = mu), nrow = n_sites, ncol = n_sps))
    )
  
  rm("ii", "i", "name", "beta0", "Xmatrix", "betamat", "umat", "gamma", "gmu", "mu")
}

## Save datasets (and their parameters; just in case) ####
save(datasets_params, U_i, X_i, n_sites, n_sps, file = "./datasets/datasets_all-params.rda")
save(datasets, file = "./datasets/datasets.rda")
