library(deSolve)

# Define the model function
carbon_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Fluxes
    # I'm using hypothetical values for demonstration; these should be based on actual research.
    flux_NPP_to_ABP = k_NPP_ABP * NPP
    flux_ABP_to_RH  = k_ABP_RH * ABP
    flux_ABP_to_SoilOrgC = k_ABP_SoilOrgC * ABP
    flux_RH_to_SoilMicro = k_RH_SoilMicro * RH
    flux_SoilMicro_to_SoilOrgC = k_SoilMicro_SoilOrgC * SoilMicro
    flux_SoilOrgC_to_SoilMicro = k_SoilOrgC_SoilMicro * SoilOrgC
    flux_SoilOrgC_to_SoilFauna = k_SoilOrgC_SoilFauna * SoilOrgC
    flux_SoilFauna_to_SoilMicro = k_SoilFauna_SoilMicro * SoilFauna
    flux_SoilMicro_to_SoilFauna = k_SoilMicro_SoilFauna * SoilMicro
    flux_SoilOrgC_to_Fire = k_SoilOrgC_Fire * SoilOrgC
    
    # Differential equations for each carbon pool
    dNPP_dt = -flux_NPP_to_ABP
    dABP_dt = flux_NPP_to_ABP - flux_ABP_to_RH - flux_ABP_to_SoilOrgC
    dRH_dt = flux_ABP_to_RH - flux_RH_to_SoilMicro
    dSoilMicro_dt = flux_RH_to_SoilMicro + flux_SoilFauna_to_SoilMicro + 
      flux_SoilOrgC_to_SoilMicro - flux_SoilMicro_to_SoilOrgC - 
      flux_SoilMicro_to_SoilFauna
    dSoilOrgC_dt = flux_ABP_to_SoilOrgC + flux_SoilMicro_to_SoilOrgC - 
      flux_SoilOrgC_to_SoilMicro - flux_SoilOrgC_to_SoilFauna -
      flux_SoilOrgC_to_Fire
    dSoilFauna_dt = flux_SoilOrgC_to_SoilFauna - flux_SoilFauna_to_SoilMicro
    
    # Return the rates of change
    list(c(dNPP_dt, dABP_dt, dRH_dt, dSoilMicro_dt, dSoilOrgC_dt, dSoilFauna_dt))
  })
}

# Initial state values for the carbon pools
state <- c(NPP = 100,  # Net Primary Productivity
           ABP = 100,  # Aboveground Plant Biomass
           RH = 100,   # Root Exudation
           SoilMicro = 100, # Soil Microbes
           SoilOrgC = 100,  # Soil Organic Carbon
           SoilFauna = 100) # Soil Fauna

# Parameters (hypothetical values)
parameters <- c(k_NPP_ABP = 0.1,
                k_ABP_RH = 0.05,
                k_ABP_SoilOrgC = 0.02,
                k_RH_SoilMicro = 0.03,
                k_SoilMicro_SoilOrgC = 0.04,
                k_SoilOrgC_SoilMicro = 0.01,
                k_SoilOrgC_SoilFauna = 0.02,
                k_SoilFauna_SoilMicro = 0.03,
                k_SoilMicro_SoilFauna = 0.02,
                k_SoilOrgC_Fire = 0.01)

# Time span for the simulation
times <- seq(0, 100, by = 1)

# Solve the differential equations
out <- ode(y = state, times = times, func = carbon_model, parms = parameters)

# Plot the results
matplot(out[, "time"], out[, -1], type = 'l', lty = 1, col = 1:6, xlab = "Time", ylab = "Carbon content")
legend("right",
       