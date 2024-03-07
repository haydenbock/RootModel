library(deSolve)



# Adjust flux rates based on soil moisture
adjust_flux_for_moisture <- function(flux, moisture, low_moisture_effect, high_moisture_effect) {
  if (moisture < 0.3) {  # Assuming a scale of 0 to 1 for moisture
    return(flux * low_moisture_effect)
  } else if (moisture > 0.7) {
    return(flux * high_moisture_effect)
  } else {
    return(flux)
  }
}

# Define the model function with soil moisture effects
carbon_model_moisture <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Adjust fluxes for moisture
    flux_NPP_to_ABP <- adjust_flux_for_moisture(k_NPP_ABP * NPP, SoilMoisture, 0.9, 1.0)  # Decrease with low moisture
    flux_ABP_to_RH <- adjust_flux_for_moisture(k_ABP_RH * ABP, SoilMoisture, 0.9, 1.0)  # Decrease with low moisture
    flux_SoilOrgC_to_SoilFauna <- adjust_flux_for_moisture(k_SoilOrgC_SoilFauna * SoilOrgC, SoilMoisture, 0.7, 1.0)  # Decrease with low moisture
    flux_SoilMicro_to_SoilFauna <- adjust_flux_for_moisture(k_SoilMicro_SoilFauna * SoilMicro, SoilMoisture, 1.0, 1.0)  # No change
    flux_SoilFauna_to_SoilMicro <- adjust_flux_for_moisture(k_SoilFauna_SoilMicro * SoilFauna, SoilMoisture, 0.7, 1.0)  # Decrease with low moisture
    
    #non-moisture fluxes
    flux_ABP_to_SoilOrgC = k_ABP_SoilOrgC * ABP
    flux_RH_to_SoilMicro = k_RH_SoilMicro * RH
    flux_SoilOrgC_to_SoilMicro = k_SoilOrgC_SoilMicro * SoilOrgC
    flux_SoilOrgC_to_Fire = k_SoilOrgC_Fire * SoilOrgC
    
    
    # Soil fungi activity increases relative to bacteria with low moisture
    soil_fungi_activity <- adjust_flux_for_moisture(1, SoilMoisture, 1.2, 0.8)  # Increase with low moisture
    flux_RH_to_SoilMicro <- k_RH_SoilMicro * RH * soil_fungi_activity
    
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
    
    # Differential equation for soil moisture (you would define this based on your system)
    dSoilMoisture_dt = -evaporation_rate * SoilMoisture + precipitation_rate * (1 - SoilMoisture)
    
    # Return the rates of change
    list(c(dNPP_dt, dABP_dt, dRH_dt, dSoilMicro_dt, dSoilOrgC_dt, dSoilFauna_dt, dSoilMoisture_dt))
  })
}


# Initial state values including soil moisture
state <- c(NPP = 100,
           ABP = 100,
           RH = 100,
           SoilMicro = 100,
           SoilOrgC = 100,
           SoilFauna = 100,
           SoilMoisture = 0.5)  # Initial soil moisture level

# Parameters including effects of soil moisture
parameters <- c(k_NPP_ABP = 0.1,
                 k_ABP_RH = 0.05,
                 k_ABP_SoilOrgC = 0.02,
                 k_RH_SoilMicro = 0.03,
                 k_SoilMicro_SoilOrgC = 0.04,
                 k_SoilOrgC_SoilMicro = 0.01,
                 k_SoilOrgC_SoilFauna = 0.02,
                 k_SoilFauna_SoilMicro = 0.03,
                 k_SoilMicro_SoilFauna = 0.02,
                 k_SoilOrgC_Fire = 0.01,
                evaporation_rate = 0.02,    # Example evaporation rate
                precipitation_rate = 0.01)  # Example precipitation rate

# Time span for the simulation
times <- seq(0, 100, by = 1)

# Solve the differential equations with moisture
out <- ode(y = state, times = times, func = carbon_model_moisture, parms = parameters)

# Plot the results with moisture
matplot(out[, "time"], out[, -1], type = 'l', lty = 1, col = 1:7, xlab = "Time", ylab = "Carbon content")
legend("right", legend = c("NPP", "ABP", "RH", "Soil Micro", "Soil OrgC", "Soil Fauna", "Soil Moisture"), col = 1:7, lty = 1)