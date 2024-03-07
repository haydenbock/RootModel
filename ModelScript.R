library(deSolve)

# Define the model function
carbon_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Adjust fluxes for soil moisture
    flux_NPP_to_ABP_adj <- ifelse(SoilMoisture < 0.3, flux_NPP_to_ABP * 0.7, flux_NPP_to_ABP)
    flux_ABP_to_RH_adj <- ifelse(SoilMoisture < 0.3, flux_ABP_to_RH * 0.5, flux_ABP_to_RH)
    flux_SoilFauna_adj <- ifelse(SoilMoisture < 0.3, flux_SoilOrgC_to_SoilFauna * 0.5, flux_SoilOrgC_to_SoilFauna)
    flux_Fungi_Bacteria_adj <- ifelse(SoilMoisture < 0.3, flux_RH_to_SoilMicro * 1.2, flux_RH_to_SoilMicro)
    
    # Define the fluxes
    flux_NPP_to_ABP <- k_NPP_ABP * NPP
    flux_ABP_to_RH <- k_ABP_RH * ABP
    flux_ABP_to_SoilOrgC <- k_ABP_SoilOrgC * ABP
    flux_RH_to_SoilMicro <- k_RH_SoilMicro * RH
    flux_SoilMicro_to_SoilOrgC <- k_SoilMicro_SoilOrgC * SoilMicro
    flux_SoilOrgC_to_SoilMicro <- k_SoilOrgC_SoilMicro * SoilOrgC
    flux_SoilOrgC_to_SoilFauna <- k_SoilOrgC_SoilFauna * SoilOrgC
    flux_SoilFauna_to_SoilMicro <- k_SoilFauna_SoilMicro * SoilFauna
    flux_SoilMicro_to_SoilFauna <- k_SoilMicro_SoilFauna * SoilMicro
    flux_SoilOrgC_to_Fire <- k_SoilOrgC_Fire * SoilOrgC
    
    # Differential equations for each carbon pool
    dNPP_dt <- -flux_NPP_to_ABP_adj
    dABP_dt <- flux_NPP_to_ABP_adj - flux_ABP_to_RH_adj - flux_ABP_to_SoilOrgC
    dRH_dt <- flux_ABP_to_RH_adj - flux_RH_to_SoilMicro
    dSoilMicro_dt <- flux_RH_to_SoilMicro + flux_SoilFauna_to_SoilMicro +
      flux_SoilOrgC_to_SoilMicro - flux_SoilMicro_to_SoilOrgC -
      flux_SoilMicro_to_SoilFauna * flux_Fungi_Bacteria_adj
    dSoilOrgC_dt <- flux_ABP_to_SoilOrgC + flux_SoilMicro_to_SoilOrgC -
      flux_SoilOrgC_to_SoilMicro - flux_SoilOrgC_to_SoilFauna_adj -
      flux_SoilOrgC_to_Fire
    dSoilFauna_dt <- flux_SoilOrgC_to_SoilFauna_adj - flux_SoilFauna_to_SoilMicro * flux_Fungi_Bacteria_adj
    
    # Differential equation for soil moisture (simplified)
    dSoilMoisture_dt <- input_rate - evaporation_rate * SoilMoisture  # Example moisture dynamics
    
    # Return the rates of change
    list(c(dNPP_dt, dABP_dt, dRH_dt, dSoilMicro_dt, dSoilOrgC_dt, dSoilFauna_dt, dSoilMoisture_dt))
  })
}

# Initial state values for the carbon pools and soil moisture
state <- c(NPP = 100, ABP = 100, RH = 100, Soil