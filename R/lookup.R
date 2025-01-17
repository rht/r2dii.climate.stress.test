#' Lookup valid values
#'
#' @name lookup
#'
#' @examples
#'
#' credit_type_lookup
NULL

# An investor name is needed for legacy reasons currently there is no
# practical purpose.
investor_name_placeholder <- "Meta Investor"

# vector holding vars to nest pacta results by
nesting_vars_lookup <- c(
  "investor_name", "portfolio_name", "equity_market", "ald_sector", "technology",
  "scenario", "allocation", "scenario_geography", "company_name"
)

asset_types_lookup <- c("equity", "bonds", "loans")

# vector holding scenarios to filter PACTA results by for stresstesting
scenarios_lookup <- c(
  "ETP2017_NPS",
  "ETP2017_SDS",
  "WEO2019_NPS",
  "WEO2019_SDS"
)

baseline_scenario_lookup <- "NPS"
shock_scenario_lookup <- "SDS"
calculation_level_lookup <- "company"

end_year_lookup <- 2040

time_horizon_lookup <- 5

price_data_version_lookup <- "2021Q1"

#' @rdname lookup
#' @export
credit_type_lookup <- c("outstanding", "credit_limit")

# vector holding considered sectors in stress testing
sectors_lookup <- c("Power", "Oil&Gas", "Coal", "Automotive")

# vector holding considered technologies in stress testing
technologies_lookup <- c(
  "Electric", "Hybrid", "ICE",
  "CoalCap", "GasCap", "RenewablesCap", "NuclearCap", "HydroCap", "OilCap",
  "Oil", "Gas",
  "Coal"
)

# holding allocation method used for stress testing
allocation_method_lookup <- "portfolio_weight"

equity_market_filter_lookup <- "GlobalMarket"

# technology and sector mapping between P4I and P4B
# HDV and shipping not consistently defined across both versions at this time
p4i_p4b_sector_technology_lookup <- tibble::tribble(
  ~sector_p4b,   ~technology_p4b,             ~sector_p4i,    ~technology_p4i,
  "automotive",  "electric",                 "Automotive",   "Electric",
  "automotive",  "hybrid",                   "Automotive",   "Hybrid",
  "automotive",  "fuelcell",                 "Automotive",   "FuelCell",
  "automotive",  "ice",                      "Automotive",   "ICE",
  "coal",        "coal",                     "Coal",         "Coal",
  "oil and gas", "gas",                      "Oil&Gas",      "Gas",
  "oil and gas", "oil",                      "Oil&Gas",      "Oil",
  "power",       "coalcap",                  "Power",        "CoalCap",
  "power",       "gascap",                   "Power",        "GasCap",
  "power",       "hydrocap",                 "Power",        "HydroCap",
  "power",       "nuclearcap",               "Power",        "NuclearCap",
  "power",       "oilcap",                   "Power",        "OilCap",
  "power",       "renewablescap",            "Power",        "RenewablesCap",
  "aviation",    "freight",                  "Aviation",     "Freight",
  "aviation",    "passenger",                "Aviation",     "Passenger",
  "cement",      "grinding",                 "Cement",       "Grinding",
  "cement",      "integrated facility",      "Cement",       "Integrated facility",
  "steel",       "ac-electric arc furnace",  "Steel",        "Ac-Electric Arc Furnace",
  "steel",       "bof shop",                 "Steel",        "Bof Shop",
  "steel",       "dc-electric arc furnace",  "Steel",        "Dc-Electric Arc Furnace",
  "steel",       "open hearth meltshop",     "Steel",        "Open Hearth Meltshop"
)

# scenario mapping between P4I and P4B
# TODO: should the implicit mappin a la sps is the follow up NPS be hapening elsewhere?
p4i_p4b_scenario_lookup <- tibble::tribble(
  ~scenario_p4b,   ~scenario_p4i,
  "target_cps",   "CPS",
  "target_rts",   "NPS",
  "target_sps",   "NPS",
  "target_steps", "NPS",
  "target_2ds",   "SDS",
  "target_sds",   "SDS",
  "target_b2ds",  "B2DS",
)

# P4B scenario list
p4b_scenarios_lookup <- c("target_b2ds", "target_cps", "target_rts",
                          "target_sps", "target_steps", "target_2ds",
                          "target_sds")

# holds names of input arguments to run_stress_test that are not model paramters
setup_vars_lookup <- c("input_path_project_agnostic", "input_path_project_specific", "output_path", "iter_var", "return_results")

terminal_value_lookup <- 0
