#' Run stress testing for provided asset type.
#'
#' This function runs the transition risk stress test. It can be desirable to
#' understand sensitivities of the scenarios, in which case the user may pass a
#' vector of values to one (and only one) of the detail arguments. This will
#' result in running the analysis multiple times in a row with the argument
#' varied.
#' NOTE: argument `asset_type` is not iterateable.
#' NOTE: if `return_results` is TRUE results will not be written to `output
#' path` but instead are returned.
#'
#' @param asset_type String holding asset_type. For accepted values compare
#'   `stress_test_arguments`.
#' @param input_path_project_specific String holding path to project specific
#'   data.
#' @param input_path_project_agnostic String holding path to project agnostic
#'   data.
#' @param output_path String holding path to which output files are written.
#'   NOTE: Results and logs per run are saved to a subdirectory of output_path
#'   that will be generated automatically. The name of the subdirectory is the
#'   timestamp of the run of the analysis.
#' @param lgd_senior_claims Numeric, holding the loss given default for senior
#'   claims, for accepted value range check `stress_test_arguments`.
#' @param lgd_subordinated_claims Numeric, holding the loss given default for
#'   subordinated claims, for accepted value range check
#'   `stress_test_arguments`.
#' @param risk_free_rate Numeric that indicates the risk free rate of interest.
#'   For accepted range compare `stress_test_arguments`.
#' @param discount_rate Numeric, that holds the discount rate of dividends per
#'   year in the DCF. For accepted range compare `stress_test_arguments`.
#' @param div_netprofit_prop_coef Numeric. A coefficient that determines how
#'   strongly the future dividends propagate to the company value. For accepted
#'   range compare `stress_test_arguments`.
#' @param shock_year Numeric, holding year the shock is applied. For accepted
#'   range compare `stress_test_arguments`.
#' @param term Numeric. A coefficient that determines for which maturity the
#'   expected loss should be calculated in the credit risk section. For accepted
#'   range compare `stress_test_arguments`.
#' @param company_exclusion Boolean, indicating if companies provided in dataset
#'   excluded_companies.csv shall be excluded. For accepted values compare
#'   `stress_test_arguments`.
#' @param return_results Boolean, indicating if results shall be exported.
#' @return NULL
#' @export
run_stress_test <- function(asset_type,
                            input_path_project_specific,
                            input_path_project_agnostic,
                            output_path,
                            lgd_senior_claims = 0.45,
                            lgd_subordinated_claims = 0.75,
                            risk_free_rate = 0.02,
                            discount_rate = 0.02,
                            div_netprofit_prop_coef = 1,
                            shock_year = 2030,
                            term = 2,
                            company_exclusion = TRUE,
                            return_results = FALSE) {
  cat("-- Running transition risk stress test. \n\n\n")

  args_list <- mget(names(formals()), sys.frame(sys.nframe())) %>%
    fail_if_input_args_are_missing()

  iter_var <- get_iter_var(args_list)

  args_list$output_path <- customise_output_path(
    output_path = args_list$output_path,
    iter_var = iter_var
  )

  args_tibble <- tibble::as_tibble(args_list) %>%
    dplyr::mutate(iter_var = .env$iter_var)

  st_results_list <- purrr::map(1:nrow(args_tibble), run_stress_test_iteration, args_tibble = args_tibble)

  result_names <- names(st_results_list[[1]])
  st_results <- result_names %>%
    purrr::map(function(tib) {
      purrr::map_dfr(st_results_list, `[[`, tib)
    }) %>%
    purrr::set_names(result_names)

  st_results_wrangled_and_checked <- wrangle_results(
    results_list = st_results,
    sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup]
  ) %>%
    check_results(
      sensitivity_analysis_vars = names(args_list)[!names(args_list) %in% setup_vars_lookup]
    ) %>%
    rename_results()

  if (return_results) {
    return(st_results_wrangled_and_checked)
  }

  write_stress_test_results(
    results_list = st_results_wrangled_and_checked,
    asset_type = asset_type,
    iter_var = iter_var,
    output_path = args_list$output_path
  )

  cat("-- Exported results to designated output path. \n")
}

#' Iterate over stress test runs
#'
#' @param n Numeric.
#' @param args_tibble  A tibble holding a set of params for
#'   `run_stress_test_imp` per row.
#'
#' @return List of stress test results.
run_stress_test_iteration <- function(n, args_tibble) {
  arg_tibble_row <- args_tibble %>%
    dplyr::slice(n)

  arg_list_row <- arg_tibble_row %>%
    dplyr::select(-.data$return_results) %>%
    as.list()

  arg_tibble_row <- arg_tibble_row %>%
    dplyr::select(-dplyr::all_of(setup_vars_lookup)) %>%
    dplyr::rename_with(~ paste0(.x, "_arg"))

  st_result <- do.call(args = arg_list_row, what = run_stress_test_impl) %>%
    purrr::map(dplyr::bind_cols, data_y = arg_tibble_row)
}


#' Run stress testing for provided asset type.
#'
#' Runs stress test per iteration.
#'
#' @inheritParams run_stress_test
#' @inheritParams write_stress_test_results
#'
#' @return A list of stress test results.
run_stress_test_impl <- function(asset_type,
                                 input_path_project_specific,
                                 input_path_project_agnostic,
                                 output_path,
                                 lgd_senior_claims,
                                 lgd_subordinated_claims,
                                 risk_free_rate,
                                 discount_rate,
                                 div_netprofit_prop_coef,
                                 shock_year,
                                 term,
                                 company_exclusion,
                                 iter_var) {
  cat("-- Validating input arguments. \n")

  validate_input_values(
    lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims,
    risk_free_rate = risk_free_rate,
    discount_rate = discount_rate,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    shock_year = shock_year,
    term = term,
    company_exclusion = company_exclusion,
    asset_type = asset_type
  )

  args_list <- mget(names(formals()), sys.frame(sys.nframe()))

  log_path <- file.path(output_path, paste0("log_file_", iter_var, ".txt"))

  paste_write("\n\nIteration with parameter settings:", log_path = log_path)
  purrr::walk(names(args_list), function(name) {
    paste(name, magrittr::extract2(args_list, name), sep = ": ") %>%
      paste_write(log_path = log_path)
  })
  paste_write("\n", log_path = log_path)

  cat("-- Configuring analysis settings. \n")

  scenario_to_follow_baseline <- baseline_scenario_lookup
  scenario_to_follow_ls <- shock_scenario_lookup
  calculation_level <- calculation_level_lookup
  end_year <- end_year_lookup
  time_horizon <- time_horizon_lookup
  flat_multiplier <- assign_flat_multiplier(asset_type = asset_type)
  lgd <- assign_lgd(
    asset_type = asset_type, lgd_senior_claims = lgd_senior_claims,
    lgd_subordinated_claims = lgd_subordinated_claims
  )
  scenario_geography_filter <- "Global"
  scenarios_filter <- unique(
    c(
      scenario_to_follow_baseline,
      scenario_to_follow_ls
    )
  )

  cat("-- Importing and preparing input data from designated input path. \n")

  pacta_based_data <- read_and_prepare_project_specific_data(
    asset_type = asset_type,
    calculation_level = calculation_level,
    time_horizon = time_horizon,
    scenario_geography_filter = scenario_geography_filter,
    scenarios_filter = scenarios_filter,
    equity_market_filter = equity_market_filter_lookup,
    term = term,
    path = input_path_project_specific
  )

  project_specific_data_list <- pacta_based_data$data_list
  start_year <- pacta_based_data$start_year

  project_agnostic_data_list <- read_and_prepare_project_agnostic_data(
    start_year = start_year,
    end_year = end_year,
    company_exclusion = company_exclusion,
    scenario_geography_filter = scenario_geography_filter,
    asset_type = asset_type,
    path = input_path_project_agnostic
  )

  input_data_list <- c(project_specific_data_list, project_agnostic_data_list) %>%
    check_and_filter_data(
      start_year = start_year,
      end_year = end_year,
      scenarios_filter = scenarios_filter,
      scenario_geography_filter = scenario_geography_filter
    )

  if (asset_type == "loans") {
    input_data_list$financial_data <- input_data_list$financial_data %>%
      dplyr::mutate(company_name = stringr::str_to_lower(.data$company_name))
  }

  report_company_drops(
    data_list = input_data_list,
    asset_type = asset_type,
    log_path = log_path
  )

  check_scenario_availability(
    portfolio = input_data_list$pacta_results,
    scen_data = input_data_list$scenario_data,
    scenarios = scenarios_filter
  )

  # TODO: validate
  port_aum <- calculate_aum(input_data_list$sector_exposures)
  transition_scenario <- generate_transition_shocks(
    start_of_analysis = start_year,
    end_of_analysis = end_year,
    shock_years = shock_year
  )

  cat("-- Calculating market risk. \n")

  annual_profits <- calculate_annual_profits(
    asset_type = asset_type,
    input_data_list = input_data_list,
    scenario_to_follow_baseline = scenario_to_follow_baseline,
    scenario_to_follow_ls = scenario_to_follow_ls,
    transition_scenario = transition_scenario,
    start_year = start_year,
    end_year = end_year,
    time_horizon = time_horizon,
    discount_rate = discount_rate,
    log_path = log_path
  )

  exposure_by_technology_and_company <- calculate_exposure_by_technology_and_company(
    asset_type = asset_type,
    input_data_list = input_data_list,
    start_year = start_year,
    scenario_to_follow_ls = scenario_to_follow_ls,
    log_path = log_path
  )

  results <- company_asset_value_at_risk(
    data = annual_profits,
    terminal_value = terminal_value_lookup,
    shock_scenario = transition_scenario,
    div_netprofit_prop_coef = div_netprofit_prop_coef,
    plan_carsten = exposure_by_technology_and_company,
    port_aum = port_aum,
    flat_multiplier = flat_multiplier,
    exclusion = input_data_list$excluded_companies
  )

  cat("-- Calculating credit risk. \n\n\n")

  overall_pd_changes <- annual_profits %>%
    calculate_pd_change_overall(
      shock_year = transition_scenario$year_of_shock,
      end_of_analysis = end_year,
      risk_free_interest_rate = risk_free_rate
    )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  expected_loss <- company_expected_loss(
    data = overall_pd_changes,
    loss_given_default = lgd,
    exposure_at_default = exposure_by_technology_and_company,
    # TODO: what to do with this? some sector level exposure for loanbook?
    port_aum = port_aum
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient output from overall pd changes or related financial data inputs

  annual_pd_changes <- calculate_pd_change_annual(
    data = annual_profits,
    shock_year = transition_scenario$year_of_shock,
    end_of_analysis = end_year,
    risk_free_interest_rate = risk_free_rate
  )

  # TODO: ADO 879 - note which companies produce missing results due to
  # insufficient input information (e.g. NAs for financials or 0 equity value)

  return(
    list(
      results = results,
      expected_loss = expected_loss,
      annual_pd_changes = annual_pd_changes,
      overall_pd_changes = overall_pd_changes
    )
  )
}
