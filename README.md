
<!-- README.md is generated from README.Rmd. Please edit that file -->

# r2dii.climate.stress.test

<!-- badges: start -->

[![R-CMD-check](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/workflows/R-CMD-check/badge.svg)](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/actions)
<!-- badges: end -->

Beta version. More soon…

The goal of r2dii.climate.stress.test is to provide a tool that can be
used to conduct what-if climate stress test analyses for financial
institutions, supervisors, regulators and other stakeholders. The tool
aims at highlighting potential financial risk in especially climate
relevant sectors, split by production technology where required. The
sectors covered by the 2Dii climate stress test and therefore by this
package, follow mostly the logic of the Paris Agreement Capital
Transition Assessment (PACTA) tool, but can in principle be adapted to
other settings. W.I.P.

## Installation

You can install development version from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("2DegreesInvesting/r2dii.climate.stress.test")
```

Tu run most of the analyses, you will need additional auxiliary data,
such as scenario data. The specific requirements are outlined below.
**TODO**

Before running the code on a local machine, set the path to the data
files either as an Environment Variable or an R option using:

    # shell
    export ST_DATA_PATH="<path to data>"

or in R:

``` r
options("ST_DATA_PATH") <- "path to data"
```

Additionally, for running the litigation risk section, you will need
access to the pacta-data repository or a similar directory and need to
point the analysis to that directory by setting the path to the
pacta-data files as an Environment Variable, using:

    # shell
    export PACTA_DATA_PATH="<path to data>"

Alternatively, setup environmental variables in a user- or
project-specific .Renviron file (see `usethis::edit_r_environ()`).

**NOTE:**

The path passed via the environment variable or option is expected to be
an absolute path that points to the top level of the directory that
contains the required data. This data directory is, by default, assumed
to be an R package or a directory which resembles that structure. If in
your case the data is not stored in a directory that resembles an R
package with a `data-raw` folder, you should consider structuring your
data directory that way or otherwise you will need to adjust the input
to the `data_location()` function throughout the scripts.

## Scope

The repository covers climate risk calculations for the following types
of risks

  - Transition Risk
  - Litigation Risk
  - Physical Risk (future extension)

The methods for all of these are currently in the development phase,
meaning there are likely going to be changes to the calculations in
future releases.

The following financial asset types are currently covered:

  - Listed Equity (EQ)
  - Corporate Bonds (CB)
  - Corporate Loans (LBK)

Other asset types may be covered in the future.

## Work flows

The repository is structured in a way that provides dedicated work flows
for each of the risk types and some of the asset types.

### Transition Risk for listed equity and corporate bonds

#### Methodological notes

The work flow for listed equity and corporate bonds mainly focuses on
calculating market risk of portfolios under a late and sudden policy
shock that initiates a transition of the real economy from a business as
usual pathway toward a sustainable pathway that is aligned with a given
climate target.

The process to derive potential losses for the companies in the
portfolio and, by extension, for the portfolio itself, is in essence
threefold:

1 - Define the impact of the policy shock on production pathways in the
real economy over time. This gives production pathways for all affected
companies under different scenarios.

2 - Calculate the impact of real economy production scenarios on future
profits for all companies in the portfolio. This involves using the
production pathways from step 1 and plugging them into profit
calculations that account for developments of prices and costs under
each of the scenarios over time, then discount these future profits.

3 - Calculate the net present values (NPVs) of companies in the analysis
based on discounted future profits from step 2. Comparing the NPVs of
companies between scenarios gives us potential changes in company value
due the the policy shock.

The exact calculations can be found in the working paper **LINK to
working paper**

##### Assumptions

There are still a few data gaps, especially on the price and cost
structure of certain markets and companies. Here we are using
placeholder data. The users can set these inputs themselves, in case
they have data that they feel is more accurate for their context of
analysis.

Beyond that, we assume that profits are paid out as dividends and that
these correspond tp the market value according to some proportionality
factor.

#### Required inputs

In order to calculate the impacts from transition risk on a corporate
loan book, the user needs to provide as input:

**Parameter settings** to locate the project in
st\_project\_settings.yml

**Model parameters** to use in the financial valuation and to select
scenarios in model\_parameters.yml

**PACTA for investors output** at the company or portfolio level.

Information on **initial probabilities of default,** ideally on the
individual loan level. If this is unfeasible, it needs to be provided on
the technology level based on relevant market data and be merged with
the input file accordingly. **OPEN**

Information on **LGDs,** ideally on the loan level, but alternatively
market averages for each of the technologies. **OPEN**

**Sector exposures** to the relevant sectors of the analysis. This can
be obtained by following the steps in the script calc\_loan\_book.R in
this repository.

Specifications of **transition scenarios** for which to calculate shocks
and impacts on the given portfolio. These are provided via a file,
transition\_scenario\_input.csv.

The expected columns are:

  - scenario\_name
  - year\_of\_shock
  - overshoot\_method
  - duration\_of\_shock
  - use\_prod\_forecasts\_ls
  - use\_prod\_forecasts\_baseline
  - … (any technology in use in the analysis)

**Capacity factors** for the power sector, to transform power capacity
into power production. The legacy live version uses the file
capacity\_factors\_WEO\_2017.csv.

Output Columns:

  - technology (usually the PACTA sectors with production pathways)
  - capacity\_factor (a number indicating the average power generation
    per unit of capacity)
  - scenario\_geography (geographic region for which the capacity
    factors apply)

A new version allows using more up to date capacity factors derived from
IEA WEO numbers. This is required in order to calculate profits on
actual quantities produced, not theoretical capacities.

Output Columns:

  - scenario (transition scenario for which the capacity factors apply)
  - scenario\_geography (geographic region for which the capacity
    factors apply)
  - technology (usually the PACTA sectors with production pathways)
  - year (temporal change of capacity factors)
  - capacity\_factor (a number indicating the average power generation
    per unit of capacity)

**Scenario data** that covers production road maps for all technologies
that are to be analysed up until 2040. The scenario data need to cover
those road maps for the scenarios selected in the parameter files and
should follow the structure found in Scenarios\_AnalysisInput\_YYYY.csv.
This data is used to extrapolate the production trajectories for
companies beyond the PACTA time frame.

**Price data** trajectories found in the file prices\_data\_YYYYQQ.csv,
which contains projections of market prices per technology from the
start data of the analysis until the year 2040. This is another required
input to obtain profits. This will likely change in the future.

**Net profit margins** are loaded via the model\_parameters.yml file up
until now. This may change in the future.

**Excluded companies** in case a specific company-technology combination
cannot be covered by the model either due to lack of data on the company
side or because of other input data gaps, the combination of
caompany\_name and technology can be specified to be excluded from risk
calculations. Such situations can be relevant for example when a company
conducts business predominantly in one country that follows transition
policies are laws that are not covered in the regionality of the
business as usual and target scenarios. For instance, Germany is phasing
out nuclear power in the first half of the 2020s. European and/or global
scenarios do not capture this, although it can be seen in the relevant
companies’ capex plans. Such situations can lead to unintended behaviour
of the model and as such it seems reasonable not to shock this part of
the business, especially since we usually model shocks happening beyond
this time frame.

**Further notes on input data:**

  - It is of particular importance that the start year of the analysis
    is available in the PACTA results, in the scenario data and in the
    price data. If either data set starts after the start year of the
    analysis, the work flow will not work.
  - Equally, the scenarios selected for the analysis must be given in
    the PACTA results as well as in the scenario data and the price
    data.

#### Steps

  - Initialize project
  - Load project parameters
  - Load all project input data
  - Wrangle input data
  - Initialise empty results object
  - Loop over transition scenarios (as defined in
    transition\_scenarios\_input.csv), calculating and row-binding
    results to the results object. Calculation entails:
      - plucking a specific scenario definition from the transition
        scenarios to obtain model inputs (1)
      - calculating the corresponding price trajectories (2)
      - calculating the corresponding production trajectories on the
        company level (3)
      - joining the price trajectories to the production trajectories
        (3)
      - joining net profit margins to the production and price
        trajectories (3)
      - calculating future net profits for the entire analysis time
        frame (3)
      - calculate discounted net profits for the entire analysis time
        frame, using the DCF model (3)
      - plucking the portfolio share (plan\_carsten) of each company
        from the pacta data input (4)
      - calculating value changes and percentage losses per transition
        scenario, by combining company level DCF output with portfolio
        share of the corresponding holdings (6)
  - write company-tech level results to initialized project folder
  - write portfolio-tech level results to initialized project folder
  - save graphs to initialized project folder **OPEN**

#### Output files

Describe structure and interpretation of output files **OPEN**

### Transition Risk for corporate loan books

#### Methodological notes

Calculating the transition risk for corporate loan book related to a
sudden policy shock differs from the calculation of such a shock for
listed equity. The value change of loans is not captured well simply by
applying a DCF model. Dedicated credit risk models are required instead.
Specifically, we want to quantify the impact of a climate transition
shock on the probabilities of default (PD) of the loans at hand. This is
a common risk metric for loans and a change in PDs is therefore expected
to be of more use to anyone applying this software, then a shock based
on changes in market value of the issuing company. The PD can then be
used to derive impacts on the value of a loan book, by plugging the
changed PD into an expected loss (EL) calculation, which is in its
fundamental form defined as:

EL = PD \* LGD \* EAD

… with LGD being the loss given default (percentage loss) and EAD the
exposure at default (value of the loan in absolute currency in the loan
book at hand).

In our case, we aim at using a structural model. Data constraints are a
limiting factor with regard to using company level information that
would normally go into a case by case rating of credit worthiness.
Hence, we use the Merton model, which requires as inputs the asset value
in t = 0 (A\_0) and the debt value (D) of the company at hand. The debt
value is assumed constant over time, the asset value in t = 0 is derived
as A\_0 = E\_0 (equity value in t = 0) \* D. We further need the
volatility of the equity value, the risk free rate and the term
structure as inputs.

The outcome of the Merton model is a PD contingent to the inputs
described above. For us, the value of interest is not an absolute PD as
a direct outcome of the model, but rather how the PD changes between a
baseline scenario and a late and sudden scenario that represents a
disorderly transition after a policy shock. We therefore keep all inputs
equal except the valuations under those two scenarios. We model a PD
under the baseline scenario and another PD under a late and sudden
scenario and calculate the change in PDs for the given set of
parameters. This will result in a change in PDs that does not reflect
the magnitude of the equity and debt values and can therefore be applied
as a shock factor on PDs that banks may have calculated with a different
model themselves:

PD\_change = PD\_late\_sudden - PD\_baseline

PD\_change is a value in percentage points and can be used to calculate
the change in expected loss due to the late and sudden transition to a
less carbon-intensive economy. For the expected loss calculation, this
would mean we calculate:

EL\_late\_sudden = (PD\_0 + PD\_change) \* LGD \* EAD

where PD\_0 is the probability of default for a loan prior to the shock
as provided by the user. If no such data is available on the loan level,
it may be an option to use sector or technology level averages as
starting points.

##### Assumptions

  - For E\_0, we use the discounted values derived from the DCF model we
    use to calculate changes in market value. This builds on the capex
    plans of the companies in the calculation of the future profits that
    are used in the DCF model. We therefore get different company values
    for the business as usual and the late and sudden cases, which we
    can use to differentiate the scenarios in the credit risk
    calculation.
  - For D, we currently use sectoral equity-debt ratios and thus derive
    the value from the equity value of the business as usual scenario.
    The current values in use are mock values for test purposes that
    need to be replaced with empirical values. This may become a user
    input option in the future. **OPEN**
  - For volatility, we currently use a fixed value of 0.2. The current
    value in use is a mock value for test purposes that needs to be
    replaced with empirical values. This may become a user input option
    in the future. **OPEN**
  - The risk free rate is currently set at 0.05. The current value in
    use is a mock value for test purposes that needs to be replaced with
    empirical values. We will switch to a default value that uses the
    yield of US Treasury 10 year bonds. This may become a user input
    option in the future. **OPEN**
  - In each transition scenario, the PD changes are calculated in two
    ways. Once, we calculate for PD changes for loans of 1 year maturity
    only, but on an annual basis until the end of the analysis period.
    In this case, the input equity values will always just consider one
    year of discounted future profits - the one related to the year that
    the specific maturity refers to. Secondly, we calculate the PD
    change for maturities between 1 and 5 years in yearly steps
    considering the entire time frame from the start year of the
    analysis. This means that the full NPV is considered in calculating
    the starting equity values. This calculation can be used for the
    expected loss calculation, so that the term structure of a portfolio
    is adequately rfelected. Maturities larger than 5 years are grouped
    in the 5 year maturity bucket.
  - On a technical level, we use implementation of the Merton model from
    the CreditRisk R package, which is described in more detail here:
    <https://cran.r-project.org/web/packages/CreditRisk/CreditRisk.pdf>

#### Required inputs

In order to calculate the impacts from transition risk on a corporate
loan book, the user needs to provide as input:

**Parameter settings** to locate the project in
st\_project\_settings.yml

**Model parameters** to use in the financial valuation and to select
scenarios in model\_parameters.yml

**PACTA for banks output** at the company level, enriched with **company
level loan shares.** This can be created by following the steps in the
script calc\_loan\_book.R in this repository. For more information on
how to use PACTA for banks, consult:
<https://2degreesinvesting.github.io/r2dii.match/> and
<https://2degreesinvesting.github.io/r2dii.analysis/index.html>

Information on **initial probabilities of default,** ideally on the
individual loan level. If this is unfeasible, it needs to be provided on
the technology level based on relevant market data and be merged with
the input file accordingly. **OPEN**

Information on **LGDs,** ideally on the loan level, but alternatively
market averages for each of the technologies. **OPEN**

**Sector exposures** to the relevant sectors of the analysis. This can
be obtained by following the steps in the script calc\_loan\_book.R in
this repository.

Specifications of **transition scenarios** for which to calculate shocks
and impacts on the given portfolio. These are provided via a file,
transition\_scenario\_input.csv

The expected columns are:

  - scenario\_name
  - year\_of\_shock
  - overshoot\_method
  - duration\_of\_shock
  - use\_prod\_forecasts\_ls
  - use\_prod\_forecasts\_baseline
  - … (any technology in use in the analysis)

**Capacity factors** for the power sector, to transform power capacity
into power production. The legacy live version uses the file
capacity\_factors\_WEO\_2017.csv.

Output Columns:

  - technology (usually the PACTA sectors with production pathways)
  - capacity\_factor (a number indicating the average power generation
    per unit of capacity)
  - scenario\_geography (geographic region for which the capacity
    factors apply)

A new version allows using more up to date capacity factors derived from
IEA WEO numbers. This is required in order to calculate profits on
actual quantities produced, not theoretical capacities.

Output Columns:

  - scenario (transition scenario for which the capacity factors apply)
  - scenario\_geography (geographic region for which the capacity
    factors apply)
  - technology (usually the PACTA sectors with production pathways)
  - year (temporal change of capacity factors)
  - capacity\_factor (a number indicating the average power generation
    per unit of capacity)

**Scenario data** that covers production road maps for all technologies
that are to be analysed up until 2040. The scenario data need to cover
those road maps for the scenarios selected in the parameter files and
should follow the structure found in Scenarios\_AnalysisInput\_YYYY.csv.
This data is used to extrapolate the production trajectories for
companies beyond the PACTA time frame.

**Price data** trajectories found in the file prices\_data\_YYYYQQ.csv,
which contains projections of market prices per technology from the
start data of the analysis until the year 2040. This is another required
input to obtain profits. This will likely change in the future.

**Net profit margins** are loaded via the model\_parameters.yml file up
until now. This may change in the future.

**Excluded companies** in case a specific company-technology combination
cannot be covered by the model either due to lack of data on the company
side or because of other input data gaps, the combination of
caompany\_name and technology can be specified to be excluded from risk
calculations. Such situations can be relevant for example when a company
conducts business predominantly in one country that follows transition
policies are laws that are not covered in the regionality of the
business as usual and target scenarios. For instance, Germany is phasing
out nuclear power in the first half of the 2020s. European and/or global
scenarios do not capture this, although it can be seen in the relevant
companies’ capex plans. Such situations can lead to unintended behaviour
of the model and as such it seems reasonable not to shock this part of
the business, especially since we usually model shocks happening beyond
this time frame.

**Further notes on input data:**

  - It is of particular importance that the start year of the analysis
    is available in the PACTA results, in the scenario data and in the
    price data. If either data set starts after the start year of the
    analysis, the work flow will not work.
  - Equally, the scenarios selected for the analysis must be given in
    the PACTA results as well as in the scenario data and the price
    data.

#### Steps

  - Initialize project
  - Load project parameters
  - Load all project input data
  - Wrangle input data
  - Initialise empty results object
  - Loop over transition scenarios (as defined in
    transition\_scenarios\_input.csv), calculating and row-binding
    results to the results object. Calculation entails:
      - plucking a specific scenario definition from the transition
        scenarios to obtain model inputs (1)
      - calculating the corresponding price trajectories (2)
      - calculating the corresponding production trajectories on the
        company level (3)
      - joining the price trajectories to the production trajectories
        (3)
      - joining net profit margins to the production and price
        trajectories (3)
      - calculating future net profits for the entire analysis time
        frame (3)
      - calculate discounted net profits for the entire analysis time
        frame, using the DCF model (3)
      - plucking the portfolio share (plan\_carsten) of each company
        from the pacta data input (4)
      - calculating value changes and percentage losses per transition
        scenario, by combining company level DCF output with portfolio
        share of the corresponding holdings (6)
      - Calculate annual PD changes per company-technology for 1 year
        maturities (7)
      - Calculate overall PD changes per company-technology and maturity
        (7)
      - Calculate changes in the expected loss based on overall PD
        changes (7)
  - write company-tech level results to initialized project folder
  - write portfolio-tech level results to initialized project folder
  - save graphs to initialized project folder **OPEN**

#### Output files

Describe structure and interpretation of output files **OPEN**

### A note on PACTA COP projects on the 2DII platform

Whenever a new PACTA COP project starts on the transition monitor that
requires the calculation of a transition risk stress test, a parameter
file has to be added to the `./model_parameters/` directory. Usually,
the existing files only have to be adapted to reflect the tag of the new
project. When in doubt, please contact Jacob.

### Litigation Risk

#### Context

Past and future contributions by companies to climate change may become
financial liabilities, if current or future lawsuits succeed in holding
them accountable for damages caused by climate change or additional
costs caused by increased required mitigation and adaptation efforts. It
is not yet absolutely certain how the climate costs caused by companies
will be measured and consequently translated into damages.

Options include accounting of historical responsibilities and/or
forward-looking approaches that use the misalignment of the production
plans of a company with its climate targets (e.g. based on the “market
share approach” used in PACTA) as the foundation to deduce company level
contribution to climate costs and corresponding liability risks.

We provide suggestions for such calculations based on three different
models. These are the Historical Emissions Responsibility model, the
Social Cost of Carbon model and the Carbon Delta Damages model.
Litigation scenarios will then depend on which (combination of) models
are applied to calculate costs and a set of global and model-specifc
parameters to be described below.

#### Models

All models and their rationale are explained in more detail in a paper
that 2DII will publish in 2021. Note that the code related to climate
litigation risk in its current state replicates the calculations put
forward in the publication. It will undergo some refactoring to ensure
better usability with other data sets. The input data is stored in
another repository, `r2dii.stress.test.data`.

##### Social cost of carbon (SCC) model

In this forward looking approach, we calculate the emissions associated
with the misalignment of the production plans of companies with the
allowed emissions per company relative to a target scenario. This makes
use of the (technology-)company level PACTA results to determine the
allowed company level targets and their yearly emissions overshoot
relative to those targets in tons of CO2 for the following five years.

Each excess ton of CO2 is priced with the social cost of carbon and a
settlement multiplier accounts for residual impacts that may reduce an
actual amount paid in a settlement (such factors may include uncertainty
about the attribution of damages, jurisdictional factors, etc.).

##### Historical emissions responsibilities (HER) model

Another possible way to to quantify the climate-related costs produced
by a company is by using backward looking data that covers the overall
emissions some of the most climate relevant companies have produced in
the past. Heede et al. (2017) have estimated such emissions in the
Carbon Majors study. The data is updated roughly on a yearly basis on
the Carbon Majors website. The amount of historical CO2 emitted per
company could then be priced, e.g. with a social cost of carbon or with
some share of the overall responsibility for the impacts of climate
change to date. Overall, regardless of which way of pricing is chosen,
the cost a company faces in this model depends on how much CO2 emissions
it is historically liable for, while the price is an exogenous input to
the model.

We calculate the costs once by applying a price of 40 USD / tCO2 for the
historically emitted CO2 of each company, based on the cost used in the
SCC model.

A second approach is to calculate the share of the CO2 emissions for
each of these companies relative to the overall amount of CO2 emitted in
the past to determine their relative contributions to global warming
thus far. This contribution is then multiplied with a proxy for overall
economic costs up until the start year of the analysis. We use an
estimate by Morgan Stanley (2019) to obtain global climate change
related costs of USD 650 billion for the past three years and extend
this time frame to 10 years for the analysis, roughly yielding overall
costs of USD 2 trillion.

The exact magnitude of these costs may be subject to discussions, as is
the SCC, which is why we allow the user to set these costs as a
parameter in the scenarios.

##### Carbon delta damages (CDD) model

In this forward looking approach, we calculate the emissions associated
with the misalignment of the production plans of companies with the
allowed emissions per company relative to a target scenario. This makes
use of the (technology-)company level PACTA results to determine the
allowed company level targets and their yearly emissions overshoot
relative to those targets in tons of CO2 for the following five years.

This overshoot is compared to the total remaining CO2 budgets (based on
the IPCC Special Report: Global Warming of 1.5°C, 2019) to move from the
target scenario to some more adverse climate scenario (e.g. from a 1.5°C
outcome to a 2°C or to a 4°C outcome). In this way, we get the relative
contribution of a company to an increased global warming from 1.5°C to
another temperature outcome.

This relative contribution to the additional increase in global
temperatures is then used to calculate the company level added overall
economic costs due to this additional warming. We use the overall costs
for such an increase in global temperatures from Burke et al. (2018).

This leads us to a measurement of costs produced at the company level
and multiplying with a settlement factor again accounts for residual
uncertainty on multiple levels.

##### Notes on capping the liabilities in the forward looking models (CDD/SCC):

**Implied upper limit of liabilities**

In the CDD model, both the additional carbon budget and the additional
economic costs of moving from a target scenario to a more adverse
scenario are capped at the outcome of that adverse scenario.

This implies that a company cannot be held accountable for contributing
to an even higher temperature outcome, should it produce more emissions
than expected even under the adverse scenario. Until there is a way to
calculate such an impact with a continuous outcome endogenously, this
will likely be the case to some degree - regardless of what the most
adverse scenario is defined as.

This is not technically necessary in the SCC model, as it does not
directly rely on carbon budgets of adverse scenarios or global costs
associated with those scenarios.

Nevertheless, calculations of SCC liabilities are currently also capped
at the upper end at level of the carbon budget of the adverse scenario.
This ensures comparability between the scenarios, but may be an
assumption the user could handle differently when applying the SCC
model.

**Lower limit of liabilities**

We also limit the company level liability on the lower end. If a company
emits less CO2 then it could based on the market share approach in the
target scenario, it will simply have zero liability.

#### Scenarios

In the basic work flow, we devise a few scenarios based on the SCC and
CDD methods to calculate company level carbon liabilities and deduce
litigation risk as an absolute value in USD. The carbon liabilities are
also calculated as a relative value compared to the yearly operating
profit (EBIT) of each analyzed company.

#### Model assumptions

**Company level costs/liabilties:**

  - For the forward looking analyses, we consider an overall time frame
    from 2019 to 2040 (plus a terminal value to capture the more distant
    future).

  - We use B2DS, SDS and CPS scenario data provided by the IEA to define
    target and adverse scenarios. In principle, any scenario that is
    compatible with PACTA would work with this model.

  - We assume the B2DS scenario corresponds roughly to a 1.5°C
    temperature increase outcome, at a probability of roughly 66%. This
    is our target scenario, against which litigation risk can be built
    up.

  - We assume the adverse scenarios can either be the SDS scenario,
    which we roughly equate with a 2°C temperature rise, and the CPS
    scenario, which we use to model a pathway towards a 4°C increase in
    global temperature - each at a probability of around 66%.

  - We acknowledge that this is a broad simplification.

  - We use PACTA to determine the yearly emissions overshoot by company,
    based on climate alignment metrics.

  - For the time being, we use 2019-2023 data to calculate the
    overshoot, but ideally this would be a calculation that starts from
    the year 2015, in order to capture the contribution since the Paris
    Agreement was ratified and climate targets were agreed upon.

  - The social cost of carbon is still a hotly debated number. We will
    use 20 and 40 USD/tCO2, both of which are likely conservative
    estimates.

  - The total CO2 budgets required to move from a 1.5°C scenario to
    either a 2°C or a 4°C scenario are taken from the IPCC Special
    Report: Global Warming of 1.5°C (2019).

  - Finally, the economic costs incurred by moving to one of these more
    adverse scenarios, is based on calculations from Burke et
    al. (2018).

  - For the historical responsibility model, we use past emissions data
    from Heede et al. (2017). This could in theory be substituted for
    another data set, possibly more comprehensive in terms of companies
    covered. Historical emissions in million tCO2 are based on scope 1
    plus scope 3 emissions.

  - Damages in the historical model are either priced with a fixed cost
    for a ton of emitted CO2 (for comparability with the SCC model, we
    use USD 40 / tCO2 here as well).

  - Alternatively, they are calculated by multiplying the historical
    share of emissions of the company with a proxy for global costs of
    climate change thus far.

  - Global costs of climate change are approximated based on a study by
    Morgan Stanley (2019) that puts the cost for the previous three
    years at USD 650 billion. Extending this time frame to 10 years
    leads to roughly USD 2 trillion in climate change costs thus far.
    The length and yearly costs are parameters the user can adjust.

  - We assume the settlement factor is 2.7%, using the Tobacco Master
    Settlement Agreement as a historical indicator on which to base this
    assumption.

  - The settlement factor is treated as a catch all variable that
    acknowledges that the costs we calculate per company are likely not
    going to have to be repaid in full - at least not on average. In
    general, we assume that the more indirect an impact is, the likelier
    a settlement factor closer to 0 percent of the liability than to 100
    percent. The reasons for that are:
    
      - National jurisdictions are likely going to treat such costs very
        differently.
      - The attribution of responsibility for climate damages is still
        somewhat imprecise (indirect impact). As such, the analytic
        approach might be challenged in court. However, legal
        uncertainties are beyond the scope of the analysis at this
        point.
      - When using forward looking production data, there is no
        guarantee that a production site will emit the exact emissions
        related to its production plans.
      - If companies change strategies within the time frame analyzed,
        production profiles may materialize differently. However,
        assumptions around business planning are static in this model.
        Of course, such strategic changes are to some degree a desired
        outcome and this paper aims to help companies and investors
        identify options for such changes before the liability risk
        materializes.
      - Externally sourced data on the carbon budgets for each of the
        scenarios and the associated global economic damages also have a
        degree of uncertainty.

**Company level impact on share prices:**

  - We employ a simple DCF model for the company valuation.
  - For each scenario, we use this to calculate the NPV of the company
    in case of a litigation event at a given point in time in the future
    and under a baseline scenario without any litigation.
  - These changes of value in the litigation scenario relative to the
    baseline scenario determine the value change of the company.
  - We assume the EBIT/dividends of the company grows at a yearly rate
    of 2%.
  - EBIT and dividends are henceforth used interchangeably, implying
    that a company pays out its full profits as dividends. This is, of
    course, a simplification.
  - In the shock year, the company level total amount of litigation
    liability is subtracted from the dividends of that year as a one
    time event. After that, the company continues to grow its dividends
    as before (This assumption could follow another dynamic in future
    version of the model).
  - We further assume a discount factor of 3%.
  - 10% of the NPV is included in the terminal value factor.

**Portfolio level impact of a litigation event:**

  - We use the company level shocks in percentage value loss of the
    equity to estimate the impact on a portfolio that holds stocks in
    that company. We assume this shock is translated to the value of a
    portfolio proportionately by the value share this stock represents
    in the portfolio.
  - This allows us to use the PACTA analysis results for a portfolio to
    obtain the value share of the equity of a company as part of the
    portfolio. We use this share and apply the shocks to the equity
    value of the company. This can be done for all relevant companies in
    the portfolio, making use of the plan\_carsten metric as the value
    share (aggregated from the technology-company level to company level
    where necessary).

**The outcome indicators are:**

  - xyz\_liability: The cost of climate damages produced by the company
    at hand in USD (xyz stands for the model).
  - xyz\_liability\_perc\_ebit: The climate liability of a company as a
    percentage of their EBIT in the start year of the analysis(xyz
    stands for the model). This allows for a comparison between
    companies.
  - percentage\_value\_change: The impact of the litigation shock on the
    total value of the listed equity related to the company.
  - value\_change: The expected loss of value in a portfolio that
    contains shares in the equity of one of the analyzed companies - due
    to climate litigation.

##### User settings

The user can set several parameters in order to reflect their beliefs
about litigation events in a scenario. This is particularly helpful to
understand how such beliefs compare to our default settings and to
understand the sensitivities of the model.

In general, results for multiple scenarios can be calculated at a time.
Some of the parameters are set at a global level though, whereas some
other ones are set at the scenario level.

**Global parameters include:**

  - *chance\_within\_target:* Chance to stay within temperature target
    (this selects the scenario-to-scenario CO2 budgets).
  - *start\_year* of the analysis (explicit forecast period).
  - *end\_year* of the analysis (explicit forecast period).
  - *years\_to\_litigation\_event* after the start year.
  - *settlement\_factor* to apply to the litigation value to be paid out
    in the DCF model. This allows for easy adjustments of the expected
    percentage of damages paid out.
  - *growth\_rate* of the dividends per year in the DCF.
  - *discount\_rate* of the dividends per year in the DCF.
  - *percentage\_in\_terminal\_value* to account for future years after
    explicit forecast period in the NPV of a company based on the DCF.
  - *backwards\_years:* The length of the time frame in years for which
    to extrapolate past global costs of climate change.
  - *reset\_post\_settlement:* How to proceed with company income
    projections after a settlement takes place. “no” will lead to
    continued growth according to the growth rate and implies a new
    buildup of litigation risk. “start” resets the dividends to the EBIT
    value in the first year of the analysis and resumes growth according
    to the growth rate from there. This is a rough proxy for the company
    following allowed targets post settlement. Further option needs to
    be added to properly reflect a company that adjusts to the emissions
    levels it is granted based on the target scenario and the market
    share approach. Currently, this also happens for the HER model.

**Scenario level parameters include:**

  - *litigation\_scenario:* Name of the scenario, currently following
    the convention MODELTYPE\_SETTLEMENT, with model type either being
    CDD or SCC\_XY, XY indicating the USD value of the social cost of
    carbon applied. Settlement indicates the rule applied to account for
    uncertainties in the amount of the costs caused that will have to be
    paid out, e. g. TMS.
  - *model:* Name of the model to apply in the calculation of the
    company level climate liabilities. Currently the choice is between
    “CDD” and “SCC”.
  - *exp\_share\_damages\_paid:* Share of the cost caused by a company
    likely to be paid out in settlement, currently defaults to 0.027
    which reflects the Tobacco Master Settlement Agreement (TMS).
  - *scc:* Social Cost of Carbon, price of one ton of excess CO2
    emissions in USD.
  - *timeframe\_emissions\_overshoot:* Number of years considered in the
    calculation of the production overshoot, based on which we calculate
    the annual excess CO2 emissions. As per standard PACTA settings,
    this defaults to 5 years.
  - *past\_yearly\_costs\_usd:* Annual global costs of climate change in
    USD thus far. Defaults to the value from Morgan Stanley (2019). A
    multiple of this cost and number of years to consider in the HER
    model is used to approximate the overall global costs of climate
    change companies can historically be held accountable for.

#### Data input requirements

**litigation\_risk\_scenarios:** A csv file with columns
“litigation\_scenario”, “model”, “exp\_share\_damages\_paid”, “scc”,
“timeframe\_emissions\_overshoot”, “past\_yearly\_costs\_usd” of data
type “ccdddd”. This table contains the scenario level parameters used to
describe different scenarios for which to run the analysis. One line
corresponds to one scenario being run. There is no limit on how many
scenarios can be added to this file. Scenarios can be defined by the
user and do not necessarily follow an external ground truth. It is
recommended though to set the parameters in line with at least
imaginably realistic values based on the literature and/or worst case
settings in case this is used for stress testing.

**company\_emissions\_data\_input:** For each asset type, an rda file
with the following columns:

  - investor\_name = “c”
  - portfolio\_name = “c”
  - company\_name = “c”
  - id = “c”/“d” (depending on asset type)
  - scenario = “c”
  - allocation = “c”
  - asset\_type = “c”
  - scenario\_geography = “c”
  - equity\_market = “c”
  - year = “d”
  - financial\_sector = “c”
  - ald\_sector = “c”
  - technology = “c”
  - plan\_tech\_prod = “d”
  - plan\_emission\_factor = “d”
  - scen\_tech\_prod = “d”
  - scen\_emission\_factor = “d”
  - plan\_carsten = “d”

These files are sourced from PACTA projects and correspond to company
level PACTA result files (for P4I). They gives us the production
forecasts per company, as well as the allowed production trajectories
for each scenario per company. Together with the planned average
emission factors and the allowed emission factors by scenario, we deduce
the emissions overshoot based on this. We also get the exposure of the
portfolio by company and technology from this file.

**sector\_eposures:** The sectorial exposures of the portfolio by asset
type:

  - investor\_name = “c”
  - portfolio\_name = “c”
  - company\_name = “c”
  - asset\_type = “c”
  - financial\_sector = “c”
  - valid\_input = “l”
  - valid\_value\_usd = “d”
  - asset\_value\_usd = “d”
  - portfolio\_value\_usd = “d”

This file is sourced from PACTA projects and corresponds to sector level
portfolio overview files, based on the processed inputs of a P4I
project.

**company\_ebit\_data\_input:** A csv file with the following columns:

  - “company\_name”
  - "isin
  - “ebit”
  - "currency
  - “sector”

… of type “ccdcc”. The data is based on three input data sets. The main
data set is an extract from Bloomberg (can be any other financial data
set) that contains the columns:

  - Name = “c”
  - ISIN = “c”
  - Curncy = “c”
  - Revenue = “d”
  - EBIT = “d”

This is merged with exchange rate infos from a currencies data set by
joining on the 3 digit currency acronym. Additionally, we merge sector
information based on the security\_financial\_data file by isin. We use
this data to classify the main sector a company is operating in and to
obtain the EBIT in the start year of the analysis.

**carbon\_delta\_plus\_damages:** A csv file with the following columns
(sources or comments):

  - “start\_scenario” (actual target scenario, change wording)
  - “target\_scenario” (more adverse scenario, change wording)
  - “target\_temp\_rise” (temp rise over pre-industrial levels in
    adverse scenario, change wording)
  - “likelihood\_stay\_within\_target” (likelihood to stay within
    temperature target of adverse scenario with the given carbon budget)
  - “delta\_carbon\_budget” (based on IPCC SR: Gloabl Warming of 1.5°C,
    2019)
  - “delta\_econ\_damages\_usd” (based on calculations by Burke et al.,
    2018)

… of type “ccdddd”. The table provides information about the total
carbon budgets in tons of CO2, that correspond to moving from one
scenario outcome to another. This can be understood as the carbon budget
of moving from a target scenario to a more adverse scenario. Such
budgets are needed for the attribution of costs in the CDD model. The
table also provides information on the global magnitude of costs (in
mitigation and adaptation) related to moving from one scenario to a more
adverse scenario. This is required information to deduce the company
level cost based on the attributed contribution the company has in
moving to that more adverse scenario.

The table has one row each to describe the corresponding carbon budget
and economic costs of moving from a target scenario to a more adverse
scenario. This means that in principle there is no limit to the number
of rows. It is required though, that information for the scenarios
(target and adverse, possibly in-between) used in the analysis (as
selected by the parameters) be given.

**historical\_emissions\_heede\_2017:** A csv file with the following
columns (sources or comments):

  - “company\_name”
  - “scope\_1”
  - “scope\_3”
  - “scope\_1\_plus\_3”
  - “share\_global\_industrial\_ghg”

All values are based on Heede et al. (2017).

#### Steps in calculating company level liabilities

The company level emissions overshoot is calculated by multiplying the
5-year production plans (current\_production) on the technology-company
level with the average emissions factors for each technology (avg\_ef).
Similarly, multiplying fair share production levels
(allowed\_production\_SCENARIO) for the company with the average
emissions factors give the allowed emissions a company is granted to
stay within its budget given a target scenario. Any actual emissions
above such a budget are considered a CO2 overshoot relative to that
scenario. Accordingly we get overshoots relative to multiple outcomes:

  - overshoot\_actual\_b2ds
  - overshoot\_actual\_sds
  - overshoot\_actual\_cps

For the SCC approach, the company level liability (scc\_liability) is
now calculated by multiplying the average yearly overshoot over the
target scenario (overshoot\_actual\_b2ds/5) with the SCC value in
USD/tCO2 defined for the scenario and with the expected share of the
damages paid out in a settlement (exp\_share\_damages\_paid). The share
of this liability relative to the annual EBIT of a company is calculated
using:

scc\_liability\_perc\_ebit = scc\_liability / ebit

For the CDD approach, the company level contribution to moving from the
target scenario to a more adverse scenario is first determined as:

overshoot\_delta\_TARGET\_ADVERSE /
delta\_carbon\_budget\_TARGET\_ADVERSE

… both of which are in units of tCO2. This share of contribution is then
then multiplied with the overall cost of reaching that adverse outcome
compared to the target scenario (delta\_carbon\_damage\_TARGET\_ADVERSE)
to obtain damage\_ADVERSE\_TARGET. This is the estimated cost caused by
the misalignment of a company in USD. Finally, we again multiply with
the expected share of the damages paid out in a settlement
(exp\_share\_damages\_paid) to get cdd\_liability. The share of this
liability relative to the annual revenue of a company is calculated
using:

cdd\_liability\_perc\_ebit = cdd\_liability / ebit

For the HER approach, we either use the overall company level historical
CO2 emissions and multiply this with a price per tCO2, similar to the
SCC approach. Or we determine the past share of the contribution to
current levels of climate change by company and multiply that share with
a proxy for total costs of climate change thus far (as explained above).
The choice between these methods is made by checking the scenario level
parameters for the HER model. If the “scc” entry is positive AND the
yearly costs are zero, the historical emissions are multiplied with an
SCC price. If the reverse is true, the historical share of emissions and
global costs are used. If neither is true, the scenario definition is
faulty and the calculation is skipped.

Finally, we again multiply with the expected share of the damages paid
out in a settlement (exp\_share\_damages\_paid) to get her\_liability.
The share of this liability relative to the annual profits of a company
is calculated using:

her\_liability\_perc\_ebit = her\_liability / ebit

#### Steps in calculating impact on company share prices

Once we have calculated a carbon liability or a social cost the company
has caused with regard to climate change, we want to determine in a next
step how a litigation event could impact the share price of the company.
This is an essential step towards understanding the potential financial
damage caused by a company building up litigation risk because it allows
us to understand how such a risk would propagate to the value of
portfolios, should it materialize.

We employ a simple DCF model to calculate that impact with the following
default assumptions:

  - start year of the forecast: 2019
  - end year of the forecast: 2040
  - years until litigation event: 5
  - settlement factor (to vary scenarios of share of damages paid out):
    1
  - growth rate of annual dividends: 0.02
  - discount rate: 0.03
  - percentage of the NPV in the terminal value: 0.1

We use the EBIT of 2019 of the company as the starting point to apply
the DCF. First we get the future net cash flows by applying the growth
rate. We then subtract the settlement amount calculated in the first
part of the exercise from the EBIT in the year of the litigation event.

**NOTE:** we are currently applying the years\_until\_litigation factor
to HER calculations as well. This may have to be changed, if it is
decided these liabilities should not grow anymore.

After the litigation event, the net dividends of the company continue to
grow at the indicated annual growth rate. At the moment, the level of
dividends at that point in time is not reset, which implies the company
continues to build up litigation risk after the settlement. If it is a
requirement in the future to reflect that companies follow their targets
after the litigation, this mechanism still has to be implemented.

We then discount the dividends of each year (once with litigation, once
without), to obtain the net present values for a baseline scenario
without litigation and for the litigation scenario as defined by the
parameters.

These NPVs are used to compute the percentage value change between the
scenarios. Such value changes can, in certain cases, be more than 100%,
which would translate into the bankruptcy of the company. The model may
not reflect all mechanisms at play in determining such an outcome.

#### Steps in calculating impact on portfolios of shareholders

Ultimately, a large part of the cost to the companies produced by such a
settlement will be carried by the shareholders of the company. For that
reason, we need to be able to translate the equity impact from the share
price level to the portfolio level. We do this in a very straightforward
manner, using information generated from a PACTA analysis of a
portfolio. This analysis provides us with a variable that gives us the
value share of the overall portfolio for each company-technology
combination. If we aggregate this to the company level, we can deduce
the share of the portfolio value that this asset accounts for and thus
we know how large the exposure to the asset at risk is within the
portfolio (company\_exposure = plan\_carsten \* portfolio\_aum).

The expected loss on the portfolio level is then calculated as:

expected loss = share price change \* company exposure

By default, the module will estimate that impact for all relevant
companies in the portfolio at hand.

-----

[CONTRIBUTING](https://github.com/2DegreesInvesting/r2dii.climate.stress.test/blob/master/.github/CONTRIBUTING.md)