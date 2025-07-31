# The Biggest US LNG Exporter is Claiming a Massive Tax Credit for Using Its Cargo as an ‘Alternative’ Fuel

Data and [R](https://www.r-project.org/) code to reproduce the analysis underlying [this Jul. 31, 2025 Inside Climate News article](https://insideclimatenews.org/news/31072025/cheniere-energy-lng-claims-massive-alternative-fuel-tax-credit/), reporting on efforts by the company Cheniere Energy to obtain substantial domestic alternative fuel tax credits for its use of liquefied natural gas rather than diesel in its chartered LNG export tankers over the period 2018 to 2024, as noted in the company's [most recent 10-K filing](https://www.documentcloud.org/documents/26002768-cheniere-energy-fy-24-10k/?mode=document#document/p47/a2664714) to the U.S. Securities and Exchange Commission.

### Methodology/Code

Our estimates of fuel use and greenhouse gas emissions by Cheniere-chartered vessels in the laden leg of their export journeys from the Sabine Pass and Corpus Christi LNG terminals are based on the methodology outlined in [this 2023 scientific paper](https://pubs.acs.org/doi/abs/10.1021/acssuschemeng.3c04269) and as described in [this previous Inside Climate News analysis](https://github.com/InsideClimateNews/2025-04-us-lng-exports).

For the current analysis, we used journey distances recorded by the global trade analytics company [Kpler](https://www.kpler.com/) analyzed by [Data Desk](https://datadesk.eco/), which performs investigative research and analysis on the global oil and gas industry. Please contact [Louis Goddard](mailto:louis@datadesk.eco) at Data Desk for queries about this data.

For each laden journey, we assumed 1.5 days docked and 1 day maneuvering, plus an additional 1 day docked and 0.5 days maneuvering if there was a second destination. We converted the quantities of LNG fuel going to the vessels' main propulsion systems and generators to [diesel gallons equivalent](https://afdc.energy.gov/fuels/equivalency-methodology) (DGE) assuming 2,204.62 pounds of LNG per metric ton and 6.06 pounds of LNG per DGE. We then multiplied by \$0.50 to calculate the size of the claimed tax credit for these journeys.

The fuel use, emissions and tax credit calculations are described in the script `cheniere.R`. The results of these calculations, by year, are in the `processed_data` folder in the file `cheniere_lng_fuel_emissions_year.csv`. Key variables discussed in the story, after being summed across years:

-   `laden_emissions_co2_equivalent` Estimated total greenhouse gas emissions in metric tons of carbon dioxide equivalent from the actual LNG-fueled export journeys, including emissions of methane slipping uncombusted through the vessels' engines. To convert methane emissions to carbon dioxide equivalent, we multiplied by 82.5, representing the 20-year global warming potential for methane.

-   `laden_boil_off_dge` Diesel gallons equivalent (DGE) of the LNG boil-off directed to the vessels' main engines and generators.

-   `laden_credit` The value of the alternative fuel tax credits corresponding to `laden_boil_off_dge`.

-   `laden_dge_total_co2_emissions` Carbon dioxide emissions in metric tons from the combustion of `laden_boil_off_dge`, plus estimated emissions of carbon dioxide from the vessels' gas combustion units (GCUs), in the hypothetical situation in which none of the gas boiling off from the cargo was used for propulsion and the ships' generators, but instead was diverted to be flared in their GCUs.

The Kpler data did not include distances for departures in 2018. To impute these and any other missing distance data, we used random forest regression, with export terminal, destination country, vessel year of construction and propulsion/engine type as training variables. This modeling, including an assessment of its performance compared to XGBoost regression, is described in the script `impute_distances.R`.

### Questions/Feedback

Email [Peter Aldhous](mailto:peter.aldhous@insideclimatenews.org).
