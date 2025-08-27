# Chicago water service lines data processing

Data and code to reproduce the analysis underlying the stories and interactive map analyzing the city of Chicago's problems with lead water service lines published on Aug. 28 by [Inside Climate News](https://insideclimatenews.org/news/28082025/chicago-lead-water-pipes-map/), Grist, and WBEZ.

Contains the following R scripts and Jupyter workbooks:

`acs.R` This pulls socioeconomic and race/ethnicity data from the 2023 5-year American Community Survey (ACS) for Census tracts in Chicago.

`process_addresses.R` This parses addresses from the 2025 Chicago water service line inventory from the file `2025_inventory.xlsx` in the `data` folder, then geocodes and associates each address with the corresponding Census tract. (Note, the pipeline also involved inspection and editing of results in [OpenRefine](https://openrefine.org/) and QGIS, spatial joins conducted in [QGIS](https://qgis.org/), plus some manual geocoding, so is not fully reproducible from this code.)

`tract-to-cca-aggregation-income.ipnyb` `tract-to-cca-aggregation-race.ipynb` These aggregate ACS data to the level of Chicago's 77 [Community Areas](https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-Map/cauq-8yn6).

`process_inventory.R`This combines the outputs of the previous scripts, workbooks and manual geocoding to create the map layers and the service line data used in the interactive app produced using the code in [this GitHub repository](https://github.com/Grist-Data-Desk/chi-pb).

`consolidate_addresses.R` This consolidates addresses from the inventory with overlapping street number ranges so that, for example, separate service lines located by the city at 11-13 E ILLINOIS ST and at 11 E ILLINOIS ST both appear in our interactive under the consolidated address 11-13 E ILLINOIS ST.

`static_maps.R` Code to generate panel of static maps, saved to the `maps` folder, subsequently edited by the partners' graphics desks to produce the versions used for publication.

### Processed data files

These are in the folder `processed_data`:

`chicago_tracts_filled.geojson` Geodata for Census tracts in the city of Chicago, for the city of Chicago plus tracts that are not part of the city but are contained within its outer perimeter. Contains the following variables:

-   `geoid` Census Bureau identifier for the tract.

-   `pct_poverty` 2023 5-year ACS estimate for the percentage of the population below the federal poverty level.

-   `median_household_income` 2023 5-year ACS estimate for median household income.

-   `pct_black_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as black alone, not Hispanic/Latino.

-   `pct_white_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as white alone, not Hispanic/Latino.

-   `pct_asian_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as Asian alone, not Hispanic/Latino.

-   `pct_minority` 2023 5-year ACS estimate for the percentage of the population identifying as anything other than white alone, not Hispanic.

-   `L` Number of service lines classified as lead, that is at least one of the gooseneck, public water system line, or customer-side is lead.

-   `GRR` Number of service lines classified as galvanized requiring replacement, that is at least one of the gooseneck, public water system line, or customer-side is GRR, where there is no lead component.

-   `U` Number of service lines classified as unknown, suspected lead; that is at least one of the gooseneck, public water system line, or customer-side is unknown, suspected lead, where there is no lead or GRR component.

-   `NL` Number of service lines classified as not lead, that is no component is lead, GRR, or unknown, suspected lead.

-   `total` Total number of service lines located to the tract.

-   `flag` TRUE if `total` is less than 25. Used to apply an gray color to tracts with very few service lines on the interactive and static maps.

-   `lead_plus_suspected` The sum of `L` and `U`.

-   `requires_replacement` The sum of `L`, `U` and `GRR`.

-   `pct_lead` Percentage of service lines classified as `L`.

-   `pct_grr` Percentage of service lines classified as `GRR`.

-   `pct_suspected_lead` Percentage of service lines classified as `U`.

-   `pct_lead_suspected` Percentage of service lines classified as `lead_plus_suspected`.

-   `pct_requires_replacement` Percentage of service lines classified as `requires_replacement`.

-   `pct_not_lead` Percentage of service lines classified as `NL`.

`chicago_community_areas.geojson` `chicago_community_areas.csv` Data from the service line inventory aggregated to Chicago Community Areas, rather than tracts. Contains the following variables:

-   `community` Name of the Community Area.

-   `area_num_1` Numerical code for the Community Area.

-   `pct_poverty` 2023 5-year ACS estimate for the percentage of the population below the federal poverty level.

-   `median_household_income` 2023 5-year ACS estimate for median household income.

-   `pct_black_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as black alone, not Hispanic/Latino.

-   `pct_white_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as white alone, not Hispanic/Latino.

-   `pct_asian_nonhispanic` 2023 5-Year ACS estimate for the percentage of the population identifying as Asian alone, not Hispanic/Latino.

-   `pct_minority` 2023 5-year ACS estimate for the percentage of the population identifying as anything other than white alone, not Hispanic.

-   `L` Number of service lines classified as lead, that is at least one of the gooseneck, public water system line, or customer-side is lead.

-   `GRR` Number of service lines classified as galvanized requiring replacement, that is at least one of the gooseneck, public water system line, or customer-side is GRR, where there is no lead component.

-   `U` Number of service lines classified as unknown, suspected lead, that is at least one of the gooseneck, public water system line, or customer-side is unknown, suspected lead, where there is no lead or GRR component.

-   `NL` Number of service lines classified as not lead, that is no component is lead, GRR, or unknown, suspected lead.

-   `total` Total number of service lines located to the tract.

-   `flag` TRUE if `total` is less than 25.

-   `lead_plus_suspected` The sum of `L` and `U`.

-   `requires_replacement` The sum of `L`, `U` and `GRR`.

-   `pct_lead` Percentage of service lines classified as `L`.

-   `pct_grr` Percentage of service lines classified as `GRR`.

-   `pct_suspected_lead` Percentage of service lines classified as `U`.

-   `pct_lead_plus_suspected` Percentage of service lines classified as `lead_plus_suspected`.

-   `pct_requires_replacement` Percentage of service lines classified as `requires_replacement`.

-   `pct_not_lead` Percentage of service lines classified as `NL`.

`service_lines1.csv` `service_lines2.csv` Parsed and geocoded data for addresses from the 2025 Chicago water service lines inventory, split into two files. They contain the following variables:

-   `row` Index from the rows in the service line inventory, range `1:491,705`.
-   `gooseneck_pigtail` Composition of the gooseneck. Codes:
    -   `U` Unknown (suspected lead);
    -   `L` Lead;
    -   `UNL` Unknown but not lead;
    -   `C` Copper
    -   GRR - Galvanized requiring replacement
    -   `O` Cast/ductile iron or transite.
-   `pws_owned_service_line_material` Composition of the public water system-owned service line, codes as above.
-   `customer_side_service_line_material` Composition of the customer side service line, codes as above.
-   `classification_for_entire_service_line` This is the variable used to aggregate results to tracts and Community Areas. Codes:
    -   `L` At least one of the gooseneck, public water system line, or customer-side line is lead.
    -   `GRR` At least one of the gooseneck, public water system line, or customer-side line is GRR, where there is no lead component.
    -   `U` At least one of the gooseneck, public water system line, or customer-side is unknown, suspected lead, where there is no lead or GRR component.
    -   `NL` No component is lead, GRR, or unknown, suspected lead. These are the only service lines that do not require replacement.
-   `full_address` Parsed address, after cleaning and consolidation with overlapping addresses, if necessary, followed by a series of variables parsed from it.
-   `is_intersection` TRUE or FALSE. Where TRUE, the series of variables beginning `st` are all null.
-   `stnum1` The street number, or in the case of a building with a range of numbers in the service line inventory, the lowest number in the range.
-   `stnum2` The street number, or in the case of a building with a range of numbers in the service line inventory, the highest number in the range.
-   `stdir` N, S, E, or W. Not present in all addresses.
-   `stname` Street name, minus the street type or suffix. Note there are edge cases that made name and type/suffix hard to parse.
-   `sttype` Street type or suffix: ST, RD, AVE etc. Same caveat as for street name.
-   `zip` 5-digit Zip code.
-   `geocoder` Geocoding service used to obtain result that passed quality screening. Includes a small number of manually geocoded addresses/intersections. There are just 21 addresses that we were unable to geocode with acceptable accuracy. Note, a few hundred of these addresses fall outside of the city of Chicago, many very near to its perimeter, and are included for the interactive lookup tool as residents may search for them.
-   `lat` Latitude returned by the geocoding service.
-   `long` Longitude returned by the geocoding service.
-   `geoid` Census Bureau identifier for the tract containing the geocoded address. Obtained directly from the geocoding results for the Census Bureau service, by spatial join to Census Bureau tracts boundary data for the other geocoding services.
-   `matched_address` Address returned by the geocoding service, converted into a standard format.
-   `m_stnum1` `m_stnum2` `m_stdir` `m_stname` `m_sttype` `m_zip` Address elements as above, but parsed from `matched_address`.

### **Questions/Feedback**

Email [Peter Aldhous](mailto:peter.aldhous@insideclimatenews.org) for questions on the R scripts or [Amy Qin](mailto:aqin@wbez.org) for questions on the Jupyter notebooks.
