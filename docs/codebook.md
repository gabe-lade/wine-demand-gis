# Codebook — public data files (`data/public/`)

All files are redistributed from public sources or are authors' constructions from public
information. The proprietary NielsenIQ panel is documented separately (not distributed; see the
README Data Availability Statement). Variable names below are as used by the pipeline.

## cpi/CPI_deflator.dta  (13 × 2)
Annual CPI deflator used to convert nominal dollars to 2019 dollars. Source: US BLS CPI.
| variable | description |
|----------|-------------|
| `year` | calendar year, 2007–2019 |
| `Deflator` | price deflator (base = 2019) |

## tax/state_excise_tax.dta  (88 × 4)
State wine excise tax rates by wine class. Source: state revenue departments / TTB.
| variable | description |
|----------|-------------|
| `fips_state_code` | state FIPS code |
| `fips_state_descr` | state name |
| `wineclass` | wine tax class |
| `excisetx_dollarpergallon` | excise tax, $/gallon |

## population/Population_2005-09.dta, Population_2010-19.dta  (× 4)
MSA (Scantrack-market) population by year. Source: US Census.
| variable | description |
|----------|-------------|
| `scantrack_market_descr` | NielsenIQ Scantrack market name |
| `year` | calendar year |
| `total_population` | market population |
| `pop_above_20` | population aged 20+ (market size base) |

## area/MSA_Area_2010Census.dta  (52 × 2)
Market land area. Source: 2010 US Census.
| variable | description |
|----------|-------------|
| `scantrack_market_descr` | Scantrack market name |
| `areainsquaremilestotalarea` | total land area, square miles |

## store_count/foodstorecount_2012.dta, foodstorecount_2017.dta  (52 × 3)
Food & beverage store counts by market. Source: Census County Business Patterns.
| variable | description |
|----------|-------------|
| `panel_year` | reference year (2012 or 2017) |
| `scantrack_market_descr` | Scantrack market name |
| `foodandbeveragestorescount_YYYY` | store count |

## distance/distance.dta  (1386 × 4)
Distance from each market to each origin state (shipping-cost instrument). Authors' construction.
| variable | description |
|----------|-------------|
| `scantrack_market_descr` | destination market |
| `origin_state` | wine origin state |
| `distance_from` | origin reference point |
| `distance_miles` | distance, miles |

## distance/diesel_price.dta  (52 × 2)
Quarterly diesel price (shipping-cost instrument). Source: US EIA.
| variable | description |
|----------|-------------|
| `date_quarter` | quarter index |
| `diesel_price` | diesel price |

## brand_crosswalk/ — RESTRICTED, NOT distributed
> The two crosswalk files below embed NielsenIQ `brand_descr` strings and are therefore treated as
> restricted (not committed; available from the authors on request to researchers with Kilts
> access). Listed here for documentation only.

## brand_crosswalk/2a_region_brand.dta  (648 × 3)
Brand → country / state crosswalk, hand-built by the authors from public web sources.
| variable | description |
|----------|-------------|
| `brand_descr` | brand name (Stata `proper()` casing) |
| `brand_country` | country (e.g. "US") |
| `brand_state_location` | US state, if domestic |

## brand_crosswalk/2b_external_match_basedonbrands.dta  (1928 × 4)
Brand → geographic-origin crosswalk for products whose NielsenIQ description lacks an explicit
appellation. Hand-built by the authors. NB: `brand_descr` uses Stata `proper()` casing — the R
port reproduces this with `stata_proper()` (capitalizes the letter after an apostrophe), which is
required for the merge to match (see `code/_config.R`).
| variable | description |
|----------|-------------|
| `brand_descr` | brand name |
| `state_appellation_m` | matched US state appellation |
| `wine_appellation_m` | matched AVA |
| `Importing_country_m` | matched foreign country |
