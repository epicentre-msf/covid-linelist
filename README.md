Covid-19 Linelist Compilation
==========================

Scripts to import, clean, and compile Covid-19 linelists.


## Cleaning and standardization

#### 1. Numeric variables

Includes variables `patinfo_ageonset`, `MSF_delay_before_admission`,
`MSF_length_stay`, and `outcome_contacts_followed`.

We coerce all non-missing values to numeric format. For values that cannot be
converted automatically, we make a manual correction (if possible) as tracked in
the dictionary data-raw/linelist/cleaning/dictionaries/dict_numeric_correct.xlsx

_Example_
```
Original                    To numeric                  Manually corrected
------------------          ------------------          ------------------
  patinfo_ageonset            patinfo_ageonset            patinfo_ageonset
1               40          1               40          1               40
2                3    ->    2                3    ->    2                3
3                ?          3             <NA>          3             <NA>
4               28          4               28          4               28
5              25,          5             <NA>          5               25
```

In the example above, original age values `?` and `11,` cannot be automatically
converted to numeric. The value `?` is converted to a missing value (`<NA>`)
which we cannot correct any further (without input from the field). The value
`25,` is manually corrected to `25` (after e.g. checking the corresponding value
of variable `MSF_job` to ensure that the patient is not a child, in which case
perhaps the age should have been `2.5`).

Based on the example above, the dictionary dict_numeric_correct.xlsx would
contain the following entry:
```
          variable   value  replacement
1 patinfo_ageonset     25,           25
```

#### 2. Date variables

Includes variables `report_date`, `Lab_date1`, `patcourse_dateonset`,
`MSF_symptom_*_date_onset`, `MSF_date_consultation`, `patcourse_presHCF`,
`patcourse_dateiso`, `expo_travel_date_*`, `expo_case_date_*`,
`outcome_submitted_date`, `outcome_patcourse_presHCF`,
`outcome_date_of_outcome`, and `outcome_lab_date`.

We coerce all non-missing values to date format (YYYY-MM-DD). For values that
cannot be converted automatically, we make a manual correction (if possible) as
tracked in the dictionary
data-raw/linelist/cleaning/dictionaries/dict_dates.xlsx

We then implement a variety of tests to check for logical consistency among the
various date values for each patient (TODO).

#### 3. Coded-list variables (excluding geocoding)

First we standardize all values using `hmatch::string_std()`, which converts
strings to lowercase, removes diacritics, and replaces sequences of
space/punctuation with "_". We then match the standardized strings to the
original coded-list selections as they appear in the drop-down menus, based on
the dictionary data-raw/linelist/cleaning/dictionaries/dict_factors.xlsx

Finally, for values that are still not matched to a dictionary entry, we make a
manual correction (if possible) as tracked in the dictionary
data-raw/linelist/cleaning/dictionaries/dict_factors_correct.xlsx

_Example_
```
Original              Standardized          Dictionary matched    Manually corrected
------------------    ------------------    ------------------    ------------------
  MSF_covid_status      MSF_covid_status      MSF_covid_status      MSF_covid_status
1 Not a case          1 not_a_case          1 Not a case          1 Not a case
2 CONFIRMED       ->  2 confirmed       ->  2 Confirmed       ->  2 Confirmed
3 Non-case            3 non_case            3 <NA>                3 Not a case
4 Not_a_case          4 not_a_case          4 Not a case          4 Not a case
5 suspected           5 suspected           5 Suspected           5 Suspected
6 Died                6 died                6 <NA>                6 <NA>
```

In the example above, original values `Non-case` and `Died` cannot be
automatically matched to a valid dictionary entry, even after
format-standardization. The value `Died` is converted to a missing value
(`<NA>`) which we cannot correct any further (without input from the field). The
format-standardized value `non_case` is manually corrected to the valid version
`Not a case`, as reflected in the dictionary dict_factors_correct.xlsx:

```
  variable          value     replacement
1 MSF_covid_status  non_case  Not a case
```

#### 4. Geocoding

The residence location variable `MSF_admin_location_past_week` represents up to
four administrative levels separated by " | ". We start by splitting this
variable into the four composite variables (the original variable is also
retained):

- `adm1_name__res_raw`
- `adm2_name__res_raw`
- `adm3_name__res_raw`
- `adm4_name__res_raw`

We then attempt to match each set of admin locations to the corresponding set
within the shapefile for the relevant country using
[`hmatch::hmatch()`](https://github.com/epicentre-msf/hmatch). This results in 8
new columns:

- `adm1_name__res`
- `adm2_name__res`
- `adm3_name__res`
- `adm4_name__res`
- `adm1_pcode__res`
- `adm2_pcode__res`
- `adm3_pcode__res`
- `adm4_pcode__res`

[TODO: Add further explanation]

## Outputs

__Global linelist (all countries combined):__

data/linelist/world/msf_covid19_linelist_global_{date}.xlsx

__OC-specific linelists (created by subsetting the global linelist):__

data/linelist/HIS-export/{OC}/msf_covid19_linelist_{OC}_{date}.xlsx
