
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Covid-19 Linelist Compilation

Scripts to import, clean, and compile Covid-19 linelists.

## Cleaning and standardization

#### 1\. Numeric variables

Includes variables `patinfo_ageonset`, `MSF_delay_before_admission`,
`MSF_length_stay`, and `outcome_contacts_followed`.

We coerce all non-missing values to numeric format. For values that
cannot be converted automatically, we make a manual correction (if
possible) as tracked in the dictionary
`data-cleaning/dictionaries/dict_numeric_correct.xlsx`

*Example*

    Original                    To numeric                  Manually corrected
    ------------------          ------------------          ------------------
      patinfo_ageonset            patinfo_ageonset            patinfo_ageonset
    1               40          1               40          1               40
    2                3    ->    2                3    ->    2                3
    3                ?          3             <NA>          3             <NA>
    4               28          4               28          4               28
    5              25,          5             <NA>          5               25

In the example above, original age values `?` and `25,` cannot be
automatically converted to numeric. The value `?` is converted to a
missing value (`<NA>`) which we cannot correct any further (without
input from the field). The value `25,` is manually corrected to `25`
(after e.g. checking the corresponding value of variable `MSF_job` to
ensure that the patient is not a child, in which case perhaps the age
should have been `2.5`), as reflected in the dictionary:

``` 
          variable   value  replacement
1 patinfo_ageonset     25,           25
```

#### 2\. Date variables

Includes variables `report_date`, `Lab_date1`, `patcourse_dateonset`,
`MSF_symptom_*_date_onset`, `MSF_date_consultation`,
`patcourse_presHCF`, `patcourse_dateiso`, `expo_travel_date_*`,
`expo_case_date_*`, `outcome_submitted_date`, `outcome_onset_symptom`,
`outcome_patcourse_presHCF`, `outcome_date_of_outcome`,
`outcome_lab_date`, and `MSF_date_treament*`.

We first attempt to coerce all non-missing values to date format
YYYY-MM-DD, and then flag values with any of the following potential
issues:

  - value cannot be automatically coerced to YYYY-MM-DD
  - date in the future (with respect to the compilation date)
  - date earlier than 2020-02-01
  - `outcome_date_of_outcome` more than 5 days before
    `MSF_date_consultation`

Flagged values from the above checks are printed to a time-stamped Excel
file, along with all other date values from the relevant `patient_id`
for context (i.e. to help make manual corrections where possible): <br>
`data-cleaning/manual-corrections/dates/dates_check_compiled_{date_time}.xlsx`

At each compilation we examine every newly-flagged date value and try to
make a manual correction if possible, using the following general
approach:

  - if the correction is relatively obvious from context, make the
    correction
  - else…
      - if a value cannot be parsed to date format, convert to missing
        value
      - else…
          - if the value is way off (e.g. \>1 month in future or earlier
            than 2020), convert to missing value
          - else leave value as is

**Note 1:** Manual date corrections are specific to a given
`patient_id`, date variable, and original date value, so any
week-to-week change to a date value within an original linelist will
override any manual correction previously made to that value.

**Note 2:** The date checks during compilation or just a subset of the
date checks implemented within the queries sent to sites every week.

*Example 1*

| patient\_id     | variable                | value      | date       | date\_correct | flag       |
| :-------------- | :---------------------- | :--------- | :--------- | :------------ | :--------- |
| FRA\_P\_PAR\_01 | patcourse\_dateonset    | 39280      | 2007-07-17 | 2020-07-17    | too\_early |
| FRA\_P\_PAR\_01 | report\_date            | 44029      | 2020-07-17 |               |            |
| FRA\_P\_PAR\_01 | MSF\_date\_consultation | 44029      | 2020-07-17 |               |            |
| FRA\_P\_PAR\_01 | upload\_date            | 2020-07-22 | 2020-07-22 |               |            |

The patient entry above is flagged because there is a date value earlier
than 2020-02-01 (the stated year is 2007). The month and day portions of
the entry for 2007 look reasonable in the context of the other date
values, so here I make the correction to 2020-07-17 (in column
`date_correct`).

The manual correction is specific to the particular `patient_id`,
variable, and original date value, so if the early date value is changed
in the original linelist the following week, the manual correction will
no longer be triggered.

*Example 2*

| patient\_id     | variable                    | value      | date       | date\_correct | flag      |
| :-------------- | :-------------------------- | :--------- | :--------- | :------------ | :-------- |
| FRA\_P\_PAR\_02 | MSF\_date\_consultation     | 44001      | 2020-06-19 |               |           |
| FRA\_P\_PAR\_02 | patcourse\_presHCF          | 44001      | 2020-06-19 |               |           |
| FRA\_P\_PAR\_02 | outcome\_patcourse\_presHCF | 44001      | 2020-06-19 |               |           |
| FRA\_P\_PAR\_02 | outcome\_date\_of\_outcome  | 44023      | 2020-07-11 |               |           |
| FRA\_P\_PAR\_02 | upload\_date                | 2020-07-18 | 2020-07-18 |               |           |
| FRA\_P\_PAR\_02 | Lab\_date1                  | 20/20/2020 |            | .na           | ambiguous |
| FRA\_P\_PAR\_02 | patcourse\_dateonset        | 7 days     |            | .na           | ambiguous |

In the example above there are two values that cannot be automatically
coerced to dates. It’s not obvious to me what the values should have
been, so I convert the entries to missing values (\<NA\>) (note that
“.na” is just a keyword we use internally to coerce values to
missing).

*Example 3*

| patient\_id     | variable                         | value      | date       | date\_correct | flag   |
| :-------------- | :------------------------------- | :--------- | :--------- | :------------ | :----- |
| FRA\_P\_PAR\_03 | report\_date                     | 44022      | 2020-07-10 |               |        |
| FRA\_P\_PAR\_03 | MSF\_date\_consultation          | 44022      | 2020-07-10 |               |        |
| FRA\_P\_PAR\_03 | outcome\_submitted\_date         | 44022      | 2020-07-10 |               |        |
| FRA\_P\_PAR\_03 | upload\_date                     | 2020-07-21 | 2020-07-21 |               |        |
| FRA\_P\_PAR\_03 | MSF\_symptom\_fever\_date\_onset | 44038      | 2020-07-26 |               | future |

The example above includes a date that was in the future at the time of
compilation. From context it’s not obvious to me what it should have
been, and the value is not too far in the future, so I leave as is and
make no correction.

#### 3\. Coded-list variables (excluding geocoding)

First we standardize all values using `hmatch::string_std()`, which
converts strings to lowercase, removes diacritics, and replaces
sequences of space/punctuation with "\_". We then match the standardized
strings to the original coded-list selections as they appear in the
drop-down menus, based on the dictionary
`data-cleaning/dictionaries/dict_factors.xlsx`

Finally, for values that are still not matched to a dictionary entry, we
make a manual correction (if possible) as tracked in the dictionary
`data-cleaning/dictionaries/dict_factors_correct.xlsx`

*Example*

    Original              Standardized          Dictionary matched    Manually corrected
    ------------------    ------------------    ------------------    ------------------
      MSF_covid_status      MSF_covid_status      MSF_covid_status      MSF_covid_status
    1 Not a case          1 not_a_case          1 Not a case          1 Not a case
    2 CONFIRMED       ->  2 confirmed       ->  2 Confirmed       ->  2 Confirmed
    3 Non-case            3 non_case            3 <NA>                3 Not a case
    4 Not_a_case          4 not_a_case          4 Not a case          4 Not a case
    5 suspected           5 suspected           5 Suspected           5 Suspected
    6 Died                6 died                6 <NA>                6 <NA>

In the example above, original values `Non-case` and `Died` cannot be
automatically matched to a valid dictionary entry, even after
format-standardization. The value `Died` is converted to a missing value
(`<NA>`) which we cannot correct any further (without input from the
field). The format-standardized value `non_case` is manually corrected
to the valid version `Not a case`, as reflected in the dictionary:

``` 
  variable          value     replacement
1 MSF_covid_status  non_case  Not a case
```

#### 4\. Geocoding

The residence location variable `MSF_admin_location_past_week`
represents up to four administrative levels separated by " | ". We start
by splitting this variable into the four composite variables, named
`adm*_name__res_raw` (the original variable is also retained).

We then attempt to match each set of admin locations to the
corresponding set within the shapefile for the relevant country using
[`hmatch::hmatch()`](https://github.com/epicentre-msf/hmatch). For
location values that remain unmatched, we make a manual match to the
shapefile (if possible) as tracked in the dictionaries: <br>
`data-cleaning/manual-corrections/geocodes/geocodes_check_{country}.xlsx`

*Example (note we omit the adm4 level here for brevity)*

``` 
  Original variable                 Original values split into separate variables
  ----------------------------      --------------------------------------------------------
  MSF_admin_location_past_week      adm1_name__res_raw adm2_name__res_raw adm3_name__res_raw
1 New York | Eerie | Buffalo        New York           Eerie              Buffalo
2 Philadelphia | Allegheny | Ville  Philadelphia       Allegheny          Ville
3 New York | Rochester              New York           Rochester          <NA>              ...
4 Philadelphia                      Philadelphia       <NA>               <NA>
5 New Jersey | NJ/Essex             New Jersey         NJ/Essex           <NA>

  Shapefile-matched admin names
  --------------------------------------------
  adm1_name__res adm2_name__res adm3_name__res
1 New York       Erie           Buffalo
2 Philadelphia   Allegheny      <NA>
3 New York       Rochester      <NA>          ...
4 Philadelphia   <NA>           <NA>
5 New Jersey     Essex          <NA>

  Shapefile-matched admin codes
  -------------------------------------------------------
  adm1_pcode__res adm2_pcode__res         adm3_pcode__res
1 new_york        new_york__erie          new_york__erie__buffalo
2 philadelphia    philadelphia__allegheny <NA>
3 new_york        new_york__rochester     <NA>
4 philadelphia    <NA>                    <NA>
5 new_jersey      new_jersey__essex       <NA>
```

In the example above there were 3 mismatches between the linelist and
shapefile:

  - Line 1: “Eerie” in the linelist is spelled “Erie” in the shapefile.
    This mismatch is close enough (≤2 characters) that fuzzy matching
    with the function `hmatch::hmatch()` will handle it automatically.
  - Line 2: There is no adm3 level “Ville” within the shapefile, and we
    are unable to find a match manually, so the clean variables
    (`adm*_name__res` and `adm*_pcode__res`) will only have non-missing
    values up to the adm2 level.
  - Line 5: There is no adm2 level “NJ/Essex” in the shapefile, but we
    determine manually that this likely corresponds to “Essex”, and add
    the correction to the relevant dictionary.

## Outputs

**Global linelist (all countries combined):**

data/linelist/world/msf\_covid19\_linelist\_global\_{date}.xlsx

**OC-specific linelists (created by subsetting the global linelist):**

data/linelist/HIS-export/{OC}/msf\_covid19\_linelist\_{OC}\_{date}.xlsx
