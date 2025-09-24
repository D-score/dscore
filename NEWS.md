---
editor_options: 
  markdown: 
    wrap: 72
---

# dscore 1.11.0

### Overview

This release brings the following enhancements to the `dscore` package:

- Adds new item codes for GSED LF and GSED SF
- Better support for D-score calculation using Bayley III
- Uses a more permissive open source license

### Major changes

- Adds item names starting with `lf` and `sf` to `builtin_itemtable` to refer to GSED LF and GSED SF, respectively
- Replaces the `by3` key in `gsed2212` and `gsed2406`. The replacement matches many more by3 items (172 instead of 67), especially for younger children. Compared to the previous by3 key, it raises the D-score estimate for by3 by approximately 2.6 D.
- Updates the LICENSE from AGPL to the permissive Apache 2.0 to conform to Gates Foundation Open Access policy

### Minor changes

- Adds support to calculate DAZ for children < 2 weeks using the reference `preliminary_standards`
- Makes `rename_vector()` part of the `dscore` package (moved from the gsedread package)
- Updates the getting started vignette.
- Changes deprecated `arma::is_finite(val)` to `std::isfinite(val)` to adhere to CRAN policy
- Rebuilds `builtin_itemtable` to resolve problems with SF items 88 and 89 and LF B43-B51.
- Correct description of A45 Stand on 1 foot < 5 seconds
- Extends the item table with SF items with mode s (self-report)
  + Mode "s" is supported in the `gsed3`, `gsed2` and `gsed` lexicons
  + Adds item with mode "s" and "gs1" instrument codes
  + NOTE: there are no gpa-items with mode "s" (gsed2 lexicon)
- It corrects an error in the definition of the gpa item names:
  + Renames `gpaclc088` --> `gpaclc089` (Can you child say five or more separate words)
  + Renames `gpasec089` --> `gpasec088` (Is your child able to pee or poo)

### Breaking changes

- Retires the key `gsed2212` (soft deprecation). This key is identical to `gsed2406` (the current default), except that it defines its default `population` as `phase1` instead of `preliminary_standards`. If you want the old behavior, specify `key = "gsed2406"` in combination with `population = "phase1"`. The key `gsed2212` will be removed in a future release.

### For developers

- Adds `.toml` and `.vscode` file` to enforce air formatting
- Initializes air format on save


# dscore 1.10.0

### Overview

This release brings two enhancements to the `dscore` package:

- More flexible options for specifying the prior mean and prior standard deviation for the D-score calculation, and a new vignette to demonstrate these options.
- An updated reference of `preliminary_standards` based on a larger sample from Bangladesh.

### Major changes

- Refreshes `preliminary_standards` with a larger sample from Bangladesh
- Implements new and more friendly options that add increased flexibility to specify prior mean and prior standard deviation for the D-score calculation
- Changes the default `prior_mean_NA` and `prior_sd_NA` to `NULL` (was 50 and 20). This is a safer option to handle missing ages. The user can emulate the previous automatic behavior (introduced in intermediate version dscore 1.9.2) by setting the `prior_mean_NA = 50` and `prior_sd_NA = 20` arguments to the `dscore()` function.
- Rebrands `count_mu()` as function `get_mu()` to extract the prior mean from a reference table. Deprecates `count_mu()`.
- Adds a vignette "Custom Priors (Advanced)" to demonstrate the new options for specifying the prior mean and prior standard deviation
- Turns ages in `get_mu()` below -1/12 into `NA` values

### Minor changes 

- Changes `warning("Reference XX for key YY not found."` into `warning("Reference XX for key YY not found. Using default."` 
- Returns `preliminary_standards` from key `gsed2406` in the above case.
- Some minor edits to the "Understanding and using DAZ" vignette
- Turns `Inf` values in `daz()` into `NA` values
- Turns `NaN` values in SEM into `NA` values
- `dscore()` and `dscore_posterior()` now accept a matrix as input
- Improves documentation for interpretation of `NA`s in D-score, SEM and DAZ
- Adds a vignette "Understanding and using DAZ" to explain and highlight DAZ (contributed Jonathan Seiden)
- Fixes typos in vignettes
- Adds tests in `testthat/test-prior.R`
- Repairs bug that occured when no items was found resulting in error "cannot coerce class 'function' to a data.frame" in `dscore()`
- Restores a datafile `data-raw/data/keys/items_gs1_gl1.txt` that was accidentally removed in a previous release
- Evades superfluous warning 'There was 1 warning in `mutate()`. In argument: `daz = daz(...)`' 
- Makes the `key` column compulsory in the `itembank` argument, and adds a check on proper column names
- Improves documentation for the `population` and `key` arguments

# dscore 1.9.0

### Overview

This is a major update of the `dscore` package featuring:

- a new default reference `"preliminary_standards"`
- a correction of an issue with the scaling factor
- a major clean-up of the itembank, references, keys, and R code
- improved documentation and examples

### Major issues

BREAKING CHANGE: On May 31, 2024 we detected a long-time error in the calculation of the D-score resulting from an incorrect scale factor that led us to believe that item characteristic curves are steeper than the actually are. The impact of the error on the result is as follows: 1) There is no effect on the difficulty estimates of the Rasch models, 2) The D-score estimates are slightly altered but changes are small, 3) The references are largely unaffected and need not to be redone, 4) The estimates of the SEMs can differ substantially, so inferences based on the SEMs should be re-evaluated, 5) When there were changes in the analyses, the results in the newer method look smoother and are preferred. The correction appeared in the development version `dscore 1.8.8`, and is now incorporated into release `dscore 1.9.0`. For backward compatibility to `dscore 1.8.7` and earlier, use the argument `algorithm = "1.8.7"` in calls to the `dscore()` function.

BREAKING CHANGE: `dscore_posterior()` now returns a `data.frame` with column names that indicate the quadrature point. This was an unnamed `matrix`. Code that expects a `matrix` as the return of `dscore_posterior()` may need to be adapted.

NEW DEFAULT KEY: Adds a new reference `"preliminary_standards"` calculated from selected subsample of the GSED Phase 1 data, and makes these the default in this release. The reference is a temporary stand-in for a future norm-based standard for normal early child development. This reference replaces the temporary reference `"phase1_healthy"` that was introduced in `dscore 1.8.7`. Compared to the `"phase1"` reference, the `"preliminary_standards"` reference has the following differences: 1) D-score estimation uses the new model 20240601 with correct scale factor, 2) Calculates the D-score for SF and LF separately (not combined), 3) Tunes the GAMSLSS model to fit the healthy subsample. The `"phase1_healthy"` object is removed.

### Major changes

- Adds a new age-conditional reference for population `"dutch"` calculated using the `"gsed2212"` key.
- Defines a new key `"gsed2406"` to accomodate for the changed prior mean because of the adoption of the new reference `"preliminary_standards"` as the base population. The key `"gsed2406"` is identical to `"gsed2212"`, and is the default key in this release.
- Adds a new `builtin_keys` table that contains proper defaults for the base reference, transformation and quadrature points per key
- Indexed a reference now by two fields: `key` and `population`. Previously the index was based on only `population`. This change allows for multiple references per key, and for references created for the same population under different keys. The `key` field is now mandatory in the reference table.
- Adds a `verbose` option to `dscore()`, `dscore_posterior()`, `get_age_equivalent()`, `get_reference()`, `get_tau()`, `daz()` and `zad()` to print progress messages to the console on the values of `key`, `population`, `transform`, `qp` and `algorithm`. This is useful for debugging and for understanding the behavior of the functions.
- Cleans up the R code to take advantage of the specification made in the new `builtin_keys` table. This makes the code more readable and maintainable.
- Retires keys `sf2206`, `lf2206`, `294_0`, `gsed2206`, `gsed2208` and removes from the `builtin_itembank`.
- `dscore()` and `dscore_posterior()` can now copy variables from the input `data` into the output through the `prepend` argument. (#46)

### Minor changes 

- Simplifies the package DESCRIPTION file
- New internal `init_key()` and `set_default_xxx()` functions to regulate values for `key`, `population`, `transform` and `qp` arguments
- Renames the files in `data-raw/data/keys` to more consistent names, adapts `data-raw/R/save_builtin_itembank.R` to reflect model history, and rebuilds `builtin_itembank`
- Removes the dependency on `tibble` and `tidyselect`, and replaces the dependency on `stringr` by the lighter `stringi`
- Rename the argument name `reference` to `reference_table` in `daz()` and `zad()` to avoid confusion with the `references` argument in `get_reference()`
- Simplifies the spelling of the term "D-score" to improve consistency and readability
- Replaces  `magrittr` pipe `%>%` by base pipe `|>`
- Make style more consistent with `styler`
- Per request from CRAN  (`Specified C++11: please drop specification unless essential`), removes a C++11 specification
- Moves error evasion code into internal `pBCT()`
- Document up-rounding to a D-score of 1 or higher when `daz()` and `zad()` using the BCT transformation for positive values 
- Removes the superfluous `names` attribute from the return value of `daz()` and `zad()`
- Evades an error produced by internal `pBCT()` when `is.na(nu)` is `TRUE`
- Renames GSED HH to GSED HF
- Change CITATION file to use the `bibtex` package
- Moved all keys to the `data-raw/data/keys` folder and renamed them to improve readability

# dscore 1.8.0

### Major changes

- Adds instrument `gh1` (GSED-HF, JAN 2023) to `builtin_itemtable` and `builtin_itembank` as part of key `gsed2212`
- Adds example datasets: `sample_sf`, `sample_lf` and `sample_hf`
- Adds vignette to calculate D-scores and DAZ dedicated to GSED instruments
- Renames streams in `gl1` instruments as: aa --> gm, bb --> lg, cc --> fm
- Replaces item name `gl1aad001` --> `gl1gmd001`, and so on

### Minor changes 

- Rewrite calls to `select()` and `pivot_longer()` to conform to `tidyselect 1.2.0` grammar

# dscore 1.7.0

### Major issue

-   On 22021130, we found errors in the LF item order. Solves a documentation error. This error was introduced on May 30, 2022 and invalidates keys `gsed2206` and `gsed2208`, as well as analyses that rely on correct LF item labels. Version 1.7.0 corrects these problems.
-   Item labels are taken from 
    -   `LF1`, corrected using RedCAP comparisons from
        `Phase_1_master_data_dictionary_V1.0_29_11_2022.xlsx`;
    -   `LF2`, from
        `GSED LF Item Guide_October22_FINAL_clean_27Nov22.docx` and
        manually matched to `LF1`.
-   Rerun core 293_0 model, check edits, redocument, regenerate
    diagnostic plots, etc. Check that result is identical.
-   Refit full 818_6 model. In general better ICC's, effect on
    D-score calculation is minor, six items were bad matches

### Major changes

-   Introduces new default key `gsed2212`
-   Introduces new instrument codes `gs1` (GSED SF V1.0) and `gl1` (GSED LF V1.0)
-   Updates `gto` labels with correct order
-   Set default key to `gsed2212`. This key repairs problems in `gsed2206` and `gsed2208`. 
-   `get_labels()` now returns the labels in the same order as `items`
-   Extends key `gsed2212` with 18 ECDI items using Phase 1 validation data
-   Updates `builtin_itemtable` and `builtin_itembank` with correct LF
    item order
-   Redocuments upper anchor item

### Minor changes

-   Solves bug that crashed `dscore::count_mu_phase1(t)` when `t` is a
    vector containing `NA`s
-   Adds example data set `gsample` with 10 cases with SF and LF scores
-   Adds `order` argument to `get_itemnames()`
-   Repairs an error in the `sort_itemnames()` example
-   Replaces bitwise by more elegant elementwise comparison in `dscore.cpp`
-   Removes the dependency on the `sirt` package

# dscore 1.6.0

### Major changes

-   Solves a long-standing issue that led to severe incongruence
    between LF and SF at the earliest ages (\<6M).
-   Adds two new keys (`gsed2208` and `293_0`) using the Phase 1
    validation data for the GSED SF and GSED LF.
-   Sets `293_0` as the **GSED core model** and extended it to include
    the 818 items that fitted the previous model ("gsed2206").
-   Adds a new reference (named `phase1`), based on LF and SF data from
    cohorts GSED-BGD, GSED-PAK and GSED-TZA studies.
-   \*\*Changes the default key to `gsed2208` and default `population`
    to "phase1"`.**. If you want the old behavior, specify`key =
    "gsed1912"`or`key =
    "gsed2206"`to functions that accept the`key`argument (`dscore()`,`dscore_posterior()`,`get_age_equivalent()`,`get_tau()\`).

### Minor changes

-   Many simplications and update to increase legibility and
    consistency.
-   Makes arguments of `get_age_equivalent()` and `get_tau()` consistent
    with `dscore()`
-   Adapts BCT functions to work with missing and out-of-range data
-   Adds support for `phase1` reference to `get_reference()`
-   Adds support for BCT references in `zad()` and `daz()`
-   Adds `count_mean_phase1()` for setting prior mean equal to the GSED
    Phase 1 reference
-   Adds round 2 estimates to `count_mean_phase1()`
-   Prepares `dscore()` prior_mean functionality to deal with the
    to-be-implemented new reference as "phase1"
-   Adds `count_mean_phase1()` function
-   Adds 18 ECDI items to keys `gsed2206` and `294_0`
-   Initialise proper default `population` when key is `294_0`.
-   Adds experimental key `294_0` to the `builtin_itembank`.
-   Adds a `relevance` argument to `dscore()` and `dscore_posterior()`
    to restrict calculation of D-scores to those items that have their
    `tau` within this relevance interval around the dynamic EAP
    estimate.
-   Repairs bug in `builtin_itembank` that inherits old (gsed1912) tau's
    into new key (gsed2206) for instruments `gto` and `gpa`. As a
    consequence, D-score were incorrectly calculated when `gto` and
    `gpa` item names were used.
-   Repairs a bug in `get_age_equivalent()` that resulted in age
    interval estimates that were too narrow on the D-score scale.

# dscore 1.5.0

### Major changes

-   Adds three new keys (`gsed2206`, `lf2206` and `sf2206`) that
    incorporate Phase 1 validation data for the GSED SF, GSED LF and
    BSID.
-   **Changes the default key to `gsed2206`**. If you want the old
    behavior, specify `key = "gsed1912"` to functions that accept the
    `key` argument (`dscore()`, `dscore_posterior()`,
    `get_age_equivalent()`, `get_tau()`).
-   Extends `builtin_itemtable` with names and labels for `gpa` (SF),
    `gto` (LF) and `gsd` (novel SF) items.
-   Solves a long-standing issue #29 that gave different D-score under
    different transformation. This change affects solutions calculated
    with the `metric = "logit"` and `transform` parameters specified.
    The D-scores calculated under different transforms are now linear
    transformations of each other.

### Minor changes

-   Repairs a few problematic outbound URLs

# dscore 1.4.0

### Major changes

-   Updates tau estimates for Mullen items in itembank covering a much
    wider age range: 65 out of the 85 Mullen items from previous
    itembank version have new tau estimates; 20 items are removed.
-   Updates itembank with new Mullen items. There are 73 new Mullen
    items added to the new itembank. In total there are now 139 Mullen
    items in the itembank, estimates are based on PROVIDE and BAMBAM
    data.
-   Moves sources to organisation repository `D-score/dscore` for
    increased visibility

### Minor changes

-   Update external links
-   Repairs six incorrect BDS numbers in `data-raw/data/bds)edited.csv`

# dscore 1.3.0

-   Adds equate groups translated from the `ddata` package to
    `builtin_itemtable`
-   Adds new `itemtable_20200424.txt` with minimal item definitions
-   Adds 522 missing items that were defined in the `ddata` package
-   Minimizes the data stored in `builtin_itemtable`
-   The `get_itemtable()` function gets a new argument `decompose` for
    adding info from decomposed item names
-   Solves warning from `get_itemtable()` when `itemtable` argument is
    specified

# dscore 1.2.0

-   Uses explicit `drop = TRUE` to account for new behavior in
    `tibble 3.0.0`
-   Adds a facility to create a dynamic (on-the-fly) itemtable from
    specified item names in `get_itemtable()`.

# dscore 1.1.0

-   The default rounding is now 2 (D-score), 3 (DAZ). Fixed rounding are
    now 4 (age) and 4 (p) decimals.
-   There was a slight discrepancy in the calculation of DAZ between the
    `dscore` and `clopus` packages. The `dscore` package now first
    rounds the D-score and then calculate DAZ.
-   The `dscore()` function now automatically select the
    `prior_mean = ".dutch"` option when the user specifies
    `key == "dutch"`. This change repair an error in the D-score
    calculation that occurs when only `key == "dutch"` was given.
-   The `dscore()` function now silently handles warnings that may
    result from taking the log of negative ages.

# dscore 1.0.0

-   Hi, welcome to `dscore 1.0.0`! For the development history, see
    <https://github.com/D-score/dscore>
-   Added a `NEWS.md` file to track changes to the package.
