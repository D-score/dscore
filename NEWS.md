# dscore 1.6.1 

* Extends `gsed2208` with 18 ECDI items using Phase 1 data

# dscore 1.6.0

### Major changes

* Solves a long-standing issue that led to severe incongruences between LF and SF at the earliest ages (<6M).
* Adds two new keys (`gsed2208` and `293_0`) using the Phase 1 validation data for the GSED SF and GSED LF.
* Sets `293_0` as the **GSED core model** and extended it to include the 818 items that fitted the previous model ("gsed2206").
* Adds a new reference (named `phase1`), based on LF and SF data from cohorts GSED-BGD, GSED-PAK and GSED-TZA studies. 
* **Changes the default key to `gsed2208` and default `population` to "phase1"`.**. If you want the old behavior, specify `key = "gsed1912"` or `key = "gsed2206"` to functions that accept the `key` argument (`dscore()`, `dscore_posterior()`, `get_age_equivalent()`, `get_tau()`).

### Minor changes

* Many simplications and update to increase legibility and consistency.
* Makes arguments of `get_age_equivalent()` and `get_tau()` consistent with `dscore()`
* Adapts BCT functions to work with missing and out-of-range data
* Adds support for `phase1` reference  to `get_reference()`
* Adds support for BCT references in `zad()` and `daz()`
* Adds `count_mean_phase1()` for setting prior mean equal to the GSED Phase 1 reference
* Adds round 2 estimates to `count_mean_phase1()`
* Prepares `dscore()` prior_mean functionality to deal with the to-be-implemented new reference as "phase1"
* Adds `count_mean_phase1()` function
* Adds 18 ECDI items to keys `gsed2206` and `294_0`
* Initialise proper default `population` when key is `294_0`.
* Adds experimental key `294_0` to the `builtin_itembank`.
* Adds a `relevance` argument to `dscore()` and `dscore_posterior()` to restrict calculation of D-scores to those items that have their `tau` within this relevance interval around the dynamic EAP estimate.
* Repairs bug in `builtin_itembank` that inherits old (gsed1912) tau's into new key (gsed2206) for instruments `gto` and `gpa`. As a consequence, D-score were incorrectly calculated when `gto` and `gpa` item names were used.
* Repairs a bug in `get_age_equivalent()` that resulted in age interval estimates that were too narrow on the D-score scale.

# dscore 1.5.0

### Major changes

* Adds three new keys (`gsed2206`, `lf2206` and `sf2206`) that incorporate Phase 1 validation data for the GSED SF, GSED LF and BSID.
* **Changes the default key to `gsed2206`**. If you want the old behavior, specify `key = "gsed1912"` to functions that accept the `key` argument (`dscore()`, `dscore_posterior()`, `get_age_equivalent()`, `get_tau()`).
* Extends `builtin_itemtable` with names and labels for `gpa` (SF), `gto` (LF) and `gsd` (novel SF) items.
* Solves a long-standing issue #29 that gave different D-score under different transformation. This change affects solutions calculated with the `metric = "logit"` and `transform` parameters specified. The D-scores calculated under different transforms are now linear transformations of each other.

### Minor changes

* Repairs a few problematic outbound URLs

# dscore 1.4.0

### Major changes

* Updates tau estimates for Mullen items in itembank covering a much wider age range: 65 out of the 85 Mullen items from previous itembank version have new tau estimates; 20 items are removed. 
* Updates itembank with new Mullen items. There are 73 new Mullen items added to the new itembank. In total there are now 139 Mullen items in the itembank, estimates are based on PROVIDE and BAMBAM data.
* Moves sources to organisation repository `D-score/dscore` for increased visibility

### Minor changes

* Update external links
* Repairs six incorrect BDS numbers in `data-raw/data/bds)edited.csv`

# dscore 1.3.0

* Adds equate groups translated from the `ddata` package to `builtin_itemtable`
* Adds new `itemtable_20200424.txt` with minimal item definitions
* Adds 522 missing items that were defined in the `ddata` package
* Minimizes the data stored in `builtin_itemtable`
* The `get_itemtable()` function gets a new argument `decompose` for adding info from decomposed item names
* Solves warning from `get_itemtable()` when `itemtable` argument is specified
 
# dscore 1.2.0

* Uses explicit `drop = TRUE` to account for new behavior in `tibble 3.0.0`
* Adds a facility to create a dynamic (on-the-fly) itemtable from 
specified item names in `get_itemtable()`.

# dscore 1.1.0

* The default rounding is now 2 (D-score), 3 (DAZ). Fixed rounding are now
4 (age) and 4 (p) decimals.
* There was a slight discrepancy in the calculation of DAZ between the
`dscore` and `clopus` packages. The `dscore` package now first rounds
the D-score and then calculate DAZ.
* The `dscore()` function now automatically select the `prior_mean = ".dutch"` 
option when the user specifies `key == "dutch"`. This change repair an error
in the D-score calculation that occurs when only `key == "dutch"` was given.
* The `dscore()` function now silently handles warnings that may result 
from taking the log of negative ages.

# dscore 1.0.0

* Hi, welcome to `dscore 1.0.0`! For the development history, see
<https://github.com/D-score/dscore>
* Added a `NEWS.md` file to track changes to the package.
