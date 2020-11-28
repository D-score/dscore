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
