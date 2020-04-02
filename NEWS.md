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
<https://github.com/stefvanbuuren/dscore>
* Added a `NEWS.md` file to track changes to the package.
