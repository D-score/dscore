# dscore 1.0.3

* There was a slight discrepancy in the calculation of DAZ between the
`dscore` and `clopus` packages. The `dscore` package now first rounds
the D-score and then calculate DAZ.
* The default rounding is now 2 (D-score), 3 (DAZ) and 4 (age) decimals.

# dscore 1.0.2

* The `dscore()` function now automatically select the `prior_mean = ".dutch"` 
option when the user specifies `key == "dutch"`. This change repair an error
in the D-score calculation that occurs when only `key == "dutch"` was given.

# dscore 1.0.1

* The `dscore()` function now silently handles warnings that may result 
from taking the log of negative ages.

# dscore 1.0.0

* Hi, welcome to `dscore 1.0.0`! For the development history, see
<https://github.com/stefvanbuuren/dscore>
* Added a `NEWS.md` file to track changes to the package.
