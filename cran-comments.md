## Resubmission 1.0.0
### Package Author Response
Thank you for reviewing this package! Your work is really appreciated.

Regarding to the extend_var.Rd comment: Removed the function from the documentation since it should only be called internally. Hence, I did not add a value to the .Rd file.

All other points have been fixed. 

### CRAN Response
If there are references describing the methods in your package, please add these in the description field of your DESCRIPTION file in the form
authors (year) <doi:...>
authors (year, ISBN:...)
or if those are not available: <https:...>
with no space after 'doi:', 'https:' and angle brackets for auto-linking. (If you want to add a title as well please put it in quotes: "Title")

Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar) -> Missing Rd-tags:
     extend_var.Rd: \value

You write information messages to the console that cannot be easily suppressed.
It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print() rather use message()/warning() or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console. (except for print, summary, interactive functions) -> R/ARD.R


## Initial Submission 1.0.0

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
