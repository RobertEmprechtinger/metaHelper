## Resubmission 1.0.0
### Package Author Response
Many thanks for your time. The issues with the references should be fixed now.

### CRAN Response
Thanks, we see:


  Found the following (possibly) invalid DOIs:
    DOI: 10.1002/1097-0258(20001130)
      From: DESCRIPTION
      Status: 404
      Message: Not Found

You may have to URLencode() the above DOI.


    DOI: 10.1136/bmj-2022-073141#'
      From: DESCRIPTION
      Status: 404
      Message: Not Found

Omit the extra "'"


    DOI: https://doi.org/10.1002/9780470743386.ch4'
      From: DESCRIPTION
      Status: 404
      Message: Not Found
    DOI: https://doi.org/10.1016/C2009-0-03396-0'
      From: DESCRIPTION
      Status: 404
      Message: Not Found
    DOI: https://doi.org/10.20982/tqmp.14.4.p242
      From: DESCRIPTION
      Status: 404
      Message: Not Found
    DOI: https://doi.org/10.2307/1164588
      From: DESCRIPTION
      Status: 404
      Message: Not Found

The doi start 10...., not https which is a URL rather than a DOI.


  The Description field contains
    <doi:https://doi.org/10.1002/9780470743386.ch4'> Chinn S. (2000)
    <https:https://handbook-5-1.cochrane.org/front_page.htm> Cochrane
    <https:https://psycnet.apa.org/record/2009-05060-000> Cohen, J. (1977)
    <https:https://psycnet.apa.org/record/1987-98267-000> Ellis, P.D.
    (2009) <https:https://www.psychometrica.de/effect_size.html>
    <doi:https://doi.org/10.20982/tqmp.14.4.p242> Hedges, L. V. (1981)
    <doi:https://doi.org/10.2307/1164588> Hedges L. V., Olkin I. (1985)
    <doi:https://doi.org/10.1016/C2009-0-03396-0'> Murad M H, Wang Z, Zhu

<https:https://search.r-project.org/CRAN/refmans/confintr/html/ci_proportion.html>

<https:https://stats.stackexchange.com/questions/82720/confidence-interval-around-binomial-estimate-of-0-or-1>
    Stackoverflow (2018) <https:https://stats.stackexchange.com/q/338043>.
  Please enclose URLs in angle brackets (<...>).
  The Description field contains
    <doi:https://doi.org/10.1002/9780470743386.ch4'> Chinn S. (2000)
    <doi:https://doi.org/10.20982/tqmp.14.4.p242> Hedges, L. V. (1981)
    <doi:https://doi.org/10.2307/1164588> Hedges L. V., Olkin I. (1985)
    <doi:https://doi.org/10.1016/C2009-0-03396-0'> Murad M H, Wang Z, Zhu
  Please write DOIs as <doi:prefix/suffix>.

Please fix and resubmit. 


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
