## Release 1.0.1

This is a maintenance release.

Changes:

* Clarified documentation for standard deviation recovery routes, especially the distinction between arm-level statistics and contrast-level statistics.
* Added a vignette, "Choosing the correct SD recovery route".
* Added a t-based p-value route to `SEp_from_TE.p()` for small independent two-group continuous outcomes.
* Updated redirected Cochrane Handbook references to the archived version 5.1.0 PDF.
* Updated the minimum R version to R 4.6.0.

## R CMD check results

Checked locally with:

* R 4.6.0 on Windows 11
* `R CMD check --as-cran`

Result:

0 errors | 0 warnings | 1 note

The remaining note is local only:

* HTML validation was skipped because the external `tidy` executable is not installed on this machine.
