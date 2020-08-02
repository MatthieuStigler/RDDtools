## Test environments

- local Windows 10 install, R 4.0.2
- local Linux (Arch) install, R 4.0.2
- GitHub Actions
   - Windows Server 2019 (10.0.17763): R 4.0.0
   - MacOS (10.15): R 4.0.0 & devel
   - Ubuntu (18.04): R 4.0.0
- win-builder
   - devel
   - release

R CMD check succeeded

* checking for code/documentation mismatches ... WARNING
Variables with usage in documentation object 'STAR_MHE' but not in code:
  'STAR_MHE'
  
STAR_MHE is documentation of a dataset, not a function

This is a rerelease of a package previously on CRAN under the same name, also maintained by me, following the changes to stringAsFactors default change.

There is a note about a possibly invalid URL, the URL works (JSTOR stable).
