# Test environments

- local Windows 10 install, R 4.2.0
- local Linux (Arch) install, R 4.2.0
- local macOS install, R 4.2.0
- GitHub Actions
   - Windows Server, R release
   - MacOS, R release
   - MacOS, R dev
   - Ubuntu, R release
- win-builder
   - devel
   - release


# R CMD check

R CMD check succeeded

── R CMD check results ─────────────────────────────────────────────────────────────────────── rddtools 1.8.0 ────
Duration: 1m 36.3s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

R CMD check succeeded


# win builder

There is a note about a possibly invalid URL, the URL works (JSTOR stable).

Found the following (possibly) invalid URLs:
  URL: https://www.jstor.org/stable/2291516
    From: README.md
    Status: 403
    Message: Forbidden

This is a valid URL that works fine.