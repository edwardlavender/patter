## Test environments

* Local M2 MacBook (2023) with macOS Sequoia 15.4, `R` 4.5.0 and `Julia` 1.11.5
    - Results: passed, 7m 6.9s, 2025-04-17
  
* Local Intel MacBook (2017) with macOS Ventura 13.5.2, `R` 4.4.1 and `Julia` 1.10.4
    - Results: passed, 30m 22.7s, 2025-04-17
  
* Local Intel MacBook (2014) with macOS Big Sur 11.7.10, `R` 4.4.2 and `Julia` 1.11.5
    - Results: failed
    - Notes: Patter precompilation is failing (2025-04-18) 
    -      : This is due to GDAL precompilation failure (even in base environment)
    
* Local Lenovo with Ubuntu 24.04 LTS, `R` 4.4.2 and `Julia` 1.10.5
    - Results with `AUTO_JULIA_INSTALL = "false"`: TO DO
    - Results with `AUTO_JULIA_INSTALL = "true"`: failed, 2025-02-28
    - Notes: Requires https://github.com/JuliaInterop/JuliaCall/issues/238 
  
* Local Optimum Desktop with Windows 10 Pro, `R` 4.4.1 and `Julia` 1.10.5
    - Results: passed, 45m 27.1s, 2025-04-18
  
* Eawag SIA-USER024-P HP workstation (SIA-USER024-P) with Windows 10 Education, `R` 4.3.1 and `Julia` 1.10.5
    - Results: passed, 28m 50.3s, 2025-02-28
  
* Eawag (x86_64 siam-linux20) with Debian 5.10.226-1, `R` 4.4.2 and `Julia` 1.11.2
    - Results with `AUTO_JULIA_INSTALL = "false"`: passed, 1m 41.8s, 2025-04-18
    - Results with `AUTO_JULIA_INSTALL = "true"`: passed, 6m 37.9s, 2025-04-18
    - Notes: See [#46](https://github.com/edwardlavender/patter/issues/46) and [#47](https://github.com/edwardlavender/patter/issues/47) for instructions with `AUTO_JULIA_INSTALL = "true"`:
        - Avoid `devtools::load_all()`
        - Unset `JULIA_PROJ`
        - Run `patter::julia_connect()` in global environment
        - Run R CMD Check 
  
* win-builder
    - Results: TO DO
    
* Rhub
    - Results: TO DO
