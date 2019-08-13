
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cimadata

<!-- badges: start -->

<!-- badges: end -->

cimadata es un paquete interno del CIMA para facilitar el acceso a los
datos hosteados en los servidores del CIMA.

## Instalación

Instalar la versión de desarollo con

``` r
# install.packages("devtools")
devtools::install_github("eliocamp/cimadata")
```

## Ejemplo

Promero hay que montar la carpeta localmente

``` r
library(cimadata)
cmip_folder_set("~/DATOS/CMIP6/")
cmip_mount("elio.campitelli")
```

Y luego se puede listar los modelos disponibles, junto con información

``` r
head(cmip_available())
#>     scenario        model timestep var date_start date_end
#> 1 Historical      CanESM5      mon  pr     185001   201412
#> 2 Historical   CNRM-CM6-1      mon  pr     185001   201412
#> 3 Historical  CNRM-ESM2-1      mon  pr     185001   201412
#> 4 Historical  GISS-E2-1-G      mon  pr     185001   201412
#> 5 Historical  GISS-E2-1-H      mon  pr     185001   201412
#> 6 Historical IPSL-CM6A-LR      mon  pr     185001   201412
#>                                                                                 file
#> 1      ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_CanESM5_Historical_185001-201412.nc4
#> 2   ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_CNRM-CM6-1_Historical_185001-201412.nc4
#> 3  ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_CNRM-ESM2-1_Historical_185001-201412.nc4
#> 4  ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_GISS-E2-1-G_Historical_185001-201412.nc4
#> 5  ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_GISS-E2-1-H_Historical_185001-201412.nc4
#> 6 ~/DATOS/CMIP6//Historical/mon/pr/pr_Amon_IPSL-CM6A-LR_Historical_185001-201412.nc4
#>   n_members lon_res  lat_res n_levs
#> 1        25 2.81250 2.812500      1
#> 2        10 1.40625 1.406250      1
#> 3         5 1.40625 1.406250      1
#> 4        10 2.50000 2.000000      1
#> 5        10 2.50000 2.000000      1
#> 6        31 2.50000 1.258741      1
```
