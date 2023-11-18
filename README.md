
<!-- README.md is generated from README.Rmd. Please edit that file -->

Este paquete ya no está en desarrollo. Usar rcmip6: https://github.com/eliocamp/rcmip6

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
```

Y luego se puede listar los modelos disponibles, junto con información

``` r
head(cmip_available())
#>   experiment_id frequency variable_id    source_id initialization_index
#> 1    historical       mon          zg   CNRM-CM6-1                    1
#> 2    historical       mon          zg  CNRM-ESM2-1                    1
#> 3    historical       mon          zg IPSL-CM6A-LR                    1
#> 4    historical       mon          zg       MIROC6                    1
#> 5    historical       mon          zg   MRI-ESM2-0                    1
#>   physics_index forcing_index grid_label datetime_start datetime_stop
#> 1             1             2         gr         185001        201412
#> 2             1             2         gr         185001        201412
#> 3             1             1         gr         185001        201412
#> 4             1             1         gn         185001        201412
#> 5             1             1         gn         185001        201412
#>                                                                                           file
#> 1   ~/DATOS/CMIP6//historical/mon/zg/zg_Amon_CNRM-CM6-1_historical_i1p1f2_gr_185001-201412.nc4
#> 2  ~/DATOS/CMIP6//historical/mon/zg/zg_Amon_CNRM-ESM2-1_historical_i1p1f2_gr_185001-201412.nc4
#> 3 ~/DATOS/CMIP6//historical/mon/zg/zg_Amon_IPSL-CM6A-LR_historical_i1p1f1_gr_185001-201412.nc4
#> 4       ~/DATOS/CMIP6//historical/mon/zg/zg_Amon_MIROC6_historical_i1p1f1_gn_185001-201412.nc4
#> 5   ~/DATOS/CMIP6//historical/mon/zg/zg_Amon_MRI-ESM2-0_historical_i1p1f1_gn_185001-201412.nc4
#>   n_members lon_res  lat_res n_levs        size
#> 1        10 1.40625 1.406250     19 23516427997
#> 2         5 1.40625 1.406250     19 11763026349
#> 3        32 2.50000 1.258741     19 48682698482
#> 4        10 1.40625 1.406250     19 22850319498
#> 5         5 1.12500 1.125000     19 17649975709
```
