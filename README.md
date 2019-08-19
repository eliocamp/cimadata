
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
```
