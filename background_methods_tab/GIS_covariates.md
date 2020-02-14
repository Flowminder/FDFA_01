Geographic Information System (GIS) Covariates
----------------------------------------------

In order to estimate internal migration movements for countries with no
available census data on internal migration, a set of 8 geospatial
datasets representing pull and push migration factors, known to
influence internal migration (Garcia et al.
<a href="https://eprints.soton.ac.uk/367712/" target="_blank">2014</a>;
Sorichetta et al,
<a href="https://www.nature.com/articles/sdata201666" target="_blank">2016</a>)
were used to improve the fit and prediction power of the gravity model
originally proposed by Zipf
(<a href="https://www.jstor.org/stable/2087063?seq=1" target="_blank">1946</a>).
They are listed below along with the covariates which were derived for
the models:

1.  Subnational admin unit boundaries (GADM, 2018): Distance\* and
    contiguity between admin units, surface area of each admin unit, and
    number of admin units
2.  Subnational admin unit boundaries (GAUL, 2015): Distance\* and
    contiguity between admin units, surface area of each admin unit, and
    number of admin units
3.  Population counts (adjusted to match UNPD estimates) (WorldPop,
    2018): Total population in each admin unit
4.  GHS Population Grid (EC-JRC and Columbia University CIESIN, 2015):
    Total population as well as proportions of population living in
    urban
5.  GHS Settlement Grid (Dijkstra and Poelmann, 2014; Pesaresi and
    Freire, 2016): centers, urban clusters, and rural areas in each
    admin unit
6.  Gridded global datasets for Gross Domestic Product (GDP) and Human
    Development Index (Kummu et al., 2018; Kummu et al., 2019): Average
    GDP per capita in each admin unit, and average HDI per admin unit
7.  Global Multi-resolution Terrain Elevation Data 2010 (GMTED2010)
    (U.S. Geological Survey): Average TRI (Terrain Roughness Index) in
    each administrative unit
8.  Accessibility to cities 2015 (Malaria Atlas Project, University of
    Oxford): Average accessibility in each administrative unit

Each geospatial dataset was sourced, harmonized, and finally processed
in a Geographic Information System (GIS) environment to derive
covariates of interest aggregated to match the desired administrative
unit level for all 121 Global South countries of interest.

For each of these countries, the surface area of each administrative
unit, as well as the geodesic distance and contiguity between the
centroids representing them, were derived from the corresponding
subnational administrative unit boundary vector dataset (Sorichetta et
al.,
<a href="https://www.nature.com/articles/sdata201666" target="_blank">2016</a>).[1]

The total population at the administrative unit level was calculated by
using the appropriate WorldPop count datasets as inputs and spatially
summing the values of all grid cells located in each unit. The total
population and its proportion in urban centres, urban clusters, and
rural areas in each unit were estimated by spatially overlapping the GHS
Settlement and Population grids.[2]

The Gross Domestic Product per capita and Human Development Index at the
administrative unit level were calculated by using the
"GDP\_per\_capita\_PPP\_1990\_2015" and "HDI\_1990\_2015 datasets (Kummu
et al.,
<a href="https://doi.org/10.5061/dryad.dk1j0" target="_blank">2019</a>)
as inputs and "spatially averaging" the values of all grid cells located
in each unit.

The Terrain Roughness Index (TRI), describing the relative change in
height of neighbouring elevation grid cells and representing one of the
main driving factors of population distribution in China (Wang and Chen,
2016), was derived from Global Multi-resolution Terrain Elevation Data
2010 (GMTED2010) following the steps described by Riley
(<a href="https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&cad=rja&uact=8&ved=2ahUKEwjt5rOjx7XnAhWH2KQKHe8qDj4QFjAAegQIBhAB&url=https%3A%2F%2Fdownload.osgeo.org%2Fqgis%2Fdoc%2Freference-docs%2FTerrain_Ruggedness_Index.pdf&usg=AOvVaw1UUsW42ccwEkkhVewCkGl9" target="_blank">1999</a>)
and summarised at the administrative unit level "spatially averaging"
the values of all grid cells located in each unit.

Similarly, using the "Accessibility to cities 2015" dataset (Weiss et
al.,
<a href="https://www.ncbi.nlm.nih.gov/pubmed/29320477" target="_blank">2018</a>),
the average accessibility for each administrative unit was calculated by
"spatially averaging" the values of all grid cells located in each unit.

[1] In particular, surface areas were calculated after reprojecting the
boundary datasets to the most appropriate country-specific coordinate
system in order to minimise areal distortions.

[2] Please refer to the GHSL Settlement Model for the details; see
<a href="https://ghsl.jrc.ec.europa.eu/data.php?sl=4" target="_blank">here</a>.
