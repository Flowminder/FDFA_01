International migration estimates
---------------------------------

The goal is to estimate sex-disaggregated international migration
movements at the subnational level, i.e. male and female migration
movements between administrative units located in different countries.

Following the approach described in Dennett and Wilson (2013), the
IPUMSI-based male and female internal migration estimates presented in
the *Tab Internal Movement* were combined with stock-based male and
female international migration estimates from the ADRI database produced
by Professor Guy Abel specifically for this project (for more details
see section <span id="country">*Estimating International migration
movements from stock migrant data at country level*</span>).

Following Dennett and Wilson (2013), international migration at the
subnational level, both for male and female, was estimated assuming
that, for each country, the distribution of internal out-migrants and
in-migrants is the same as the distribution of international emigrants
and immigrants. In other words, we assumed that for each administrative
unit in the country of origin, the proportion of internal out-migrants
matches the proportion of international emigrants. Similarly, it was
assumed that for each administrative unit in the country of destination,
the proportion of internal in-migrants matches the proportion of
international immigrants.

Thus, an IPF procedure (Willekens, 1999) was used to spatially
disaggregate the male and female international migration estimates to
the subnational level according to the corresponding migration rates.
The procedure was done within a data table (Lomax and Norman, 2016)
where initial values (i.e. seeds) were changed iteratively until the sum
along rows and columns equalled the international migration estimates.

To measure the robustness of the IPF results, we used three different
initial values (ie, seeds): one for all table cells, the inverse of the
distance between each admin unit in "A" and each admin unit in "B", and
a value derived from the original gravity model proposed by Zipf (1946),
simply based on the populations at the origin and destination and the
distance between them. The use of different seeds led to very similar
predictions of international subnational migration (rank correlations
between the three results higher than 0.99). We concluded that IPF is
robust to the different seed values and we proceeded with the IPF
results obtained with the gravity model seeds.

### Validation of the international migration results

The IPUMSI database provides data for some countries that allow the
partial validation of the international migration movements obtained in
the previous step. Specifically, from the census microdata, for several
countries, we can extract the country of origin of international
migrants (mostly from neighbouring countries) in addition to the
administrative unit of residence in the census year.

For example, for Argentina, we can calculate the number of males and
females that migrated to each one of its administrative units from 15
low- and middle-income countries during the 5 years prior to the census.
The full list of countries used for this validation step can be found in
annex of the final report.

Unfortunately, the assumption that for each administrative unit in the
country of origin the proportion of internal out-migrants matches the
proportion of international emigrants could not be tested because
censuses do not contain information about the administrative unit of
residence in the country of origin prior emigration.

We extracted and summarised the number of international female and male
migrants recorded by the censuses in these selected countries in the
IPUMSI database. We then compared these values to the estimates
calculated through the IPF procedure. We calculated R2 [1] between the
IPUMSI-based values and the IPF estimates and we obtained 0.27 for both
male and female migrants. This is a relatively high value considering
the uncertainties regarding international migration and the assumptions
we had to make to address them[2].

### Estimating International migration movements from stock migrant data at country level

Abel and Cohen (2019) compared six different methods for estimating
international migration movements from stock migrant data. Using three
bilateral migration measures, including count, logarithm of count, and
proportions, the estimated 5-year movements based on the method proposed
by Azose and Raftery (2018) had the highest Pearson correlations with
equivalent observed flows (bottom row of Figure 4 in Abel and Cohen,
2019).

Thus, in the framework of this project, gender-disaggregated
international migration movements between all pairs of mid- and
low-income countries were estimated from the 2005 and 2010 UNDESA
gender-disaggregated bilateral migrant stock data (2017 Revision), using
the demographic accounting method proposed by Azose and Raftery
(2019)[3]. It has five main steps:

1.  Adjusting migrant stock tables for accounting for mortality and
    birth (Abel, 2013);
2.  Further alter the migrant stock tables in order to account for
    differences in how the stock data are collected in each country
    (Abel and Sander, 2014);
3.  The stock tables are arranged in an arrays of birthplace-specific
    tables representing 2005-2010 movements between all countries of
    residence: the columns' and rows' sums equates the migrant stocks in
    2005 and 2010, respectively, while the entries of the table,
    representing the movements, are left blank and estimated in the next
    step (Abel and Sander, 2014);
4.  Two models are then used to fill the entries of each
    birthplace-specific table and then summed over birth places in order
    to estimate the movements between each country (Azose and Raftery,
    2019);
5.  Finally, the estimates produced by each model are weighted to create
    the final international migration estimates (Azose and Raftery,
    2019).

The five steps are presented in detail in the final report.

[1] squared coefficient of rank correlation for non-normal data

[2] In terms of mean absolute errors, we obtained an average of 498 for
the female migration model and 550 for the male migration model

[3] The Azose and Raftery (2019) approach advances the methods described
in Abel (2013; 2017) and Abel and Sander (2014).
