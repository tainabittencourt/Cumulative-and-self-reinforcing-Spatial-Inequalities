# Cumulative-and-self-reinforcing-Spatial-Inequalities
R Scripts for Bittencourt, T. A., Giannotti, M. and Marques, E. (2020) ‘Cumulative (and self-reinforcing) spatial inequalities: Interactions between accessibility and segregation in four Brazilian metropolises’, Environment and Planning B: Urban Analytics and City Science. doi: 10.1177/2399808320958426.

The codes presented in this directory are divided into 4 sections:

0. Downloads
Script to download and store sociodemographic data from 2010 Brazilian Census (Sample and Universe)

1.1 Urban structure
Script to:
(i) redefine metropolitan regions based on density and work trips;
(ii) classify urban workers into occupational classes according to the EGP class scheme;
(iii) group occupational classes into 3 social groups based on cluster analysis;
(iv) perform spatial microssimulation to estimate class and race data at the census tract level;
(v) create hexagonal urban grid and reallocate census tract data;
(vi) calculate urban inequality and segregation indices;
(vii) create urban occupation maps

1.2 Accessibility
Script to:
(i) get employment data from the Brazilian Ministry of Economy;
(ii) prepare data for Google Earth geocoding by postal code and distribute the jobs into the hexagonal grid;
(iii) calculate 2-step accessibility levels for multiple grid sizes;
(iv) calculate median levels of accessibility for each social group (class and race);
(v) create accessibility maps

1.3 Segregation 
Script to:
(i) create plots and maps related to the interaction of segregation and accessibility after GEODA processing.


Big thanks for all GitHub pages which immensely contributed to the scripts used in this project, particularly Rafael Pereira (https://github.com/rafapereirabr), Robin Lovelace (https://github.com/Robinlovelace) and all countless contributors of Stack Overflow.
