# bayesLopod
###### Bayesian Inference of Landscape Occupancy from Presence-Only Data

Natural history museums and herbaria collectively hold hundreds of millions of zoological, botanical, and paleontological specimens. These collections serve as the foundation for understanding the distribution of life on Earth and the basis for addressing loss of biodiversity, emerging diseases, and other pressing global problems as well as important question in ecology and evolution. One of the short comings of these kind of data is that the lack of evidence of the presence of a species in a certain region does not mean the species is truly absent there. Likewise, specimens are often misidentified, and therefore the report of a species in a locality is not always evidence that a viable population occurs there. The goal of this project is to develop a method which could be used to estimate the probability of presence of a species in sampling units of a study region based on certain sampling effort and presence detections pattern.

***

## Model

### `bayesLopod` as an occupancy model

[BRIEF EXPLANATION OF OCCUPANCY MODELS, VARIATION ACROSS SAMPLING EVENTS USING BENULLI PROCESSES]

`bayesLopod` assumes that on average, detectability is the same in a sampling unit, and therefore the probability of observing a certain sampling pattern (i.e. number of detection given a number of sampling events; `theta`), if the species occurs in a sampling unit, follows a binomial distribution:   

![Binomial Expanded](/gif/binomial_exp_eq.gif)
 in Stan, the log-probability of `y` detections, given `N` sampling events and, `p` detectability for each sampling unit, can be added to the target log-probability in the model block:

``` Stan
target  +=  binomial_lpmf(y | N, p);
```
#### Detectability

Because `bayesLopod` is designed to for sampling events that have not been systematically designed, detectability (`p`) has a broader interpretation than in traditional occupancy models.  In this case, `p` does not only account for the imperfect capacity of a researcher to detect a species he or she is looking for, but for other complexities of using non systematically collected presence only data. For example `p` would be decreased by reporting bias, in case that a researcher detects an individual of the species the species, but for whatever reason it is not registered in the database used (e.g. is a common species, other individuals have been detected previously, or it is not a species of interest for the collector) as well as the fact that a species might not occupy the entirety of a sampling unit (sampling units that include the range limits, would have smaller `p`).

Additionally, `bayesLopod` allow flexibility in what `N` represents, and it does not need to be a "sampling event", if this information is not available. `N`, for example, can be the number of detections of individuals in a "target group" (a group of species that a researcher is as likely to collect as the focal species): in this case, `p` represents the ratio of records of the focal species `y` and records of all species in the target group `N`.

#### (Site-level) False positives

Because presence-only data are often not collected by the researcher performing the `bayesLopod` model, there is limitations assessing the quality of every record. To allow flexibility in this regard, `bayesLopod` includes a rate of false negatives as a parameter `q`. This parameter can be estimated by the models or defined by the user (for example, if the user is confident there are no false positives, `q` can be set to 0).

It is worth noting that false positives are assessed at the "sampling-unit" level and not at the "record" level. This means that whether a detection is a false positive or not, depends only on the species occurrence in a sampling unit. Importantly, this will leave deem as true detections a set of false detections (those that were made in sampling units in which the species occurs) but will have no effect in the geographical range occupied.

Like probability of detection (`p`), the rate of false positives `q` has a broader interpretation than purely misidentified and mislabeled records. In `bayesLopod`, `q` would also include individuals that are indeed member of the focal species, but not in its main geographical range, for example vagrants, migrants, or invasive species. More research is needed to determine the way range dynamics interact with this parameter.

Finally, the probability of observing a certain sampling pattern (i.e. number of detection given a number of sampling events; `theta`), if the species **does not** occur in a sampling unit, follows a binomial distribution, with `q` as the "success" rate.  A key distinction with traditional occupancy models, is that to  account for false detections, they require a set of observations that are certain. However, `bayesLopod` estimates the rate of false detections based on two assumptions:

- The rate of false positives `q` is the same across all sampling units.
- The rate of true detections `p` is greater than that of  false positives `q`

The prior distribution of `qRate` (`q` / `p`) is:


 ![qRate Prior](/gif/qRate_prior.gif)

And is included in the model block of the Stan script as:
``` Stan
  target += normal_lpdf(qRate | 0, 0.05);
```

#### Probability of Occupancy (psi)

For each sampling unit, `bayesLopod` estimate a probability of occurrence (psi) maximizing the probability of a sampling pattern `thata` given a detection rate `p` and false positive rate `q`:

![Double Binomial](/gif/binomialpq_eq.gif)

Which is included in the `model` block of the Stan model.

``` Stan
target  += log_mix( psi_Sampled[cell],
                   binomial_lpmf(y[cell] | N[cell] , p),
                   binomial_lpmf(y[cell] | N[cell] , q)
                   );
```

Finally, the prior distribution of psi follows a Beta distribution with larger probabilities for values around 0 or 1.

![psi Prior](/gif/psi_prior.gif)
``` Stan
  target += binomial_lpdf(psy | 0.5, 0.5);
```
#### Variable detection rates across sampling units

The examples above include models in which detectability `p` is the same across all sampling units. However, `bayesLopod` allow for variation in detectability by assigning a `p` rate for each of the sampling units:

![Double Binomial pi](/gif/binomialpiq_eq.gif)

 To do so, it first determines a range (between `pmax` and `pmin`), and then determines a number between 0 and 1 for each of the sampling units (`praw`). If `praw` = 0, then the `p` value for that cell is `pmin`, and if `praw` = 1,  the `p` value for that cell is `pmax`. This is calculated in the `transformed parameters` block in the Stan scripts in which `p` varies:

``` Stan
  pRange = pmax - pmin;
  p = (p_raw * pRange) + pmin ;
```

Then, the 'model' block in these scripts is modified slightly to reflect the variable rate (note that `p[cell]` refers to a sampling unit value, whereas `q` is global)

``` Stan
  target  += log_mix( psi_Sampled[cell],
                      binomial_lpmf(y[cell] | N[cell] , p[cell]),
                      binomial_lpmf(y[cell] | N[cell] , q)
                      );
```

Lastly, `pRange` and `praw` have priors as follows:

- `pRange` around 0, making `pmin` and `pmax` close to each other.
- `praw` around 1, making most values of `p` close to `pmax`

| ![pRange Prior](/gif/pRange_prior.gif) | ![Praw Prior](/gif/praw_prior.gif) |
|---|---|


``` Stan
  target += normal_lpdf(pRange | 0, 0.1);
  target += normal_lpdf(p_raw  | 1, 0.25);
```

While `pmin` and `pmin` have broadly non informative priors:

| ![pRange Prior](/gif/pmin_prior.gif) | ![Praw Prior](/gif/pmax_prior.gif) |
|---|---|

``` Stan
  target += normal_lpdf(pmin | 0.5, 0.25);
  target += normal_lpdf(pmax | 0.5, 0.25);
```
#### CAR model

`bayesLopod` allows unsampled or poorly sampled sampling units to borrow power to predict occupancy based on the patterns of its neighboring units (i.e. a cell is more likely to be occupied if it is surrounded by cells in which the species is present). We do so by implementing a  conditional autoregressive (CAR) model, in which psi is spatially autocorrelated. We thank  @mbjoseph for making the code for this analyses publically available  [(see github repository for more details about this model in Stan and CAR in general)](https://github.com/mbjoseph/CARstan).

In addition to the `function` block defining `sparse_car_lpdf` the followin lines are added to the `model` block in the Stan files for models in which CAR is used. 

``` Stan
  target += sparse_car_lpdf(psi_i | tau, alpha, W_sparse, D_sparse, lambda, n, W_n);
  target += gamma_lpdf(tau | 2, 2);
  // prior for alpha does not add to target, thus it is an uniform distribution between 0 and 1
```

## Examples
### Case Study: _Andropogon gerardii_ (Shape File)

``` R
install.packages("bayesLopod", dependencies = T)

library(bayesLopod)
packageVersion("bayesLopod")
```
Make sure the version installed is > 1.0.1, the most recent version of bayesLopod can be installed from github usung the `devtools` package:
``` R
devtools::install_github( repo = "bayesLopod",
                          username = "camilosanin")

library(bayesLopod)
packageVersion("bayesLopod")
```

``` R
data("Andropogon_shape", package = "bayesLopod")
androSEff = spplot(Andropogon_shape, zcol = c("sampEffort"), main = "Sampling Effort")
androDetect = spplot(Andropogon_shape, zcol = c("detections"), main = "Detections")
plot(androSEff, split = c(1,1,2,1), more = T)
plot(androDetect,  split = c(2,1,2,1), more = F)
```

![Andropogon-input](/gif/Andropogon_input.gif)

``` R
ld_Shape = shapeLopodData( Shapefile = Andropogon_shape,
                           fieldN = "sampEffort",
                           fieldY = "detections",  
                           Adjacency = T,
                           keepFields = F)

```

``` R
mLopodShape = modelLopod( LopodData = ld_Shape,
                          varP = T,
                          q = NULL,
                          pmin = 0,
                          CAR = T,
                          nChains = 3,
                          warmup = 400,
                          sampling = 100,
                          nCores =3)
```
``` R
lopodTrace( mLopodShape, inc_warmup = T)
```
![Andropogon-trace](/gif/Andropogon_trace.gif)

``` R
lopodDens(mLopodShape, c("q", "pmin", "pmax"))
```
![Andropogon-dens](/gif/Andropogon_dens.gif)

``` R
AndroShape = mLopodShape@LopodData@geoDataObject

psiShape = lopodShape(mLopodShape, "psi_i", extrapolate = T,  quant = 0.05)
AndroShape@data[,"psi_05"] = psiShape@data[,"psi_i"]

psiShape = lopodShape(mLopodShape, "psi_i", extrapolate = T,  quant = 0.5)
AndroShape@data[,"psi_50"] = psiShape@data[,"psi_i"]

psiShape = lopodShape(mLopodShape, "psi_i", extrapolate = T,  quant = 0.95)
AndroShape@data[,"psi_95"] = psiShape@data[,"psi_i"]

spplot( AndroShape,
        zcol = c("psi_05", "psi_50", "psi_95"),
        names.attr = c("Psi (5% quantile)", "Psi (median)", "Psi (95% quantile)"), main = "Occupancy (Psi)")
        )
```
![Andropogon-psi-spplot](/gif/Andropogon_psi_spplot.gif)

### Case Study: Simulated Species (Coordinates and Raster)
``` R
data("simSpRecords", package = "bayesLopod")
data("simSpSamplingEffort", package = "bayesLopod")
```
``` R
simSpRasters = xyToRaster( xyRecords = simSpRecords,x
                           ySamplingEffort = simSpSamplingEffort,
                           basemap = NULL,
                           nrows = 50,
                           extentExpansion = 0)

spplot(simSpRasters, names.attr	=c("Species Detections","Sampling Effort"))
```
![XY-input](/gif/XY_Input.gif)
``` R
ld_Raster_adMatrix = rasterLopodData( rasterN = simSpRasters[["samplingEffort"]],
                                      rasterY = simSpRasters[["spDetections"]],
                                      Adjacency = T,
                                      extSample = 1.0,
                                      extDetection = 1.0 )

```
``` R
mLopodRaster = modelLopod( LopodData = ld_Raster_adMatrix,
                           varP = F,
                           q = NULL,
                           pmin = 0,
                           CAR = T,
                           nChains = 4,
                           warmup = 100,
                           sampling = 50,
                           nCores = 4 )
```

``` R
lopodTrace( mLopodRaster, inc_warmup = T)
```
![XY-trace](/gif/XYTrace.gif)
``` R
lopodDens(mLopodRaster, c("q", "p"))
```
![XY-dens](/gif/XYDens.gif)
```R
ppRaster = lopodRaster(mLopodRaster, param = "pp", extrapolate = F, metric = "mean")
psiRaster = lopodRaster(mLopodRaster, param = "psi_i", extrapolate = T, metric = "mean")
```

![Probability of presence](/gif/prPres_eq.gif)
``` R
spplot( raster::stack(psiRaster,ppRaster),  
        names.attr = c("Occupancy (Psi)", "Probability of Presence"))
```
![XY-psi-spplot](/gif/XY_raster.gif)
