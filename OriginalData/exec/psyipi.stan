data{
   int<lower=1> nSampledCells; //Number of cells that have been sampled
  int<lower=1> N[nSampledCells];   //Number of sampling events
  int<lower=0> y[nSampledCells];   //Number of detections
  real<lower=0,upper=1> q; // Values for rate of false positives
  real<lower=q,upper=1> minP; //Minimum value for true detectability

}

transformed data{

}

parameters{
  vector <lower=0, upper=1> [nSampledCells] psy_Sampled; // Probability of occupancy sampled cell
  vector <lower=0, upper=1> [nSampledCells] p_raw;
  ordered [2] odds;

}

transformed parameters {

  real <lower=0, upper= 1> pRange;
  vector<lower=0, upper=1> [nSampledCells] p;
  real <lower=fmax(minP,q), upper=1> pmax;
  real <lower=fmax(minP,q), upper=1> pmin;

  pmin = (inv_logit(odds[1]) * (1-fmax(minP,q)))+fmax(minP,q);
  pmax = (inv_logit(odds[2]) * (1-fmax(minP,q)))+fmax(minP,q);


  pRange = pmax-pmin;

  p = (p_raw * pRange)+pmin;




}

model
  {


    target += normal_lpdf(pmin | 0.5, 0.25);
    target += normal_lpdf(p_raw | 1, 0.25);

    target += normal_lpdf(pmax | 0.5, 0.25);

    target += beta_lpdf(psy_Sampled | 0.5, 0.5);


    for (cell in 1:nSampledCells){

  target += log_mix(psy_Sampled[cell],binomial_lpmf(y[cell] | N[cell],p[cell]),
                              binomial_lpmf(y[cell] | N[cell] , q)

                            );
    }

  }

generated quantities
  {


int<lower=0> sim_y[nSampledCells]; //Simulated Sampling
int<lower=0> sim_true_y[nSampledCells]; //Simulated True Detections
int<lower=0> sim_false_y[nSampledCells]; //Simulated False Detections

real<lower=0, upper=1> psy; //Global Occupancy
real<lower=0, upper=1> cellpres_i[nSampledCells];
real<lower=0, upper=1> pCorr[nSampledCells];
vector <lower=0, upper=1> [nSampledCells] pp; //Probability of presence


for (ncell in 1:nSampledCells ){

    pp[ncell] = exp(
    log(psy_Sampled[ncell])+binomial_lpmf(y[ncell] | N[ncell],p[ncell]) -
    log_mix(psy_Sampled[ncell],binomial_lpmf(y[ncell] | N[ncell],p[ncell]),
                              binomial_lpmf(y[ncell] | N[ncell] , q))
                              );  // Probability of presence

      if(bernoulli_rng(pp[ncell])){
         cellpres_i[ncell] = 1;
         pCorr[ncell] = p[ncell];
         sim_true_y[ncell]=binomial_rng(N[ncell],p[ncell]);
         sim_false_y[ncell]=0;

      }else{
         cellpres_i[ncell] = 0;
         pCorr[ncell] = 0;
         sim_true_y[ncell]=0;
         sim_false_y[ncell]=binomial_rng(N[ncell],q);
      }

  sim_y[ncell] = sim_true_y[ncell]+sim_false_y[ncell];

  }

 psy = sum(cellpres_i)/nSampledCells;


}
