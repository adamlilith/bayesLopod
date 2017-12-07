data{
   int<lower=1> nSampledCells; //Number of cells that have been sampled
  int<lower=1> N[nSampledCells];   //Number of sampling events
  int<lower=0> y[nSampledCells];   //Number of detections
  real<lower=0,upper=1> minP; //Minimum value for true detectability
}

transformed data{

}

parameters{
  vector <lower=0, upper=1> [nSampledCells] psi_Sampled; // Probability of occupancy sampled cell
  vector <lower=0, upper=1> [nSampledCells] p_raw;
  ordered [3] odds;



}

transformed parameters {
 real <lower=0, upper=1> q;
  real <lower=minP, upper=1> pmax;
  real <lower=minP, upper=1> pmin;
  real <lower=0, upper= 1> qRate;
  real <lower=0, upper= 1> pRange;
  vector<lower=0, upper=1> [nSampledCells] p;
  vector [nSampledCells] lLh_cell;

  q = inv_logit(odds[1]);
  pmin = inv_logit(odds[2]);
  pmax = inv_logit(odds[3]);

  pRange = pmax-pmin;
  qRate = q/pmin;
  p = (p_raw * pRange)+pmin;

  for (cell in 1:nSampledCells){

  lLh_cell[cell] = log_mix(psi_Sampled[cell],binomial_lpmf(y[cell] | N[cell],p[cell]),
                              binomial_lpmf(y[cell] | N[cell] , q)

                            );
    }


}

model
  {


    target += normal_lpdf(qRate | 0,0.05);
    target += normal_lpdf(pRange | 0,0.1);


    target += normal_lpdf(pmin | 0.5, 0.25);
    target += normal_lpdf(p_raw | 1, 0.25);

    target += normal_lpdf(pmax | 0.5, 0.25);

    target += beta_lpdf(psi_Sampled | 0.5, 0.5);

    target += lLh_cell;


  }

generated quantities
  {


int<lower=0> sim_y[nSampledCells]; //Simulated Sampling
int<lower=0> sim_true_y[nSampledCells]; //Simulated True Detections
int<lower=0> sim_false_y[nSampledCells]; //Simulated False Detections

real<lower=0, upper=1> psi; //Global Occupancy
real<lower=0, upper=1> cellpres_i[nSampledCells];
real<lower=0, upper=1> pCorr[nSampledCells];
vector <lower=0, upper=1> [nSampledCells] pp; //Probability of presence
vector [nSampledCells] expRec; //
real chi_sq; //
real npars;
real lLh;
real AIC;
real AICc;
real bAIC;

npars = nSampledCells + nSampledCells +  3;


lLh = sum(lLh_cell);
AIC = 2 * npars - 2 * lLh;
AICc = AIC + ((2*npars*(npars+1))/(nSampledCells-npars-1));
bAIC = log(nSampledCells) * npars - 2 * lLh;


expRec = (psi_Sampled .* to_vector(N)) .* p  + ((1-psi_Sampled) .* to_vector(N)) * q;
chi_sq = sum(((expRec - to_vector(y)) .* (expRec - to_vector(y))) ./ expRec);


for (ncell in 1:nSampledCells ){

    pp[ncell] = exp(
    log(psi_Sampled[ncell])+binomial_lpmf(y[ncell] | N[ncell],p[ncell]) -
    log_mix(psi_Sampled[ncell],binomial_lpmf(y[ncell] | N[ncell],p[ncell]),
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

 psi = sum(cellpres_i)/nSampledCells;


}
