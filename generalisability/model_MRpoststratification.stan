// Rewrite of GP model to be consisntent with newest version of cmdstan.
functions{
//Function for Gaussian Process kernel
  matrix GPL(int K, real C, real D, real S){
   matrix[K,K] Rho;
   real KR;
   KR = K;
   for(i in 1:(K-1)){
   for(j in (i+1):K){
    Rho[i,j] = C * exp(-D * ( (j-i)^2 / KR^2) );
    Rho[j,i] = Rho[i,j];
    }}
   for (i in 1:K){ls
    Rho[i,i] = 1;
    }
   return S*cholesky_decompose(Rho);
  }

}

// Define the observed variables we feed into the model as data
data {
  int N;                                 // Sample size
  int MA;                                // Number of age categories
  array[N] int age;                            // Age categories
  array[N] int outcome;                        // Choice (1 means prosocial)
  array[N] int gender;                         // Gender (1 means male, 2 means female)
  array[2, MA] int<lower =0> P_other;          // Population demography from other target population
  array[2, MA] int<lower =0> P_Pop;            // Population demography from population from which sample was taken
}

// Define the unobserved variables (parameters) that we estimate from the data
parameters {
  vector[2] alpha;            //Gender-specific intercepts
  matrix[2,MA] age_effect;    //Matrix for Gaussian process age effects

  //Here we define the Control parameters for the Gaussian processes; they determine how covariance changes with increasing distance in age
  array[2] real<lower=0> eta;
  array[2] real<lower=0> sigma;
  array[2]real<lower=0, upper=1> rho;
}

model {
  vector[N] p;

  // Define priors for parameters
  alpha ~ normal(0, 3);
  eta ~ exponential(2);
  sigma ~ exponential(1);
  rho ~ beta(10, 1);

  //We compute age-specific offsets for each sex
  for ( i in 1:2){
   age_effect[i,] ~ multi_normal_cholesky( rep_vector(0, MA) , GPL(MA, rho[i], eta[i], sigma[i]) );
  }

  //This is the linear model: Choice probabilities are composed of (gender-specific) intercept and gender-specific offset for each age category
  for ( i in 1:N ) {
   p[i] = alpha[gender[i]] + age_effect[gender[i],age[i]];
  }

  //Finally, we need a likelihood function for the observed outcomes
 outcome ~ binomial_logit(1, p);
}

 //We use the generated quantities section for the poststratification and to compute age and gender specific estimates
generated quantities{
   real<lower = 0, upper = 1> p_pop;     // This is value for p in the population from which sample was taken
   real<lower = 0, upper = 1> p_other;   // This is value for p in the other population

   real expect_pos = 0;
   int total = 0;

   //Here we compute predictions for each age and gender class
   vector[MA] pred_p_m;
   vector[MA] pred_p_f;

   pred_p_m = inv_logit(alpha[1] + age_effect[1,]');
   pred_p_f = inv_logit(alpha[2] + age_effect[2,]');

   //Here we do the actual poststratification
   //Poststratified to the population from which sample was taken
    for (a in 1:2){
      for (b in 1:MA){
        total += P_Pop[a,b];
        expect_pos += P_Pop[a,b] * inv_logit(alpha[a] + age_effect[a,b]);
      }
    }
    p_pop = expect_pos / total;

    //Poststratified to other population
    total = 0;
    expect_pos = 0;
       for (a in 1:2){
         for (b in 1:MA){
          total += P_other[a,b];
          expect_pos += P_other[a,b] * inv_logit(alpha[a] + age_effect[a,b]);
         }
       }
     p_other = expect_pos / total;

}
