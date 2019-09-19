from bench_data import *

## PyStan

import pystan


model_code = '''
data {
    int N;
    int D;

    int y[N];
    matrix[N, D] x;
}
parameters {
    vector[D] beta;
}
model {
    beta ~ normal(0, 1);
    y ~ bernoulli_logit(x * beta);
}
'''
model = pystan.StanModel(model_code=model_code)


t0 = time.time()
control = {'stepsize':step_size, 'int_time':n_steps*step_size,
           'adapt_engaged':False, 'stepsize_jitter':0, 'metric':'unit_e'}
fit = model.sampling(data={'N':N, 'D':D, 'y':y.astype(np.int32), 'x':x},
                     warmup=0, verbose=True, chains=1, iter=n_iterations, algorithm='HMC', init=0,
                     control=control)
stan_time = time.time() - t0


#print 'Stan (CPU, single-threaded) took %.3f seconds (%.3f Edward time)' % (stan_time, stan_time / ed_time)
print 'Stan (CPU, single-threaded) took %.3f seconds' % stan_time


stan_samples = fit.extract()['beta']
#plot(stan_samples[:, 0])
#show()
#plot(fit.extract()['lp__'])
#show()
