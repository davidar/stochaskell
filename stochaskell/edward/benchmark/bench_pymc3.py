from bench_data import *

## PyMC3

import pymc3
import theano
import theano.tensor as T


import pymc3.math
import time


#%pdb


basic_model = pymc3.Model()

with basic_model:
    pymc_beta = pymc3.Normal('beta', mu=0, sd=1, shape=D)
    pymc_y = pymc3.Bernoulli('y', p=pymc3.math.sigmoid(pymc3.math.dot(x, pymc_beta)), observed=y)

    step = pymc3.HamiltonianMC(step_scale=step_size*(1./D)**(-1./4), path_length=step_size*n_steps,
                               step_rand=lambda x:x, scaling=np.ones(D))#, np.float32))
    t0 = time.time()
    #pymc_samples = pymc3.sample(n_iterations, step=step, init=None, njobs=1)
    pymc_samples = pymc3.sample(n_iterations, step=step, init=None, njobs=1, chains=1, tune=0)
pymc_time = time.time() - t0
print('pymc (CPU multithreaded) took %.3f seconds' % pymc_time)


#print 'PyMC3 (CPU, multi-threaded) took %.3f seconds (%.3f Edward time)' % (pymc_time, pymc_time / ed_time)


#plot(pymc_samples.get_values('beta')[:, 0])
