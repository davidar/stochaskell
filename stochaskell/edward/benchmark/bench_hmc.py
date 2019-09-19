import scipy
from bench_data import *

## Handwritten HMC code

def leapfrog(logp_grad, step_size, z, momentum, last_grad):
    momentum = momentum + 0.5 * step_size * last_grad
    z = z + step_size * momentum
    logp, grad = logp_grad(z)
    momentum = momentum + 0.5 * step_size * grad
    return z, momentum, logp, grad

def hmc_update(logp_grad, last_z, step_size, n_steps):
    D = len(last_z)
    logp, grad = logp_grad(last_z)
    momentum = np.random.randn(D).astype(np.float32)
    new_momentum = momentum.copy()
    new_z = last_z.copy()
    new_grad = grad.copy()
    for l in xrange(n_steps):
        new_z, new_momentum, new_logp, new_grad = leapfrog(logp_grad, step_size, new_z, new_momentum, new_grad)
    alpha = -0.5 * np.square(new_momentum).sum() + new_logp
    alpha -= -0.5 * np.square(momentum).sum() + logp
    if np.log(np.random.rand()) < alpha:
        z = new_z
    else:
        z = last_z
    return z

def logp_grad(beta):
    logp = -0.5 * np.square(beta).sum()
    logit_preds = x.dot(beta)
    y_logit_preds = (2*y-1) * logit_preds
    logp += -np.log(1 + np.exp(-y_logit_preds)).sum()
    grad = -beta
    grad += ((2*y-1) * scipy.special.expit(-y_logit_preds)).dot(x)
    return logp, grad


t0 = time.time()
beta = np.zeros(D, np.float32)
np_samples = []
for i in xrange(n_iterations):
    if i % 10 == 0:
        print i
    beta = hmc_update(logp_grad, beta, step_size, n_steps)
    np_samples.append(beta)
np_samples = np.array(np_samples)
print 'Numpy (CPU) took %.3f seconds' % (time.time() - t0)


#plot(np_samples[:, 0])
