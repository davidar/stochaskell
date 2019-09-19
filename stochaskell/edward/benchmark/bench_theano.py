from bench_data import *

## Handwritten code with Theano for gradients

import theano
import theano.tensor as T


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

def make_logp_grad():
    beta = T.vector('beta')
    t_x = theano.shared(x)
    t_y = theano.shared(y)
    logit_preds = t_x.dot(beta)
#     y_logit_preds = (2*t_y-1) * logit_preds
#     logp = -T.log(1 + T.exp(-y_logit_preds)).sum()
    logp = -0.5 * T.square(logit_preds - y).sum()
    logp -= 0.5 * T.square(beta).sum()
    grad = T.grad(logp, beta)
    return theano.function([beta], [logp, grad])

logp_grad = make_logp_grad()


beta = np.zeros(D, np.float32)
#%timeit logp_grad(beta)
# while True:
#     logp_grad(beta)


t0 = time.time()
beta = np.zeros(D, np.float32)
samples = []
for i in xrange(100):
    beta = hmc_update(logp_grad, beta, np.sqrt(step_size), n_steps)
    samples.append(beta)
samples = np.array(samples)
print 'Theano (GPU) took %.3f seconds' % (time.time() - t0)
