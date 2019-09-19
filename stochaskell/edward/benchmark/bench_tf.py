import tensorflow as tf
from bench_data import *

## Handwritten code with TF for gradients

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

g = tf.Graph()
with g.as_default():
    with tf.device('/gpu:0'):
        tf_beta = tf.placeholder(np.float32, shape=D, name='beta')
        #tf_x = tf.Variable(x.T, trainable=False)
        tf_x = tf.Variable(x.T, trainable=False, dtype=tf.float32)
        #tf_y = tf.Variable(y, trainable=False)
        tf_y = tf.Variable(y.astype(np.float32), trainable=False)
        tf_logit_preds = tf.matmul(tf.expand_dims(tf_beta, 0), tf_x)
        tf_y_logit_preds = (2. * tf_y - 1.) * tf_logit_preds
        tf_logp = -tf.reduce_sum(tf.nn.softplus(-tf_y_logit_preds))
        tf_logp -= 0.5 * tf.reduce_sum(tf.square(tf_beta))
        tf_grad = tf.gradients(tf_logp, tf_beta)[0]
    initializer = tf.initialize_all_variables()
    sess = tf.Session()
    sess.run(initializer)

def logp_grad(beta):
#     return sess.run([tf_logp, tf_grad], {tf_beta: beta[:, np.newaxis]})
    return sess.run([tf_logp, tf_grad], {tf_beta: beta})


beta = np.zeros(D, np.float32)
# %timeit logp_grad(beta)
#%timeit sess.run(tf_grad, {tf_beta:beta})


t0 = time.time()
beta = np.zeros(D, np.float32)
samples = []
for i in xrange(100):
    beta = hmc_update(logp_grad, beta, np.sqrt(step_size), n_steps)
    samples.append(beta)
samples = np.array(samples)
print 'TF (GPU) took %.3f seconds' % (time.time() - t0)


#plot(samples[:, 0])


#%timeit logp_grad(beta)


#plot(samples[:, 0])
