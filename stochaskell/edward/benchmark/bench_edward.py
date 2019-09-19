from bench_data import *

## Edward code

import tensorflow as tf
#sys.path.insert(0, '/usr/local/google/home/mhoffman/research/edward/')

import edward as ed

# Hack for backwards compatibility
# tf.select = tf.where

ed.set_seed(42)


# MODEL
#tf_x = tf.Variable(x.T, trainable=False)
tf_x = tf.Variable(x.T, trainable=False, dtype=tf.float32)

# Standard normal prior on coefficients
#beta = ed.models.Normal(mu=tf.zeros(D), sigma=tf.ones(D))
beta = ed.models.Normal(loc=tf.zeros(D), scale=tf.ones(D))

logit_pred = tf.squeeze(tf.matmul(tf.expand_dims(beta, 0), tf_x))
#ed_y = ed.models.BernoulliWithSigmoidP(p=logit_pred)
ed_y = ed.models.Bernoulli(logits=logit_pred)

# INFERENCE
qbeta = ed.models.Empirical(params=tf.Variable(tf.zeros([n_iterations, D])))
#inference = ed.HMC({beta:qbeta}, data={ed_y:y})
inference = ed.HMC({beta:qbeta}, data={ed_y:y.astype(int)})
t0 = time.time()
inference.run(step_size=step_size, n_steps=n_steps)

ed_time = time.time() - t0


print 'Edward took %.3f seconds' % ed_time


sess = ed.get_session()
ed_samples = sess.run(qbeta.params)
#plot(ed_samples[:, 0])
