import time

# code in this directory is based on
# https://htmlpreview.github.io/?https://github.com/davidar/pymc3/blob/docs-hmc/notebooks/hmc_experiments.html
# originally at http://docs.pymc.io/notebooks/hmc_experiments.html

import sklearn.datasets
import numpy as np


data = sklearn.datasets.covtype.fetch_covtype()
x = data.data#.astype(np.float32)
y = data.target

x = x - x.mean(0)
x = x / x.std(0)

N = x.shape[0]
D = x.shape[1] + 1

# Add a constant column for the intercept
x = np.hstack([x, np.ones([N, 1])])#, np.float32)])


# Which category to binarize on
values, counts = np.unique(y, return_counts=True)
binarize_on = np.argmax(counts)
y = (y == binarize_on)#.astype(np.float32)


step_size = np.sqrt(0.5 / N)
n_steps = 10
#n_iterations = 1000
n_iterations = 100
