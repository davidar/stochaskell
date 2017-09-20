import numpy as np

data = np.loadtxt('covtype.data', delimiter=',')
m = data[:,:10].mean(0)
s = data[:,:10].std(0)
with open('covtype.data','r') as f:
    for l in f:
        r = l.strip().split(',')
        for i in range(10):
            r[i] = str((float(r[i]) - m[i]) / s[i])
        print(','.join(r))
