#! /usr/local/bin/python

import sys
import numpy as np

num_cases = int(sys.argv[1])

# mean, std.dev, num. of samples
deltas = np.random.normal(0, 0.1, num_cases)

# low inclusive, hi exclusive, num. of samples
xs = np.random.uniform(0, 3.1415 * 2, num_cases)

for i in range(0, len(xs)):
    x = xs[i]
    delta = deltas[i]
    print "{0}, {1}".format(x, (np.sin(x) + delta))
