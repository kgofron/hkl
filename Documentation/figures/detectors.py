import numpy

import matplotlib.pyplot as plt

from mpl_toolkits.mplot3d import proj3d


def plot_detector(detector: int):
    data = numpy.load(f'/tmp/coordinates_{detector}.npy')
    mask = numpy.load(f'/tmp/mask_{detector}.npy')

    x = data[0]
    y = data[1]
    z = data[2]

    if numpy.min(x) == numpy.max(x):
        # 2D
        fig = plt.figure(figsize=(8, 8))

        # global
        ax = fig.add_subplot(221)
        ax.set_aspect(1)
        ax.set_xlabel('Y meter')
        ax.set_ylabel('Z meter')
        ax.scatter(y, z, marker=',', linewidths=None)
        ax.scatter(y[0,0], z[0,0], marker=',')
        ax.scatter(y[-1,-1], z[-1,-1], marker=',')

        zz = 10

        # mask
        ax = fig.add_subplot(222)
        ax.set_aspect(1)
        ax.matshow(mask)
        ax.scatter(0, 0, marker=',', color='orange')
        ax.scatter(mask.shape[1]-1, mask.shape[0]-1, marker=',', color='g')
        ax.set_xlabel('mask pixels')
        ax.set_ylabel('pixels')

        # zoom start
        ax = fig.add_subplot(223)
        ax.set_aspect(1)
        ax.scatter(y[0:zz, 0:zz], z[0:zz, 0:zz], marker=',', linewidths=None)
        ax.scatter(y[0,0], z[0,0], marker=',')
        ax.set_xlabel('Y (zoom start) meter')
        ax.set_ylabel('Z meter')

        # zoom end
        ax = fig.add_subplot(224)
        ax.set_aspect(1)
        ax.scatter(y[-zz:, -zz:], z[-zz:, -zz:], marker=',', linewidths=None)
        ax.scatter(y[-1,-1], z[-1,-1], marker=',', color='g')
        ax.set_xlabel('Y (zoom end) meter')
        ax.set_ylabel('Z meter')
    else:
        # 3D
        fig = plt.figure(figsize=(8, 8))
        ax = fig.add_subplot(121, projection='3d')
        ax.set_box_aspect(aspect=(1, 1, 1))
        ax.scatter(x, y, z, marker=',')
        ax.scatter(x[0,0], y[0,0], z[0,0], marker=',', color='orange')
        ax.scatter(x[-1,-1], y[-1,-1], z[-1,-1], marker=',', color='green')
        ax.set_xlabel('X meter')
        ax.set_ylabel('Y meter')
        ax.set_zlabel('Z meter')

        # mask
        ax = fig.add_subplot(122)
        ax.set_aspect(1)
        ax.matshow(mask)
        ax.scatter(0, 0, marker=',', color='orange')
        ax.scatter(mask.shape[1]-1, mask.shape[0]-1, marker=',', color='green')
        ax.set_xlabel('mask pixels')
        ax.set_ylabel('pixels')

for d in range(8):
    plot_detector(d)

plt.show()
