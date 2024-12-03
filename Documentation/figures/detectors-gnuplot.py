import numpy

import gnuplotlib


def plot_detector(detector: int):
    data = numpy.load(f'/tmp/coordinates_{detector}.npy')
    mask = numpy.load(f'/tmp/mask_{detector}.npy')

    zoom=160

    x = data[0]
    y = data[1]
    z = data[2]

    # closeup
    g = gnuplotlib.gnuplotlib(multiplot='title "Xpad S140 pixels closeup" layout 1,2',
                              terminal='canvas mousing rounded size 800,600',
                              hardcopy=f"detector_closeup_{detector}.html")

    g.plot(
        # start
        (
            (y[0,0:2], z[0,0:2], dict(_with='points lc "orange" pt 7 ps 1')),
            (y[0:zoom, 0:zoom], z[0:zoom, 0:zoom], dict(_with='points lc "blue" pt 0 ps 1')),
            dict(title="first points", square=True, xlabel='y in meter', ylabel='z in meter')
        ),

        # end
        (
            (y[-1,-2:], z[-1,-2:], dict(_with='points lc "green" pt 7 ps 1')),
            (y[-zoom:, -zoom:], z[-zoom:, -zoom:], dict(_with='points lc "blue" pt 0 ps 1')),
            dict(title="last points", square=True, xlabel='y in meter', ylabel='z in meter')
        )
    )


    # full detector

    g = gnuplotlib.gnuplotlib(multiplot='title "Xpad S140 pixels" layout 1,1',
                              terminal='canvas mousing',
                              hardcopy=f"detector_{detector}.html")

    g.plot(
        (
            (y, z, dict(_with='points lc "blue" pt 0 ps 1')),
            (y[0, 0], z[0, 0], dict(_with='points lc "orange" pt 7 ps 1', legend='first point')),
            (y[-1, -1], z[-1, -1], dict(_with='points lc "green" pt 7 ps 1', legend='last point')),
            dict(square=True, xlabel='y in meter', ylabel='z in meter', set='key outside',
                 _xrange=[numpy.min(y), numpy.max(y)], _yrange=[numpy.min(z), numpy.max(z)])
        )
    )


    # if numpy.min(x) == numpy.max(x):
    #     # 2D
    #     fig = plt.figure(figsize=(8, 8))

    #     # global
    #     ax = fig.add_subplot(221)
    #     ax.set_aspect(1)
    #     ax.set_xlabel('Y meter')
    #     ax.set_ylabel('Z meter')
    #     ax.scatter(y, z, marker=',', linewidths=None)
    #     ax.scatter(y[0,0], z[0,0], marker=',')
    #     ax.scatter(y[-1,-1], z[-1,-1], marker=',')

    #     zz = 10

    #     # mask
    #     ax = fig.add_subplot(222)
    #     ax.set_aspect(1)
    #     ax.matshow(mask)
    #     ax.scatter(0, 0, marker=',', color='orange')
    #     ax.scatter(mask.shape[1]-1, mask.shape[0]-1, marker=',', color='g')
    #     ax.set_xlabel('mask pixels')
    #     ax.set_ylabel('pixels')

    #     # zoom start
    #     ax = fig.add_subplot(223)
    #     ax.set_aspect(1)
    #     ax.scatter(y[0:zz, 0:zz], z[0:zz, 0:zz], marker=',', linewidths=None)
    #     ax.scatter(y[0,0], z[0,0], marker=',')
    #     ax.set_xlabel('Y (zoom start) meter')
    #     ax.set_ylabel('Z meter')

    #     # zoom end
    #     ax = fig.add_subplot(224)
    #     ax.set_aspect(1)
    #     ax.scatter(y[-zz:, -zz:], z[-zz:, -zz:], marker=',', linewidths=None)
    #     ax.scatter(y[-1,-1], z[-1,-1], marker=',', color='g')
    #     ax.set_xlabel('Y (zoom end) meter')
    #     ax.set_ylabel('Z meter')
    # else:
    #     # 3D
    #     fig = plt.figure(figsize=(8, 8))
    #     ax = fig.add_subplot(121, projection='3d')
    #     ax.set_box_aspect(aspect=(1, 1, 1))
    #     ax.scatter(x, y, z, marker=',')
    #     ax.scatter(x[0,0], y[0,0], z[0,0], marker=',', color='orange')
    #     ax.scatter(x[-1,-1], y[-1,-1], z[-1,-1], marker=',', color='green')
    #     ax.set_xlabel('X meter')
    #     ax.set_ylabel('Y meter')
    #     ax.set_zlabel('Z meter')

    #     # mask
    #     ax = fig.add_subplot(122)
    #     ax.set_aspect(1)
    #     ax.matshow(mask)
    #     ax.scatter(0, 0, marker=',', color='orange')
    #     ax.scatter(mask.shape[1]-1, mask.shape[0]-1, marker=',', color='green')
    #     ax.set_xlabel('mask pixels')
    #     ax.set_ylabel('pixels')

#plot_detector(0)
for d in range(8):
    plot_detector(d)

#plt.show()
