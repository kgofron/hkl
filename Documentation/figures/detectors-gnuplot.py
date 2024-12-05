import numpy

import gnuplotlib


def plot_detector(detector: int):
    data = numpy.load(f'/tmp/coordinates_{detector}.npy')
    mask = numpy.load(f'/tmp/mask_{detector}.npy')

    zoom=160

    x = data[0]
    y = data[1]
    z = data[2]

    # closeup first
    g = gnuplotlib.gnuplotlib(terminal='canvas mousing',
                              hardcopy=f"detector_closeup_first_{detector}.html")

    g.plot( (y[0,0:2].flatten(), z[0,0:2].flatten(), dict(_with='points lc "orange" pt 7 ps 1')),
            (y[0:zoom, 0:zoom].flatten(), z[0:zoom, 0:zoom].flatten(), dict(_with='points lc "blue" pt 0 ps 1')),
            title="first points",
            square=True,
            xlabel='y in meter',
            ylabel='z in meter',
            unset='grid'
    )

    # closeup last
    g = gnuplotlib.gnuplotlib(terminal='canvas mousing',
                              hardcopy=f"detector_closeup_last_{detector}.html")

    g.plot( (y[-1,-2:].flatten(), z[-1,-2:].flatten(), dict(_with='points lc "green" pt 7 ps 1')),
            (y[-zoom:, -zoom:].flatten(), z[-zoom:, -zoom:].flatten(), dict(_with='points lc "blue" pt 0 ps 1')),
            title="last points",
            square=True,
            xlabel='y in meter',
            ylabel='z in meter',
            unset='grid'
    )

    # full detector

    g = gnuplotlib.gnuplotlib(terminal='canvas mousing',
                              hardcopy=f"detector_{detector}.html")

    g.plot(
            (y.flatten(), z.flatten(), dict(_with='points lc "blue" pt 0 ps 1')),
            (y[0, 0], z[0, 0], dict(_with='points lc "orange" pt 7 ps 3', legend='first point')),
            (y[-1, -1], z[-1, -1], dict(_with='points lc "green" pt 7 ps 3', legend='last point')),
            square=True, xlabel='y in meter', ylabel='z in meter', set='key outside',
        _xrange=[numpy.min(y), numpy.max(y)], _yrange=[numpy.min(z), numpy.max(z)]
    )


for d in range(8):
    plot_detector(d)
