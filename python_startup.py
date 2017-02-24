try:
    import matplotlib as mpl
    import matplotlib.pyplot as plt
except Exception as e:
    pass
try:
    mpl.style.use( 'seaborn-talk' )
    mpl.rcParams['axes.linewidth'] = 0.1
    plt.rc('text', usetex=True)
    plt.rc('font', family='serif')
except Exception as e:
    pass

try:
    import numpy as np
except Exception as e:
    pass
