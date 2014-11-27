
"""nargis.py: Connect to nargis and submit the jobs.

Last modified: Sat Jan 18, 2014  05:01PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import  saga
import sys

class Nargis():

    def __init__(self, addr):
        self.addr = addr
        self.ctx = saga.Context("ssh")
        self.ctx.user_id = "dilawar"
        self.session = saga.Session()
        self.session.add_context(self.ctx)

    def environment(self, **kwargs):
        print("Setting up environment")
        self.jd = saga.job.Description()
        self.jd.environment = {'MYOUTPUT' : "Hellow from nargis"}
        self.jd.executable = 'echo "Dilawar" >> log.txt'
        self.jd.arguments = ['$MYOUTPUT']
        self.jd.output = 'mysagajob.stdout'
        self.jd.error = 'mysagajob.stderr'
        self.jd.project = 'mybayes'

    def submit(self, **kwargs):
        try:
            self.js = saga.job.Service("ssh://nargis", session=self.session)
        except Exception as e:
            print("Failed to connect to nargis with error: %s " % e)
            sys.exit(0)
        self.environment()


def main():
    nargis = Nargis("nargis")
    nargis.submit()

if __name__ == '__main__':
    main()
