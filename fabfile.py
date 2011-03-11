# -------------------------------------------------------------------
#  @author Justin Kirby <jkirby@voalte.com>
#  @copyright (C) 2011 Justin Kirby
#  @end
# 
#  This source file is subject to the New BSD License. You should have received
#  a copy of the New BSD license with this software. If not, it can be
#  retrieved from: http://www.opensource.org/licenses/bsd-license.php
# -------------------------------------------------------------------


from fabric.api import *
from datetime import datetime

env.now = datetime.now().strftime("%Y%m%d%H%M")
env.ver = "00.01.00"


def release(version=None):
    dirnm = "%s/%s/%s"%(env.pdir,env.ver,env.now)
    ln = "%s/emetric"%(env.pdir,)
    run("mkdir -p %s"%(dirnm,))
    put("emetric",dirnm)
    run("if test -e %s; then rm -f %s; fi"%(ln,ln))
    run("ln -s %s %s"%(dirnm+"/emetric",ln))
    
    
    
    
