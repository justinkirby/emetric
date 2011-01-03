from fabric.api import *
from datetime import datetime

env.now = datetime.now().strftime("%Y%m%d%H%M")
env.ver = "00.01.00"
env.pdir = "/Volumes/OurData/WebServer/kickstart.voalte.net/emetric"
env.hosts = ["jkirby@office.voalte.net:64322"]

def release(version=None):
    dirnm = "%s/%s/%s"%(env.pdir,env.ver,env.now)
    ln = "%s/emetric"%(env.pdir,)
    run("mkdir -p %s"%(dirnm,))
    put("emetric",dirnm)
    run("if test -e %s; then rm -f %s; fi"%(ln,ln))
    run("ln -s %s %s"%(dirnm+"/emetric",ln))
    
    
    
    
