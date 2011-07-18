from fabric.api import *
from datetime import datetime

env.now = datetime.now().strftime("%Y%m%d%H%M")
env.ver = "00.02.00"
env.pdir = "/Volumes/OurData/WebServer/kickstart.voalte.net/emetric"
env.hosts = ["jkirby@office.voalte.net:64322"]

def release(version=None):
    """
    scp current ./emetric to voalte kickstart server and setup
    dirs. Then ln latest to this current version.

    version: The version of this release
    """
    dirnm = "%s/%s/%s"%(env.pdir,env.ver,env.now)
    ln = "%s/emetric"%(env.pdir,)
    run("mkdir -p %s"%(dirnm,))
    put("emetric",dirnm)
    run("if test -e %s; then rm -f %s; fi"%(ln,ln))
    run("ln -s %s %s"%(dirnm+"/emetric",ln))
