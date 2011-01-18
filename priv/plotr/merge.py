#!/usr/bin/env python
# -------------------------------------------------------------------
#  @author Justin Kirby <jkirby@voalte.com>
#  @copyright (C) 2011 Justin Kirby
#  @end
# 
#  This source file is subject to the New BSD License. You should have received
#  a copy of the New BSD license with this software. If not, it can be
#  retrieved from: http://www.opensource.org/licenses/bsd-license.php
# -------------------------------------------------------------------


import os
import sys
import getopt
import re

import numpy as np
from matplotlib import mlab


def file_dict(opts):
    rv=[]
    for f in opts["file"]:
        uniq = re.match(opts["re"],f).group(1)
        rv.append({"file":f,
                   "uniq":uniq})
    return rv


def append_rec(recs):
    base = mlab.csv2rec(recs[0]["file"])

    for nw in recs[1:]:
        append = mlab.csv2rec(nw["file"])
        for k,v in append.dtype.fields.iteritems():
            base = mlab.recs_join("sys_tick",k,[base,append],missing=0)
    return base
    


class Usage(Exception):
    def __init__(self,msg):
        self.msg = msg
def usage():
    return """
merge.py [options]

This will attempt to merge csv files generated from emetric from multiple nodes

-h|--help:
    print this help message

-f file|--file=file
   File to append. This can be specified multiple times. The first file will be used as the base and any files after that will be appended.
   merge.py --file=one.csv --file=two.csv

-r regex|--re=regex
   The regex to use to extract unique name from files. most likely: 'emetric_(.*)@.*.csv' which is the default.  Note this uses the python re.match(re,filename).group(1) 

"""
def get_options(argv):
    if argv is None:
        argv = sys.argv
    


    try:
        try:
            opts,args = getopt.getopt(argv[1:], "hf:r:",
                                      ["help","file=","re="])
        except getopt.error,msg:
            raise Usage(msg)
    except Usage,err:
        print >>sys.stderr,err.msg
        print >>sys.stderr," for help use --help"
        return (1,{})

    config = {"file":[],
              "re":"emetric_(.*)@.*.csv"
              }
    for o,a in opts:
        if o in ("-h","--help"):
            print usage()
            return (2,{})
        if o in ("-f","--file"):
            config["file"].append(a)
        if o in ("-r","--re"):
            config["re"] = a
              
            
    return (0,config)
        
    
def main(argv=None):

    print """
********************
********************
********************
********************
THIS DOES NOT WORK
********************
********************
********************
********************
"""
    return 255
    rv,options = get_options(argv)
    if rv: return rv #we die if there was a problem

    if len(options["file"]) <= 0:
        print usage()
        return 2

    print append_rec(file_dict(options))
    return 0

if __name__ == "__main__":
    sys.exit(main())

    
