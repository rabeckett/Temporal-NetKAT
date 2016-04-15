import os

direct = os.path.dirname(os.path.realpath(__file__))
zoo_data_dir = direct + "/../data/zoo/"
zoo_dir = direct + "/zoo/"

cmd = direct + "/../tkat.native -no-opt -stats -in " + "zoo/"
cmd_opt = direct + "/../tkat.native -stats -in " + "zoo/"

def is_gml(f):
  return len(f) > 4 and f[-4:] == ".gml"

def parse_gml(f):
    lines = f.readlines()
    nodes = []
    edges_src = []
    edges_dst = []
    in_node = False 
    in_edge = False
    for line in lines:
        line = line.strip()
        if in_node:
            arr = line.split(" ")
            if arr[0] == "id":
                nodes.append(arr[1])
        if in_edge:
            arr = line.split(" ")
            if arr[0] == "source":
                edges_src.append(arr[1])
            if arr[0] == "target":
                edges_dst.append(arr[1])
        if len(line) > 0 and line[0] == "]":
            in_node = False 
            in_edge = False
        if len(line) > 4 and line[:4] == "node":
            in_node = True
        if len(line) > 4 and line[:4] == "edge":
            in_edge = True
    assert(len(edges_src) == len(edges_dst))
    return nodes, zip(edges_src, edges_dst)

def zoo_get_choices():
    if not (os.path.isdir(zoo_dir)):
        os.mkdir(zoo_dir)
    files = os.listdir(zoo_data_dir)
    files = sorted([f for f in files if is_gml(f)])
    sizes = []
    for fname in files: 
        name = fname.split(".")[0]
        f = open(zoo_data_dir + fname, 'r')
        nodes, _ = parse_gml(f) 
        sizes.append( (len(nodes), name) )
        f.close()
    sizes.sort()
   
    curr = 5
    chosen = []
    for (x,y) in sizes:
        if x >= curr and x <= 70:
            chosen.append((x,y))
            curr = curr + 5

    return chosen


import subprocess, shlex
from threading import Timer

def parseLine(output, opt):
    data = output.split(", ")
    t = float(data[7])
    r = int(data[5]) if opt else int(data[3])
    return r, t

def run(cmd, opt, timeout_sec):
  proc = subprocess.Popen(shlex.split(cmd), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
  kill_proc = lambda p: p.kill()
  timer = Timer(timeout_sec, kill_proc, [proc])
  try:
    timer.start()
    stdout, _ = proc.communicate()
    timer.cancel()
    return parseLine(stdout, opt)
  except:
    timer.cancel()
    return None

rules = []
rules_opt = []
times = []
times_opt = []

def updateValues(arr1, arr2, res):
    if res is None:
        arr1.append(None)
        arr2.append(None)
    else:
        r,t = res 
        arr1.append(r)
        arr2.append(t)

chosen = zoo_get_choices()
for (x,fname) in chosen:
    print "Comparing " + fname + " with size " + str(x) + "..."
    res = run(cmd + fname + "_global_ddos.tkat", False, 300)
    res_opt = run(cmd_opt + fname + "_global_ddos.tkat", True, 300)
    updateValues(rules,times,res)
    updateValues(rules_opt,times_opt,res_opt)

f = open('output/optimizations.csv', 'w')
for i in range(len(rules)):
    size, name = chosen[i]
    num_rules = rules[i]
    num_rules_opt = rules_opt[i]
    total_time = times[i]
    total_time_opt = times_opt[i]
    ratio = float(num_rules_opt) / float(num_rules) if num_rules is not None and num_rules_opt is not None else None
    output = map(lambda x: str(x), [name,size,num_rules,num_rules_opt,total_time,total_time_opt, ratio])
    f.write(",".join(output) + "\n")
f.close()