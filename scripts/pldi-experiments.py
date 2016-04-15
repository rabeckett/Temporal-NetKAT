import os
import sys
import subprocess as sub
import networkx as nx


NO_QUERY = 0
SIMPLE_PATH = 1
SLICE_ISOLATION = 2
PHYSICAL_ISOLATION = 3
DDOS_SOURCES = 4
CONGESTED_LINK = 5
TRAFFIC_MATRIX = 6

direct = os.path.dirname(os.path.realpath(__file__))

cmd = [direct + "/../tkat.native", "-no-opt", "-stats", "-in"]
cmd_opt = [direct + "/../tkat.native", "-stats", "-in"]


header = "Name[$], KAT size[$], Temp size[$], Num Queries[$], Rules[$], Rules opt1[$], Rules opt2[$], Tags[$], Time(Total)[$], Time(Temporal Automaton)[$], Time (Policy Automaton)[$], Time (Determinization)[$], Time (Minimize)[$], Time (Intersection)[$], Time (Extraction)[$], Time (Rule Gen)[$], "

zoo_data_dir = direct + "/../data/zoo/"
zoo_dir = direct + "/zoo/"
stanford_data_dir = direct + "/../data/stanford/"
stanford_dir = direct + "/stanford/"
output_dir = direct + "/output/"

def is_tkat(f):
    return len(f) > 5 and f[-5:] == ".tkat"

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


# ===========================================================
#
# Queries
#
# ===========================================================

def no_query(f):
    f.write("query = drop,\n")
    return 0

def simple_path(x,y,f):
    f.write("query = sw=" + x + ";ever(sw=" + y + ");pt<-bucket1,\n\n")
    return 1

def slice_isolation(s1, s2, f):
    f.write("query = \n\t(")
    for x in s1:
        f.write("sw=" + x + " or ")
    f.write("drop);last(")
    for x in s2:
        f.write("sw=" + x + " or ")
    f.write("drop);pt<-bucket1 +\n\t(")
    for y in s2:
        f.write("sw=" + y + " or ")
    f.write("drop);last(")
    for x in s1:
        f.write("sw=" + x + " or ")
    f.write("drop);pt<-bucket2,\n")
    return 2*(len(s1) + len(s2))

def physical_isolation(nodes, f):
    f.write("query = always(\n\t")
    for x in nodes:
        f.write("sw=" + x + " or \n\t")
    f.write("drop),\n\n")
    return 1

def ddos_sources(server, nodes, f):
    f.write("query = sw=" + server + ";(\n\t")
    i = 0
    for y in nodes:
        if server != y:
            f.write("ever(start and sw=" + y + ");pt<-bucket" + str(i) + " +\n\t")
            i = i + 1
    f.write("\tdrop),\n\n")
    return i

def congested_link(sw, x, y, f):
    f.write("query = sw=" + sw + ";(\n\t")
    f.write("ever(sw=" + x + " and last(sw=" + y + "));pt<-bucket1 +\n\t")
    f.write("ever(sw=" + y + " and last(sw=" + x + "));pt<-bucket2),\n\n")
    return 2

def traffic_matrix(x, ports, f):
    f.write("query = sw=" + x + ";(\n\t")
    i = 0
    for pt1 in ports:
        for pt2 in ports:
            if pt1 != pt2:
                pt = "pt=" + str(pt1)
                last = ";last(pt=" + str(pt2) + ")"
                bucket = ";pt<-bucket" + str(i)
                f.write(pt + last + bucket + " +\n\t")
                i = i + 1
    f.write("\tdrop),\n")
    return i


def compile_all(directory, out, cmd):
    files = os.listdir(directory)
    files = sorted([f for f in files if is_tkat(f)])
    of = open(out, 'w')
    of.write(header.replace("$", "No Query"))
    of.write(header.replace("$", "Congested"))
    of.write(header.replace("$", "DDOS"))
    of.write(header.replace("$", "Port Matrix"))
    of.write(header.replace("$", "Pys. Isolation"))
    of.write(header.replace("$", "Simple Path"))
    of.write(header.replace("$", "Slice Isolation"))
    of.write("\n")
    of.flush()
    for fname in files:
        print "Compiling " + fname + "..."
        arr = fname.split("_")
        of.write(arr[0] + ", ")
        of.flush()
        sub.call(cmd + [directory + fname], stdout=of)
        of.flush()
        if len(arr) > 2 and arr[2][:-5] == "slice":
          of.write("\n")
          of.flush()
    of.close()


# ===========================================================
#
# Topology Zoo
#
# ===========================================================

def generate_zoo_policy(nodes, edges, is_local, f, scenario):
    rev_edges = [(y,x) for (x,y) in edges]
    g = nx.Graph()
    g.add_nodes_from(nodes)
    g.add_edges_from(edges)
    g.add_edges_from(rev_edges)
    
    # assign address block per node
    dst_map = {}
    for i, n in enumerate(nodes):
        dst_map[n] = i
    
    # assign unique ports for simplicity
    port = 0
    port_map = {}
    for x,y in edges:
        port_map[(x,y)] = port
        port_map[(y,x)] = port + 1
        port = port + 2
    
    if scenario == NO_QUERY:
        no_query(f)

    if scenario == SIMPLE_PATH:
        x,y = nodes[len(nodes)/4], nodes[3*len(nodes)/4]
        simple_path(x, y, f)

    if scenario == SLICE_ISOLATION:
        order = list(nx.dfs_preorder_nodes(g))
        s1, s2 = order[:len(order)/2], order[len(order)/2:]
        slice_isolation(s1, s2, f)

    if scenario == PHYSICAL_ISOLATION:
        physical_isolation(nodes, f)

    if scenario == DDOS_SOURCES:
        x = nodes[len(nodes)/2]
        num = min([20, len(nodes)/4])
        xs = nodes[:num]
        if x in xs:
            xs.remove(x)
        ddos_sources(x, xs, f)

    if scenario == CONGESTED_LINK:
        x,y = edges[len(edges)/4]
        sw = nodes[3*len(nodes)/4]
        congested_link(sw,x, y, f)

    if scenario == TRAFFIC_MATRIX:
        x = nodes[len(nodes)/4]
        ports = set()
        for (a,b) in edges:
            if a == x:
                ports.add(port_map[(a,b)])
            if b == x:
                ports.add(port_map[(b,a)])
        if len(ports) > 1:
            traffic_matrix(x, ports, f)
        else:
            x = nodes[3*len(nodes)/4]
            ports = set()
            for (a,b) in edges:
                if a == x:
                    ports.add(port_map[(a,b)])
                if b == x:
                    ports.add(port_map[(b,a)])
            traffic_matrix(x, ports, f)


    # build policy based on destination-based shortest path routing
    per_sw = {}
    f.write("pol = ")
    for nsrc in nodes: 
        for ntgt in nodes:
            if nsrc == ntgt:
                tmp = "dst=" + str(dst_map[ntgt]) + ";pt<-200 +\n\t\t" # special port for end host
                if nsrc in per_sw.keys():
                    per_sw[nsrc] = per_sw[nsrc] + tmp
                else:
                    per_sw[nsrc] = tmp
            else:
                dst = dst_map[ntgt]
                try:
                    spath = nx.shortest_path(g, nsrc, ntgt)
                    nhop = spath[1]
                    tmp = "dst=" + str(dst) + ";pt<-" + str(port_map[(nsrc,nhop)]) + " +\n\t\t"
                    if nsrc in per_sw.keys():
                        per_sw[nsrc] = per_sw[nsrc] + tmp
                    else:
                        per_sw[nsrc] = tmp
                except nx.NetworkXNoPath:
                  pass
    for sw, pol in per_sw.iteritems():
        f.write("\n\tsw=" + sw + ";(\n\t\t")
        f.write(pol)
        f.write("drop) + \n")
    f.write("\tquery,\n\n")

    # build topology term
    per_sw = {}
    f.write("topo = ")
    for x,y in edges:
        tmp = "pt=" + str(port_map[(x,y)]) + ";sw<-" + y + ";pt<-" + str(port_map[(y,x)]) + " +\n\t\t"
        if x in per_sw.keys():
            per_sw[x] = per_sw[x] + tmp
        else:
            per_sw[x] = tmp
        tmp = "pt=" + str(port_map[(y,x)]) + ";sw<-" + x + ";pt<-" + str(port_map[(x,y)]) + " +\n\t\t"
        if y in per_sw.keys():
            per_sw[y] = per_sw[y] + tmp 
        else:
            per_sw[y] = tmp

    for sw, pol in per_sw.iteritems():
        f.write("\n\tsw=" + sw + ";(\n\t\t")
        f.write(pol)
        f.write("drop) + \n")
    f.write("\tdrop,\n\n")

    if is_local:
        f.write("pol")
    else:
        f.write("(pol;dup;topo;dup)*;pol")


def zoo_create_tkat():
    if not (os.path.isdir(zoo_dir)):
        os.mkdir(zoo_dir)
    files = os.listdir(zoo_data_dir)
    files = sorted([f for f in files if is_gml(f)])
    print "Generating Temporal NetKAT files..."
    for fname in files: 
        f = open(zoo_data_dir + fname, 'rw')
        nodes, edges = parse_gml(f) 
        f.close()
        out_fname = fname.split(".")[0]
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- No Query")
        f = open(zoo_dir + out_fname + "_global.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, NO_QUERY)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- Simple Path")
        f = open(zoo_dir + out_fname + "_global_simple.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, SIMPLE_PATH)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- Slice Isolation")
        f = open(zoo_dir + out_fname + "_global_slice.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, SLICE_ISOLATION)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- Physical Isolation")
        f = open(zoo_dir + out_fname + "_global_physical.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, PHYSICAL_ISOLATION)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- DDOS Sources")
        f = open(zoo_dir + out_fname + "_global_ddos.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, DDOS_SOURCES)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- Congested Link")
        f = open(zoo_dir + out_fname + "_global_congested.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, CONGESTED_LINK)
        f.close()
        print "Generating .tkat file: " + (zoo_data_dir + fname + " -- Port Matrix")
        f = open(zoo_dir + out_fname + "_global_matrix.tkat", 'w')
        generate_zoo_policy(nodes, edges, False, f, TRAFFIC_MATRIX)
        f.close()

def zoo_compile_all():
    compile_all(zoo_dir, direct + "/output/zoo.csv", cmd_opt)


# ===========================================================
#
# Stanford Network
#
# ===========================================================

import json

def read_port_map(fname):
    f = open(fname)
    lines = f.readlines()
    pt_map = {}
    sw = lines[0][1:].replace("\n", "")
    for line in lines[1:]:
        if line[0] == "$":
            sw = line[1:-1].replace("\n","")
        else:
            arr = line.split(":")
            pt = arr[1].replace("\n", "")
            pt_map[pt] = sw
    f.close()
    return pt_map

def read_topology(fname, pt_map):
    f = open(fname)
    lines = f.readlines()[2:]
    topo = []
    for line in lines:
        arr = line.split("$")
        from_pt = arr[1][1:-1]
        to_pt = arr[7][1:-1]
        topo.append( (pt_map[from_pt], from_pt, pt_map[to_pt], to_pt) )
    f.close()
    return topo

def read_pol(fname):
    f = open(fname)
    parsed = json.loads(f.read())
    rules = parsed['rules']
    ofrules = []
    base_dsts = []
    for rule in rules:
        in_ports = rule['in_ports']
        match = rule['ip_dst_match']
        out_ports = rule['out_ports']
        #wc = rule['ip_dst_wc']
        ofrules.append( (in_ports, match, out_ports) )
    f.close()
    return ofrules
 
"""   
def expand_wildcards(pols):
    others = 0
    base_dsts = []
    for (sw, pol) in pols:
        for (_,match,wc,_) in pol:
            if int(wc) == 0:
                base_dsts.append(match)
            else:
                others = others + 1
    print "there are " + str(len(base_dsts)) + " dsts"
    print "there are " + str(others) + " wildcards"
    return pols
"""

def generate_stanford_pol(pols, topo, nodes, f, scenario):
    # update policy to expand wildcards
    #pols = expand_wildcards(pols)

    if scenario == NO_QUERY:
        no_query(f)

    if scenario == SIMPLE_PATH:
        x,y = nodes[len(nodes)/4], nodes[3*len(nodes)/4]
        simple_path(x, y, f)

    if scenario == SLICE_ISOLATION:
        s1, s2 = nodes[:len(nodes)/2], nodes[len(nodes)/2:]
        slice_isolation(s1, s2, f)

    if scenario == PHYSICAL_ISOLATION:
        physical_isolation(nodes, f)

    if scenario == DDOS_SOURCES:
        x = nodes[len(nodes)/2]
        xs = nodes[:len(nodes)/3]
        if x in xs:
            xs.remove(x)
        ddos_sources(x, xs, f)

    if scenario == CONGESTED_LINK:
        x, _, y, _ = topo[len(topo)/4]
        sw = nodes[3*len(nodes)/4]
        congested_link(sw, x, y, f)

    if scenario == TRAFFIC_MATRIX:
        x = nodes[len(nodes)/2]
        ports = set()
        for (a, pta, b, ptb) in topo:
            if a == x:
                ports.add(pta)
            if b == x:
                ports.add(ptb)
        traffic_matrix(x, ports, f)

    string = ""
    for sw, pol in pols:
        string = string + (sw + "=")
        string = string + ("\n\tsw=" + sw + ";(") 
        for rule in pol: 
            (_, match, outs) = rule
            if outs == []:
                continue
            assigns = ""
            for out in outs:
                tmp = "pt<-" + str(out)
                assigns = tmp if assigns == "" else assigns + " + " + tmp
            string = string + ("\n\t\tdst=" + str(match) + ";(" + assigns + ") + ")
        string = string + ("drop),\n\n")
    string = string + "pol= "
    acc = ""
    for sw, _ in pols:
        acc = sw if acc == "" else acc + " + " + sw
    string = string + (acc + " + query,\n\n")
    string = string + ("topo=")
    for (from_sw, from_pt, to_sw, to_pt) in topo:
        fr = "sw=" + from_sw + ";pt=" + from_pt
        to = ";sw<-" + to_sw + ";pt<-" + to_pt
        string = string + ("\n\t" + fr + to + " + ")
    string = string + ("drop,\n\n")
    f.write(string + "(pol;dup;topo;dup)*;pol")


def stanford_create_tkat():
    if not os.path.isdir(stanford_dir):
        os.mkdir(stanford_dir)
    pt_map = read_port_map(stanford_data_dir + "port_map.txt")
    topo = read_topology(stanford_data_dir + "backbone_topology.tf", pt_map)
    sws = list(set( pt_map.values() ))
    pols = []
    for sw in sws:
        pol = read_pol(stanford_data_dir + sw + ".of")
        pols.append( (sw, pol) )

    print "Generating .tkat files: " + (stanford_dir + "stanford...")

    f = open(stanford_dir + "stanford_global.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, NO_QUERY)
    f.close()
    f = open(stanford_dir + "stanford_global_simple.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, SIMPLE_PATH)
    f.close()
    f = open(stanford_dir + "stanford_global_slice.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, SLICE_ISOLATION)
    f.close()
    f = open(stanford_dir + "stanford_global_physical.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, PHYSICAL_ISOLATION)
    f.close()
    f = open(stanford_dir + "stanford_global_ddos.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, DDOS_SOURCES)
    f.close()
    f = open(stanford_dir + "stanford_global_congested.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, CONGESTED_LINK)
    f.close()
    f = open(stanford_dir + "stanford_global_matrix.tkat", 'w')
    generate_stanford_pol(pols, topo, sws, f, TRAFFIC_MATRIX)
    f.close()

def stanford_compile_all():
    compile_all(stanford_dir, direct + "/output/stanford-unopt.csv", cmd)
    compile_all(stanford_dir, direct + "/output/stanford.csv", cmd_opt)

def make_dir(folder):
    if not os.path.exists(folder):
        os.makedirs(folder)


# Main program

if len(sys.argv) != 2:
    print "Error: provide argument: (stanford | zoo | all)"

arg = sys.argv[1]
compile_s = (arg == "stanford" or arg == "all")
compile_z = (arg == "zoo" or arg == "all")

make_dir(output_dir)

if compile_s:
    make_dir(stanford_dir)
    stanford_create_tkat()
if compile_z:
    make_dir(zoo_dir)
    zoo_create_tkat()
if compile_s:
    stanford_compile_all()
if compile_z:
    zoo_compile_all()
