import os

direct = os.path.dirname(os.path.realpath(__file__))
zoo_data_dir = direct + "/../data/zoo/"
zoo_dir = direct + "/zoo/"

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

def zoo_get_nodes():
    if not (os.path.isdir(zoo_dir)):
        os.mkdir(zoo_dir)
    files = os.listdir(zoo_data_dir)
    files = sorted([f for f in files if is_gml(f)])
    sizes = []
    for fname in files: 
        f = open(zoo_data_dir + fname, 'r')
        nodes, _ = parse_gml(f) 
        sizes.append( len(nodes) )
        f.close()
    csvfile = direct + '/output/zoo.csv'
    f = open(csvfile, 'r')
    lines = f.readlines()
    f.close()
    os.remove(csvfile)
    f = open(csvfile, 'w')
    first = True
    i = 0
    for line in lines:
        if first: 
            f.write("Num Nodes," + line)
            first = False 
        else:
            f.write(str(sizes[i]) + "," + line)
            i = i + 1
    f.close()

zoo_get_nodes()

