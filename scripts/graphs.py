import matplotlib.pyplot as plt
import numpy as np
import os.path
import csv


# noquery, congested, ddos, portmatrix, physicaliso, simple, sliceiso

data = {}
new_data = {}

direct = os.path.dirname(os.path.realpath(__file__))
with open(direct + '/output/zoo.csv') as f:
    r = csv.reader(f)
    i = 0
    for row in r:
        i = i + 1 
        if i == 1: 
            continue
        nnodes = int(row[0])
        entry = (float(row[7]), float(row[9]), float(row[23]), float(row[25]), float(row[39]), float(row[41]), float(row[55]), float(row[57]), float(row[71]), float(row[73]), float(row[87]), float(row[89]), float(row[103]), float(row[105]))

        if nnodes in data:
            data[nnodes].append(entry)
        else:
            data[nnodes] = [entry]

        for n, es in data.iteritems():
            avgs = []
            for tup in zip(*es):
                avg = np.mean(map(float, list(tup)))
                avgs.append(avg)
            new_data[n] = avgs

num_nodes = []
noquery_rules = []
noquery_time = []
congested_rules = []
congested_time = []
ddos_rules = [] 
ddos_time = []
portmatrix_rules = []
portmatrix_time = []
physiso_rules = []
physiso_time = []
simple_rules = []
simple_time = []
sliceiso_rules = []
sliceiso_time = []

entries = []
for n, entry in new_data.iteritems():
    entries.append( (n,entry) )
entries = sorted(entries)[1::2]


for (n,entry) in entries:
    num_nodes.append(n)
    (a,b,c,d,e,f,g,h,i,j,k,l,m,n) = entry
    noquery_rules.append(a)
    noquery_time.append(b)
    congested_rules.append(c)
    congested_time.append(d)
    ddos_rules.append(e)
    ddos_time.append(f)
    portmatrix_rules.append(g)
    portmatrix_time.append(h)
    physiso_rules.append(i)
    physiso_time.append(j)
    simple_rules.append(k)
    simple_time.append(l)
    sliceiso_rules.append(m)
    sliceiso_time.append(n)

# Compilation times
fig = plt.figure()
plt.plot(num_nodes, noquery_time, label='No Query')
plt.plot(num_nodes, congested_time, linestyle=':', label="Congested Link", dashes=[8, 4, 2, 4, 2, 4])
plt.plot(num_nodes, ddos_time, linestyle='-.', label="DDOS Sources", dashes=[5,2,10,5])

#plt.plot(num_nodes, portmatrix_time, label="Port Matrix")
plt.plot(num_nodes, physiso_time, label="Physical Iso.", dashes=[14,2])
plt.plot(num_nodes, simple_time, label="Simple Path", linestyle=':')
plt.plot(num_nodes, sliceiso_time, label="Slice Iso.", linestyle='--')
plt.xlabel('Topology Size')
plt.ylabel('Time (sec)')
plt.legend(loc=4)
plt.yscale('log')
fig.savefig(direct + '/output/compilation-time.png')

fig = plt.figure()
plt.plot(num_nodes, noquery_rules, label='No Query')
plt.plot(num_nodes, congested_rules, label="Congested Link", dashes=[8, 4, 2, 4, 2, 4])
plt.plot(num_nodes, ddos_rules, label="DDOS Sources", dashes=[5,2,10,5])
#plt.plot(num_nodes, portmatrix_rules, label="Port Matrix")
plt.plot(num_nodes, physiso_rules, label="Physical Iso.", dashes=[10,2])
plt.plot(num_nodes, simple_rules, label="Simple Path", linestyle=':')
plt.plot(num_nodes, sliceiso_rules, label="Slice Iso.", linestyle='--')
plt.xlabel('Topology Size')
plt.ylabel('Number of Rules')
plt.legend(loc=4)
plt.yscale('log')
fig.savefig(direct + '/output/rule-overhead.png')
