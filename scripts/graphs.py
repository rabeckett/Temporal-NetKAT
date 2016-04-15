import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import os.path
import csv


#==============================================================
#
# Compilation Time and Rule Overhead vs. Topology size
#
#==============================================================

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
entries = sorted(entries)#[1::2]

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


def perSwitch(rules, nodes, offset=0):
  averaged = map(lambda (x,y): float(x) / float(y), zip(rules, nodes))
  offset = map(lambda x: x + offset, averaged)
  return offset

# add offsets to make graph readable
noquery_rules   = perSwitch(noquery_rules, num_nodes)
congested_rules = perSwitch(congested_rules, num_nodes)#, offset=17)
ddos_rules      = perSwitch(ddos_rules, num_nodes )
physiso_rules   = perSwitch(physiso_rules, num_nodes)#, offset=6)
simple_rules    = perSwitch(simple_rules, num_nodes)#, offset=17)
sliceiso_rules  = perSwitch(sliceiso_rules, num_nodes)#, offset=11)


# Compilation times
fig = plt.figure()
plt.plot(num_nodes, congested_time, label="Congested Link", linewidth=3.0) #, linestyle=':', dashes=[8, 4, 2, 4, 2, 4])
plt.plot(num_nodes, physiso_time, label="Physical Iso.", linewidth=3.0) #, dashes=[14,2])
plt.plot(num_nodes, ddos_time, label="DDOS Sources", linewidth=3.0) #, linestyle='-.', dashes=[5,2,10,5])
plt.plot(num_nodes, simple_time, label="Simple Path", linewidth=3.0) #, linestyle=':')
plt.plot(num_nodes, sliceiso_time, label="Slice Iso.", linewidth=3.0) #, linestyle='--')
plt.plot(num_nodes, noquery_time, label='No Query', linewidth=3.0)
plt.xlabel('Topology Size', fontsize=20)
plt.ylabel('Avg. Time (sec)', fontsize=20)
plt.tick_params(axis='both', which='major', labelsize=25)
plt.tick_params(axis='both', which='minor', labelsize=25)
plt.xticks([0,25,50,75,100,125,150])
plt.ylim([.01, 10*10*10])
plt.xlim([0, 150])
plt.legend(loc=4, fontsize=16)
plt.yscale('log')
fig.savefig(direct + '/output/compilation-time.png', bbox_inches='tight')

# Number of rules
fig = plt.figure()
plt.plot(num_nodes, congested_rules, label="Congested Link", linewidth=3.0) #, dashes=[8, 4, 2, 4, 2, 4])
plt.plot(num_nodes, physiso_rules, label="Physical Iso.", linewidth=3.0) #, dashes=[10,2])
plt.plot(num_nodes, ddos_rules, label="DDOS Sources", linewidth=3.0) #, dashes=[5,2,10,5])
plt.plot(num_nodes, simple_rules, label="Simple Path", linewidth=3.0) #, linestyle=':')
plt.plot(num_nodes, sliceiso_rules, label="Slice Iso.", linewidth=3.0) #, linestyle='--')
plt.plot(num_nodes, noquery_rules, label='No Query', linewidth=3.0)
plt.xlabel('Topology Size', fontsize=20)
plt.ylabel('Avg. # Rules per Switch', fontsize=20)
plt.tick_params(axis='both', which='major', labelsize=25)
plt.tick_params(axis='both', which='minor', labelsize=25)
plt.xticks([0,25,50,75,100,125,150])
plt.xlim([0, 150])
plt.ylim([0, 350])
plt.legend(loc=2, fontsize=16)
#plt.yscale('log')
fig.savefig(direct + '/output/rule-overhead.png', bbox_inches='tight')


#==============================================================
#
# Optimizations vs. No optimizations for DDOS query
#
#==============================================================

"""
nodes = []
rules = []
rules_opt = []
times = [] 
times_opt = []

def add_when_value(arr, v, f): 
    if v != "None":
        arr.append( f(v) )

f = open(direct + '/output/optimizations.csv')
for line in f.readlines():
    data = line.split(",")
    add_when_value(nodes, data[1], int)
    add_when_value(rules, data[2], int)
    add_when_value(rules_opt, data[3], int)
    add_when_value(times, data[4], float)
    add_when_value(times_opt, data[5], float)
f.close()

# Number of rules
fig = plt.figure()
plt.plot(nodes, rules_opt, label='Optimizations', linewidth=3.0)
plt.plot(nodes[:len(rules)], rules, label="No Optimizations", linewidth=3.0) #, dashes=[8, 4, 2, 4, 2, 4])
plt.xlabel('Topology Size', fontsize=25)
plt.ylabel('Total Rules', fontsize=25)
plt.tick_params(axis='both', which='major', labelsize=25)
plt.tick_params(axis='both', which='minor', labelsize=25)
#plt.xticks([0,25,50,75,100,125,150])
plt.xlim([0, 50])
plt.legend(loc=2, fontsize=16)
#plt.yscale('log')
fig.savefig(direct + '/output/rule-optimizations.png', bbox_inches='tight')

# Time
fig = plt.figure()
plt.plot(nodes, times_opt, label='Optimizations', linewidth=3.0)
plt.plot(nodes[:len(times)], times, label="No Optimizations", linewidth=3.0) #, dashes=[8, 4, 2, 4, 2, 4])
plt.xlabel('Topology Size', fontsize=25)
plt.ylabel('Total Time (sec)', fontsize=25)
plt.tick_params(axis='both', which='major', labelsize=25)
plt.tick_params(axis='both', which='minor', labelsize=25)
#plt.xticks([0,25,50,75,100,125,150])
plt.legend(loc=2, fontsize=16)
#plt.yscale('log')
fig.savefig(direct + '/output/time-optimizations.png', bbox_inches='tight')
"""

#==============================================================
#
# Rule overhead box plot
#
#==============================================================

def overhead(rules):
    return map(lambda (x,y): float(y) / float(x), zip(noquery_rules, rules))

congested_overhead = overhead(congested_rules)[30:]
ddos_overhead      = overhead(ddos_rules)[30:]
physiso_overhead   = overhead(physiso_rules)[30:]
simple_overhead    = overhead(simple_rules)[30:]
sliceiso_overhead  = overhead(sliceiso_rules)[30:]

all_data = [congested_overhead, ddos_overhead, physiso_overhead, simple_overhead, sliceiso_overhead]

fig = plt.figure()
ax1 = fig.add_subplot(111)

# plot box plot
box = ax1.boxplot(all_data, patch_artist=True, sym='')

"""
colors = ['cyan', 'lightblue', 'lightgreen', 'tan', 'pink']
for patch, color in zip(box['boxes'], colors):
    patch.set(linewidth=2)
    patch.set_facecolor(color)
"""
colors = ['cyan', 'red', 'yellow', 'gray', 'blue']

## change outline color, fill color and linewidth of the boxes
for b, c in zip(box['boxes'], colors):
    # change outline color
    #b.set( color='#7570b3', linewidth=2)
    # change fill color
    b.set_facecolor( c )

## change color and linewidth of the whiskers
for whisker in box['whiskers']:
    whisker.set(color='#7570b3', linewidth=2)

## change color and linewidth of the caps
#for cap in box['caps']:
#    cap.set(linewidth=2)

## change color and linewidth of the medians
#for median in box['medians']:
#    median.set(color='#b2df8a', linewidth=2)

## change the style of fliers and their fill
for flier in box['fliers']:
    flier.set(marker='o', color='#e7298a', alpha=0.5)

## Custom x-axis labels
ax1.set_xticklabels(['Congested', 'DDOS', 'Physical Iso.', 'Simple Path', 'Slice Iso.'])

ax1.get_xaxis().tick_bottom()
ax1.get_yaxis().tick_left()

"""
for box in bp['boxes']:
    print str(box)
    # change outline color
    box.set_color( '#7570b3') #, linewidth=2)
    # change fill color
    box.set_facecolor( '#1b9e77' )
    pass
"""

ax1.set_ylabel('Query Rule Overhead')
ax1.set_xticks([y+1 for y in range(len(all_data))])
plt.ylim([1.0, 2.6])
fig.savefig(direct + '/output/rule-overhead-box.png', bbox_inches='tight')
