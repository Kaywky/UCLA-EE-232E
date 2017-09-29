import numpy as np
import csv
import pandas as pd
from collections import defaultdict
import igraph
from pprint import pprint
from igraph import *


def find_euler_tour(graph):
    tour = []
    E = graph

    numEdges = defaultdict(int)

    def find_tour(u):
        for e in E:
            if u == e[0]:
                u,v = e
                E.remove(e)
                find_tour(v)
            elif u == e[1]:
                v,u = e
                E.remove(e)
                find_tour(v)
        tour.insert(0,u)

    for i,j in graph:
        numEdges[i] += 1
        numEdges[j] += 1

    start = graph[0][0]
    for i,j in numEdges.iteritems():
        if j % 2 > 0:
            start = i
            break

    current = start
    find_tour(current)

    if tour[0] != tour[-1]:
        return None
    return tour


g = Graph.Read_Ncol('./finance_data/dup_mst.txt', directed=False, weights = True)
el = g.get_edgelist()

tour = find_euler_tour(el)

tsp = []
tsp_name = []

for i in tour:
    if i not in tsp:
        tsp.append(i)
        tsp_name.append(g.vs[i]['name'])
tsp_name.append(g.vs[0]['name'])
#we need to go back to the first node
'''
f = open('./finance_data/tsp_name.txt', 'w')
for item in tsp_name:
    print >> f, item

f.close()
'''
ff = open('./finance_data/tsp_all.txt', 'w')
for i in tour:
    print >> ff, g.vs[i]['name']
ff.close()
    

