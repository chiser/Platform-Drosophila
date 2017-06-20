# -*- coding: utf-8 -*-
"""
Created on Tue Jun 13 11:54:48 2017

@author: LocalAdmin
"""
import numpy as np
light_change=np.diff(side=='ON')
light_change=np.insert(light_change,False,0,axis=1)
ind_light_change=np.where(light_change)

fly_change=np.where(np.diff(ind_light_change[0]))
ind_light_change2=np.insert(ind_light_change[1],[fly_change[0][0]+1,fly_change[0][1]+1,len(ind_light_change[1])],24000)
ind_light_change2=np.insert(ind_light_change2,[0,fly_change[0][0]+2,fly_change[0][1]+3],0)

ind_fly_change=np.insert(ind_light_change[0],[fly_change[0][0]+1,fly_change[0][1]+1,len(ind_light_change[0])],[0,1,2])
ind_fly_change=np.insert(ind_fly_change,[0,fly_change[0][0]+2,fly_change[0][1]+3],[0,1,2])

fly1=ind_light_change2[ind_fly_change==0]
fly2=ind_light_change2[ind_fly_change==1]
fly3=ind_light_change2[ind_fly_change==2]

traces_length=np.diff(fly1);

trace_bout=np.array([[None]*max(traces_length)]*len(traces_length),dtype=float)
trace1=fly_data.loc[:,2]
for i in range(len(traces_length)):
             trace_bout[i,range(traces_length[i])]=trace1[fly1[i]:fly1[i+1]]

plt.figure()
plt.plot(np.transpose(trace_bout))

plt.figure()
plt.plot(np.transpose(trace_bout[0::2,]))

plt.figure()
plt.plot(np.transpose(trace_bout[1::2,]))
