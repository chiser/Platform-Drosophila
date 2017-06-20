# -*- coding: utf-8 -*-
"""
Created on Tue Apr 25 10:50:11 2017

@author: LocalAdmin
"""

# -*- coding: utf-8 -*-
"""
######################## Importing libraries   ####################
"""
#import plotly.plotly as py
#import plotly.figure_factory as ff
#from plotly.graph_objs import graph_objs
#from scipy import stats
from scipy import stats
import math
import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import re
from matplotlib import pylab
import matplotlib.patches as patches
from tkinter.filedialog import askopenfilename
import warnings
#from itertools import islice
#import time

#import tkinter

#import plotly.plotly as py
"""
######################## Functions   ####################
"""
_digits = re.compile('\d')
def contains_digits(d):
     return bool(_digits.search(d))

def extract_ONOFFbouts(side,fly_data):
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
    
    traces_length1=np.diff(fly1);
    traces_length2=np.diff(fly2);
    traces_length3=np.diff(fly3);
    
    trace_bout1=np.array([[None]*max(traces_length1)]*len(traces_length1),dtype=float)
    trace_bout2=np.array([[None]*max(traces_length2)]*len(traces_length2),dtype=float)
    trace_bout3=np.array([[None]*max(traces_length3)]*len(traces_length3),dtype=float)
    trace1=fly_data.loc[:,2]
    trace2=fly_data.loc[:,3]
    trace3=fly_data.loc[:,4]
    for i in range(len(traces_length1)):
                 trace_bout1[i,range(traces_length1[i])]=trace1[fly1[i]:fly1[i+1]]
    for i in range(len(traces_length2)):
                 trace_bout2[i,range(traces_length2[i])]=trace2[fly2[i]:fly2[i+1]]
    for i in range(len(traces_length3)):
                 trace_bout3[i,range(traces_length3[i])]=trace3[fly3[i]:fly3[i+1]]
    return [trace_bout1,trace_bout2,trace_bout3]

## Side vector to know where the fly is: in the light On side, off side or in between the hysteresis 
def side_which(lengthExp,fly_data,Hysteresis,platform,rightside,leftside):
    side = [''  for x in range(lengthExp-1)];
    for i in range(0,lengthExp-1):
        if rightside:
            if (fly_data.loc[i,(platform+1)]>=Hysteresis):
                side[i]= 'ON';
            elif (-Hysteresis<=fly_data.loc[i,(platform+1)]<= Hysteresis):
                side[i]= 'MIDDLE';
            elif (fly_data.loc[i,(platform+1)]<= -Hysteresis):
                side[i]= 'OFF'
        elif leftside:
            if (fly_data.loc[i,(platform+1)]<=Hysteresis):
                side[i]= 'ON';
            elif (-Hysteresis<=fly_data.loc[i,(platform+1)]<= Hysteresis):
                side[i]= 'MIDDLE';
            elif (fly_data.loc[i,(platform+1)]>= -Hysteresis):
                side[i]= 'OFF'
        else:
            warnings.warn("the side of the closed loop changed", Warning)
    return side
        
## Light vector to know when is it on and off
def light_state(lengthExp,side):           
    light=np.ones((lengthExp-1), dtype=bool);
    for i in range(0,lengthExp-1):
        if (i==0) and (side[i] == 'MIDDLE'):
            light[i] = False
        if i>1:
            if side[i] == 'ON':
                light[i]= True
            elif side[i] == 'MIDDLE': 
                light[i]= light[i-1]
            elif side[i]== 'OFF':
                light[i]= False
    return light
                 
def PI_time(fly,digitized):
    PI=[None]*10;
    data=fly_data[fly+1];
    for i in range(min(digitized),max(digitized)+1):
        PI[i-1]=[data[digitized == i].mean()];    
    return np.asarray(PI)

def fetch_PIfrommeta(line):    
    segment=[None]*10;
    a=fly_metadata[line];
    a= a.split();       
    subst=0;
    for i in range(1, len(a)):
        if contains_digits(a[i]):
            segment[i-1-subst]= float(a[i]);
        else:
            if a[i]=='n.':
                segment[i-1-subst]= float('nan');
                subst=subst+1;
    return np.asarray(segment) 

def fetch_modefrommeta(line,fromm):
    segment=[None]*10;
    a=fly_metadata[line];
    a= a.split();       
    segment=a[fromm:(fromm+10)]
    return segment          


def PI_discrete(light,digitized):    
    PI_discrete=[None]*10;
    light=np.insert(light,[0],True,axis=0);
    if len(light)<len(digitized):
        light=np.insert(light,[1],True,axis=0);               
    for i in range(min(digitized),max(digitized)+1):
        PI_discrete[i-1]=(sum(light[digitized == i]==True)-sum(light[digitized == i]==False))/len(light[digitized == i]);
    return np.asarray(PI_discrete)

def PI_discrete_ohnehyst(fly_data,digitized):    
    PI_discrete_ohne=[None]*10;               
    for i in range(min(digitized),max(digitized)+1):
        PI_discrete_ohne[i-1]=(sum(fly_data[digitized == i]>0)-sum(fly_data[digitized == i]<0))/len(fly_data[digitized == i]);
    return np.asarray(PI_discrete_ohne)

def range_threshold(fly_data,TimeExp,num_segs):
    segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=num_segs+1, endpoint=True, retstep=False, dtype=None);
    digitized = np.digitize(fly_data[1], segments);
    digitized[digitized==(num_segs+1)]=num_segs;
    fly_moving=np.array([np.ones(10)]*3,dtype=bool);
    keep_fly=np.array(np.ones(3),dtype=bool);
    for oo in range(3):
        trace=fly_data[oo+2];
        for i in range(min(digitized),max(digitized)+1):
            fly_trace_segment=trace[digitized == i];
            fly_moving[oo,i-1]=(max(fly_trace_segment)-min(fly_trace_segment))>1
        keep_fly[oo]=all(fly_moving[oo,:]);
    return keep_fly


## I just do not understand the function completely to add stride argument
'''def window(seq, n):
    "Returns a sliding window (of width n) over data from the iterable"
    "   s -> (s0,s1,...s[n-1]), (s1,s2,...,sn), ...                   "
    it = iter(seq)
    result = tuple(islice(it,n))
    if len(result) == n:
        yield result    
    for elem in it:
        result = result[1:] + (elem,)
        #yield elem
        yield result
'''
"""
######################## Importing the data   ####################            
"""            
os.getcwd() # Prints the working directory

#To set the working directory:

os.chdir('c:\\Users\LocalAdmin\Desktop\JoystickPraktikum')

# Ask for the number and store it in nFlies
nFlies = input('Give me an integer number: ')

# Make sure the input is an integer number
nFlies = int(nFlies)
#nFlies=1
#nFlies=2;
ON_wiggle2=np.array([[None]*3]*nFlies,dtype=float);
OFF_wiggle2=np.array([[None]*3]*nFlies,dtype=float);
ON_wiggle=np.array([[None]*3]*nFlies,dtype=float);
OFF_wiggle=np.array([[None]*3]*nFlies,dtype=float);
software_PI=np.array([[None]*10]*nFlies,dtype=float);
PI_disc_all=np.asarray([[[None]*10]*3]*nFlies);
PI_time_all=np.asarray([[[None]*10]*3]*nFlies);            
right_batch=[None]*nFlies;
left_batch=[None]*nFlies;
flies_used=np.array([None]*3*nFlies,dtype=bool);            
for oo in range(nFlies):
    #filename= 'trpa1gr28bdChrimson6.dat';
    #filename_modified=filename+'_modif';                     
    filename = askopenfilename()    
    ## with is a context manager: this avoids you needing to close the file once opened with object.close()
    #with open(filename,'r') as file:
        #text=file.read();
        #print(text); 
    ## Save in different files and variables
    #metadata
    f = open(filename,'r')
    fly_metadata = f.readlines()[0:35];
    f.close()
    
    #data
    aligned=fly_metadata[34]=='------------------------------------\t------------------------------------\n';
    if aligned:
        line_no=35;
    else:
        line_no=36;
    fly_data=pd.read_csv(filename, header=None, delim_whitespace=True, skiprows=line_no);                          
    
    isi=np.diff(fly_data.loc[:,1]);
    
    #Hysteresis
    Hysteresis = fly_metadata[31];                          
    Hysteresis = float(Hysteresis.split()[-1]);
                      
    #length of the experiment
    lengthExp = fly_metadata[29];
    lengthExp= int(lengthExp.split()[-1]);
    lengthExp = 24001;              
    #Time of the experiment
    TimeExp = max(fly_data[1]);
    
    
    ## Show an example of how fly data looks like
    segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=11, endpoint=True, retstep=False, dtype=None);
                          
    # Three subplots, the axes array is 1-d
    '''
    f, axarr = plt.subplots(3, sharex=True)
    axarr[0].plot(fly_data[1],fly_data[2]);
    axarr[0].set_title('Fly traces')
    axarr[1].plot(fly_data[1],fly_data[3]);
    axarr[2].plot(fly_data[1],fly_data[4]);                             
    '''
    ## Trying sliding window     
    #fly2=window(fly_data[2], n=2400)
    #indexx=0;
    #range_fly=np.array(np.zeros(len(fly_data[2])-2400),dtype=float);
    #for indexx,w in enumerate(window(fly_data[2], n=2400)):
    #    range_fly[indexx] = max(w)-min(w);
    #Sampling rate in Hz
    Sampling = fly_metadata[30];
    Sampling= int(Sampling.split()[-1]);
    
    ##Fetching the calculated PIs from the experiment
    PI1s= fetch_PIfrommeta(11);
    PI2s= fetch_PIfrommeta(18);
    PI3s= fetch_PIfrommeta(25);
    
    ## Master/Yoked/Test segment info                        
    Mode1= fetch_modefrommeta(7,1);
    Mode2= fetch_modefrommeta(14,1);
    Mode3= fetch_modefrommeta(21,1);
    
    #time.sleep(10)
                
    ## Make a boolean array to know which segments are test
    mode_test=np.array([[None]*3]*10,dtype=bool)
    test_segments=np.array([None]*10,dtype=bool)                         
    for seg in range(10):                         
        mode_test[seg,0]= Mode1[seg] =='Te'                         
        mode_test[seg,1]= Mode1[seg] =='Te'
        mode_test[seg,2]= Mode1[seg] =='Te'
    if all(mode_test[:,0]==mode_test[:,1]) & all(mode_test[:,1]==mode_test[:,2]):
        test_segments= mode_test[:,0];
    else:
        warnings.warn("the conditions tested in each of the platforms are different", Warning)    
                 
    ## Heated side info                         
    Heated1= fetch_modefrommeta(9,2);
    Heated2= fetch_modefrommeta(16,2);
    Heated3= fetch_modefrommeta(23,2);
    
    h1bool=[None]*10;
    h2bool=[None]*10;
    h3bool=[None]*10;
    for i in range(len(Heated3)):
        h1bool[i]= Heated1[i] == 'right' #or Heated3[i] == '->M'
        h2bool[i]= Heated2[i] == 'right' #or Heated3[i] == '->M'                           
        h3bool[i]= Heated3[i] == 'right' #or Heated3[i] == '->M'
    
    rightside= all([all(h1bool),all(h2bool),all(h3bool)]);

    h1bool=[None]*10;
    h2bool=[None]*10;
    h3bool=[None]*10;
    leftside=False;
    if rightside==False:
         for i in range(len(Heated3)):
             h1bool[i]= Heated1[i] == 'left' #or Heated3[i] == '->M'
             h2bool[i]= Heated2[i] == 'left' #or Heated3[i] == '->M'                           
             h3bool[i]= Heated3[i] == 'left' #or Heated3[i] == '->M'
    
         leftside= all([all(h1bool),all(h2bool),all(h3bool)]);             
                  
                  
    ## Concatenate all calculated PIs together                           
    total=[[None]*10]*3;
    total[0]=PI1s;
    total[1]=PI2s;     
    total[2]=PI3s;  
    software_PI[oo,:]=np.nanmean(total,axis=0);
    """
    ######################## Working with the data   ####################
    """

    segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=11, endpoint=True, retstep=False, dtype=None);
          
    side1= side_which(lengthExp,fly_data,Hysteresis,1,rightside,leftside);
    side2= side_which(lengthExp,fly_data,Hysteresis,2,rightside,leftside);
    side3= side_which(lengthExp,fly_data,Hysteresis,3,rightside,leftside);
    side=np.vstack((side1,side2,side3));
    ## Light vector to know when is it on and off
    light1=light_state(lengthExp,side1);
    light2=light_state(lengthExp,side2);                  
    light3=light_state(lengthExp,side3);
    light=np.vstack((light1,light2,light3));            

    ## digitized contains time points corresponding to each of the experiments segments
    digitized = np.digitize(fly_data[1], segments);
    digitized[digitized==11]=10;
    
    ## keep fly is true when the range of fly behavior is less than one. The name should be del_fly rather         
    #keep_fly=[None]*3;        
    #for fly in range(3):             
    #    if (max(fly_data[fly+2])-min(fly_data[fly+2]))<=1:         
    #       keep_fly[fly]=True;
    #    else:
    #       keep_fly[fly]=False;
                   
                                     
    ## Calculates PI from -4 to +4 by time, thus real PI. There is one bug I think         
    PItime1=PI_time(1,digitized);
    PItime2=PI_time(2,digitized);
    PItime3=PI_time(3,digitized);
    PItime=np.hstack((PItime1,PItime2,PItime3));
    #PItime[:,keep_fly]=float('nan');
    # Ask for the number and store it in nFlies
    #del_flies = input('Give one if keep or zero if delete for the three platforms with space in between: ')
    #del_flies =[bool(del_flies.split()[oo]) for oo in range(3)];
    num_segs=5;
    keep_fly=range_threshold(fly_data,TimeExp,num_segs);    
    del_fly= np.array([not i for i in keep_fly],dtype=bool);
    PItime[:,del_fly]=float('nan');
               
    PI_discrete1=PI_discrete(light1,digitized);
    PI_discrete2=PI_discrete(light2,digitized);
    PI_discrete3=PI_discrete(light3,digitized);
    #PI_discrete1=PI_discrete_ohnehyst(fly_data.loc[:,2],digitized);
    #PI_discrete2=PI_discrete_ohnehyst(fly_data.loc[:,3],digitized);
    #PI_discrete3=PI_discrete_ohnehyst(fly_data.loc[:,4],digitized);
    PI_disc=np.vstack((PI_discrete1,PI_discrete2,PI_discrete3));

    #PI_discrete1_ohne=PI_discrete_ohnehyst(fly_data.loc[:,2],digitized);
    #PI_discrete2_ohne=PI_discrete_ohnehyst(fly_data.loc[:,3],digitized);
    #PI_discrete3_ohne=PI_discrete_ohnehyst(fly_data.loc[:,4],digitized);
    #PI_disc[keep_fly]=float('nan');
    PI_disc[del_fly]=float('nan');

    #fly_movement=np.diff(fly_data.loc[:,[2,3,4]],axis=0);
    #fly_movement=np.transpose(np.insert(fly_movement,[0],0,axis=0));
    #mov_light=np.cumsum(np.abs(fly_movement[light]));
    #mov_nolight=np.cumsum(np.abs(fly_movement[np.invert(light)]));
    #switch=np.diff(light,axis=1)
    #switch=np.insert(switch,[0],False,axis=1)
    ## To plot one of them separatedly
#    plt.figure()               
#    xplot = range(len(PItime1));             
#    plt.bar(xplot,PItime1, 0.9, color="blue")
#    plt.xlabel("Training segment")
#    plt.ylabel("Position (a.u.)")
#    plt.title("PI by segment (in time)")
    
    #A = np.array((PItime1,PItime2,PItime3), dtype=float)
    ## Plot PIs by time from the three platforms together
#    x = pylab.arange(len(PItime1));
#    multiple_bars = plt.figure()
#    ax = plt.subplot(111)
#    ax.bar(x-0.2, PItime1,width=0.2,color='b',align='center')
#    ax.bar(x,PItime2,width=0.2,color='g',align='center')
#    ax.bar(x+0.2, PItime3,width=0.2,color='r',align='center')
#    plt.xlabel("Training segment")
#    plt.ylabel("Position PI")
#    plt.title("PI by segment (in time)")
    
    #reinforced=np.mean([PItime1[np.array([not i for i in test_segments])],PItime2[np.array([not i for i in test_segments])],PItime3[np.array([not i for i in test_segments])]]);
    #notreinforced=np.mean([PItime1[test_segments],PItime2[test_segments],PItime3[test_segments]]);                  
    
    #testvstraining=np.asarray([reinforced,notreinforced]);

#    plt.figure()
#    plt.bar(range(len(testvstraining)),testvstraining, 0.9, color="blue")
#    my_xticks = ['reinforcement','not reinforcement']
#    plt.xticks([0,1], my_xticks)

    #A = np.array((fly_data[2],fly_data[3],fly_data[4]), dtype=float);
    
    ## Extracting bouts while light is on and off and plotting the traces
    [trace_bout1,trace_bout2,trace_bout3]=extract_ONOFFbouts(side,fly_data)
    '''            
    plt.figure()
    plt.plot(np.transpose(trace_bout1))
    plt.title("Traces ON/OFF bouts fly1")
    plt.figure()
    plt.plot(np.transpose(trace_bout2))
    plt.title("Traces ON/OFF bouts fly2")
    plt.figure()
    plt.plot(np.transpose(trace_bout3))
    plt.title("Traces ON/OFF bouts fly3")
    '''
    ### Quantifying wiggle while lights are on/off        
    for i in range(3):
        if i==0:
            fly_num=trace_bout1
        elif i==1:
            fly_num=trace_bout2
        elif i==2:
            fly_num=trace_bout3            
        
        a=fly_num[0::2,]>0
        b=fly_num[1::2,]>0                     
        
        if rightside:
            if a.any():
                ON_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,])))
                OFF_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,])))
                ON_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,],n=2)))
                OFF_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,],n=2)))                
            elif b.any():
                OFF_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,])))
                ON_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,])))
                OFF_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,],n=2)))
                ON_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,],n=2)))                
        elif leftside:
            if a.any():
                OFF_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,])))
                ON_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,])))
                OFF_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,],n=2)))
                ON_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,],n=2)))                
 
            elif b.any():
                ON_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,])))
                OFF_wiggle[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,]))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,])))
                ON_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[0::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[0::2,],n=2)))
                OFF_wiggle2[oo,i]=np.nansum(np.nansum(abs(np.diff(fly_num[1::2,],n=2))))/np.count_nonzero(~np.isnan(np.diff(fly_num[1::2,],n=2)))                
 
        
           
    flies_used[[(3*oo),(3*oo+1),(3*oo+2)]]=keep_fly;            
    right_batch[oo]=rightside;
    left_batch[oo]=leftside;        
    PI_time_all[oo,:,:]=np.transpose(PItime);            
    PI_disc_all[oo,:,:]=PI_disc;
               
PI_time_all=np.array(np.reshape(PI_time_all, (nFlies*3, 10)),dtype=float)
PI_disc_all=np.array(np.reshape(PI_disc_all, (nFlies*3, 10)),dtype=float)

right=np.array(np.zeros(nFlies*3),dtype=bool);
left=np.array(np.zeros(nFlies*3),dtype=bool);
for i in range(len(right_batch)):
    right[[i*3,i*3+1,i*3+2]]=right_batch[i]
    left[[i*3,i*3+1,i*3+2]]=left_batch[i]

right=right & flies_used;
left=left & flies_used;             
## Mean of the the three platforms and of batches
if any(right):
    PI_time_popul_right=np.nanmean(PI_time_all[right,:],axis=0,dtype=float);
    #PI_time_popul_right = np.vstack(PI_time_popul_right[:]).astype(np.float);
    Sem_time_popul_right=np.array([None]*10,dtype=float);
    for a in range(10):
        data=np.array(PI_time_all[right,a],float);
        Sem_time_popul_right[a]=np.nanstd(data)/math.sqrt(len(data)-sum(np.isnan(data)));
                                                    
    PI_disc_popul_right=np.nanmean(PI_disc_all[right,:],axis=0,dtype=float);
    #PI_disc_popul_right = np.vstack(PI_disc_popul_right[:]).astype(np.float);
    Sem_disc_popul_right=np.array([None]*10,dtype=float);
    for a in range(10):
        data=np.array(PI_disc_all[right,a],float);
        Sem_disc_popul_right[a]=np.nanstd(data)/math.sqrt(len(data)-sum(np.isnan(data)));

if any(left):                               
    PI_time_popul_left=np.nanmean(PI_time_all[left,:],axis=0,dtype=float);
    #PI_time_popul_left = np.vstack(PI_time_popul_left[:]).astype(np.float);
    Sem_time_popul_left=np.array([None]*10,dtype=float);
    for a in range(10):
        data=np.array(PI_time_all[left,a],float);
        Sem_time_popul_left[a]=np.nanstd(data)/math.sqrt(len(data)-sum(np.isnan(data)));

    PI_disc_popul_left=np.nanmean(PI_disc_all[left,:],axis=0,dtype=float);
    #PI_disc_popul_left = np.vstack(PI_disc_popul_left[:]).astype(np.float);
    Sem_disc_popul_left=np.array([None]*10,dtype=float);
    for a in range(10):
        data=np.array(PI_disc_all[left,a],float);
        Sem_disc_popul_left[a]=np.nanstd(data)/math.sqrt(len(data)-sum(np.isnan(data)));

    ## Plot PIs by time from the three platforms together
x = pylab.arange(10);
fig1=plt.figure()
ax = plt.subplot()
if any(right) & any(left):
    rightbar_time=ax.bar(x-0.2, PI_time_popul_right,width=0.3,color='b',yerr=Sem_time_popul_right,align='center')
    leftbar_time=ax.bar(x+0.2,PI_time_popul_left,width=0.3,color='g',yerr=Sem_time_popul_left,align='center')
    ax.legend((rightbar_time, leftbar_time), ('Right', 'Left'))    
elif not any(left) & any(right):
    leftbar_time=ax.bar(x,PI_time_popul_left,width=0.8,color='g',yerr=Sem_time_popul_left,align='center')
    ax.legend(leftbar_time, ['Left'])
else:    
    rightbar_time=ax.bar(x, PI_time_popul_right,width=0.8,color='b',yerr=Sem_time_popul_right,align='center')
    ax.legend((rightbar_time), ['Right'])
my_xticks = Mode1;
plt.xticks(range(0,10), my_xticks)
plt.xlabel("Training segment")
plt.ylabel("Weighted PI")
plt.title("PI by segment (weighted)")
fig1.savefig('Extended_weighted.png', dpi=90, bbox_inches='tight')

x = pylab.arange(10);
fig2=plt.figure()
ax2 = plt.subplot()
if any(right) & any(left):
    rightbar=ax2.bar(x-0.2,PI_disc_popul_right,width=0.3,color='b',yerr=Sem_disc_popul_right,align='center')
    leftbar=ax2.bar(x+0.2,PI_disc_popul_left,width=0.3,color='g',yerr=Sem_disc_popul_left,align='center')
    ax2.legend((rightbar, leftbar), ('Right', 'Left'))    
elif (not any(left)) & any(right):
    rightbar=ax2.bar(x,PI_disc_popul_right,width=0.8,color='g',yerr=Sem_disc_popul_right,align='center')
    ax2.legend(rightbar, ['Right'])    
else: 
    leftbar=ax2.bar(x,PI_disc_popul_left,width=0.8,color='g',yerr=Sem_disc_popul_left,align='center')
    ax2.legend(leftbar, ['Left'])
my_xticks = Mode1
plt.xticks(range(0,10), my_xticks)
ax2.add_patch(patches.Rectangle((-1, 0), 11, 1,alpha=0.2,facecolor="red"))
plt.xlabel("Training segment")
plt.ylabel("Logical PI")
plt.title("PI by segment (logical)")
fig2.savefig('Extended_logical.png', dpi=90, bbox_inches='tight')

if any(left):
    Te_sem_left_disc=float(stats.sem(PI_disc_all[left,-1]));    
    Te_sem_left_time=float(stats.sem(PI_time_all[left,-1]));   
    Te_mean_left_disc=np.mean(PI_disc_all[left,-1]);    
    Te_mean_left_time=np.mean(PI_time_all[left,-1]);                                        
    preTe_sem_left_disc=float(stats.sem(PI_disc_all[left,0]));    
    preTe_sem_left_time=float(stats.sem(PI_time_all[left,0]));    
    preTe_mean_left_disc=np.mean(PI_disc_all[left,0]);    
    preTe_mean_left_time=np.mean(PI_time_all[left,0]);
    Re_sem_left_disc=float(stats.sem(np.mean(PI_disc_all[left,1:9],axis=1),axis=0));
    Re_sem_left_time=float(stats.sem(np.mean(PI_disc_all[left,1:9],axis=1),axis=0));                       
    Re_mean_left_disc=np.mean(np.mean(PI_disc_all[left,1:9],axis=1),axis=0);                       
    Re_mean_left_time=np.mean(np.mean(PI_disc_all[left,1:9],axis=1),axis=0); 
                              
'''                             
    Te_sem_left_disc=float(stats.sem(PI_disc_popul_left[-1]));
    Te_sem_left_time=float(stats.sem(PI_time_popul_left[-1]));
    Te_mean_left_disc=np.mean(PI_disc_popul_left[-1]);
    Te_mean_left_time=np.mean(PI_time_popul_left[-1]);
    preTe_sem_left_disc=float(stats.sem(PI_disc_popul_left[0]));
    preTe_sem_left_time=float(stats.sem(PI_time_popul_left[0]));
    preTe_mean_left_disc=np.nanmean(PI_disc_all[left,0],axis=0,dtype=float);                         
    preTe_mean_left_time=np.mean(PI_time_popul_left[0]);                          
    Re_sem_left_disc=float(stats.sem(PI_disc_popul_left[1::2]));
    Re_sem_left_time=float(stats.sem(PI_time_popul_left[1::2]));
    Re_mean_left_disc=np.mean(PI_disc_popul_left[1::2]);
    Re_mean_left_time=np.mean(PI_time_popul_left[1::2]);                            
'''
Te_sem_all_disc=float(stats.sem(PI_disc_all[~np.isnan(PI_disc_all[:,-1]),-1]));    
Te_sem_all_time=float(stats.sem(PI_time_all[~np.isnan(PI_time_all[:,-1]),-1]));   
Te_mean_all_disc=np.nanmean(PI_disc_all[:,-1]);    
Te_mean_all_time=np.nanmean(PI_time_all[:,-1]);                                        
preTe_sem_all_disc=float(stats.sem(PI_disc_all[~np.isnan(PI_disc_all[:,0]),0]));    
preTe_sem_all_time=float(stats.sem(PI_time_all[~np.isnan(PI_time_all[:,0]),0]));    
preTe_mean_all_disc=np.nanmean(PI_disc_all[:,0]);    
preTe_mean_all_time=np.nanmean(PI_time_all[:,0]);
Re_sem_all_disc=stats.sem(np.nanmean(PI_disc_all[~np.isnan(PI_disc_all[:,0]),1:9],axis=1),axis=0);
Re_sem_all_time=stats.sem(np.nanmean(PI_disc_all[~np.isnan(PI_disc_all[:,0]),1:9],axis=1),axis=0);                       
Re_mean_all_disc=np.nanmean(np.nanmean(PI_disc_all[:,1:9],axis=1),axis=0);                       
Re_mean_all_time=np.nanmean(np.nanmean(PI_disc_all[:,1:9],axis=1),axis=0); 
                              
if any(right):                             
    Te_sem_right_disc=float(stats.sem(PI_disc_all[right,-1]));    
    Te_sem_right_time=float(stats.sem(PI_time_all[right,-1]));   
    Te_mean_right_disc=np.mean(PI_disc_all[right,-1]);    
    Te_mean_right_time=np.mean(PI_time_all[right,-1]);                                        
    preTe_sem_right_disc=float(stats.sem(PI_disc_all[right,0]));    
    preTe_sem_right_time=float(stats.sem(PI_time_all[right,0]));    
    preTe_mean_right_disc=np.mean(PI_disc_all[right,0]);    
    preTe_mean_right_time=np.mean(PI_time_all[right,0]);
    Re_sem_right_disc=stats.sem(np.mean(PI_disc_all[right,1:9],axis=1),axis=0);
    Re_sem_right_time=stats.sem(np.mean(PI_disc_all[right,1:9],axis=1),axis=0);                       
    Re_mean_right_disc=np.mean(np.mean(PI_disc_all[right,1:9],axis=1),axis=0);                       
    Re_mean_right_time=np.mean(np.mean(PI_disc_all[right,1:9],axis=1),axis=0);                       
                           
#Te_sem_right_disc=float(stats.sem(PI_disc_popul_right[0::2]));
#Te_sem_left_disc=float(stats.sem(PI_disc_popul_left[0::2]));
#Te_sem_right_time=float(stats.sem(PI_time_popul_right[0::2]));
#Te_sem_left_time=float(stats.sem(PI_time_popul_left[0::2]));
#Te_mean_right_disc=np.mean(PI_disc_popul_right[0::2]);
#Te_mean_left_disc=np.mean(PI_disc_popul_left[0::2]);
#Te_mean_right_time=np.mean(PI_time_popul_right[0::2]);
#Te_mean_left_time=np.mean(PI_time_popul_left[0::2]);                         
                         

x = pylab.arange(1);
fig3=plt.figure()
ax2 = plt.subplot()
if any(right) & any(left):
    x = pylab.arange(2);
    preTe=ax2.bar(x-0.32,[preTe_mean_right_disc,preTe_mean_left_disc],width=0.3,color='g', yerr=[preTe_sem_right_disc,preTe_sem_left_disc],align='center')                   
    Reinf=ax2.bar(x,[Re_mean_right_disc,Re_mean_left_disc],width=0.3,color='b', yerr=[Re_sem_right_disc,Re_sem_left_disc],align='center')
    Test=ax2.bar(x+0.32,[Te_mean_right_disc,Te_mean_left_disc],width=0.3,color='g', yerr=[Te_sem_right_disc,Te_sem_left_disc],align='center')
    ax2.legend((preTe, Reinf, Test), ('Pretest','Reinforcement', 'Test'))
    my_xticks = ['Right','Left']
    plt.xticks(range(0,2), my_xticks)
elif (not any(left)) & any(right):
    preTe=ax2.bar(x-0.32,preTe_mean_right_disc ,width=0.3,color='g', yerr=preTe_sem_right_disc,align='center')
    Reinf=ax2.bar(x,Re_mean_right_disc ,width=0.3,color='b', yerr=Re_sem_right_disc,align='center')
    Test=ax2.bar(x+0.32,Te_mean_right_disc ,width=0.3,color='g', yerr=Te_sem_right_disc,align='center')
    ax2.legend((preTe,Reinf, Test), ('Pretest','Reinforcement', 'Test'))
    my_xticks = ['Right']
    plt.xticks(range(0,1), my_xticks)
else:
    preTe=ax2.bar(x-0.32,preTe_mean_left_disc ,width=0.3,color='g', yerr=preTe_sem_left_disc,align='center')
    Reinf=ax2.bar(x,Re_mean_left_disc ,width=0.3,color='b', yerr=Re_sem_left_disc,align='center')
    Test=ax2.bar(x+0.32,Te_mean_left_disc ,width=0.3,color='g', yerr=Te_sem_left_disc,align='center')
    ax2.legend((preTe,Reinf, Test), ('Pretest','Reinforcement', 'Test'))
    my_xticks = ['Left']
    plt.xticks(range(0,1), my_xticks)    
plt.xlabel("Treatment")
plt.ylabel("Logical PI")
plt.title("PI by treatment")
ax2.add_patch(patches.Rectangle((-1, 0), 3, 1,alpha=0.2,facecolor="red"))
fig3.savefig('Summary_logical.png', dpi=90, bbox_inches='tight')

x = pylab.arange(1);
fig4=plt.figure()
ax2 = plt.subplot()
if any(right) & any(left):
    x = pylab.arange(2);    
    preTe=ax2.bar(x-0.32,[preTe_mean_right_time,preTe_mean_left_time],width=0.3,color='g', yerr=[preTe_sem_right_time,preTe_sem_left_time],align='center')
    Reinf=ax2.bar(x,[Re_mean_right_time,Re_mean_left_time],width=0.3,color='b', yerr=[Re_sem_right_time,Re_sem_left_time],align='center')
    Test=ax2.bar(x+0.32,[Te_mean_right_time,Te_mean_left_time],width=0.3,color='g', yerr=[Te_sem_right_time,Te_sem_left_time],align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Right','Left']
    plt.xticks(range(0,2), my_xticks)    
elif (not any(left)) & any(right):
    preTe=ax2.bar(x-0.32,preTe_mean_right_time ,width=0.3,color='g', yerr=preTe_sem_right_time,align='center')
    Reinf=ax2.bar(x,Re_mean_right_time ,width=0.3,color='b', yerr=Re_sem_right_time,align='center')
    Test=ax2.bar(x+0.32,Te_mean_right_time ,width=0.3,color='g', yerr=Te_sem_right_time,align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Right']
    plt.xticks(range(0,1), my_xticks)
else:   
    preTe=ax2.bar(x-0.32,preTe_mean_left_time ,width=0.3,color='g', yerr=preTe_sem_left_time,align='center')
    Reinf=ax2.bar(x,Re_mean_left_time ,width=0.3,color='b', yerr=Re_sem_left_time,align='center')
    Test=ax2.bar(x+0.32,Te_mean_left_time ,width=0.3,color='g', yerr=Te_sem_left_time,align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Left']
    plt.xticks(range(0,1), my_xticks)
plt.xlabel("Treatment")
plt.ylabel("Weighted PI")
plt.title("PI by treatment")
fig4.savefig('Summary_weighted.png', dpi=90, bbox_inches='tight')


x = pylab.arange(1);
fig3=plt.figure()
ax2 = plt.subplot()
preTe=ax2.bar(x-0.32,preTe_mean_all_disc ,width=0.3,color='g', yerr=preTe_sem_all_disc,align='center')
Reinf=ax2.bar(x,Re_mean_all_disc ,width=0.3,color='b', yerr=Re_sem_all_disc,align='center')
Test=ax2.bar(x+0.32,Te_mean_all_disc ,width=0.3,color='g', yerr=Te_sem_all_disc,align='center')
ax2.legend((preTe,Reinf, Test), ('Pretest','Reinforcement', 'Test'))
my_xticks = ['Pooled']
plt.xticks(range(0,1), my_xticks)

#soft=np.nanmean(software_PI,axis=0);
plt.figure()                           
plt.plot(np.transpose(PI_time_all[right,:]))

plt.figure()                           
plt.plot(np.transpose(PI_time_all[left,:]))

plt.figure()                           
plt.plot(np.transpose(PI_disc_all[right,:]))

plt.figure()                           
plt.plot(np.transpose(PI_disc_all[left,:]))

OFF_wiggle=np.reshape(OFF_wiggle,[1,nFlies*3])
ON_wiggle=np.reshape(ON_wiggle,[1,nFlies*3])

OFF_wiggle2=np.reshape(OFF_wiggle2,[1,nFlies*3])
ON_wiggle2=np.reshape(ON_wiggle2,[1,nFlies*3])

sem_ON=stats.sem(ON_wiggle[~np.isnan(ON_wiggle)]);                       
mean_ON=np.nanmean(ON_wiggle);
sem_OFF=stats.sem(OFF_wiggle[~np.isnan(OFF_wiggle)]);                       
mean_OFF=np.nanmean(OFF_wiggle);

sem_ON2=stats.sem(ON_wiggle2[~np.isnan(ON_wiggle2)]);                       
mean_ON2=np.nanmean(ON_wiggle2);
sem_OFF2=stats.sem(OFF_wiggle2[~np.isnan(OFF_wiggle2)]);                       
mean_OFF2=np.nanmean(OFF_wiggle2);                
                
fig5=plt.figure()
ar = plt.subplot()
on_plot=ar.bar(0,mean_ON,width=0.6,color='b', yerr=sem_ON,align='center')
off_plot=ar.bar(1,mean_OFF,width=0.6,color='g', yerr=sem_OFF,align='center')
my_xticks = ['ON','OFF']
plt.xticks(range(2), my_xticks)  
plt.xlabel("Light state")
plt.ylabel("Wiggling score")
plt.title("Wiggling on light/darkness")
fig5.savefig('wiggle.png', dpi=90, bbox_inches='tight') 


fig6=plt.figure()
ar = plt.subplot()
on_plot=ar.bar(0,mean_ON2,width=0.6,color='b', yerr=sem_ON2,align='center')
off_plot=ar.bar(1,mean_OFF2,width=0.6,color='g', yerr=sem_OFF2,align='center')
my_xticks = ['ON','OFF']
plt.xticks(range(2), my_xticks)  
plt.xlabel("Light state")
plt.ylabel("Wiggling score")
plt.title("Wiggling on light/darkness, derivative tau=2")
fig6.savefig('wiggle2.png', dpi=90, bbox_inches='tight') 