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
    light=np.insert(light,[1],True,axis=0);
    if len(light)<len(digitized):
        light=np.insert(light,[1],True,axis=0);               
    for i in range(min(digitized),max(digitized)+1):
        PI_discrete[i-1]=(sum(light[digitized == i]==True)-sum(light[digitized == i]==False))/len(light[digitized == i]);
    return np.asarray(PI_discrete)

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
#nFlies = input('Give me an integer number: ')

# Make sure the input is an integer number
#nFlies = int(nFlies)
nFlies=1
#nFlies=2;
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
                  
    #Time of the experiment
    TimeExp = max(fly_data[1]);
    
    
    ## Show an example of how fly data looks like
    segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=11, endpoint=True, retstep=False, dtype=None);
                          
    # Three subplots, the axes array is 1-d
    f, axarr = plt.subplots(3, sharex=True)
    axarr[0].plot(fly_data[1],fly_data[2]);
    axarr[0].set_title('Fly traces')
    axarr[1].plot(fly_data[1],fly_data[3]);
    axarr[2].plot(fly_data[1],fly_data[4]);                             
    
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
    PI_disc=np.vstack((PI_discrete1,PI_discrete2,PI_discrete3));                         
    #PI_disc[keep_fly]=float('nan');
    PI_disc[del_fly]=float('nan');                        
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

    PI_disc_popul_left=np.nanmean(np.nanmean(PI_disc_all[left,:,:],axis=0,dtype=float),axis=0,dtype=float);
    #PI_disc_popul_left = np.vstack(PI_disc_popul_left[:]).astype(np.float);
    Sem_disc_popul_left=np.array([None]*10,dtype=float);
    for a in range(10):
        data=np.array(PI_disc_all[left,:,a],float);
        Sem_disc_popul_left[a]=np.nanstd(data)/math.sqrt(data.shape[1]);

    ## Plot PIs by time from the three platforms together
x = pylab.arange(len(PI_time_popul_right));
plt.figure()
ax = plt.subplot()
if any(right) & any(left):
    rightbar_time=ax.bar(x-0.2, PI_time_popul_right,width=0.3,color='b',yerr=Sem_time_popul_right,align='center')
    leftbar_time=ax.bar(x+0.2,PI_time_popul_left,width=0.3,color='g',yerr=Sem_time_popul_left,align='center')
    ax.legend((rightbar_time, leftbar_time), ('Right', 'Left'))    
elif not any(left) & any(right):
    rightbar_time=ax.bar(x, PI_time_popul_right,width=0.8,color='b',yerr=Sem_time_popul_right,align='center')
    ax.legend((rightbar_time), ['Right'])
else:    
    leftbar_time=ax.bar(x,PI_time_popul_left,width=0.8,color='g',yerr=Sem_time_popul_left,align='center')
    ax.legend(leftbar_time, ['Left'])
my_xticks = Mode1;
plt.xticks(range(0,10), my_xticks)
plt.xlabel("Training segment")
plt.ylabel("Weighted PI")
plt.title("PI by segment (weighted)")

x = pylab.arange(len(PI_disc_popul_right));
plt.figure()
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
plt.xlabel("Training segment")
plt.ylabel("Logical PI")
plt.title("PI by segment (logical)")


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
plt.figure()
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
    preTe=ax2.bar(x-0.32,preTe_mean_left_disc ,width=0.4,color='g', yerr=preTe_sem_left_disc,align='center')
    Reinf=ax2.bar(x,Re_mean_left_disc ,width=0.4,color='b', yerr=Re_sem_left_disc,align='center')
    Test=ax2.bar(x+0.32,Te_mean_left_disc ,width=0.4,color='g', yerr=Te_sem_left_disc,align='center')
    ax2.legend((preTe,Reinf, Test), ('Pretest','Reinforcement', 'Test'))
    my_xticks = ['Left']
    plt.xticks(range(0,1), my_xticks)    
plt.xlabel("Treatment")
plt.ylabel("Logical PI")
plt.title("PI by treatment")



x = pylab.arange(1);
plt.figure()
ax2 = plt.subplot()
if any(left):
    preTe=ax2.bar(x-0.32,preTe_mean_left_time ,width=0.4,color='g', yerr=preTe_sem_left_time,align='center')
    Reinf=ax2.bar(x,Re_mean_left_time ,width=0.3,color='b', yerr=Re_sem_left_time,align='center')
    Test=ax2.bar(x+0.32,Te_mean_left_time ,width=0.3,color='g', yerr=Te_sem_left_time,align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Left']
    plt.xticks(range(0,1), my_xticks)
elif any(right):
    preTe=ax2.bar(x-0.32,preTe_mean_right_time ,width=0.3,color='g', yerr=preTe_sem_right_time,align='center')
    Reinf=ax2.bar(x,Re_mean_right_time ,width=0.3,color='b', yerr=Re_sem_right_time,align='center')
    Test=ax2.bar(x+0.32,Te_mean_right_time ,width=0.3,color='g', yerr=Te_sem_right_time,align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Right']
    plt.xticks(range(0,1), my_xticks)
else:
    x = pylab.arange(2);    
    preTe=ax2.bar(x-0.32,[preTe_mean_right_disc,preTe_mean_left_time],width=0.3,color='g', yerr=[preTe_sem_right_disc,preTe_sem_left_time],align='center')
    Reinf=ax2.bar(x,[Re_mean_right_time,Re_mean_left_time],width=0.3,color='b', yerr=[Re_sem_right_time,Re_sem_left_time],align='center')
    Test=ax2.bar(x+0.32,[Te_mean_right_time,Te_mean_left_time],width=0.3,color='g', yerr=[Te_sem_right_time,Te_sem_left_time],align='center')
    ax2.legend((Reinf, Test), ('Reinforcement', 'Test'))
    my_xticks = ['Right','Left']
    plt.xticks(range(0,2), my_xticks)
plt.xlabel("Treatment")
plt.ylabel("Weighted PI")
plt.title("PI by treatment")
                           
