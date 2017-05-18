# -*- coding: utf-8 -*-
"""
Created on Tue May  2 12:01:57 2017

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

import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import re
from matplotlib import pylab
from tkinter.filedialog import askopenfilename
#import tkinter

#import plotly.plotly as py
"""
######################## Functions   ####################
"""
_digits = re.compile('\d')
def contains_digits(d):
     return bool(_digits.search(d))
 
## Side vector to know where the fly is: in the light On side, off side or in between the hysteresis 
def side_which(lengthExp,fly_data,Hysteresis,platform):
    side = [''  for x in range(lengthExp-1)];
    for i in range(0,lengthExp-1):
        if (fly_data.loc[i,(platform+1)]>=Hysteresis):
            side[i]= 'ON';
        elif (-Hysteresis<=fly_data.loc[i,(platform+1)]<= Hysteresis):
            side[i]= 'MIDDLE';
        elif (fly_data.loc[i,(platform+1)]<= -Hysteresis):
            side[i]= 'OFF'
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
          
"""
######################## Importing the data   ####################            
"""            
os.getcwd() # Prints the working directory

#To set the working directory:

os.chdir('c:\\Users\LocalAdmin\Desktop\platform_data2017')
nFlies=6
for oo in range(nFlies):
                    
    filename = askopenfilename()    
    
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
    
    #Sampling rate in Hz
    Sampling = fly_metadata[30];
    Sampling= int(Sampling.split()[-1]);
    
    ## Fetch PIs calculated by the recording software  
    PI1s= fetch_PIfrommeta(11);
    PI2s= fetch_PIfrommeta(18);
    PI3s= fetch_PIfrommeta(25);
    
    ## Master/Yoked/Test segment info                        
    Mode1= fetch_modefrommeta(7,1);
    Mode2= fetch_modefrommeta(14,1);
    Mode3= fetch_modefrommeta(21,1);
    
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
    
    rightside= [all(h1bool),all(h2bool),all(h3bool)];
    
    ## Concatenate all calculated PIs together                           
    total=[[None]*10]*3;
    total[0]=PI1s;
    total[1]=PI2s;     
    total[2]=PI3s;  
    
    """
    ######################## Working with the data   ####################
    """
    
    segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=11, endpoint=True, retstep=False, dtype=None);
    
    ## Show an example of how fly data looks like
    for fly in range(0,3):
    
        plt.figure()
        
        plt.plot(fly_data[1],fly_data[fly+2]);
        for i in segments:
            plt.axvline(x=i, ymin=min(fly_data[fly]), ymax=max(fly_data[fly]), color='k', linestyle='--')    
        plt.xlabel('Time (Measured Points)')
        plt.ylabel('Fly turning behavior (A.U.)')
        plt.show()
#        if (max(fly_data[fly+2])-min(fly_data[fly+2]))<=1