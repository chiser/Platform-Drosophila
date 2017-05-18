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

#Sampling rate in Hz
Sampling = fly_metadata[30];
Sampling= int(Sampling.split()[-1]);


##Fetching the calculated PIs from the experiment
             
#PI1s=[None]*10;
#a=fly_metadata[11];
#a= a.split();
#subst=0;
          
#for i in range(1, len(a)):
#    if contains_digits(a[i]):
#        PI1s[i-1-subst]= float(a[i]);
#    else:
#        if a[i]=='n.':
#            PI1s[i-1-subst]= float('nan');
#            subst=subst+1;

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
     
         
## Less optimized way of doing the same as above
#file = open(filename,'r'); #open the conection to the file in read only
#text = file.read();
#readthirdline = f.readline(3);
#file.close();#close the connection is a good practice
          
## Open data with numpy. I guess this is just a different way of doing it. Numpy is good for flat files but better when all numbers
## Efficient and clean. Use arguments like skiprows=1, usecols=[0,1,2,..], dtype=float,str,...when necessary                    
#data = np.loadtxt(filename,delimiter=';',skiprows=37);
#print(data[10]); #print the 10th value of the file

# Plot a scatterplot of the data
#plt.scatter(data[:, 0], data[:, 1])
#plt.xlabel('time (min.)')
#plt.ylabel('percentage of larvae')
#plt.show()

## When the data consists of a mix of types, namely a dataframe, genfromtxt might be better option than
## loadtxt. Set the argument to dtype=None
#data = np.genfromtxt(filename,delimiter=';', dtype=None,names=True);
#np.shape(data);  #to know the shape
# to access rows one has to put the number data[number], when columns is the header name data['Fares']
#d=np.recfromcsv(file); ##  this is a similar way as genfromtxt but with many defaults already set for the csv mixed types purposes   

# I have to try this out at some point
#with open(filename, 'r') as file:
#    line_no = 0
#    for line in file.read():
#        line_no += 1
#        if line.startswith('-'*37):
#            # do sth
#            f.readlines()[line_no];
#            break
#        else:
#            # do sth  

              
## Read the file into a DataFrame: df
#df = pd.read_csv(file)

## View the head of the DataFrame
#print(df.head())

# Read the first 5 rows of the file into a DataFrame: data
#data= pd.read_csv(file,nrows=5,header=40);

# Build a numpy array from the DataFrame: data_array
#data_array= data.values;

# Print the datatype of data_array to the shell
#print(type(data_array))

# Import file: data
#data = pd.read_csv(file, sep='\t', comment='#', na_values=['Nothing'])

# Print the head of the DataFrame
#print(data.head())

# Plot 'Age' variable in a histogram
#pd.DataFrame.hist(data[['Age']])
#plt.xlabel('Age (years)')
#plt.ylabel('count')
#plt.show()
"""
######################## Working with the data   ####################
"""
## ISI plot
#%matplotlib qt
#plt.figure()
#plt.plot(isi);
#plt.xlabel('Measured Points')
#plt.ylabel('ISI (ms)')
#plt.show()
segments = np.linspace(fly_data.loc[0,1], TimeExp ,num=11, endpoint=True, retstep=False, dtype=None);
#segment_int=segments*10;
#segment_int=segment_int.astype(int);
## Show an example of how fly data looks like
plt.figure()
fly=2;
plt.plot(fly_data[1],fly_data[fly]);
for i in segments:
    plt.axvline(x=i, ymin=min(fly_data[fly]), ymax=max(fly_data[fly]), color='k', linestyle='--')    
plt.xlabel('Time (Measured Points)')
plt.ylabel('Fly turning behavior (A.U.)')
plt.show()
      
## Side vector to know where the fly is: in the light On side, off side or in between the hysteresis
#side=np.ones((lengthExp-1, 1), dtype=str);
#side = [''  for x in range(lengthExp-1)];
#for i in range(0,lengthExp-1):
#  if (fly_data.loc[i,2]>=Hysteresis):
#    side[i]= 'ON';
#  elif (-Hysteresis<=fly_data.loc[i,2]<= Hysteresis):
#    side[i]= 'MIDDLE';
#  elif (fly_data.loc[i,2]<= -Hysteresis):
#    side[i]= 'OFF'
side1= side_which(lengthExp,fly_data,Hysteresis,1);
side2= side_which(lengthExp,fly_data,Hysteresis,2);
side3= side_which(lengthExp,fly_data,Hysteresis,3);
side=np.vstack((side1,side2,side3));
## Light vector to know when is it on and off
light1=light_state(lengthExp,side1);
light2=light_state(lengthExp,side2);                  
light3=light_state(lengthExp,side3);
light=np.vstack((light1,light2,light3));            
#light=np.ones((lengthExp-1), dtype=bool);
#for i in range(0,lengthExp-1):
#   if (i==0) and (side[i] == 'MIDDLE'):
#       light[i] = False
#   if i>1:
#     if side[i] == 'ON':
#       light[i]= True
#     elif side[i] == 'MIDDLE': 
#       light[i]= light[i-1]
#     elif side[i]== 'OFF':
#       light[i]= False

## digitized contains time points corresponding to each of the experiments segments
digitized = np.digitize(fly_data[1], segments);
digitized[digitized==11]=10;

## Calculates PI from -4 to +4 by time, thus real PI. There is one bug I think         
PItime1=PI_time(1,digitized);
PItime2=PI_time(2,digitized);
PItime3=PI_time(3,digitized);

## To plot one of them separatedly
plt.figure()               
xplot = range(len(PItime1));             
plt.bar(xplot,PItime1, 0.9, color="blue")
plt.xlabel("Training segment")
plt.ylabel("Position (a.u.)")
plt.title("PI by segment (in time)")

#A = np.array((PItime1,PItime2,PItime3), dtype=float)
## Plot PIs by time from the three platforms together
x = pylab.arange(len(PItime1));
multiple_bars = plt.figure()
ax = plt.subplot(111)
ax.bar(x-0.2, PItime1,width=0.2,color='b',align='center')
ax.bar(x,PItime2,width=0.2,color='g',align='center')
ax.bar(x+0.2, PItime3,width=0.2,color='r',align='center')
plt.xlabel("Training segment")
plt.ylabel("Position PI")
plt.title("PI by segment (in time)")

reinforced=np.mean([PItime1[1::2],PItime2[1::2],PItime3[1::2]]);
notreinforced=np.mean([PItime1[0::2],PItime2[0::2],PItime3[0::2]]);                  

testvstraining=np.asarray([reinforced,notreinforced]);

plt.figure()
plt.bar(range(len(testvstraining)),testvstraining, 0.9, color="blue")
my_xticks = ['reinforcement','not reinforcement']
plt.xticks([0,1], my_xticks)
#plt.axis([0, 6, 0, 20])

A = np.array((fly_data[2],fly_data[3],fly_data[4]), dtype=float);
#plt.figure()
#plt.boxplot(A,1)

##
plt.figure()
for i in [0,1,2]:
    y = A[i,:]
    # Add some random "jitter" to the x-axis
    x = np.random.normal(i, 0.04, size=len(y))
    plt.plot(x, y, 'r.', alpha=0.2)
my_xticks = ['platform 1','platform 2','platform 3']
plt.xticks(range(3), my_xticks)
plt.xlabel("Platform")
plt.ylabel("Position in platform")
plt.title("Measured points in each of the platforms")



ONdynamics=A[light==True];
OFFdynamics=A[light==False];
timexthree=np.vstack((fly_data[1],fly_data[1],fly_data[1]));
timeON= timexthree[light==True];
timeOFF= timexthree[light==False];

plt.figure()
plt.plot(timeON,ONdynamics);
plt.xlabel('Measured Points')
plt.ylabel('Position')
pylab.show()

plt.figure()
plt.plot(timeOFF,OFFdynamics,'ro');
plt.xlabel('Measured Points')
plt.ylabel('Position')
pylab.show()

chunk_bound=np.diff(timeOFF);
chunk_bound[chunk_bound>max(isi)]=0
new_chunk=chunk_bound>max(isi);
new_chunk2=np.hstack([aligned,new_chunk])
chunk_number=np.cumsum(new_chunk2);

#dynamics4plot=np.transpose(np.vstack([OFFdynamics,chunk_number]));
#df = pd.DataFrame({'R':dynamics4plot[:,0],'G':dynamics4plot[:,1]});
#df = df.sort_values(by='G');
#grouped = df.groupby(['R'])
#fig, ax = plt.subplots()
#for key, group in grouped:
#    group.plot('G', 'R', label=key, ax=ax)                         
#plot_url = py.plot_mpl(multiple_bars, filename='mpl-multiple-bars')
#time=fly_data[1];
#bin_means = [time[segments == i].mean() for i in range(1, len(segments))]

  

#fig = ff.create_violin(df, data_header='Score', group_header='Group',
#                       height=500, width=800)
#py.iplot(fig, filename='Multiple Violins')