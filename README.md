# STAT 243 Statistical Computing - Final Project

- Genetic Algorithm based Linear Regression R Package

Group Members: Yuxuan Fu, Preeti Gondi, Wenxin Zhang

Idea of selection mechanism is presented as:

Selection mechanism: process by which parents are chosen to produce offspring
Three methods of parent selection:

- Method 1: FitnessRandom
Select one parent with probability proportional to fitness and select the other
parent completely at random

- Method 2: FitnessFitness
Select each parent independent with probability proportional to fitness

- Method 3: Tournament
Tournament selection: set of chromosomes in generation t is randomly partitioned into 
k disjoint subsets of equal size; best individual in each group is chosen as a parent;
additional random partitionings are carried out until sufficient parents have been 
generated; parents are pairred for breeding.

