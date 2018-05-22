#!/bin/bash

#$ -N accounting
#$ -cwd -V -l h_rt=01:00:00,nodes=1
#$ -m bea -M m.wallis@leeds.ac.uk

module load R
R CMD BATCH runtime_vs_waittime.R
