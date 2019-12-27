#!/bin/bash 
#SBATCH --partition=main 
# Name of the partition 
#SBATCH --job-name=one_div # Name of the job 
#SBATCH --ntasks=1 # Number of tasks 
#SBATCH --cpus-per-task=2 # Number of CPUs per task 
#SBATCH --mem=192GB # Requested memory 

#SBATCH --time=72:00:00 # Total run time limit (HH:MM:SS)
#SBATCH --output=slurm.%N.%j.out # STDOUT file 
#SBATCH --error=slurm.%N.%j.err  # STDERR file 

# This is to get e-mail notifications
# when the jobs start and end
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=mroswell.rutgers@gmail.com

module load intel/17.0.4

module load R-Project/3.4.1

Rscript scripts/asy_SAD13.R 
