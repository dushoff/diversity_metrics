#!/bin/bash 
#SBATCH --partition=main 
# Name of the partition 
#SBATCH --job-name=asymptot # Name of the job 
#SBATCH --ntasks=1 # Number of tasks 
#SBATCH --cpus-per-task=6 # Number of CPUs per task 
#SBATCH --mem=192GB # Requested memory 
#SBATCH --array=0-23 # Array job will submit 24 jobs
#SBATCH --time=72:00:00 # Total run time limit (HH:MM:SS)
#SBATCH --output=slurm.%N.%j.out # STDOUT file 
#SBATCH --error=slurm.%N.%j.err  # STDERR file 


module load intel/17.0.4

module load R-Project/3.4.1

srun Rscript scripts/asy_SAD$SLURM_ARRAY_TASK_ID.R 