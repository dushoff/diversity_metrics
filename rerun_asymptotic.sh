#!/bin/bash 
#SBATCH --partition=main 
# Name of the partition 
#SBATCH --job-name=asysock # Name of the job 
#SBATCH --ntasks=1 # Number of tasks 
#SBATCH --cpus-per-task=1 # Number of CPUs per task 
#SBATCH --mem=192GB # Requested memory 
#SBATCH --array=0-1091 # Array job will submit 71 jobs
#SBATCH --time=72:00:00 # Total run time limit (HH:MM:SS)
#SBATCH --output=slurm.%N.%j.out # STDOUT file 
#SBATCH --error=slurm.%N.%j.err  # STDERR file 

cd /scratch/mr984
module load intel/17.0.4

module load R-Project/3.4.1

srun Rscript /home/mr984/diversity_metrics/scripts/asy_$SLURM_ARRAY_TASK_ID.R 
