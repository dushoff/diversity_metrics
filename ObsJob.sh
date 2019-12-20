#!/bin/bash 
#SBATCH --partition=main 
# Name of the partition 
#SBATCH --job-name=several_div # Name of the job 
#SBATCH --ntasks=1 # Number of tasks 
#SBATCH --cpus-per-task=6 # Number of CPUs per task 
#SBATCH --mem=1500GB # Requested memory 
#SBATCH --array=0-23%8 # Array job will submit 24 jobs, 8 at a time
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

srun Rscript scripts/obs_SAD$SLURM_ARRAY_TASK_ID.R 