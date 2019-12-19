#!/bin/bash 

# Name of the partition 
#SBATCH --partition=main 

#SBATCH --job-name=severaldiv2 # Name of the job 
#SBATCH --ntasks=1 # Number of tasks 
#SBATCH --cpus-per-task=6 # Number of CPUs per task 
#SBATCH --mem=192GB # Requested memory 
#SBATCH --array=0-23%2 # Array job will submit 24 jobs
#SBATCH --time=72:00:00 # Total run time limit (HH:MM:SS)

# This is to get e-mail notifications
# when the jobs start and end
#SBATCH --mail-type=begin
#SBATCH --mail-type=end
#SBATCH --mail-user=mroswell.rutgers@gmail.com

#SBATCH --output=slurm.%N.%j.out # STDOUT file 
#SBATCH --error=slurm.%N.%j.err  # STDERR file 

srun
module load intel/17.0.4

module load R-Project/3.4.1
myfilename<-paste("scripts/asy_SAD", $SLURM_ARRAY_TASK_ID, ".R")
Rscript $myfilename