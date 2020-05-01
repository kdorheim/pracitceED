#!/bin/bash
#SBATCH --account forteproject
#SBATCH --time 02-00:00
#SBATCH --nodes 1

#SBATCH --job-name=forte_ed2
#SBATCH --output=/qfs/projects/forteproject/forte-ed-runs/logs/%A_%a.log
#SBATCH --error=/qfs/projects/forteproject/forte-ed-runs/logs/%A_%a.log
#SBATCH --array=1-167

# This runs 24 instances of ED2 on a node
# This means it takes 167 jobs to run everything (4000 / 24),
# with the last job only running 16 cases.

trap "exit" INT TERM
trap "kill 0" EXIT

N=24

A=$((1 + $N * $SLURM_ARRAY_TASK_ID - $N))
B=$(($SLURM_ARRAY_TASK_ID * $N))
# If B is greater than 4000, trim it to just 4000
B=$(($B > 4000 ? 4000 : $B))
echo "Running case folders numbered $A to $B"
sleep 3
for I in $(seq $A $B); do
    ED2_DIRECTORY=$(find /qfs/projects/forteproject/forte-ed-runs/cases/ -mindepth 1 -maxdepth 1 | sort | sed "${I}q;d")
    echo "Working with directory: $ED2_DIRECTORY"
    CASE=${ED2_DIRECTORY: -6}
    bash run-ed.sh "$ED2_DIRECTORY" >& "logs/zzED-$CASE" &
done

wait

echo "Done!"