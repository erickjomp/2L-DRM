#conda init
#conda activate fpm

####module load gnu/gnu-9.3.0
export FPM_FCFLAGS="-cpp -fopenmp"
fpm install
#fpm install --flag '-g -mcmodel=large'
#fpm install --flag '-mcmodel=medium -fcheck=all -Wall'
