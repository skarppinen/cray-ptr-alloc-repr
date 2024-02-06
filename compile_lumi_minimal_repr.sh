module load craype-accel-amd-gfx90a
module load rocm
module load cce/16.0.1

FLAGS="-hacc"
FILES="minimal_repr_mod.F90 minimal_repr_main.F90"
ftn ${FLAGS} ${FILES} -o minimal_repr
ftn ${FLAGS} ${FILES} -DUSE_ALLOCATABLE -o minimal_repr_alloc
