# run2LDRM
This repository contains the moisture tracking model 2L-DRM. To build and compile it, you need to have the following installed:
1. A Fortran compiler that supports the module ieee_arithmetic is required (any compiler that implements the Fortran 2003 standard or higher would be fine). I did it with the GNU/GCC 9.3.0 compiler.
2. Fortran Package Manager (fpm). See intructions for installing it here: https://github.com/fortran-lang/fpm. I installed it using a conda environment.

Then you can clone this repository in a local directory: 

    clone https://github.com/erickjomp/2L-DRM.git

And you can build and compile the 2L-DRM program using:

    fpm build

    fpm install


