# run2LDRM
This repository contains the moisture tracking model 2L-DRM. To build and compile it, you need to have the following installed:
1. A Fortran compiler that supports the module ieee_arithmetic is required (any compiler that implements the Fortran 2003 standard or higher would be fine). I did it with the GNU/GCC 9.3.0 compiler.
2. Fortran Package Manager (fpm). See intructions for installing it here: https://github.com/fortran-lang/fpm. I installed it using a conda environment.

Then you can clone this repository in a local directory: 

    clone https://github.com/erickjomp/2L-DRM.git

And you can build the 2L-DRM program using:

    fpm build

And install it using:

    fpm install

In addition, if using a Linux system, you may want to add the `~/.local/bin/` to your PATH environment. For that, you can edit your `~/.profile` file by adding a line like this:

   export PATH=~/.local/bin/:$PATH

Then you will be able to call the program `run2LDRM` from any directory. For a list of the required and optional inputs of this program, please use:

   run2LDRM --help


