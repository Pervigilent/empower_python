# QUICK FEM

QUICK FEM (C) 1997 G.PELOSI R. COCCIOLI S. SELLERI

## Introduction

**Editor**: This codebase was taken from *Quick Finite Elements for Electromagnetic Waves* by Guiseppe Pelosi, Roberto Coccioli, and Stefano Selleri. The data files and meshes have been removed, but I may add some examples back in if appropriate as I make my way through the text. Otherwise, I will not touch this reference. I plan to translate all of the source code to Python. I may even remove this particular folder (directory), but it is probably best to keep it as a reference to verify any Python scripts. However, I anticipate that the Python scripts will diverge significantly from the original Fortran source code eventually.

## Content Description

(Taken from pages 59 to 60 of the text.)

- **TOOLS**: The codes of this chapter include
    - **LIB**: The subroutines described in this chapter, and a couple of subroutines that are so general as to deserve to be included in the library but are described in the following chapters, where they are first needed.
        - **BAND.FOR**: The functions BPUT, BGET, CBPUT, and CBGET used to access matrices in band storage mode.
        - **BANDEX.FOR**: Straightforward band occupation estimator.
        - **BSOLVE.FOR**: Block sovler, described in Chapter 4, is the only library subroutine that exploits LAPACK.
        - **DELAUNAY.FOR**: The Delaunay regularization subroutine.
        - **ELEE.FOR**: Edge element matrices.
        - **ELEN.FOR**: Nodal element matrices.
        - **FEMPS.FOR**: Subroutine to translate meshes into PostScript files.
        - **FINDEQN.FOR**: Find equivalent nodes in periodic meshes, as described in Chapter 5.
        - **PNJEXP.FOR**: Evaluates the integral of the product of the element basis function and a complex exponential, as described in Chapter 4.
        - **READ.FOR**: Subroutine to read a mesh file.
        - **READN.FOR**: Subroutine to read a mesh file that skips edges.
        - **RENUMBER.FOR**: Subroutine to perform optimization.
        - **WRITE.FOR**: Subroutine to write mesh file.
        - **WRITECR.FOR**: Subroutine to write results in a form usable by the post-processor.
    - **MAINS**: The driver programs to exploit part of the library interactively. All of thes files should be compiled singularly by resorting to the above-mentioned library.
        - **DO_BEX.FOR**: Driver for the band estimator.
        - **DO_DEL.FOR**: Driver for the Delaunay regularizator.
        - **DO_F2PS.FOR**: Driver for the translator mesh PostScript.
        - **DO_REN.FOR**: Driver for the number optimizator.
        - **MESH.FOR**: The mesh generator.
        - **POSTPRO.FOR**: The postprocessor.
- **CODES**: This directory contains all the programs discussed in the following chapters. The single subdirectories will be discussed in the relative chapters, but all share the same organization:
    1. A top-level subdirectory containing the source code(s).
    2. A series of subdirectories, one for each example discussed in the text.

## Folders

**Editor**: This description of the folders is may seem to duplicate the previous section, but is retained from the original README file.

1. laplace
2. tools
3. codes
4. lapack (omitted)
5. gnuplot (omitted)

### LAPLACE

Chapter 1 Stand-Alone FEM Software

### TOOLS

Chapter 2 FEM library and drivers

### CODES

Root of the tree containing all the FEM software of Chapters 3,4,5,6a

### LAPACK (Omitted)

Root of the tree containing the LAPACK and BLAS package. This package is provided as it is, downloaded from its main site on November 1997. The tree contains all the appropriate Copyright notices.

### GNUPLOT (Omitted)

Root of the tree containing the GNUPLOT package. This package is provided as it is, downloaded from its main site on November 1997. The tree contains all the appropriate Copyright notices.




