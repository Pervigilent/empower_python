# EMPower

EMPower Python Application

## Introduction

EMPower Python is a prototype of the EMPower application (under development). After EMPower Python is completed, a C++ implementation will be creted (hopefully). The Python implementation is not fully optimized for performance, or rather, performance is not a primary concern of this implementation. While the tool is powerful and can be used for any commercial or academic workflows, a primary goal of this application is to serve as an instructive tool: it is hoped that this repository will serve to educate people on application of the finite element method, and possibly also the FDTD method, to electromagnetic phenomena.

A primary consideration in this repository is compatibility with Ansys. As such, the EMP file type should be easily converted (exported) to the AEDT file type within the application. It is also desirable that this application integrates well with other popular open source software: primarily MEEP and secondarily FreeCAD. MEEP will be integrated into the software as a secondary solver, similar to the IcePAK solver in AEDT -- which though a heat and mass transfer engine, demonstrates the use of FDTD in a majority FEM application (it would be somewhat difficult in that it would require much additional effort with little additional gain to duplicate the Lumerical interface into this application for optical frequency simulation, so I will likely leave that to the open source community). FreeCAD is a complex application which relies on many libraries, so I intend to sequester its compatibility with this application, as I feel it would obfuscate the operation of the code, and the emphasis is on a clear and readable implementation fo the finite element method. This is a longer term secondary effort, but may prove necessary if a simple and clear example of geometric modeling and mesh generation is not found elsewhere. It is hoped that EMPower will become the primary graphical interface used to interact with MEEP. 

## EMPower

To run the application, use the following command

```
user@computer:~/empower$ python3 -m empower.main
```

### FEM

FEM is intended to be the primary solver for the EMPower application. For now, the documentation from Pelosi et al. has been included.

### FDTD

FDTD is an accessory solver integrating MEEP.

## Mie Scattering

EMPower will eventually have a robust Mie scattering capability. For now, the Mie scattering codes and documentation of Christian Maetzler has been included as a template for developing documentation and algorithms for Mie Scattering.



