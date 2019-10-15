# smallpt-haskell
This repository includes very high level computation model in a project which aims on constructing a general, mapping-to-hardware-friendly ray tracing module. The start point is [**smallpt**](https://www.kevinbeason.com/smallpt/) . Currently, after several iteration, code are totally refactored, and the core computation model has been abstracted by **pure function**, **partial function** and **high order function**.

## Computation Model
The computation model for the radiance stage (app/Function.hs) can be considered as following mult-processes system. Note that currently it is a recursive model, which is not *ready-to-map* to hardware. 
![alt text](./sample_image/radiance.jpg)

## Usage
This repository are constructed by [Stack](https://docs.haskellstack.org/en/stable/README/).

### Build
To build the project, enter and excute following command in project folder:  
` $ stack build`

### Execute
After the project is compiled, use following command to execute the program, where *\<spp\>* stands for sample times pre-pixel,  *\<output\>* stands for output file name and *\<RTS..\>* stand for RTS commands for **ghc**:  
` $ stack exec smallpt-haskell-exe `*`<spp>`*` `*`<output>`*` `*`<RTS..> `*

### Expect output
After long-enough waiting you should obtain a *ppm* file like:  
![alt text](./sample_image/2000.png)
