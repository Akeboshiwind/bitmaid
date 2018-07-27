# bitmaid

The core of a house automation system created for my final year project.

This project has not been tested on any operating system other than `linux` and
so may not work on other operating systems such as `windows`.

## Build
### Leiningen

To build `bitmaid` the build tool `leiningen` is required. To install follow the
instructions at https://leiningen.org/#install.

Once leiningen is installed, run `lein uberjar` to create a .jar file in the
/target/uberjar/ folder. The jar file will be named something along the lines of
bitmaid-<version-number>-standalone.jar.

## Run
To run the generated .jar file a folder identical to the resources/ folder in
the base of this directory must be created in the folder that you wish to run
project in.

An example directory has been created with the correct environment in the folder
env/. This env/ folder also contains a folder of scripts that can `build`,
`clean` and `run` the project.

Domain extension files and a problem file must be placed in the base of the env/
directory. Examples of these domain extension files and problem files have been
created in the folder examples/ in the base of the project directory.
