# jekor.com

jekor.com is a mostly static website built by a mix of experimental tools. It's my personal site and laboratory.

To understand the motivation for this, see http://jekor.com/article/towards-a-lifelong-content-management-system

## Static Content

Most content is generated from markdown files and HTML templates by a Makefile driven by Nix. Content is then served by the fsrest web server.

## Dynamic Content

The primary dynamic content is user comments. These are handled by the `www/post-comment` program.

Additionally, jekor.com hosts a copy of the [gressgraph](https://github.com/jekor/gressgraph) program so that users don't have to install it. Uploads are handled by `www/gressgraph/graph/POST`.

## Tools Used

* [pandoc](http://johnmacfarlane.net/pandoc/) converts the markdown to HTML
* [jigplate](https://github.com/jekor/jigplate) places JSON data into HTML templates
* [jsonwrench](https://github.com/jekor/jsonwrench) massages JSON data in the pipeline (and is used for dynamic comment posting)
* [jcoreutils](https://github.com/jekor/jcoreutils) provides some glue for some Make rules
* [GNU Make](http://www.gnu.org/software/make/) builds the static content
* [Bash](http://www.gnu.org/software/bash/) is used by dnyamic helper programs and the Nix build script
* [Nix](http://nixos.org/nix/) provides a repeatable build environment
* [fsrest](https://github.com/jekor/fsrest) serves the pages and accepts user input

## How it Works

First, Nix uses the `default.nix` to set up a strict environment for performing the build of the site. Next, in the clean sandbox it invokes the `builder.sh` script. Then, `builder.sh` invokes Make on the Makefile. Finally, `builder.sh` passes along the required programs to the dynamic scripts (mentioned in the Dnyamic Content section above) so that no only the build environment is repeatable but also the runtime environment.

### The Makefile

The Makefile contains all rules necessary to build the static resources from sources. In this sense, jekor.com is compiled like other software. Stretching the analogy, user comments are provided at runtime as configuration (and defaults, such as empty lists, are compiled in).

The result is placed in the Nix store. (This assures that it will be read-only.) Deployment and management of dynamic content are handled elsewhere (in [jen](https://github.com/jekor/jen)).
