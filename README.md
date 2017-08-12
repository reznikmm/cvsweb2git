# cvsweb2git

This tool imports CVS repository shared on
[Ada Conformity Assessment Authority](http://www.ada-auth.org/)
into the [Git repository](https://github.com/reznikmm/ada-auth).

# Dependency & Build

This project requires [Matreshka](http://forge.ada-ru.org/matreshka)
and (AWS)[http://libre.adacore.com/tools/aws/] libraries to build.

To build it:

    gprbuild -P gnat/cvsweb2git.gpr

# Usage

    git init /tmp/ada-auth/
    .obj/cvsweb2git http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/ /tmp/ada-auth/

