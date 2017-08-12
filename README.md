# cvsweb2git

This tool imports CVS repository shared on
[Ada Conformity Assessment Authority](http://www.ada-auth.org/)
into the [Git repository](https://github.com/reznikmm/ada-auth).

# Dependency & Build

This project requires [Matreshka](http://forge.ada-ru.org/matreshka)
and [AWS](http://libre.adacore.com/tools/aws/) libraries to build.

You also need libxml2.so installed (from libxml2-devel package).

To build it:

    gprbuild -p -P gnat/cvsweb2git.gpr

# Usage

    git init /tmp/ada-auth/
    (cd /tmp/ada-auth && git config core.autocrlf true &&
    git config user.email "you@example.com" &&
    git config user.name "Your Name")
    .obj/cvsweb2git http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/ /tmp/ada-auth/

