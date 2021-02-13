AdaBaseXClient
=====

Ada client for [BaseX](https://basex.org/).
Works with BaseX 7.0 and later.

This package depends on `GNAT.Sockets` and `GNAT.MD5`.


UNIX BUILD
=====

I only test on openSUSE LEAP 15.2.

Users need use `make` to build:

    make

And install:

    make install

If users want to build tests/examples:

    make build_examples

If users want to check test with Ahven:

    make test

Default PREFIX is `/usr/local`, and LIBDIR is `lib`.
If users want to change, below is the example:

    make PREFIX=/usr LIBDIR=lib64

and install:

    make PREFIX=/usr LIBDIR=lib64 install


Example
=====

Below is a simple example:

    with Ada.Text_IO;
    with Ada.Exceptions;
    with AdaBaseXClient;
    use Ada.Text_IO;
    use Ada.Exceptions;
    use AdaBaseXClient;
    
    procedure Example is
    begin
    
       if (Connect ("localhost", 1_984) = False) then
          Put_Line ("Connect failed.");
          return;
       else
          if (Authenticate ("admin", "admin") = False) then
             Put_Line ("Authenticate failed.");
          end if;
       end if;
    
       Close;
    
    exception
       when Error : BaseXException =>
          Ada.Text_IO.Put ("Exception: ");
          Ada.Text_IO.Put_Line (Exception_Name (Error));
          Ada.Text_IO.Put (Exception_Message (Error));
    end Example;

Then write a example.gpr file:

    with "adabasexclient";

    project Example is
        for Languages use ("Ada");
        for Exec_Dir use ".";
        for Source_Files use ("example.adb");
        for Main use ("example.adb");
        package Builder is
            for Executable ("example") use "example";
        end Builder;
        package Compiler is
        for Default_Switches ("Ada")
            use ("-O2");
        end Compiler;
    end Example;

Then build this project:

    gnatmake -Pexample

