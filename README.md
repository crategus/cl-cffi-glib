## cl-cffi-glib

### General information

The `cl-cffi-glib` library is a Lisp binding of parts of the GLib, GObject and
GIO libraries, which is the basis for projects like GTK and Pango. The library
is loaded from the `cl-cffi-gtk4` library for a Lisp binding to GTK 4, the
deprecated `cl-cffi-gtk3` library for a Lisp binding to GTK 3, and the
`cl-cffi-pango` library  for a Lisp binding to Pango.

The `cl-cffi-glib` library was part of the `cl-cffi-gtk` library and has been
separated out to be used independently. The former `cl-cffi-gtk` library is
replaced by the `cl-cffi-gtk4` and `cl-cffi-gtk3` libraries for GTK 4 and GTK 3,
respectively.

This work is based on the `cl-gtk2` library developed by Kalyanov Dmitry.

### License

The `cl-cffi-glib` library is licensed under the MIT license.

### Installation

The `cl-cffi-glib` library is ASDF installable:
```
(asdf:load-system :cl-cffi-glib)
```
After loading the library information about the installed version is available:
```
* (glib:cl-cffi-glib-build-info)
cl-cffi-glib build date: 13:11 4/1/2025
GLIB version: 2.82.1
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-5200U CPU @ 2.20GHz
Software type: Linux
Software version: 6.11.0-21-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.2.9.debian
```

The `cl-cffi-glib` library depends on the following Lisp libraries:

* `cffi`

    The Common Foreign Function Interface, purports to be a portable foreign
    function interface for Common Lisp.
    See [common-lisp.net/project/cffi](http://common-lisp.net/project/cffi/).

    **Warning:** Yout must use the version 0.22.0 or newer of the CFFI library.
    Older versions of CFFI are not compatible with the implementation of the
    `cl-cffi-glib` library.

* `iterate`

    Is a lispy and extensible replacement for the LOOP macro.
    See [common-lisp.net/project/iterate](http://common-lisp.net/project/iterate/).

* `bordeaux-threads`

    Lets you write multi-threaded applications in a portable way.
    See [common-lisp.net/project/bordeaux-threads](http://common-lisp.net/project/bordeaux-threads/).

* `closer-mop`

    Closer to MOP is a compatibility layer that rectifies many of the absent or
    incorrect MOP features as detected by MOP Feature Tests.
    See [common-lisp.net/project/closer/closer-mop.html](http://common-lisp.net/project/closer/closer-mop.html)

* `trivial-garbage`

    Provides a portable API to finalizers, weak hash-tables and weak pointers
    on all major CL implementations.
    See [common-lisp.net/project/trivial-garbage](http://common-lisp.net/project/trivial-garbage/).

* `trivial-features`

    Ensures consistent `*features*` across multiple Common Lisp implementations.
    See [cliki.net/trivial-features](https://www.cliki.net/trivial-features).

### Packages

The `cl-cffi-glib` library defines the following packages:

* `glib` package

    Contains the exported symbols of the Lisp binding to GLib.

* `gobject` package

    Contains the exported symbols of the Lisp binding to GObject.

* `gio` package

    Contains the exported symbols of the Lisp binding to GIO.

* `glib-user` package with nickname `g`

    Imports all symbols from the `glib`, `gobject`, and `gio` packages and
    exports the symbols for usage with the nickname `g`. Therefore you can
    access a symbol, for example, the `gobject:signal-connect` function in the
    `gobject` package as `g:signal-connect`, or the `gio:application-run`
    function in the `gio` package as `g:application-run`.

### Testsuite

The `cl-cffi-glib` library comes with a testsuite. The testsuite can be
performed with
```
(asdf:test-system :cl-cffi-glib)
```
or loaded with
```
(asdf:load-system :cl-cffi-glib/test)
```
The tests are included in the `glib-test` package, organized in suites and can
be performed per suite or as a single test after loading the testsuite:
```
* (in-package :glib-test)
#<PACKAGE "GLIB-TEST">
* (run! 'gio-application)

Running test suite GIO-APPLICATION
 Running test G-APPLICATION-FLAGS ......
 Running test G-APPLICATION-CLASS ........
 Running test G-APPLICATION-SIGNALS ...
 Running test G-APPLICATION-ACTION-GROUP-PROPERTY ......
 Running test G-APPLICATION-APPLICATION-ID-PROPERTY .....
 Running test G-APPLICATION-FLAGS-PROPERTY .........
 Running test G-APPLICATION-INACTIVITY-TIMEOUT-PROPERTY .....
 Running test G-APPLICATION-IS-BUSY-PROPERTY ....
 Running test G-APPLICATION-IS-REGISTERED-PROPERTY ....
 Running test G-APPLICATION-IS-REMOTE-PROPERTY ...
 Running test G-APPLICATION-RESOURCE-BASE-PATH-PROPERTY .....
 Running test G-APPLICATION-ID-IS-VALID ..
 Running test G-APPLICATION-OPEN
 Did 60 checks.
    Pass: 60 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)
```
Unfortunately, many of the tests depend on the hardware or local environment
you are using and will therefore fail. This may be fixed in the future.

The testsuite also requires the following packages:

* `fiveam`

    FiveAM is a framework for regression testing. It was designed with Common
    Lisp's interactive development model in mind.
    See [fiveam.common-lisp.dev](https://fiveam.common-lisp.dev/).


* `local-time`

    The `local-time` library is a library for manipulating date and time
    information in a semi-standard manner.
    See [local-time.common-lisp.dev](https://local-time.common-lisp.dev/).

### Documentation

The API documentation is part of the
[GTK 4 API documentation](https://crategus.com/books/cl-cffi-gtk4).
