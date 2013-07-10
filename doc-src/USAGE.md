## Usage

The postfix crate provides a `server-spec` function that returns a
server-spec. This server spec will install and run the postfix server (not the
dashboard).  You pass a map of options to configure postfix.  The `:config`
value should be a form that will be output as the
[postfix configuration](http://postfix.io/howto.html).

The `server-spec` provides an easy way of using the crate functions, and you can
use the following crate functions directly if you need to.

The `settings` function provides a plan function that should be called in the
`:settings` phase.  The function puts the configuration options into the pallet
session, where they can be found by the other crate functions, or by other
crates wanting to interact with the postfix server.  The settings are made up of
a `:master` key, with a value as a sequence of maps, each specifying a daemon
as specified in [master.cnf](http://www.postfix.org/master.5.html).

The `:supervision` key in the settings allows running postfix under `:runit`,
`:upstart` or `:nohup`.

The `install` function is responsible for actually installing postfix.  At
present installation from tarball url is the only supported method.
Installation from deb or rpm url would be nice to add, as these are now
available from the postfix site.

The `configure` function writes the postfix configuration file, using the form
passed to the :config key in the `settings` function.

The `run` function starts the postfix server.


## Live test on vmfest

For example, to run the live test on VMFest, using Ubuntu 12.04:

```sh
lein with-profile +vmfest pallet up --selectors ubuntu-12-04 --phases install,configure,test
lein with-profile +vmfest pallet down --selectors ubuntu-12-04
```
