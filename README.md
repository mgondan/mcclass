# McClass
MisConception-aware Competence Learning and Assessment Smart System

If you wish, try it at https://mcclass.vps.webdock.io/mcclass

## Step 1: install R
Install a current version of R from www.r-project.org. 

Invoke R, and then

* install.packages("Rserve",,"http://rforge.net/",type="source")
* library(Rserve)
* Rserve()

## Step 2: install Prolog
Install a current version of SWI-Prolog from www.swi-prolog.org.

Invoke swipl, and then

* pack_install(mathml).
* pack_install(quantity).
* pack_install(rserve_client).
* halt.

## Start the server
To serve incoming connections at 8001 under linux, enter the folder mcclass and call 

`swipl server.pl --port=8001 --pidfile=http.pid`

Use `kill $(cat http.pid)` to stop the server.

## Behind an Apache proxy
Let's assume you have a working https-configuration. Then add the following two lines to your, e.g.,
/etc/apache2/sites-enabled/000-default-le-ssl.conf

        ProxyPass               /mcclass        http://localhost:8001/mcclass
        ProxyPassReverse        /mcclass        http://localhost:8001/mcclass

Then enable the reverse proxy with `sudo a2enmod proxy`, `sudo a2enmod proxy_http`, `sudo apache2 restart`.

This "worked for me", but I am no expert in network stuff.
