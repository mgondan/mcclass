# McClass
MisConception-aware Competence Learning and Assessment Smart System

If you wish, try it at https://mcclass.vps.webdock.cloud/mcclass

## Step 1: install R

Install a current version of R

`sudo apt install r-base`

Invoke R, and then

* `install.packages("RInside")`
* You also need WriteXLS for exporting Excel files: `install.packages("WriteXLS")`
* `install.packages("ggplot2")`
* `install.packages("svglite")`
* `quit()`

## Step 2: install postgresql

`sudo apt install unixodbc-dev odbc-postgresql postgresql-contrib`

The rest is described in database.txt.

Later, you just invoke `sudo service postgresql restart` to start the database.

## Step 3: install Prolog

Install a current version of SWI-Prolog from www.swi-prolog.org.

`sudo apt install swi-prolog-odbc`

Invoke swipl, and then

* `pack_install(quantity).`
* `pack_install(mathml).`
* `pack_install(rolog).`
* `pack_install(interval).`
* `halt.`

## Start the server
To serve incoming connections at 8001 under linux, enter the folder mcclass and call 

`swipl server.pl --port=8001 --pidfile=http.pid`

Use `kill $(cat http.pid)` to stop the server. Sometimes this doesn't work, then use `killall -9 swipl` :-)

## Watch the program

Open a web browser and go to http://localhost:8001/mcclass

## Behind an Apache proxy

Let's assume you have a working https-configuration. Then add the following two lines to your, e.g.,
/etc/apache2/sites-enabled/000-default-le-ssl.conf

        ProxyPass               /mcclass        http://localhost:8001/mcclass
        ProxyPassReverse        /mcclass        http://localhost:8001/mcclass

Then enable the reverse proxy with `sudo a2enmod proxy`, `sudo a2enmod proxy_http`, `systemctl restart apache2`.

This "worked for me", but I am no expert in network stuff.
