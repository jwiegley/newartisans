---
title: Serving up Mercurial using mod_python
---

The following information resulted from several hours of battling with SELinux and Apache, attempting to find some way of serving up my Mercurial repository (now at [http://hg.newartisans.com](http://hg.newartisans.com)) over HTTP.  In short, I found that `cgi-bin` would not work at all with SELinux, for reasons I couldn't figure out (it didn't generate any AVC messages once I extended Apache's permissions -- it just wouldn't serve any data).

But getting mod_python to work turned out to be no picnic either.  The data is out there, on the Web, but nothing from any one place worked for me.  So after much effort, here's what I discovered, boiled down into nice easy chunks for the tired reader.

<!--more-->
## Steps to follow

1. Install Mercurial.  I'm using CentOS 5, and Mercurial was not available through `yum`.  I installed it from Dag Wieers repository.

2. Install mod_python.  This you can do through yum with `yum install mod_python`.

3. Create a directory for your Mercurial projects in `/srv/hg`.  My ledger project ended up having all the Mercurial files in `/srv/hg/ledger/.hg`.  Note that unlike systems like git, you do need the files in the `.hg` subdirectory.  The rest of the parent directory will remain empty.  Make sure that the security context on all of these directories is `user_u:object_r:public_content_rw_t`, if you intend to allow others to post changes.  Otherwise, make it `user_u:object_r:public_content_t`.  The files will have to be readable and writable by Apache if you want to allow commits.  To change the security context, you can use this command: `chcon -R user_u:object_r:public_content_t /srv/hg`.

4. Create a directory called `/etc/hgweb`.  In it you will have two files, `hgwebdir.conf` and `users.htdigest`.  I'll give you some example files in the appendices to this article.  The security context for this directory and all the files in it should be `system_u:object_r:httpd_config_t`.  The `users.htdigest` file should be owned by `apache`, and have permissions set to 400.

5. Create a directory called `/var/hg`.  It will have two files in it, `hgwebdir.py` and `modpython_gateway.py`.  Both of those are found below.  The security context for this directory and its contents should be `system_u:object_r:httpd_sys_content_t`.  It can be owned by root, just make sure it's world readable.

6. Configure your Apache server.  I'm using a virtual host to serve pages on `hg.newartisans.com`.  I leave it up to you to modify this for running on your main server.  See below.

7. I still can't get pushes to work over HTTP (I'm using ssh).  If you figure this out or have an answer, please send me a note!

## `/etc/hgweb/hgwebdir.conf`

The only part of this you'll need to change is the name and e-mail address of the administrator.  And change `push_ssl` if you need to deliver encrypted data (such as work-related sources).  Note: if you change `push_ssl`, you're going to have to configure Apache to use SSL for your host, which is beyond the scope of this article.

    [collections]
    /srv/hg = /srv/hg
    
    [web]
    style = gitweb
    allow_archive = bz2 gz zip
    contact = John Wiegley, johnw@newartisans.com
    push_ssl = false

## `/etc/hgweb/users.htdigest`

You can make this file very easily.  Here's what I typed to create an entry for myself:

    htdigest -c users.htdigest "New Artisans LLC" johnw

It will ask you for the password to use.  You'll need an entry for everyone who will have commit access to your repository.  If you are using the `htdigest` command to make these entries, remove the `-c` flag!  That causes the file to get created the first time.

## `/var/hg/hgwebdir.py`

Strangely enough, there is no working version of this on the Web that I could find.  But here's what worked for me:

    #!/usr/bin/env python
    #
    # An example CGI script to export multiple hgweb repos,
    # edit as necessary
    
    import cgitb
    cgitb.enable()
    
    from mercurial.hgweb.hgwebdir_mod import hgwebdir
    from mercurial.hgweb.request import wsgiapplication
    import mercurial.hgweb.wsgicgi as wsgicgi
    
    def make_web_app():
        return hgwebdir("/etc/hgweb/hgwebdir.conf")
    
    def start(environ, start_response):
        toto = wsgiapplication(make_web_app)
        return toto (environ, start_response)

## `/var/hg/modpython_gateway.py`

This is quite a bit longer, so I'm just going to link to the copy of [modpython_gateway.py](/downloads_files/modpython_gateway.py) in my Downloads section.

## changes to `/etc/httpd/conf/httpd.conf`

There are a couple of oddities in here: First, the rewrite rule.  You'll want this.  Ignore the fact that it contains the string `hgwebdir.cgi` in it.  Apparently `hgwebdir` depends on this and then ignores it, but since we're running with mod_python it doesn't actually do anything.  If you get rid of it, the errors you get will be rather hard to figure out!

Also, be careful with the `AuthName`.  This is the same exact string that you have to pass to the `htdigest` command (see above), or else user authentications will fail.

    
        ServerAdmin webmaster@newartisans.com
        ServerName hg.newartisans.com
    
        ErrorLog /var/log/httpd/error_log
        CustomLog /var/log/httpd/access_log combined
    
        RewriteEngine On
        RewriteRule ^/(.*) /hgwebdir.cgi/$1
    
        
            PythonPath "sys.path + ['/var/hg']"
            SetHandler mod_python
            PythonHandler modpython_gateway::handler
            PythonOption wsgi.application hgwebdir::start
    
            Order allow,deny
            Allow from all
    
            # authentication
            AuthType Digest
            AuthName "New Artisans LLC"
            AuthDigestDomain hg.newartisans.com
    
            AuthDigestProvider file
            AuthUserFile "/etc/hgweb/users.htdigest"
    
            
                Require valid-user
            
        
    
    

