---
date: 2014-11-26 01:05:27 PST
title: Single CoreOS Droplet on DigitalOcean
tags: Mad Coding, CoreOS, Docker, DigitalOcean
---
**Introduction**
----------------

I just finished converting my blog to run on [Docker][1] and sit on top of
[CoreOS][2]. Since it's my blog and I don't actually want to spend money on a
cluster of VMs, I use just a single server (single instance/droplet) of CoreOS.
Below is a list of steps on how to do the same on DigitalOcean. I took
instructions from [DigitalOcean's guide][3] on setting up a CoreOS cluster for
my case.

**Prerequisites**
=================

**Docker**
----------
I assume you have whatever application you want to run in a Docker image.
Docker makes it super easy to replicate an environment. I have the same thing
running on my laptop as on DigitalOcean except for some SSL files.

One downside of Docker though is the image size. When I first started with
Docker, my image size was 1.4G. I've done some work to half the size for now
while utilizing the image cache as much as I can.

**SSH Keys**
------------
This section in [DigitalOcean's guide][3] is applicable. Below is what it says
about SSH keys:

Every CoreOS server that you create will need to have at least one SSH public
key installed during its creation process. The key(s) will be installed to the
`core` user's authorized keys file, and you will need the corresponding private
key(s) to log in to your CoreOS server.

If you do not already have any SSH keys associated with your DigitalOcean
account, do so now by following steps 1-3 of this tutorial: [How To Use SSH
Keys with DigitalOcean Droplets][4]. Then you will want to add your private key
to your SSH agent on your client machine by running the following command:

<pre class="brush:bash">
ssh-add
</pre>

**Write a Cloud-Config File**
=============================
For our purpose of a single server CoreOS, the cloud-config file is really just
applicable for configuring CoreOS's reboot strategy after installing an update.
There's a lot more to cloud-config that's documented [here][5], but we don't
need that right now.

**Minimal Cloud-Config**
------------------------
My cloud-config file is very simple and only configures the reboot-strategy:

<pre>
#cloud-config
coreos:
  update:
    reboot-strategy: reboot
</pre>

**Create Droplet on DigitalOcean**
==================================
If you have the prerequisite and the minimal cloud-config file, then you're
ready to create a new droplet on DigitalOcean.

You would go through the typical steps:

1. Give the droplet a name
1. Select a droplet size
1. Select region
1. Then when it comes to the "Available Settings" section, check the "Enable
   User Data" checkbox and put the content of cloud-config in there.
1. Select CoreOS image in the next section
1. Add your SSH key
1. Click the "Create Droplet" button

**SSH to CoreOS**
-----------------
Your DigitalOcean Control Panel should show the IP of the droplet. You can SSH
into CoreOS by typing:

<pre class="brush:bash">
ssh core@[ip]
</pre>

**Setup systemd Service to Run Docker**
=======================================
The last major step is to setup a systemd service to automatically run a docker
container to serve your application.

**Login to Docker Hub**
-----------------------
If you are not using a private image on [Docker Hub][6], then you can skip this
step.

Since downloading Docker images can take a while, it's a good idea to grab the
image prior to doing anything else. For example, I grab my private image from
Docker Hub this way:

<pre class="brush:bash">
docker login
docker pull dannysu/mystuff
</pre>

You'll be prompted for username, password and email for the `docker login`
command.

**Writing the systemd Service File**
------------------------------------
A minimal systemd service file that you can use as an example is shown below.
You'll save it as `mystuff.service`. It basically defines what to run when you
type `systemctl start mystuff` or `systemctl stop mystuff`. Save this file to
`/etc/systemd/system/mystuff.service` on your CoreOS instance.

<pre>
[Unit]
Description=My Cool Stuff
After=docker.service

[Service]
User=core
TimeoutStartSec=0
KillMode=none
EnvironmentFile=/etc/environment
ExecStart=/home/core/mystuff.service.sh ${COREOS_PUBLIC_IPV4}
ExecStop=/usr/bin/docker stop mystuff

[Install]
WantedBy=default.target
</pre>

In my example, ExecStart runs a shell script named mystuff.service.sh. I use it
to decide what docker commands to run. You can use it as an example:
<pre class="brush:bash">
#!/bin/bash

if [ -n "$(docker ps -l -q)" ]; then
    /usr/bin/docker start -i mystuff
else
    /usr/bin/docker login -u dannysu
    /usr/bin/docker pull dannysu/mystuff
    /usr/bin/docker run --name mystuff -p $1:80:80 dannysu/mystuff nginx
fi
</pre>

Finally, you can start the service and make it run upon reboot as well:
<pre>
systemctl start mystuff
systemctl enable mystuff
</pre>


  [1]: https://www.docker.com/
  [2]: https://coreos.com/
  [3]: https://www.digitalocean.com/community/tutorials/how-to-set-up-a-coreos-cluster-on-digitalocean
  [4]: https://www.digitalocean.com/community/tutorials/how-to-use-ssh-keys-with-digitalocean-droplets
  [5]: https://coreos.com/docs/cluster-management/setup/cloudinit-cloud-config/
  [6]: https://hub.docker.com
