# rbmc
Repo for the Risk Based Monitoring Calculator app from the SCTO Monitoring platform.

The app is hosted on CTU Berns shiny server at [https://shiny.ctu.unibe.ch/rbmc](https://shiny.ctu.unibe.ch/rbmc).

Build the app on a Ubuntu 20.04 LTS VM using docker:

```
sudo docker build -t rbmc .
```

Check that the app runs (note that you need to have a REDCap API token)

```
sudo docker run -d --rm -p 3838:3838 rbmc
```

Save the image to a tarball, with a suitable version number (the current version can be found [here](https://shiny.ctu.unibe.ch/version/rbmc))

```
sudo docker image save rbmc | gzip > rbmc-v1.1.tar.gz
```

Transfer the tarball to the shiny server and unpack it.

```
scp tdl-v1.1.tar.gz username@shiny-02.ctu.unibe.ch:/home/shiny

cd ..
cd shiny
gzip tarball -dk rbmc-v1.1.tar.gz
```

(At this point the [version  number](https://shiny.ctu.unibe.ch/version/rbmc) is automatically updated)

Connect to the shiny server 

```
ssh 'username@shiny-02.ctu.unibe.ch'
```
Check if the app is running

```
sudo docker ps
```

If it is, it can be stopped via (note that current users will probably be confused and have to start again)

```
sudo docker stop <containerid>
sudo docker rm <containerid>
```

Load the docker image.

```
sudo docker image load --input rbmc-v1.1.tar
```

Remove the old version (and any other old ones from other apps)
```
sudo docker image prune
```


Cleanup the shiny folder (the following will remove both tar and tar.gz files)

```
rm *.tar*
```

Disconnect from the shiny server 

```
exit
```
