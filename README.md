toothpick analysis code
=========

This repo contains the code which manages and analyses the various open health data sources which go into the overall 'Toothpick heathiness score' for areas of England.

Code is in Cascalog (for the heavy data lifting) and R (for the analysis part).

Data sources include:
- walking and cycling frequency data from the Department for Transport
- use of green space data from Natural England
- access to GPs data from HSCIC
- friends and family hospital recommendation data from NHS England
- access to dentists data from Toothpick

The visualisation of the data is in a separate 'site' repo.


Toothpick Data
==============

You can download the data [here](http://toothpick-study-data.s3-website-us-east-1.amazonaws.com/toothpick-datasets.zip)


GeoJSON
=======

The borough boundary data is supplied in [GeoJson](http://geojson.org/) format. We prefer the less verbose (and thus less bytes over the wire) [TopoJson](https://github.com/mbostock/topojson) format. Conversion between the two is achieved via a command line tool. Unfortunately installation of the tool is somewhat cumbersome. We provide a [Docker](http://www.docker.com) image to do the hard work. Assuming docker is installed.

```
docker run --volume=${PWD}:/docker mastodonc/topojson -p LA_code <input-file> > <output_file>
```

This docker image will be pulled from the public repo. We provide the Dockerfile in the ``topojson-build`` directory for reference, but you shouldn't need to build the image yourself.
