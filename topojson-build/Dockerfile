FROM ubuntu:14.04
MAINTAINER Neale Swinnerton <neale@mastodonc.com>

RUN apt-get update
RUN apt-get -y install software-properties-common git python make gcc g++
RUN add-apt-repository ppa:chris-lea/node.js
RUN apt-get update
RUN apt-get install -y nodejs

RUN npm install -g topojson

ENTRYPOINT ["topojson"]
