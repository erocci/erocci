FROM debian:jessie
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

ADD occi.xml /tmp/occi.xml
ADD sys.config /tmp/sys.config
ADD run.sh /root/run.sh
ADD erocci-rel*.tar.xz /opt/

RUN apt-get update && \
    apt-get -y install --no-install-recommends \
    libexpat1 libaprutil1 libssl1.0.0 libavahi-compat-libdnssd1 ca-certificates tar xz-utils && \
    apt-get -y clean && \
    chmod a+x /root/run.sh

CMD [ "/root/run.sh" ]

EXPOSE 80
