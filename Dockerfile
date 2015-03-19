FROM jeanparpaillon/erlang-mini
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

ADD occi.xml /tmp/occi.xml
ADD sys.config /tmp/sys.config
ADD run.sh /root/run.sh

RUN apt-get update

RUN apt-get -y install --no-install-recommends \
    build-essential autoconf automake libtool git libssl-dev libexpat1-dev libaprutil1-dev \
    libavahi-compat-libdnssd-dev rebar wget && \
    apt-get clean

RUN apt-get -y install --no-install-recommends \
    erlang-cowboy \
    libexpat1 libaprutil1 libssl1.0.0 libavahi-compat-libdnssd1 ca-certificates && \
    apt-get clean

RUN git -C /root clone https://github.com/erocci/erocci.git && \
    cd /root/erocci && \
    ./autogen.sh --disable-listener-xmpp --disable-backend-dbus --disable-backend-admin && \
    make deps rel && \
    mv /root/erocci/rel/erocci /opt/erocci && \
    mkdir -p /var/log/erocci && \
    chmod a+x /root/run.sh

RUN apt-get -y remove \
    build-essential autoconf automake libtool git libssl-dev libexpat1-dev libaprutil1-dev \
    libavahi-compat-libdnssd-dev rebar wget && \
    apt-get -y autoremove

CMD [ "/root/run.sh" ]

EXPOSE 80
