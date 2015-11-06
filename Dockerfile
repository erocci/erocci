FROM jeanparpaillon/erlang-mini
MAINTAINER Jean Parpaillon <jean.parpaillon@free.fr>

ENV DEBIAN_FRONTEND noninteractive

ADD occi.xml /tmp/occi.xml
ADD sys.config /tmp/sys.config
ADD run.sh /root/run.sh
ADD COMMIT_ID /root/COMMIT_ID

RUN apt-get update
RUN apt-get -y install --no-install-recommends git \
    build-essential pkg-config ca-certificates libxml2-dev && \
    apt-get -y clean
RUN git clone https://github.com/erocci/erocci.git && \
    cd erocci && \
    git checkout `cat /root/COMMIT_ID`  && \
    ./bootstrap && \
    make

CMD [ "/root/run.sh" ]

EXPOSE 80
