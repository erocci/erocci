# erocci-docker
erocci Dockerfile and resources

Will run a local erocci on port 

## Usage

```
$ docker pull erocci/erocci
$ docker run --name demo_erocci -p 80 -d erocci/erocci
```

Now, erocci is running on port 80 in the container, check on which
port is erocci accessible from outside of the container with:

```
$ docker ps
```

You will see something like 0.0.0.0:XXXXX->80/tcp

Query erocci with curl, for instance:
```
$ curl -v -H 'accept: application/json' http://localhost:XXXXX/-/
```

## Customization

* Would you just need to change the exposed OCCI schema, map an OCCI
  XML schema to `/tmp/occi.xml`

* Would you need to change global erocci configuration, map your
  erocci config file to `/tmp/sys.config`
  