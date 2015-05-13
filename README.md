# erocci-docker
erocci Dockerfile and resources

Will run a local erocci on port 

## Usage

```
$ docker pull erocci/erocci
$ docker run --name demo_erocci -p 80 -d erocci/erocci
```

## Customization

* Would you just need to change the exposed OCCI schema, map an OCCI
  XML schema to `/tmp/occi.xml`

* Would you need to change global erocci configuration, map your
  erocci config file to `/tmp/sys.config`
  