```
$ make 01
DOCKER_BUILDKIT=1 docker build -t foo:0.1.0 . -f Dockerfile.cached
[+] Building 28.0s (9/9) FINISHED
 => [internal] load build definition from Dockerfile.cached                                            0.1s
 => => transferring dockerfile: 223B                                                                   0.1s
 => [internal] load .dockerignore                                                                      0.0s
 => => transferring context: 2B                                                                        0.0s
 => resolve image config for docker.io/docker/dockerfile:experimental                                  0.0s
 => CACHED docker-image://docker.io/docker/dockerfile:experimental                                     0.0s
 => [internal] load metadata for docker.io/library/python:3.9-slim                                     0.0s
 => CACHED [internal] settings cache mount permissions                                                 0.0s
 => [stage-0 1/2] FROM docker.io/library/python:3.9-slim                                               0.1s
 => => resolve docker.io/library/python:3.9-slim                                                       0.0s
 => [stage-0 2/2] RUN --mount=type=cache,mode=0755,target=/root/.cache/pip python3 -m pip install bo  25.1s
 => exporting to image                                                                                 2.1s
 => => exporting layers                                                                                2.1s
 => => writing image sha256:7c6ea4f4d15443e6d44427bd3643074d469c292faae5363ca191a5ea8254389f           0.0s
 => => naming to docker.io/library/foo:0.1.0


$ make 02
DOCKER_BUILDKIT=1 docker build -t foo:0.1.1 . -f Dockerfile.cached2
[+] Building 20.0s (9/9) FINISHED
 => [internal] load build definition from Dockerfile.cached2                                           0.0s
 => => transferring dockerfile: 230B                                                                   0.0s
 => [internal] load .dockerignore                                                                      0.0s
 => => transferring context: 2B                                                                        0.0s
 => resolve image config for docker.io/docker/dockerfile:experimental                                  0.0s
 => CACHED docker-image://docker.io/docker/dockerfile:experimental                                     0.0s
 => [internal] load metadata for docker.io/library/python:3.9-slim                                     0.0s
 => CACHED [stage-0 1/2] FROM docker.io/library/python:3.9-slim                                        0.0s
 => CACHED [internal] settings cache mount permissions                                                 0.0s
 => [stage-0 2/2] RUN --mount=type=cache,mode=0755,target=/root/.cache/pip python3 -m pip install bo  17.1s
 => exporting to image                                                                                 2.4s
 => => exporting layers                                                                                2.4s
 => => writing image sha256:3aa07bfd1069b09ea6b46d373381315bb382f4dc75d831b9d0f448a445f83a26           0.0s
 => => naming to docker.io/library/foo:0.1.1                               
 ```

```
#8 3.665 Collecting boto3
#8 3.667   Using cached boto3-1.17.112-py2.py3-none-any.whl (131 kB)
#8 4.185 Collecting pandas
#8 4.219   Using cached pandas-1.3.0-cp39-cp39-manylinux_2_5_x86_64.manylinux1_x86_64.whl (10.6 MB)
#8 4.405 Collecting s3transfer<0.5.0,>=0.4.0
#8 4.408   Using cached s3transfer-0.4.2-py2.py3-none-any.whl (79 kB)
#8 4.453 Collecting jmespath<1.0.0,>=0.7.1
#8 4.456   Using cached jmespath-0.10.0-py2.py3-none-any.whl (24 kB)
#8 5.512 Collecting botocore<1.21.0,>=1.20.112
#8 5.530   Using cached botocore-1.20.112-py2.py3-none-any.whl (7.7 MB)
#8 5.709 Collecting urllib3<1.27,>=1.25.4
#8 5.713   Using cached urllib3-1.26.6-py2.py3-none-any.whl (138 kB)
#8 5.773 Collecting python-dateutil<3.0.0,>=2.1
#8 5.777   Using cached python_dateutil-2.8.2-py2.py3-none-any.whl (247 kB)
#8 5.836 Collecting six>=1.5
#8 5.840   Using cached six-1.16.0-py2.py3-none-any.whl (11 kB)
#8 6.357 Collecting pytz>=2017.3
#8 6.360   Using cached pytz-2021.1-py2.py3-none-any.whl (510 kB)
#8 7.048 Collecting numpy>=1.17.3
#8 7.097   Using cached numpy-1.21.0-cp39-cp39-manylinux_2_12_x86_64.manylinux2010_x86_64.whl (15.7 MB)
```

```console
$ (docker images | ioknife rest | ggrep -P 'foo') |& gsed 's/   */\t/g'
REPOSITORY      TAG     IMAGE ID        CREATED SIZE
foo     0.2.0   d981b2beccb7    2 minutes ago   293MB
foo     0.1.1   3aa07bfd1069    7 minutes ago   293MB
foo     0.1.0   7c6ea4f4d154    8 minutes ago   293MB
foo     0.0.0   fb6ec39eb3b5    10 minutes ago  329MB
```


more

```
$ make 01
DOCKER_BUILDKIT=1 docker build -t foo:0.1.0 . -f Dockerfile.cached --no-cache
[+] Building 24.8s (9/9) FINISHED
 => [internal] load build definition from Dockerfile.cached                                                              0.0s
 => => transferring dockerfile: 223B                                                                                     0.0s
 => [internal] load .dockerignore                                                                                        0.0s
 => => transferring context: 2B                                                                                          0.0s
 => resolve image config for docker.io/docker/dockerfile:experimental                                                    0.0s
 => CACHED docker-image://docker.io/docker/dockerfile:experimental                                                       0.0s
 => [internal] load metadata for docker.io/library/python:3.9-slim                                                       0.0s
 => CACHED [internal] settings cache mount permissions                                                                   0.0s
 => CACHED [stage-0 1/2] FROM docker.io/library/python:3.9-slim                                                          0.0s
 => [stage-0 2/2] RUN --mount=type=cache,mode=0755,target=/root/.cache/pip python3 -m pip install boto3 pandas          20.9s
 => exporting to image                                                                                                   2.4s
 => => exporting layers                                                                                                  2.3s
 => => writing image sha256:dcae12aebff6bff55d9a443e5596f6e0b660171c1788eb842743d07ab0feb43b                             0.0s
 => => naming to docker.io/library/foo:0.1.0                                                                             0.0s
(my) [~/venvs/my/individual-sandbox/daily/20210715/example_python/ 20:24:29]$ make 01
DOCKER_BUILDKIT=1 docker build -t foo:0.1.0 . -f Dockerfile.cached --no-cache
[+] Building 19.4s (9/9) FINISHED
 => [internal] load build definition from Dockerfile.cached                                                              0.0s
 => => transferring dockerfile: 44B                                                                                      0.0s
 => [internal] load .dockerignore                                                                                        0.0s
 => => transferring context: 2B                                                                                          0.0s
 => resolve image config for docker.io/docker/dockerfile:experimental                                                    0.0s
 => CACHED docker-image://docker.io/docker/dockerfile:experimental                                                       0.0s
 => [internal] load metadata for docker.io/library/python:3.9-slim                                                       0.0s
 => CACHED [internal] settings cache mount permissions                                                                   0.0s
 => CACHED [stage-0 1/2] FROM docker.io/library/python:3.9-slim                                                          0.0s
 => [stage-0 2/2] RUN --mount=type=cache,mode=0755,target=/root/.cache/pip python3 -m pip install boto3 pandas          16.6s
 => exporting to image                                                                                                   2.3s
 => => exporting layers                                                                                                  2.3s
 => => writing image sha256:d4a67db6ae9982a8cd55b675c94cfab05dff9fd11a4519a9cd1f837cbd46668b                             0.0s
 => => naming to docker.io/library/foo:0.1.0                                     
 ```
