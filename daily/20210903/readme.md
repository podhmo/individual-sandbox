## ghcr 利用方法

- [GitHub Container Registry(ghcr.io)にDockerイメージをpushする手順 - Qiita](https://qiita.com/zembutsu/items/1effae6c39ceae3c3d0a)

buil,push

```
# docker.pkg.github.com/OWNER/REPOSITORY/IMAGE_NAME:VERSION でイメージを構築
# Dockerfileはカレントワーキングディレクトリ (.)にあるものとする
$ docker build -t docker.pkg.github.com/octocat/octo-app/monalisa:1.0 .
$ docker build -t docker.pkg.github.com/octocat/octo-app/monalisa:1.0 .

# Push the image to GitHub Packages
$ docker push docker.pkg.github.com/octocat/octo-app/monalisa:1.0
```

pull

```
$ docker pull docker.pkg.github.com/OWNER/REPOSITORY/IMAGE_NAME:TAG_NAME
```

## arm64用のdocker imageをビルド

- [Dockerレジストリの利用 - GitHub Docs](https://docs.github.com/ja/packages/working-with-a-github-packages-registry/working-with-the-docker-registry)
- [Github ActionsでGithub Container RegistryにDocker imageをpushする最小Workflow - Qiita](https://qiita.com/kawakawaryuryu/items/b0291c1bc1141a535263)

あと

- https://github.com/multiarch/qemu-user-static
