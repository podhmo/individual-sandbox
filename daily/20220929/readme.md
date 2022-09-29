# go io/fs

- https://pkg.go.dev/io/fs@go1.19.1#FS

ディレクトリを模すにはどうすれば良いんだろう？

とりあえず触ってみる

- 00: テキトーに木をつくる
- 01: mapでfs.FSを実装してみる (失敗)
- 02: testing/fstestを思い出す
- 03: 02をtesting/fstestを利用して作る
- 最終的にはs3でfstestをやりたいのだけど

探せばあるね。。

- [jszwec/s3fs: S3 FileSystem (fs.FS) implementation](https://github.com/jszwec/s3fs)