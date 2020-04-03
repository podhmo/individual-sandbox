## altair触ってみたいかも？

- https://altair-viz.github.io/gallery/index.html
- https://vega.github.io/vega-lite/ecosystem.html

## jupyter notebook python codeの変換

```console
$ jupyter-nbconvert <> --to python
```

## とりあえずテキトーなdatasetを使ってみる

- rの組み込み
- sklern.datasets
- vega_datasets

vega_datasetsの方が軽い。

### iris

```console
$ r
help(iris)
summary(iris)
```

[./example_iris](./example_iris)

### r -> pandas

- https://pandas.pydata.org/docs/getting_started/comparison/comparison_with_r.html

## jupyter notebookでRを動かそうとしてみる

```console
$ brew cask install r
$ r
r > install.packages(c('repr', 'IRdisplay', 'evaluate', 'crayon', 'pbdZMQ', 'devtools', 'uuid', 'digest'))
r > devtools::install_github('IRkernel/IRkernel')
r > IRkernel::installspec()
```

devtoolsのインストールに失敗する

<details>

```
> install.packages(c('devtools'))
also installing the dependencies ‘later’, ‘promises’, ‘DT’

Warning: unable to access index for repository https://cran.ism.ac.jp/bin/macosx/el-capitan/contrib/3.6:
  cannot open URL 'https://cran.ism.ac.jp/bin/macosx/el-capitan/contrib/3.6/PACKAGES'
Packages which are only available in source form, and may need
  compilation of C/C++/Fortran: ‘later’ ‘promises’
Do you want to attempt to install these from sources? (Yes/no/cancel) yes
installing the source packages ‘later’, ‘promises’, ‘DT’, ‘devtools’

trying URL 'https://cran.ism.ac.jp/src/contrib/later_1.0.0.tar.gz'
Content type 'application/x-gzip' length 56563 bytes (55 KB)
==================================================
downloaded 55 KB

trying URL 'https://cran.ism.ac.jp/src/contrib/promises_1.1.0.tar.gz'
Content type 'application/x-gzip' length 2659515 bytes (2.5 MB)
==================================================
downloaded 2.5 MB

trying URL 'https://cran.ism.ac.jp/src/contrib/DT_0.13.tar.gz'
Content type 'application/x-gzip' length 1500559 bytes (1.4 MB)
==================================================
downloaded 1.4 MB

trying URL 'https://cran.ism.ac.jp/src/contrib/devtools_2.2.2.tar.gz'
Content type 'application/x-gzip' length 375464 bytes (366 KB)
==================================================
downloaded 366 KB

* installing *source* package ‘later’ ...
** package ‘later’ successfully unpacked and MD5 sums checked
** using staged installation
** libs
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c RcppExports.cpp -o RcppExports.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c callback_registry.cpp -o callback_registry.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c debug.cpp -o debug.o
clang -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c init.c -o init.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c later.cpp -o later.o
clang++ -std=gnu++11 -I"/Library/Frameworks/R.framework/Resources/include" -DNDEBUG -pthread -DSTRICT_R_HEADERS -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/Rcpp/include" -I"/Library/Frameworks/R.framework/Versions/3.6/Resources/library/BH/include" -isysroot /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk -I/usr/local/include  -fPIC  -Wall -g -O2  -c later_posix.cpp -o later_posix.o
In file included from later_posix.cpp:6:
In file included from /Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:655:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/gethostuuid.h:39:17: error: C++ requires a type specifier for
      all declarations
int gethostuuid(uuid_t, const struct timespec *) __OSX_AVAILABLE_STARTING(__MAC_10_5, __IPHONE_NA);
                ^
In file included from later_posix.cpp:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:662:27: error: unknown type name 'uuid_t'; did you
      mean 'uid_t'?
int      getsgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from later_posix.cpp:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:664:27: error: unknown type name 'uuid_t'; did you
      mean 'uid_t'?
int      getwgroups_np(int *, uuid_t);
                              ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from later_posix.cpp:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:727:31: error: unknown type name 'uuid_t'; did you
      mean 'uid_t'?
int      setsgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
In file included from later_posix.cpp:6:
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/unistd.h:729:31: error: unknown type name 'uuid_t'; did you
      mean 'uid_t'?
int      setwgroups_np(int, const uuid_t);
                                  ^
/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h:31:31: note: 'uid_t' declared here
typedef __darwin_uid_t        uid_t;
                              ^
later_posix.cpp:195:13: warning: unused variable 'ret' [-Wunused-variable]
    ssize_t ret = write(dummy_pipe_in, "a", 1);
            ^
1 warning and 5 errors generated.
```

</details>

### ついでに

```
r > install.packages(c('tidyverse'))
```

- https://heavywatal.github.io/rstats/ggplot2.html
- http://www.sthda.com/english/wiki/ggplot2-density-plot-quick-start-guide-r-software-and-data-visualization
- https://stats.biopapyrus.jp/r/ggplot/geom_histogram.html
- http://bioinfo-dojo.net/2016/01/19/dataset_iris/

## jupyter notebook shortcut

- C P
- ESC m markdown
- ESC y code
- ESC h help
- ESC x cut
- ESC v paste cell below
- ESC V paste cell above
- ESC a insert cell above
- ESC b insert cell below
- Ctrl Shift - split cell at cursor

## visualize してみたい

以下でvisualizeしてみたい。

- python, seaborn (scipy)
- python, skleran
- python, statsmodel
- python, altair
- r, ggplot2

とりあえず何を表示してみよう？

- histgram
- KDE (kernel density
