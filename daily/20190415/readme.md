## macでmakeのbash completion

```
$ sudo port install bash-completion
$ port contents bash-completion | grep make
  /opt/local/share/bash-completion/completions/automake
  /opt/local/share/bash-completion/completions/automake-1.10
  /opt/local/share/bash-completion/completions/automake-1.11
  /opt/local/share/bash-completion/completions/automake-1.12
  /opt/local/share/bash-completion/completions/automake-1.13
  /opt/local/share/bash-completion/completions/automake-1.14
  /opt/local/share/bash-completion/completions/automake-1.15
  /opt/local/share/bash-completion/completions/colormake
  /opt/local/share/bash-completion/completions/gmake
  /opt/local/share/bash-completion/completions/gnatmake
  /opt/local/share/bash-completion/completions/gnumake
  /opt/local/share/bash-completion/completions/make
  /opt/local/share/bash-completion/completions/makepkg
  /opt/local/share/bash-completion/completions/pmake
  /opt/local/share/cmake/bash-completion/bash-completion-config-version.cmake
  /opt/local/share/cmake/bash-completion/bash-completion-config.cmake

$ port note bash-completion
Warning: xcodebuild exists but failed to execute
bash-completion has the following notes:
  To use bash_completion, add the following lines at the end of your .bash_profile:
    if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
        . /opt/local/etc/profile.d/bash_completion.sh
    fi

  The port bash-completion >=2.0 requires bash >=4.1; please make sure
  you are using /opt/local/bin/bash by changing the preferences of your
  terminal accordingly. If your version of bash is too old, the script
  above will not modify your shell environment and no extended completion
  will be available.
```

なので `~/.bash_profile` に以下を追加。

```
if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
fi
```

## make

- .DEAFAULT_GOAL
- MAKEFILE_LIST

