# CLI

Awesome CLI user manager.

**Usage**:

```console
$ [OPTIONS] COMMAND [ARGS]...
```

**Options**:

* `--install-completion`: Install completion for the current shell.
* `--show-completion`: Show completion for the current shell, to copy it or customize the installation.
* `--help`: Show this message and exit.

**Commands**:

* `create`: Create a new user with USERNAME.
* `delete`: Delete a user with USERNAME.
* `delete-all`: Delete ALL users in the database.
* `init`: Initialize the users database.

## `create`

Create a new user with USERNAME.

**Usage**:

```console
$ create [OPTIONS] USERNAME
```

**Arguments**:

* `USERNAME`: [required]

**Options**:

* `--help`: Show this message and exit.

## `delete`

Delete a user with USERNAME.

If --force is not used, will ask for confirmation.

**Usage**:

```console
$ delete [OPTIONS] USERNAME
```

**Arguments**:

* `USERNAME`: [required]

**Options**:

* `--force / --no-force`: Force deletion without confirmation.  [required]
* `--help`: Show this message and exit.

## `delete-all`

Delete ALL users in the database.

If --force is not used, will ask for confirmation.

**Usage**:

```console
$ delete-all [OPTIONS]
```

**Options**:

* `--force / --no-force`: Force deletion without confirmation.  [required]
* `--help`: Show this message and exit.

## `init`

Initialize the users database.

**Usage**:

```console
$ init [OPTIONS]
```

**Options**:

* `--help`: Show this message and exit.

