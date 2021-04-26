import typer

app = typer.Typer(help="Awesome CLI")


@app.command()
def create(username: str):
    """
    Create a new user with USERNAME.
    """
    typer.echo(f"Creating user: {username}")


@app.command()
def delete(
    username: str,
    *,
    force: bool,
):
    """
    Delete a user with USERNAME.

    If --force is not used, will ask for confirmation.
    """
    if force:
        typer.echo(f"Deleting user: {username}")
    else:
        typer.echo("Operation cancelled")

@app.command()
def delete_all(
    *,
    force: bool,
):
    """
    Delete ALL users in the database.

    If --force is not used, will ask for confirmation.
    """
    if force:
        typer.echo("Deleting all users")
    else:
        typer.echo("Operation cancelled")

from functools import cache

# xx
@app.command()
@cache # yy
def init():
    """
    Initialize the users database.
    """
    typer.echo("Initializing user database")


if __name__ == "__main__":
    app()

