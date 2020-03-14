import asyncio
from graphql import graphql
from handofcats import as_command

from graphql import (
    GraphQLArgument,
    GraphQLEnumType,
    GraphQLEnumValue,
    GraphQLField,
    GraphQLInterfaceType,
    GraphQLList,
    GraphQLNonNull,
    GraphQLObjectType,
    GraphQLSchema,
    GraphQLString,
    ExecutionResult,
)

# from:
# https://graphql-core-next.readthedocs.io/en/latest/usage/schema.html
# https://graphql-core-next.readthedocs.io/en/latest/usage/resolvers.html
# https://graphql-core-next.readthedocs.io/en/latest/usage/queries.html

luke = dict(
    id="1000",
    name="Luke Skywalker",
    homePlanet="Tatooine",
    friends=["1002", "1003", "2000", "2001"],
    appearsIn=[4, 5, 6],
)

vader = dict(
    id="1001",
    name="Darth Vader",
    homePlanet="Tatooine",
    friends=["1004"],
    appearsIn=[4, 5, 6],
)

han = dict(
    id="1002",
    name="Han Solo",
    homePlanet=None,
    friends=["1000", "1003", "2001"],
    appearsIn=[4, 5, 6],
)

leia = dict(
    id="1003",
    name="Leia Organa",
    homePlanet="Alderaan",
    friends=["1000", "1002", "2000", "2001"],
    appearsIn=[4, 5, 6],
)

tarkin = dict(
    id="1004", name="Wilhuff Tarkin", homePlanet=None, friends=["1001"], appearsIn=[4]
)

human_data = {"1000": luke, "1001": vader, "1002": han, "1003": leia, "1004": tarkin}

threepio = dict(
    id="2000",
    name="C-3PO",
    primaryFunction="Protocol",
    friends=["1000", "1002", "1003", "2001"],
    appearsIn=[4, 5, 6],
)

artoo = dict(
    id="2001",
    name="R2-D2",
    primaryFunction="Astromech",
    friends=["1000", "1002", "1003"],
    appearsIn=[4, 5, 6],
)

droid_data = {"2000": threepio, "2001": artoo}


def get_character_type(character, _info, _type):
    return "Droid" if character["id"] in droid_data else "Human"


def get_character(id):
    """Helper function to get a character by ID."""
    return human_data.get(id) or droid_data.get(id)


def get_friends(character, _info):
    """Allows us to query for a character's friends."""
    return map(get_character, character.friends)


def get_hero(root, _info, episode):
    """Allows us to fetch the undisputed hero of the trilogy, R2-D2."""
    if episode == 5:
        return luke  # Luke is the hero of Episode V
    return artoo  # Artoo is the hero otherwise


def get_human(root, _info, id):
    """Allows us to query for the human with the given id."""
    return human_data.get(id)


def get_droid(root, _info, id):
    """Allows us to query for the droid with the given id."""
    return droid_data.get(id)


def get_secret_backstory(_character, _info):
    """Raise an error when attempting to get the secret backstory."""
    raise RuntimeError("secretBackstory is secret.")


episode_enum = GraphQLEnumType(
    "Episode",
    {
        "NEWHOPE": GraphQLEnumValue(4, description="Released in 1977."),
        "EMPIRE": GraphQLEnumValue(5, description="Released in 1980."),
        "JEDI": GraphQLEnumValue(6, description="Released in 1983."),
    },
    description="One of the films in the Star Wars Trilogy",
)


character_interface = GraphQLInterfaceType(
    "Character",
    lambda: {
        "id": GraphQLField(
            GraphQLNonNull(GraphQLString), description="The id of the character."
        ),
        "name": GraphQLField(GraphQLString, description="The name of the character."),
        "friends": GraphQLField(
            GraphQLList(character_interface),
            description="The friends of the character,"
            " or an empty list if they have none.",
        ),
        "appearsIn": GraphQLField(
            GraphQLList(episode_enum), description="Which movies they appear in."
        ),
        "secretBackstory": GraphQLField(
            GraphQLString, description="All secrets about their past."
        ),
    },
    resolve_type=get_character_type,
    description="A character in the Star Wars Trilogy",
)

human_type = GraphQLObjectType(
    "Human",
    lambda: {
        "id": GraphQLField(
            GraphQLNonNull(GraphQLString), description="The id of the human."
        ),
        "name": GraphQLField(GraphQLString, description="The name of the human."),
        "friends": GraphQLField(
            GraphQLList(character_interface),
            description="The friends of the human,"
            " or an empty list if they have none.",
            resolve=get_friends,
        ),
        "appearsIn": GraphQLField(
            GraphQLList(episode_enum), description="Which movies they appear in."
        ),
        "homePlanet": GraphQLField(
            GraphQLString,
            description="The home planet of the human, or null if unknown.",
        ),
        "secretBackstory": GraphQLField(
            GraphQLString,
            resolve=get_secret_backstory,
            description="Where are they from" " and how they came to be who they are.",
        ),
    },
    interfaces=[character_interface],
    description="A humanoid creature in the Star Wars universe.",
)

droid_type = GraphQLObjectType(
    "Droid",
    lambda: {
        "id": GraphQLField(
            GraphQLNonNull(GraphQLString), description="The id of the droid."
        ),
        "name": GraphQLField(GraphQLString, description="The name of the droid."),
        "friends": GraphQLField(
            GraphQLList(character_interface),
            description="The friends of the droid,"
            " or an empty list if they have none.",
            resolve=get_friends,
        ),
        "appearsIn": GraphQLField(
            GraphQLList(episode_enum), description="Which movies they appear in."
        ),
        "secretBackstory": GraphQLField(
            GraphQLString,
            resolve=get_secret_backstory,
            description="Construction date and the name of the designer.",
        ),
        "primaryFunction": GraphQLField(
            GraphQLString, description="The primary function of the droid."
        ),
    },
    interfaces=[character_interface],
    description="A mechanical creature in the Star Wars universe.",
)

query_type = GraphQLObjectType(
    "Query",
    lambda: {
        "hero": GraphQLField(
            character_interface,
            args={
                "episode": GraphQLArgument(
                    episode_enum,
                    description=(
                        "If omitted, returns the hero of the whole saga."
                        " If provided, returns the hero of that particular episode."
                    ),
                )
            },
            resolve=get_hero,
        ),
        "human": GraphQLField(
            human_type,
            args={
                "id": GraphQLArgument(
                    GraphQLNonNull(GraphQLString), description="id of the human"
                )
            },
            resolve=get_human,
        ),
        "droid": GraphQLField(
            droid_type,
            args={
                "id": GraphQLArgument(
                    GraphQLNonNull(GraphQLString), description="id of the droid"
                )
            },
            resolve=get_droid,
        ),
    },
)
schema = GraphQLSchema(query_type)


async def query(q: str) -> ExecutionResult:
    return await graphql(schema, q,)


@as_command
def run():
    q = """
    {
      droid(id: "2001") {
        name
        primaryFunction
      }
    }
    """
    result: ExecutionResult = asyncio.run(query(q))
    print(result.data, result.errors)
