from enum import Enum
from graphql import build_schema
from graphql import graphql_sync


# https://graphql-core-next.readthedocs.io/en/latest/usage/sdl.html
class EpisodeEnum(Enum):
    NEWHOPE = 4
    EMPIRE = 5
    JEDI = 6


schema = build_schema(
    """

    enum Episode { NEWHOPE, EMPIRE, JEDI }

    interface Character {
      id: String!
      name: String
      friends: [Character]
      appearsIn: [Episode]
    }

    type Human implements Character {
      id: String!
      name: String
      friends: [Character]
      appearsIn: [Episode]
      homePlanet: String
    }

    type Droid implements Character {
      id: String!
      name: String
      friends: [Character]
      appearsIn: [Episode]
      primaryFunction: String
    }

    type Query {
      hero(episode: Episode): Character
      human(id: String!): Human
      droid(id: String!): Droid
    }
    """
)

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


schema.query_type.fields["hero"].resolve = get_hero
schema.get_type("Character").resolve_type = get_character_type

for name, value in schema.get_type("Episode").values.items():
    value.value = EpisodeEnum[name].value


result = graphql_sync(
    schema,
    """
    {
      hero(episode: EMPIRE) {
        name
        appearsIn
      }
    }
    """,
)
print(result)

# introspection query

# from graphql import get_introspection_query  # noqa 401

# query = get_introspection_query(descriptions=True)
# introspection_query_result = graphql_sync(schema, query)
# from dictknife import loading
# loading.dumpfile(introspection_query_result.data)
