def _output(x):
    import json

    print(json.dumps(x, indent=2, ensure_ascii=False))


def people() -> list:
    import faker

    faker.Faker.seed(0)
    fake = faker.Faker("ja_JP")
    return [{"no": i, "name": fake.name(), "address": fake.address()} for i in range(5)]
