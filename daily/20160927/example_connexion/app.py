import connexion


class MyResolver(connexion.Resolver):
    def resolve_operation_id(self, operation):
        spec = operation.operation
        return spec.get("x-python-operationId")


def main():
    app = connexion.App(__name__)
    app.add_api("my_api.yaml", resolver=MyResolver())
    app.run(port=4040)


if __name__ == "__main__":
    main()
