try:
    from importlib import resources
except ImportError:
    import pkg_resources

    class resources:
        @staticmethod
        def resource_text(package, resource, encoding="utf-8"):
            return pkg_resources.resource_string(package, resource).decode(encoding)
