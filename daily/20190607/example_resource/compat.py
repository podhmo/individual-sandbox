try:
    from importlib.resources import read_text as resource_text
except ImportError:
    import pkg_resources

    def resource_text(package, resource, encoding="utf-8"):
        return pkg_resources.resource_string(package, resource).decode(encoding)
