class X(Namespace):
    import people

    mount_module(people)
    import hero

    mount(hero.Hero, as_="HERO")


# why not use context manager
