from miniconfig import ConfiguratorCore


class Configurator(ConfiguratorCore):
    def make_app(self):
        self.commit()
        return self.settings["action"]

    def hmm(self):
        self.include("./csv")
