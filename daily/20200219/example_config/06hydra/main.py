import hydra
from omegaconf import DictConfig

# pip install hydra-core
# see: https://hydra.cc/docs/tutorial/simple_cli


@hydra.main(config_path="config.yaml")
def my_app(cfg: DictConfig) -> None:
    print(cfg.pretty())


if __name__ == "__main__":
    my_app()
