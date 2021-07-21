from diagrams import Cluster, Diagram
from diagrams.aws.compute import ECS
from diagrams.aws.database import ElastiCache, RDS
from diagrams.aws.network import ELB
from diagrams.aws.network import Route53


def draw(name: str, *, show: bool = False) -> None:
    with Diagram(name):
        dns = Route53()
        lb = ELB()

        with Cluster() as Services:
            web1 = ECS()
            web2 = ECS()
            web3 = ECS()

        with Cluster() as DBCluster:
            userdb = RDS()
            userdb_ro = RDS()
            userdb - [userdb_ro]

        memcached = ElastiCache()

        dns >> lb >> Services
        Services >> DBCluster
        Services >> memcached
